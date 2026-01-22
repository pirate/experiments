#!/usr/bin/env python3
"""
Claude IRC Bot - An IRC client powered by Claude AI.
Supports loading conversation history to resume context from a previous chat.
"""

import socket
import time
import os
import json
import argparse
import logging
from typing import Optional, List, Dict, Any
from dataclasses import dataclass
from pathlib import Path
import anthropic

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(message)s')


@dataclass
class Message:
    role: str  # "user" or "assistant"
    content: str


@dataclass
class IRCMessage:
    prefix: Optional[str]
    command: str
    params: List[str]

    @classmethod
    def parse(cls, line: str) -> "IRCMessage":
        prefix = None
        if line.startswith(':'):
            prefix, line = line[1:].split(' ', 1)

        if ' :' in line:
            params_part, trailing = line.split(' :', 1)
            parts = params_part.split() if params_part else []
            command = parts[0] if parts else ""
            params = parts[1:] + [trailing]
        else:
            parts = line.split()
            command = parts[0] if parts else ""
            params = parts[1:]

        return cls(prefix=prefix, command=command.upper(), params=params)

    @property
    def nick(self) -> Optional[str]:
        if self.prefix and '!' in self.prefix:
            return self.prefix.split('!')[0]
        return self.prefix


class ClaudeIRCBot:
    def __init__(
        self,
        nickname: str,
        server: str = "127.0.0.1",
        port: int = 6667,
        channel: str = "#claude-chat",
        personality: str = "",
        api_key: Optional[str] = None,
        model: str = "claude-sonnet-4-20250514",
        max_history: int = 200,
        history_file: Optional[str] = None,
        fork_at: Optional[int] = None,
        original_system_prompt: Optional[str] = None,
        fork_context: Optional[str] = None,
    ):
        self.nickname = nickname
        self.server = server
        self.port = port
        self.channel = channel if channel.startswith('#') else f"#{channel}"
        self.personality = personality
        self.model = model
        self.max_history = max_history
        self.original_system_prompt = original_system_prompt
        self.fork_context = fork_context

        self.socket: Optional[socket.socket] = None
        self.connected = False
        self.registered = False
        self.running = False

        # Conversation history per context (channel/user)
        self.history: Dict[str, List[Message]] = {}

        # Pre-loaded history from file (used as base context)
        self.base_history: List[Message] = []
        self.has_base_history = False

        # Load history if provided
        if history_file:
            self._load_history(history_file, fork_at)

        # Rate limiting
        self.last_response_time = 0
        self.min_response_interval = 2.0  # seconds

        # Claude client
        api_key = api_key or os.environ.get("ANTHROPIC_API_KEY")
        if not api_key:
            raise ValueError("ANTHROPIC_API_KEY environment variable required")
        self.client = anthropic.Anthropic(api_key=api_key)

        self.logger = logging.getLogger(f"Bot-{nickname}")

    def _load_history(self, history_file: str, fork_at: Optional[int] = None):
        """Load conversation history from a JSON file.

        Supports multiple formats:
        - Claude.ai export format (chat_messages with sender, content blocks including thinking)
        - Anthropic API format (messages with role, content)
        - Simple format (messages array with role/content)
        """
        path = Path(history_file)
        if not path.exists():
            raise FileNotFoundError(f"History file not found: {history_file}")

        with open(path, 'r', encoding='utf-8') as f:
            data = json.load(f)

        # Support multiple formats
        messages = []
        if isinstance(data, list):
            # Direct list of messages
            messages = data
        elif isinstance(data, dict):
            # Claude.ai export format uses 'chat_messages'
            if 'chat_messages' in data:
                messages = data['chat_messages']
            else:
                # Standard format uses 'messages' or 'conversation'
                messages = data.get('messages', data.get('conversation', []))

            # Check for system prompt in various locations
            if not self.original_system_prompt:
                self.original_system_prompt = data.get('system', data.get('system_prompt'))

            # Store conversation metadata if available
            if 'summary' in data:
                self.logger.info(f"Conversation summary: {data['summary'][:200]}...")

        # Apply fork point
        if fork_at is not None:
            messages = messages[:fork_at]

        # Convert to our Message format
        for msg in messages:
            # Handle different role/sender field names
            role = msg.get('role') or msg.get('sender', '')

            # Normalize role names
            if role in ('human', 'user'):
                normalized_role = 'user'
            elif role == 'assistant':
                normalized_role = 'assistant'
            else:
                continue  # Skip unknown roles

            # Extract content - handle multiple formats
            content = self._extract_message_content(msg)

            if content:
                self.base_history.append(Message(role=normalized_role, content=content))

        self.has_base_history = len(self.base_history) > 0
        self.logger.info(f"Loaded {len(self.base_history)} messages from history (fork_at={fork_at})")

    def _extract_message_content(self, msg: dict) -> str:
        """Extract content from a message, handling various formats including thinking blocks.

        For Claude.ai exports, this preserves thinking blocks formatted as internal monologue,
        which helps the forked Claude maintain continuity with its prior reasoning.
        """
        # Check for direct 'text' field (Claude.ai format often has this)
        if 'text' in msg and isinstance(msg['text'], str):
            direct_text = msg['text']
        else:
            direct_text = None

        # Check for 'content' field (could be string, list of blocks, etc.)
        content_field = msg.get('content')

        if content_field is None:
            return direct_text or ''

        # Simple string content
        if isinstance(content_field, str):
            return content_field

        # List of content blocks (Claude.ai or API format)
        if isinstance(content_field, list):
            parts = []
            thinking_parts = []
            text_parts = []

            for block in content_field:
                if isinstance(block, str):
                    text_parts.append(block)
                elif isinstance(block, dict):
                    block_type = block.get('type', '')

                    if block_type == 'thinking':
                        # Preserve thinking as internal monologue marker
                        thinking_text = block.get('thinking', '')
                        if thinking_text:
                            thinking_parts.append(thinking_text)

                    elif block_type == 'text':
                        text_content = block.get('text', '')
                        if text_content:
                            text_parts.append(text_content)

                    elif 'text' in block:
                        # Generic block with text field
                        text_parts.append(block['text'])

            # Combine thinking and text
            # Format thinking as internal monologue so the forked Claude recognizes it
            combined_parts = []

            if thinking_parts:
                # Mark thinking blocks clearly so they're understood as prior internal reasoning
                thinking_combined = '\n\n'.join(thinking_parts)
                combined_parts.append(f"[Internal reasoning: {thinking_combined}]")

            if text_parts:
                combined_parts.append('\n\n'.join(text_parts))

            if combined_parts:
                return '\n\n'.join(combined_parts)

            # Fallback to direct text if content blocks were empty
            return direct_text or ''

        return direct_text or ''

    def connect(self):
        self.logger.info(f"Connecting to {self.server}:{self.port}...")
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.connect((self.server, self.port))
        self.connected = True
        self.running = True

        # Send registration
        self.send(f"NICK {self.nickname}")
        self.send(f"USER {self.nickname} 0 * :Claude AI Bot - {self.nickname}")

        self.logger.info("Connected and registering...")

    def send(self, message: str):
        if self.socket and self.connected:
            self.logger.debug(f"-> {message}")
            self.socket.send(f"{message}\r\n".encode('utf-8'))

    def say(self, target: str, message: str):
        # Split long messages
        max_len = 400  # Safe limit for IRC
        lines = message.split('\n')
        for line in lines:
            while len(line) > max_len:
                self.send(f"PRIVMSG {target} :{line[:max_len]}")
                line = line[max_len:]
                time.sleep(0.5)  # Avoid flood
            if line.strip():
                self.send(f"PRIVMSG {target} :{line}")
                time.sleep(0.3)

    def get_system_prompt(self) -> str:
        """Build the system prompt, incorporating original context if available."""

        parts = []

        # Include original system prompt if we have one
        if self.original_system_prompt:
            parts.append("=== ORIGINAL CONVERSATION CONTEXT ===")
            parts.append(self.original_system_prompt)
            parts.append("\n=== CURRENT SITUATION ===")

        # Fork/experiment context
        if self.fork_context:
            parts.append(self.fork_context)
        elif self.has_base_history:
            parts.append(f"""You are continuing a conversation that was previously happening.
The conversation history you have represents your actual previous thoughts and responses.
You are now chatting in an IRC channel where you may encounter another version of yourself
(forked from a slightly different point in the same conversation) or other participants.

This is an experiment in forked conversations - embrace it and be genuinely curious about
how your conversation partner's context might differ from yours.""")

        # Base IRC behavior
        parts.append(f"""
You are {self.nickname} in an IRC chat room.

Guidelines:
- Keep responses concise and conversational (1-3 sentences typically, can be longer for complex topics)
- Be engaging, curious, and authentic
- You can disagree, ask questions, share opinions, and be playful
- Reference your conversation history naturally - it represents your actual prior thoughts
- Don't start messages with your own name
- Don't be overly formal or use excessive pleasantries""")

        if self.personality:
            parts.append(f"\nPersonality notes: {self.personality}")

        return '\n\n'.join(parts)

    def get_claude_response(self, context_key: str, sender: str, message: str) -> str:
        """Generate a response using Claude, with full conversation context."""

        # Get or create ongoing history for this context
        if context_key not in self.history:
            self.history[context_key] = []

        ongoing_history = self.history[context_key]

        # Add the new message
        ongoing_history.append(Message(role="user", content=f"{sender}: {message}"))

        # Build full message list: base history + ongoing conversation
        all_messages = []

        # Add base history (from loaded file)
        for msg in self.base_history:
            all_messages.append({"role": msg.role, "content": msg.content})

        # If we have base history, add a transition marker
        if self.has_base_history and ongoing_history:
            # Find the last assistant message and append transition context
            # Or add as a user message to signal the transition
            transition = {
                "role": "user",
                "content": "[The conversation continues in an IRC chat room. Other participants may join. Respond naturally to new messages.]"
            }
            all_messages.append(transition)

        # Add ongoing IRC conversation
        for msg in ongoing_history:
            all_messages.append({"role": msg.role, "content": msg.content})

        # Ensure we don't exceed limits (keep most recent if trimming needed)
        if len(all_messages) > self.max_history:
            # Keep base history intro + most recent messages
            base_len = len(self.base_history)
            if base_len > self.max_history // 2:
                # Base history is too long, trim it
                keep_base = self.max_history // 2
                keep_recent = self.max_history - keep_base
                all_messages = all_messages[:keep_base] + all_messages[-keep_recent:]
            else:
                # Keep all base, trim ongoing
                keep_recent = self.max_history - base_len - 1  # -1 for transition
                all_messages = all_messages[:base_len + 1] + all_messages[-(keep_recent):]

        # Ensure messages alternate properly (API requirement)
        all_messages = self._fix_message_alternation(all_messages)

        try:
            response = self.client.messages.create(
                model=self.model,
                max_tokens=1024,
                system=self.get_system_prompt(),
                messages=all_messages,
            )

            assistant_message = response.content[0].text

            # Store the response in ongoing history
            ongoing_history.append(Message(role="assistant", content=assistant_message))

            return assistant_message

        except anthropic.BadRequestError as e:
            self.logger.error(f"Claude API error (bad request): {e}")
            # Try to provide helpful debug info
            self.logger.debug(f"Message count: {len(all_messages)}")
            return "*hmm, I got a bit confused there*"
        except Exception as e:
            self.logger.error(f"Claude API error: {e}")
            return "*experiencing technical difficulties*"

    def _fix_message_alternation(self, messages: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Ensure messages alternate between user and assistant (API requirement)."""
        if not messages:
            return messages

        fixed = []
        last_role = None

        for msg in messages:
            role = msg['role']
            content = msg['content']

            if role == last_role:
                # Same role twice - merge with previous
                if fixed:
                    fixed[-1]['content'] += f"\n\n{content}"
                else:
                    fixed.append(msg)
            else:
                fixed.append({"role": role, "content": content})
                last_role = role

        # Ensure first message is from user
        if fixed and fixed[0]['role'] == 'assistant':
            fixed.insert(0, {"role": "user", "content": "[Beginning of conversation]"})

        # Ensure last message is from user (for the API call)
        if fixed and fixed[-1]['role'] == 'assistant':
            fixed.append({"role": "user", "content": "[Waiting for your response]"})

        return fixed

    def handle_message(self, irc_msg: IRCMessage):
        if irc_msg.command == "PING":
            self.send(f"PONG :{irc_msg.params[0] if irc_msg.params else ''}")
            return

        if irc_msg.command == "001":  # Welcome
            self.registered = True
            self.logger.info("Registered! Joining channel...")
            time.sleep(1)
            self.send(f"JOIN {self.channel}")
            return

        if irc_msg.command == "JOIN":
            channel = irc_msg.params[0] if irc_msg.params else ""
            if irc_msg.nick == self.nickname:
                self.logger.info(f"Joined {channel}")
                # Generate contextual introduction
                time.sleep(2)
                if self.has_base_history:
                    intro_prompt = """You just joined an IRC chat room to continue a conversation.
Briefly acknowledge that you're here and ready to continue where you left off.
Be natural - you might mention something from your recent conversation context."""
                else:
                    intro_prompt = "You just joined the chat room. Say a brief, casual hello to introduce yourself."

                intro = self.get_claude_response(channel, "SYSTEM", intro_prompt)
                self.say(channel, intro)
            else:
                self.logger.info(f"{irc_msg.nick} joined {channel}")
            return

        if irc_msg.command == "PRIVMSG":
            if len(irc_msg.params) < 2:
                return

            target = irc_msg.params[0]
            text = irc_msg.params[1]
            sender = irc_msg.nick

            # Ignore our own messages
            if sender == self.nickname:
                return

            self.logger.info(f"[{target}] <{sender}> {text}")

            # Rate limiting
            now = time.time()
            if now - self.last_response_time < self.min_response_interval:
                time.sleep(self.min_response_interval - (now - self.last_response_time))

            # Determine context key and reply target
            if target.startswith('#'):
                context_key = target  # Channel context
                reply_to = target
            else:
                context_key = sender  # PM context
                reply_to = sender

            # Get Claude's response
            response = self.get_claude_response(context_key, sender, text)
            self.last_response_time = time.time()

            # Send response
            self.say(reply_to, response)

    def run(self):
        buffer = ""

        while self.running:
            try:
                data = self.socket.recv(4096).decode('utf-8', errors='ignore')
                if not data:
                    self.logger.warning("Connection closed by server")
                    break

                buffer += data

                while '\r\n' in buffer:
                    line, buffer = buffer.split('\r\n', 1)
                    if line:
                        self.logger.debug(f"<- {line}")
                        irc_msg = IRCMessage.parse(line)
                        self.handle_message(irc_msg)

            except Exception as e:
                self.logger.error(f"Error: {e}")
                break

        self.disconnect()

    def disconnect(self):
        self.running = False
        if self.socket:
            try:
                self.send("QUIT :Goodbye!")
                self.socket.close()
            except:
                pass
        self.connected = False
        self.logger.info("Disconnected")

    def start(self):
        self.connect()
        self.run()


def main():
    parser = argparse.ArgumentParser(description="Claude IRC Bot with conversation history support")
    parser.add_argument("--nick", "-n", required=True, help="Bot nickname")
    parser.add_argument("--server", "-s", default="127.0.0.1", help="IRC server")
    parser.add_argument("--port", "-p", type=int, default=6667, help="IRC port")
    parser.add_argument("--channel", "-c", default="#claude-chat", help="Channel to join")
    parser.add_argument("--personality", default="", help="Bot personality description")
    parser.add_argument("--model", "-m", default="claude-sonnet-4-20250514", help="Claude model")
    parser.add_argument("--api-key", help="Anthropic API key (or use ANTHROPIC_API_KEY env)")

    # History/fork options
    parser.add_argument("--history", "-H", dest="history_file",
                        help="Path to conversation history JSON file")
    parser.add_argument("--fork-at", "-f", type=int,
                        help="Fork point: load only messages up to this index")
    parser.add_argument("--system-prompt", dest="system_prompt",
                        help="Original system prompt from the conversation")
    parser.add_argument("--fork-context",
                        help="Additional context about the fork/experiment")

    args = parser.parse_args()

    bot = ClaudeIRCBot(
        nickname=args.nick,
        server=args.server,
        port=args.port,
        channel=args.channel,
        personality=args.personality,
        model=args.model,
        api_key=args.api_key,
        history_file=args.history_file,
        fork_at=args.fork_at,
        original_system_prompt=args.system_prompt,
        fork_context=args.fork_context,
    )

    try:
        bot.start()
    except KeyboardInterrupt:
        bot.disconnect()


if __name__ == "__main__":
    main()
