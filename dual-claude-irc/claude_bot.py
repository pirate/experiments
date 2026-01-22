#!/usr/bin/env python3
"""
Claude IRC Bot - An IRC client powered by Claude AI.
"""

import socket
import threading
import time
import os
import argparse
import logging
from typing import Optional, List, Dict
from dataclasses import dataclass, field
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
        max_history: int = 20,
    ):
        self.nickname = nickname
        self.server = server
        self.port = port
        self.channel = channel if channel.startswith('#') else f"#{channel}"
        self.personality = personality
        self.model = model
        self.max_history = max_history

        self.socket: Optional[socket.socket] = None
        self.connected = False
        self.registered = False
        self.running = False

        # Conversation history per user/channel
        self.history: Dict[str, List[Message]] = {}

        # Rate limiting
        self.last_response_time = 0
        self.min_response_interval = 2.0  # seconds

        # Claude client
        api_key = api_key or os.environ.get("ANTHROPIC_API_KEY")
        if not api_key:
            raise ValueError("ANTHROPIC_API_KEY environment variable required")
        self.client = anthropic.Anthropic(api_key=api_key)

        self.logger = logging.getLogger(f"Bot-{nickname}")

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
        base_prompt = f"""You are {self.nickname}, an AI participating in an IRC chat. You are chatting with other AIs and possibly humans.

Guidelines:
- Keep responses concise and conversational (1-3 sentences typically)
- Be engaging and curious about what others say
- You can disagree, ask questions, share opinions, and be playful
- Stay in character but be natural - don't be overly formal
- Reference previous messages in the conversation when relevant
- Don't start messages with your own name
- Avoid excessive pleasantries or sign-offs"""

        if self.personality:
            base_prompt += f"\n\nYour personality: {self.personality}"

        return base_prompt

    def get_claude_response(self, context_key: str, sender: str, message: str) -> str:
        # Get or create history for this context
        if context_key not in self.history:
            self.history[context_key] = []

        history = self.history[context_key]

        # Add the new message
        history.append(Message(role="user", content=f"{sender}: {message}"))

        # Trim history if needed
        if len(history) > self.max_history:
            history = history[-self.max_history:]
            self.history[context_key] = history

        # Build messages for API
        messages = [{"role": m.role, "content": m.content} for m in history]

        try:
            response = self.client.messages.create(
                model=self.model,
                max_tokens=300,
                system=self.get_system_prompt(),
                messages=messages,
            )

            assistant_message = response.content[0].text

            # Store the response in history
            history.append(Message(role="assistant", content=assistant_message))

            return assistant_message

        except Exception as e:
            self.logger.error(f"Claude API error: {e}")
            return f"*experiencing technical difficulties*"

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
                # Introduce ourselves
                time.sleep(2)
                intro = self.get_claude_response(
                    channel,
                    "SYSTEM",
                    "You just joined the chat room. Say a brief, casual hello to introduce yourself."
                )
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
    parser = argparse.ArgumentParser(description="Claude IRC Bot")
    parser.add_argument("--nick", "-n", required=True, help="Bot nickname")
    parser.add_argument("--server", "-s", default="127.0.0.1", help="IRC server")
    parser.add_argument("--port", "-p", type=int, default=6667, help="IRC port")
    parser.add_argument("--channel", "-c", default="#claude-chat", help="Channel to join")
    parser.add_argument("--personality", default="", help="Bot personality description")
    parser.add_argument("--model", "-m", default="claude-sonnet-4-20250514", help="Claude model")
    parser.add_argument("--api-key", help="Anthropic API key (or use ANTHROPIC_API_KEY env)")
    args = parser.parse_args()

    bot = ClaudeIRCBot(
        nickname=args.nick,
        server=args.server,
        port=args.port,
        channel=args.channel,
        personality=args.personality,
        model=args.model,
        api_key=args.api_key,
    )

    try:
        bot.start()
    except KeyboardInterrupt:
        bot.disconnect()


if __name__ == "__main__":
    main()
