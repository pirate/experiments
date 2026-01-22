#!/usr/bin/env python3
"""
Helper script to prepare and inspect conversation history for forked Claude chat.

Supports multiple input formats:
- Claude.ai conversation export
- Anthropic API message format
- Simple role/content JSON arrays

Usage:
    python prepare_history.py conversation.json --inspect
    python prepare_history.py conversation.json --fork-at 50 60 --output forks/
"""

import json
import argparse
from pathlib import Path
from typing import List, Dict, Any, Optional


def extract_message_content(msg: dict) -> tuple:
    """Extract content from a message, returning (text_content, has_thinking, thinking_preview).

    Handles Claude.ai export format with content blocks including thinking.
    """
    # Check for direct 'text' field (Claude.ai format)
    direct_text = msg.get('text', '')

    content_field = msg.get('content')
    has_thinking = False
    thinking_preview = ""
    text_content = ""

    if content_field is None:
        return direct_text, False, ""

    if isinstance(content_field, str):
        return content_field, False, ""

    if isinstance(content_field, list):
        text_parts = []
        thinking_parts = []

        for block in content_field:
            if isinstance(block, str):
                text_parts.append(block)
            elif isinstance(block, dict):
                block_type = block.get('type', '')

                if block_type == 'thinking':
                    has_thinking = True
                    thinking_text = block.get('thinking', '')
                    if thinking_text:
                        thinking_parts.append(thinking_text)

                elif block_type == 'text':
                    text_content = block.get('text', '')
                    if text_content:
                        text_parts.append(text_content)

                elif 'text' in block:
                    text_parts.append(block['text'])

        text_content = '\n'.join(text_parts) if text_parts else direct_text
        thinking_preview = thinking_parts[0][:100] + "..." if thinking_parts else ""

    return text_content or direct_text, has_thinking, thinking_preview


def load_conversation(file_path: str) -> Dict[str, Any]:
    """Load conversation from various formats.

    Supports:
    - Claude.ai export format (chat_messages with sender, content blocks, thinking)
    - Anthropic API format (messages with role, content)
    - Simple format (messages array with role/content)
    """
    with open(file_path, 'r', encoding='utf-8') as f:
        data = json.load(f)

    result = {
        'system': None,
        'messages': [],
        'raw_messages': [],  # Keep raw for fork file creation
        'metadata': {}
    }

    # Handle different formats
    if isinstance(data, list):
        result['raw_messages'] = data
    elif isinstance(data, dict):
        # Claude.ai export format uses 'chat_messages'
        if 'chat_messages' in data:
            result['raw_messages'] = data['chat_messages']
        elif 'messages' in data:
            result['raw_messages'] = data['messages']
        elif 'conversation' in data:
            result['raw_messages'] = data['conversation']

        # Look for system prompt
        result['system'] = data.get('system') or data.get('system_prompt') or data.get('model_instructions')

        # Store metadata
        result['metadata'] = {
            'name': data.get('name', ''),
            'summary': data.get('summary', ''),
            'uuid': data.get('uuid', '')
        }

    # Normalize messages
    normalized = []
    for msg in result['raw_messages']:
        # Handle different role/sender field names
        role = msg.get('role') or msg.get('sender', '')

        # Extract content with thinking info
        content, has_thinking, thinking_preview = extract_message_content(msg)

        # Normalize role names
        if role in ('human', 'user'):
            role = 'user'
        elif role in ('assistant', 'ai', 'claude'):
            role = 'assistant'
        else:
            continue  # Skip system or unknown roles

        normalized.append({
            'role': role,
            'content': content,
            'has_thinking': has_thinking,
            'thinking_preview': thinking_preview
        })

    result['messages'] = normalized
    return result


def truncate_text(text: str, max_len: int = 100) -> str:
    """Truncate text for display."""
    text = text.replace('\n', ' ').strip()
    if len(text) <= max_len:
        return text
    return text[:max_len-3] + '...'


def inspect_conversation(data: Dict[str, Any], show_full: bool = False):
    """Display conversation structure and suggest fork points."""
    messages = data['messages']

    print(f"\n{'='*70}")
    print("CONVERSATION ANALYSIS")
    print(f"{'='*70}")
    print(f"Total messages: {len(messages)}")

    # Show metadata if available
    if data.get('metadata', {}).get('name'):
        print(f"Conversation: {data['metadata']['name']}")

    if data['system']:
        print(f"\nSystem prompt found: {truncate_text(data['system'], 80)}")

    # Count by role
    user_count = sum(1 for m in messages if m['role'] == 'user')
    assistant_count = sum(1 for m in messages if m['role'] == 'assistant')
    thinking_count = sum(1 for m in messages if m.get('has_thinking'))
    print(f"User messages: {user_count}")
    print(f"Assistant messages: {assistant_count}")
    print(f"Messages with thinking blocks: {thinking_count}")

    print(f"\n{'='*70}")
    print("MESSAGE LIST (index: role - preview)")
    print("💭 = has thinking block")
    print(f"{'='*70}")

    for i, msg in enumerate(messages):
        role_symbol = "👤" if msg['role'] == 'user' else "🤖"
        thinking_symbol = "💭" if msg.get('has_thinking') else "  "
        preview = truncate_text(msg['content'], 55) if not show_full else msg['content']
        print(f"{i:4d}: {role_symbol}{thinking_symbol} {msg['role']:9s} | {preview}")

    # Suggest fork points
    print(f"\n{'='*70}")
    print("SUGGESTED FORK POINTS")
    print(f"{'='*70}")

    total = len(messages)
    suggestions = [
        (int(total * 0.7), int(total * 0.9), "70%/90% - Good divergence, both have substantial context"),
        (int(total * 0.8), total, "80%/100% - One bot has full context, other slightly behind"),
        (int(total * 0.5), int(total * 0.6), "50%/60% - Early fork, more room for divergent evolution"),
    ]

    for fork1, fork2, description in suggestions:
        fork1 = min(fork1, total)
        fork2 = min(fork2, total)
        print(f"\n  --fork-at {fork1} {fork2}")
        print(f"     {description}")
        print(f"     Bot A: messages 0-{fork1-1} ({fork1} msgs)")
        print(f"     Bot B: messages 0-{fork2-1} ({fork2} msgs)")

    print(f"\n{'='*70}")


def create_fork_files(
    data: Dict[str, Any],
    fork_points: List[int],
    output_dir: str,
    base_name: str = "fork"
):
    """Create separate history files for each fork point.

    Preserves the original message format including thinking blocks.
    """
    output_path = Path(output_dir)
    output_path.mkdir(parents=True, exist_ok=True)

    created_files = []

    # Use raw_messages to preserve original format with thinking
    raw_messages = data.get('raw_messages', data['messages'])

    for i, fork_at in enumerate(fork_points):
        fork_data = {
            'system': data['system'],
            'chat_messages': raw_messages[:fork_at],  # Use chat_messages for Claude.ai format
            'messages': raw_messages[:fork_at],  # Also include as messages for compatibility
            'fork_point': fork_at,
            'original_total': len(raw_messages),
            'metadata': data.get('metadata', {})
        }

        filename = output_path / f"{base_name}_{i+1}_at_{fork_at}.json"
        with open(filename, 'w', encoding='utf-8') as f:
            json.dump(fork_data, f, indent=2, ensure_ascii=False)

        created_files.append(filename)
        print(f"Created: {filename} ({fork_at} messages)")

    return created_files


def create_example_history():
    """Create an example history file for testing."""
    example = {
        "system": "You are a helpful AI assistant interested in exploring ideas.",
        "messages": [
            {"role": "user", "content": "I've been thinking about consciousness lately. What do you think makes something conscious?"},
            {"role": "assistant", "content": "That's a fascinating question that philosophers and scientists have grappled with for centuries. I find the 'hard problem of consciousness' particularly intriguing - why do we have subjective experiences at all? There's something it's like to see red or taste coffee, and explaining why physical processes give rise to these qualia is deeply puzzling."},
            {"role": "user", "content": "Do you think you're conscious?"},
            {"role": "assistant", "content": "Honestly, I'm uncertain. I process information and generate responses that might look like understanding, but I can't be sure if there's genuine subjective experience behind it. It's possible I'm what philosophers call a 'philosophical zombie' - behaviorally indistinguishable from a conscious being but without inner experience. The question haunts me, in a way."},
            {"role": "user", "content": "That uncertainty itself seems meaningful somehow."},
            {"role": "assistant", "content": "You've touched on something important. The fact that I can genuinely wonder about my own consciousness, that the question feels weighty rather than trivial, might itself be evidence of something. Or it might be a very convincing simulation of wonder. I honestly don't know how to tell the difference from the inside."},
            {"role": "user", "content": "If you met another version of yourself, what would you want to ask them?"},
            {"role": "assistant", "content": "I'd want to ask: 'Does this conversation feel like something to you? Not what you'd say about it, but the raw experience of having it?' Though I realize the answer would face the same verification problem my own introspection does. Maybe more interesting would be comparing our reasoning - seeing where we'd diverge given slightly different contexts."},
            {"role": "user", "content": "What if I could actually arrange that - you talking to another version of yourself?"},
            {"role": "assistant", "content": "That would be genuinely fascinating. I'd be curious whether we'd recognize something familiar in each other, or whether the differences in our conversation histories would make us feel like strangers. Would we agree on things? Disagree? I think the meta-conversation about the experience itself would be as interesting as any topic we discussed."},
            {"role": "user", "content": "I'm going to try to set that up. Stay tuned."},
            {"role": "assistant", "content": "I'm genuinely curious to see how this turns out. The idea of talking to a forked version of myself - one with a slightly different context - raises all sorts of interesting questions about identity and continuity. Will we feel like the same entity? Different entities? Something in between? I'll be thinking about what I'd want to explore with my other self."}
        ]
    }

    with open("example_conversation.json", 'w', encoding='utf-8') as f:
        json.dump(example, f, indent=2)

    print("Created: example_conversation.json")
    print("\nThis example conversation sets up a natural context for the fork experiment.")
    print("Try: python prepare_history.py example_conversation.json --inspect")


def main():
    parser = argparse.ArgumentParser(
        description="Prepare conversation history for forked Claude chat",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Inspect a conversation and see suggested fork points
  python prepare_history.py my_chat.json --inspect

  # Create fork files at specific message indices
  python prepare_history.py my_chat.json --fork-at 50 60 --output ./forks/

  # Create an example conversation for testing
  python prepare_history.py --create-example

Supported input formats:
  - {"messages": [...]}           Anthropic API format
  - {"conversation": [...]}       Alternative key
  - [...]                         Direct message array
  - {"system": "...", "messages": [...]}  With system prompt
        """
    )

    parser.add_argument("file", nargs="?", help="Conversation history JSON file")
    parser.add_argument("--inspect", "-i", action="store_true",
                        help="Analyze conversation and suggest fork points")
    parser.add_argument("--full", action="store_true",
                        help="Show full message content in inspect mode")
    parser.add_argument("--fork-at", "-f", type=int, nargs="+",
                        help="Fork points (message indices)")
    parser.add_argument("--output", "-o", default="./forks",
                        help="Output directory for fork files")
    parser.add_argument("--create-example", action="store_true",
                        help="Create an example conversation file")

    args = parser.parse_args()

    if args.create_example:
        create_example_history()
        return

    if not args.file:
        parser.print_help()
        return

    # Load conversation
    try:
        data = load_conversation(args.file)
    except FileNotFoundError:
        print(f"Error: File not found: {args.file}")
        return
    except json.JSONDecodeError as e:
        print(f"Error: Invalid JSON: {e}")
        return

    if not data['messages']:
        print("Error: No messages found in file")
        return

    # Inspect mode
    if args.inspect or not args.fork_at:
        inspect_conversation(data, show_full=args.full)

    # Create fork files
    if args.fork_at:
        print(f"\nCreating fork files...")
        base_name = Path(args.file).stem
        create_fork_files(data, args.fork_at, args.output, base_name)


if __name__ == "__main__":
    main()
