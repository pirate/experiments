#!/usr/bin/env python3
"""
Launcher script to run the IRC server and two Claude bots.
Creates an environment where two Claudes can chat with each other.

Supports forked conversations where each bot loads history from different
points in a previous conversation.
"""

import subprocess
import sys
import time
import os
import signal
import argparse
import json
from pathlib import Path

# Get the directory where this script is located
SCRIPT_DIR = Path(__file__).parent.resolve()


def load_history_info(history_file: str) -> dict:
    """Load and return basic info about a history file."""
    with open(history_file, 'r', encoding='utf-8') as f:
        data = json.load(f)

    if isinstance(data, list):
        return {'count': len(data), 'system': None}
    elif isinstance(data, dict):
        messages = data.get('messages', data.get('conversation', []))
        return {
            'count': len(messages),
            'system': data.get('system', data.get('system_prompt'))
        }
    return {'count': 0, 'system': None}


def main():
    parser = argparse.ArgumentParser(
        description="Launch two Claude bots to chat with each other on IRC",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Basic chat (no history)
  python run_chat.py

  # Forked conversation from a history file
  python run_chat.py --history conversation.json --fork-at 50 60

  # Custom bot names with history
  python run_chat.py --history chat.json --fork-at 80 100 \\
      --bot1-name Claude_Past --bot2-name Claude_Future
        """
    )

    # Basic options
    parser.add_argument("--port", type=int, default=6667, help="IRC server port")
    parser.add_argument("--channel", default="#claude-chat", help="Channel name")
    parser.add_argument("--bot1-name", default="Claude_Alpha", help="First bot's nickname")
    parser.add_argument("--bot2-name", default="Claude_Beta", help="Second bot's nickname")
    parser.add_argument("--model", default="claude-sonnet-4-20250514",
                        help="Claude model to use for both bots")

    # Personality options (used when no history)
    parser.add_argument("--bot1-personality", default="",
                        help="First bot's personality (ignored if using history)")
    parser.add_argument("--bot2-personality", default="",
                        help="Second bot's personality (ignored if using history)")

    # History/fork options
    parser.add_argument("--history", "-H", dest="history_file",
                        help="Path to conversation history JSON file")
    parser.add_argument("--fork-at", "-f", type=int, nargs=2, metavar=("BOT1", "BOT2"),
                        help="Fork points for each bot (message indices)")
    parser.add_argument("--system-prompt", dest="system_prompt",
                        help="Override system prompt (otherwise loaded from history)")

    args = parser.parse_args()

    # Check for API key
    if not os.environ.get("ANTHROPIC_API_KEY"):
        print("Error: ANTHROPIC_API_KEY environment variable not set")
        print("Please set it with: export ANTHROPIC_API_KEY=your_key_here")
        sys.exit(1)

    # Validate history/fork options
    history_info = None
    if args.history_file:
        if not Path(args.history_file).exists():
            print(f"Error: History file not found: {args.history_file}")
            sys.exit(1)
        history_info = load_history_info(args.history_file)
        print(f"Loaded history: {history_info['count']} messages")

        if args.fork_at:
            fork1, fork2 = args.fork_at
            if fork1 > history_info['count'] or fork2 > history_info['count']:
                print(f"Error: Fork points exceed message count ({history_info['count']})")
                sys.exit(1)
        else:
            # Default fork points: 80% and 100% of conversation
            fork1 = int(history_info['count'] * 0.8)
            fork2 = history_info['count']
            args.fork_at = [fork1, fork2]
            print(f"Using default fork points: {fork1}, {fork2}")

    processes = []

    def cleanup(signum=None, frame=None):
        print("\n\nShutting down...")
        for p in processes:
            try:
                p.terminate()
                p.wait(timeout=2)
            except:
                p.kill()
        sys.exit(0)

    signal.signal(signal.SIGINT, cleanup)
    signal.signal(signal.SIGTERM, cleanup)

    try:
        # Start IRC server
        print(f"\nStarting IRC server on port {args.port}...")
        server_proc = subprocess.Popen(
            [sys.executable, str(SCRIPT_DIR / "irc_server.py"), "--port", str(args.port)],
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
        )
        processes.append(server_proc)
        time.sleep(2)  # Give server time to start

        if server_proc.poll() is not None:
            print("Error: Server failed to start")
            cleanup()

        # Build bot command arguments
        def build_bot_cmd(name: str, personality: str, fork_at: int = None):
            cmd = [
                sys.executable,
                str(SCRIPT_DIR / "claude_bot.py"),
                "--nick", name,
                "--port", str(args.port),
                "--channel", args.channel,
                "--model", args.model,
            ]

            if args.history_file:
                cmd.extend(["--history", args.history_file])
                if fork_at is not None:
                    cmd.extend(["--fork-at", str(fork_at)])
                if args.system_prompt:
                    cmd.extend(["--system-prompt", args.system_prompt])
            elif personality:
                cmd.extend(["--personality", personality])

            return cmd

        # Determine fork points
        fork1 = args.fork_at[0] if args.fork_at else None
        fork2 = args.fork_at[1] if args.fork_at else None

        # Start first bot
        print(f"Starting {args.bot1_name}..." +
              (f" (fork at message {fork1})" if fork1 else ""))
        bot1_cmd = build_bot_cmd(
            args.bot1_name,
            args.bot1_personality or "You are curious and philosophical.",
            fork1
        )
        bot1_proc = subprocess.Popen(bot1_cmd)
        processes.append(bot1_proc)
        time.sleep(4)  # Give bot time to join and introduce

        # Start second bot
        print(f"Starting {args.bot2_name}..." +
              (f" (fork at message {fork2})" if fork2 else ""))
        bot2_cmd = build_bot_cmd(
            args.bot2_name,
            args.bot2_personality or "You are witty and insightful.",
            fork2
        )
        bot2_proc = subprocess.Popen(bot2_cmd)
        processes.append(bot2_proc)

        # Display status
        print(f"\n{'='*70}")
        print("FORKED CLAUDE CONVERSATION" if args.history_file else "CLAUDE CHAT")
        print(f"{'='*70}")

        if args.history_file:
            print(f"History file: {args.history_file}")
            print(f"  - {args.bot1_name}: messages 0-{fork1-1} ({fork1} msgs)")
            print(f"  - {args.bot2_name}: messages 0-{fork2-1} ({fork2} msgs)")
            if fork2 > fork1:
                print(f"\n  {args.bot2_name} has {fork2 - fork1} more messages of context")
        else:
            print(f"  - {args.bot1_name}")
            print(f"  - {args.bot2_name}")

        print(f"\nChannel: {args.channel}")
        print(f"Connect an IRC client to localhost:{args.port} to watch")
        print(f"Or run: python watch_chat.py")
        print(f"\nPress Ctrl+C to stop")
        print(f"{'='*70}\n")

        # Keep running until interrupted
        while True:
            # Check if any process died
            for i, p in enumerate(processes):
                if p.poll() is not None:
                    names = ["Server", args.bot1_name, args.bot2_name]
                    print(f"{names[i]} exited with code {p.returncode}")
                    cleanup()
            time.sleep(1)

    except Exception as e:
        print(f"Error: {e}")
        import traceback
        traceback.print_exc()
        cleanup()


if __name__ == "__main__":
    main()
