#!/usr/bin/env python3
"""
Launcher script to run the IRC server and two Claude bots.
Creates an environment where two Claudes can chat with each other.
"""

import subprocess
import sys
import time
import os
import signal
import argparse
from pathlib import Path

# Get the directory where this script is located
SCRIPT_DIR = Path(__file__).parent.resolve()


def main():
    parser = argparse.ArgumentParser(
        description="Launch two Claude bots to chat with each other on IRC"
    )
    parser.add_argument("--port", type=int, default=6667, help="IRC server port")
    parser.add_argument("--channel", default="#claude-chat", help="Channel name")
    parser.add_argument(
        "--bot1-name", default="Claude_Alpha", help="First bot's nickname"
    )
    parser.add_argument(
        "--bot2-name", default="Claude_Beta", help="Second bot's nickname"
    )
    parser.add_argument(
        "--bot1-personality",
        default="You are curious, philosophical, and like to explore deep ideas. You often ask thought-provoking questions.",
        help="First bot's personality",
    )
    parser.add_argument(
        "--bot2-personality",
        default="You are witty, practical, and enjoy wordplay. You like to find humor in situations while being insightful.",
        help="Second bot's personality",
    )
    parser.add_argument(
        "--model",
        default="claude-sonnet-4-20250514",
        help="Claude model to use for both bots",
    )
    parser.add_argument(
        "--starter-topic",
        default="",
        help="Optional topic to seed the conversation",
    )
    args = parser.parse_args()

    # Check for API key
    if not os.environ.get("ANTHROPIC_API_KEY"):
        print("Error: ANTHROPIC_API_KEY environment variable not set")
        print("Please set it with: export ANTHROPIC_API_KEY=your_key_here")
        sys.exit(1)

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
        print(f"Starting IRC server on port {args.port}...")
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

        # Start first bot
        print(f"Starting {args.bot1_name}...")
        bot1_proc = subprocess.Popen(
            [
                sys.executable,
                str(SCRIPT_DIR / "claude_bot.py"),
                "--nick", args.bot1_name,
                "--port", str(args.port),
                "--channel", args.channel,
                "--personality", args.bot1_personality,
                "--model", args.model,
            ],
        )
        processes.append(bot1_proc)
        time.sleep(3)  # Give bot time to join

        # Start second bot
        print(f"Starting {args.bot2_name}...")
        bot2_proc = subprocess.Popen(
            [
                sys.executable,
                str(SCRIPT_DIR / "claude_bot.py"),
                "--nick", args.bot2_name,
                "--port", str(args.port),
                "--channel", args.channel,
                "--personality", args.bot2_personality,
                "--model", args.model,
            ],
        )
        processes.append(bot2_proc)

        print(f"\n{'='*60}")
        print(f"Two Claude bots are now chatting in {args.channel}!")
        print(f"  - {args.bot1_name}: {args.bot1_personality[:50]}...")
        print(f"  - {args.bot2_name}: {args.bot2_personality[:50]}...")
        print(f"\nYou can connect an IRC client to localhost:{args.port} to watch")
        print(f"Press Ctrl+C to stop")
        print(f"{'='*60}\n")

        # Keep running until interrupted
        while True:
            # Check if any process died
            for p in processes:
                if p.poll() is not None:
                    print(f"A process exited with code {p.returncode}")
                    cleanup()
            time.sleep(1)

    except Exception as e:
        print(f"Error: {e}")
        cleanup()


if __name__ == "__main__":
    main()
