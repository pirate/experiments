#!/usr/bin/env python3
"""
Simple IRC viewer - connects to the IRC server and displays the conversation.
You can also type messages to inject into the chat.
"""

import socket
import sys
import threading
import argparse
from datetime import datetime


class ChatViewer:
    def __init__(self, server: str = "127.0.0.1", port: int = 6667, channel: str = "#claude-chat"):
        self.server = server
        self.port = port
        self.channel = channel if channel.startswith('#') else f"#{channel}"
        self.socket = None
        self.nickname = f"Viewer_{datetime.now().strftime('%H%M%S')}"
        self.running = False

    def connect(self):
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.connect((self.server, self.port))
        self.running = True

        self.send(f"NICK {self.nickname}")
        self.send(f"USER viewer 0 * :Chat Viewer")

    def send(self, message: str):
        self.socket.send(f"{message}\r\n".encode('utf-8'))

    def receiver(self):
        buffer = ""
        while self.running:
            try:
                data = self.socket.recv(4096).decode('utf-8', errors='ignore')
                if not data:
                    break

                buffer += data
                while '\r\n' in buffer:
                    line, buffer = buffer.split('\r\n', 1)
                    self.handle_line(line)

            except Exception as e:
                if self.running:
                    print(f"\n[Error: {e}]")
                break

    def handle_line(self, line: str):
        # Parse IRC message
        if line.startswith(':'):
            parts = line[1:].split(' ', 2)
            if len(parts) >= 2:
                prefix = parts[0]
                command = parts[1]
                rest = parts[2] if len(parts) > 2 else ""

                # Handle PING
                if command == "PING":
                    self.send(f"PONG {rest}")
                    return

                # Welcome - join channel
                if command == "001":
                    print(f"[Connected as {self.nickname}]")
                    self.send(f"JOIN {self.channel}")
                    return

                # Join notification
                if command == "JOIN":
                    nick = prefix.split('!')[0]
                    if nick != self.nickname:
                        print(f"\n[{nick} joined the channel]")
                    else:
                        print(f"\n[Now watching {self.channel}]\n")
                        print("=" * 60)
                        print("Type a message and press Enter to send, or Ctrl+C to quit")
                        print("=" * 60)
                    return

                # Private message
                if command == "PRIVMSG":
                    if ' :' in rest:
                        target, message = rest.split(' :', 1)
                        nick = prefix.split('!')[0]
                        timestamp = datetime.now().strftime('%H:%M:%S')
                        print(f"\n[{timestamp}] <{nick}> {message}")
                    return

                # Part notification
                if command == "PART" or command == "QUIT":
                    nick = prefix.split('!')[0]
                    print(f"\n[{nick} left]")
                    return

        elif line.startswith("PING"):
            self.send(f"PONG {line[5:]}")

    def input_handler(self):
        while self.running:
            try:
                message = input()
                if message.strip():
                    self.send(f"PRIVMSG {self.channel} :{message}")
            except EOFError:
                break
            except KeyboardInterrupt:
                break

    def run(self):
        self.connect()

        # Start receiver thread
        recv_thread = threading.Thread(target=self.receiver, daemon=True)
        recv_thread.start()

        # Handle input in main thread
        try:
            self.input_handler()
        except KeyboardInterrupt:
            pass
        finally:
            self.running = False
            self.send("QUIT :Viewer disconnected")
            self.socket.close()
            print("\n[Disconnected]")


def main():
    parser = argparse.ArgumentParser(description="Watch Claude IRC Chat")
    parser.add_argument("--server", "-s", default="127.0.0.1", help="IRC server")
    parser.add_argument("--port", "-p", type=int, default=6667, help="IRC port")
    parser.add_argument("--channel", "-c", default="#claude-chat", help="Channel")
    args = parser.parse_args()

    print(f"Connecting to {args.server}:{args.port}...")
    viewer = ChatViewer(args.server, args.port, args.channel)

    try:
        viewer.run()
    except ConnectionRefusedError:
        print(f"Could not connect to {args.server}:{args.port}")
        print("Make sure the IRC server is running (use run_chat.py)")
        sys.exit(1)


if __name__ == "__main__":
    main()
