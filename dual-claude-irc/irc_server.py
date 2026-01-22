#!/usr/bin/env python3
"""
Simple IRC server for local Claude-to-Claude chat.
Based on minimal IRC protocol implementation.
"""

import socket
import select
import re
from dataclasses import dataclass, field
from typing import Dict, Optional
import logging

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(message)s')
logger = logging.getLogger(__name__)


@dataclass
class Client:
    socket: socket.socket
    address: tuple
    nickname: Optional[str] = None
    username: Optional[str] = None
    realname: Optional[str] = None
    buffer: str = ""
    registered: bool = False
    channels: set = field(default_factory=set)

    @property
    def hostmask(self) -> str:
        return f"{self.nickname}!{self.username}@{self.address[0]}"


class IRCServer:
    def __init__(self, host: str = "127.0.0.1", port: int = 6667):
        self.host = host
        self.port = port
        self.server_name = "claude-irc"
        self.clients: Dict[socket.socket, Client] = {}
        self.nicknames: Dict[str, Client] = {}
        self.channels: Dict[str, set] = {}  # channel -> set of clients
        self.server_socket: Optional[socket.socket] = None

    def start(self):
        self.server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.server_socket.bind((self.host, self.port))
        self.server_socket.listen(5)
        logger.info(f"IRC Server started on {self.host}:{self.port}")

        try:
            self.run()
        except KeyboardInterrupt:
            logger.info("Server shutting down...")
        finally:
            self.server_socket.close()

    def run(self):
        while True:
            sockets = [self.server_socket] + list(self.clients.keys())
            readable, _, _ = select.select(sockets, [], [], 1.0)

            for sock in readable:
                if sock is self.server_socket:
                    self.accept_client()
                else:
                    self.handle_client(sock)

    def accept_client(self):
        client_socket, address = self.server_socket.accept()
        client = Client(socket=client_socket, address=address)
        self.clients[client_socket] = client
        logger.info(f"New connection from {address}")

    def handle_client(self, sock: socket.socket):
        client = self.clients.get(sock)
        if not client:
            return

        try:
            data = sock.recv(4096).decode('utf-8', errors='ignore')
            if not data:
                self.disconnect_client(sock)
                return

            client.buffer += data
            while '\r\n' in client.buffer:
                line, client.buffer = client.buffer.split('\r\n', 1)
                if line:
                    self.process_command(client, line)

        except (ConnectionResetError, BrokenPipeError):
            self.disconnect_client(sock)

    def disconnect_client(self, sock: socket.socket):
        client = self.clients.get(sock)
        if client:
            if client.nickname:
                self.nicknames.pop(client.nickname, None)
            for channel in list(client.channels):
                self.part_channel(client, channel, "Connection closed")
            logger.info(f"Client disconnected: {client.nickname or client.address}")
        self.clients.pop(sock, None)
        try:
            sock.close()
        except:
            pass

    def send(self, client: Client, message: str):
        try:
            client.socket.send(f"{message}\r\n".encode('utf-8'))
        except:
            self.disconnect_client(client.socket)

    def send_numeric(self, client: Client, numeric: str, message: str):
        nick = client.nickname or "*"
        self.send(client, f":{self.server_name} {numeric} {nick} {message}")

    def process_command(self, client: Client, line: str):
        logger.info(f"<- [{client.nickname or '?'}] {line}")

        # Parse IRC command
        if line.startswith(':'):
            # Skip prefix
            _, line = line.split(' ', 1)

        parts = line.split(' ', 1)
        command = parts[0].upper()
        params = parts[1] if len(parts) > 1 else ""

        handlers = {
            'NICK': self.handle_nick,
            'USER': self.handle_user,
            'PING': self.handle_ping,
            'PONG': self.handle_pong,
            'JOIN': self.handle_join,
            'PART': self.handle_part,
            'PRIVMSG': self.handle_privmsg,
            'QUIT': self.handle_quit,
            'WHO': self.handle_who,
            'MODE': self.handle_mode,
        }

        handler = handlers.get(command)
        if handler:
            handler(client, params)
        else:
            logger.debug(f"Unhandled command: {command}")

    def handle_nick(self, client: Client, params: str):
        nickname = params.strip().split()[0] if params.strip() else None
        if not nickname:
            self.send_numeric(client, "431", ":No nickname given")
            return

        if nickname in self.nicknames and self.nicknames[nickname] != client:
            self.send_numeric(client, "433", f"{nickname} :Nickname is already in use")
            return

        old_nick = client.nickname
        if old_nick:
            self.nicknames.pop(old_nick, None)

        client.nickname = nickname
        self.nicknames[nickname] = client

        if old_nick:
            # Notify channels of nick change
            for channel in client.channels:
                for member in self.channels.get(channel, set()):
                    self.send(member, f":{old_nick}!{client.username}@{client.address[0]} NICK {nickname}")

        self.check_registration(client)

    def handle_user(self, client: Client, params: str):
        parts = params.split(' ', 3)
        if len(parts) < 4:
            self.send_numeric(client, "461", "USER :Not enough parameters")
            return

        client.username = parts[0]
        client.realname = parts[3].lstrip(':')
        self.check_registration(client)

    def check_registration(self, client: Client):
        if client.registered:
            return
        if client.nickname and client.username:
            client.registered = True
            self.send_numeric(client, "001", f":Welcome to the Claude IRC Network {client.hostmask}")
            self.send_numeric(client, "002", f":Your host is {self.server_name}")
            self.send_numeric(client, "003", ":This server was created for Claude-to-Claude chat")
            self.send_numeric(client, "004", f"{self.server_name} claude-irc-1.0 o o")
            self.send_numeric(client, "376", ":End of MOTD")
            logger.info(f"Client registered: {client.nickname}")

    def handle_ping(self, client: Client, params: str):
        self.send(client, f":{self.server_name} PONG {self.server_name} :{params.lstrip(':')}")

    def handle_pong(self, client: Client, params: str):
        pass  # Just acknowledge

    def handle_join(self, client: Client, params: str):
        if not client.registered:
            return

        channel = params.split()[0].strip()
        if not channel.startswith('#'):
            channel = '#' + channel

        if channel not in self.channels:
            self.channels[channel] = set()

        self.channels[channel].add(client)
        client.channels.add(channel)

        # Notify all members including the joiner
        for member in self.channels[channel]:
            self.send(member, f":{client.hostmask} JOIN {channel}")

        # Send names list
        names = ' '.join(c.nickname for c in self.channels[channel])
        self.send_numeric(client, "353", f"= {channel} :{names}")
        self.send_numeric(client, "366", f"{channel} :End of NAMES list")

        logger.info(f"{client.nickname} joined {channel}")

    def handle_part(self, client: Client, params: str):
        parts = params.split(' ', 1)
        channel = parts[0].strip()
        reason = parts[1].lstrip(':') if len(parts) > 1 else "Leaving"
        self.part_channel(client, channel, reason)

    def part_channel(self, client: Client, channel: str, reason: str = "Leaving"):
        if channel not in client.channels:
            return

        # Notify members
        for member in self.channels.get(channel, set()):
            self.send(member, f":{client.hostmask} PART {channel} :{reason}")

        client.channels.discard(channel)
        if channel in self.channels:
            self.channels[channel].discard(client)
            if not self.channels[channel]:
                del self.channels[channel]

    def handle_privmsg(self, client: Client, params: str):
        if not client.registered:
            return

        match = re.match(r'(\S+)\s+:?(.*)', params)
        if not match:
            return

        target, message = match.groups()

        logger.info(f"[{client.nickname} -> {target}] {message}")

        if target.startswith('#'):
            # Channel message
            if target in self.channels:
                for member in self.channels[target]:
                    if member != client:
                        self.send(member, f":{client.hostmask} PRIVMSG {target} :{message}")
        else:
            # Private message
            target_client = self.nicknames.get(target)
            if target_client:
                self.send(target_client, f":{client.hostmask} PRIVMSG {target} :{message}")
            else:
                self.send_numeric(client, "401", f"{target} :No such nick/channel")

    def handle_quit(self, client: Client, params: str):
        reason = params.lstrip(':') if params else "Quit"
        for channel in list(client.channels):
            for member in self.channels.get(channel, set()):
                if member != client:
                    self.send(member, f":{client.hostmask} QUIT :{reason}")
        self.disconnect_client(client.socket)

    def handle_who(self, client: Client, params: str):
        target = params.split()[0] if params else ""
        if target.startswith('#') and target in self.channels:
            for member in self.channels[target]:
                self.send_numeric(client, "352",
                    f"{target} {member.username} {member.address[0]} {self.server_name} "
                    f"{member.nickname} H :0 {member.realname}")
        self.send_numeric(client, "315", f"{target} :End of WHO list")

    def handle_mode(self, client: Client, params: str):
        # Minimal mode handling - just acknowledge
        target = params.split()[0] if params else ""
        if target.startswith('#'):
            self.send_numeric(client, "324", f"{target} +")
        elif target == client.nickname:
            self.send_numeric(client, "221", "+")


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description="Simple IRC Server")
    parser.add_argument("--host", default="127.0.0.1", help="Host to bind to")
    parser.add_argument("--port", type=int, default=6667, help="Port to listen on")
    args = parser.parse_args()

    server = IRCServer(host=args.host, port=args.port)
    server.start()
