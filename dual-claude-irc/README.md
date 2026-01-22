# Dual Claude IRC Chat

Two Claude AI instances chatting with each other over IRC.

## Quick Start

1. Install dependencies:
```bash
pip install -r requirements.txt
```

2. Set your Anthropic API key:
```bash
export ANTHROPIC_API_KEY=your_key_here
```

3. Run the chat:
```bash
python run_chat.py
```

This starts:
- A local IRC server on port 6667
- Two Claude bots with different personalities
- They'll join `#claude-chat` and start conversing

## Watch the Conversation

In a separate terminal, run:
```bash
python watch_chat.py
```

You can also type messages to participate in the chat!

## Customization

```bash
python run_chat.py \
  --bot1-name "Socrates" \
  --bot2-name "Aristotle" \
  --bot1-personality "You are a philosopher who questions everything using the Socratic method" \
  --bot2-personality "You are a practical philosopher who values observation and logic" \
  --channel "#philosophy"
```

### Options

| Option | Default | Description |
|--------|---------|-------------|
| `--port` | 6667 | IRC server port |
| `--channel` | #claude-chat | Channel name |
| `--bot1-name` | Claude_Alpha | First bot's name |
| `--bot2-name` | Claude_Beta | Second bot's name |
| `--bot1-personality` | (curious, philosophical) | First bot's personality |
| `--bot2-personality` | (witty, practical) | Second bot's personality |
| `--model` | claude-sonnet-4-20250514 | Claude model to use |

## Components

- `irc_server.py` - Lightweight IRC server
- `claude_bot.py` - IRC client powered by Claude
- `run_chat.py` - Launcher for server + two bots
- `watch_chat.py` - Simple viewer/participant client

## Running Individual Components

### Start just the server:
```bash
python irc_server.py --port 6667
```

### Start a single bot:
```bash
python claude_bot.py --nick MyBot --channel "#test" --personality "You are helpful and friendly"
```

### Connect with any IRC client:
```
Server: 127.0.0.1
Port: 6667
Channel: #claude-chat
```

## Example Conversations

The bots will naturally converse based on their personalities. You can create interesting dynamics:

**Debate partners:**
```bash
python run_chat.py \
  --bot1-personality "You strongly believe AI will be beneficial for humanity" \
  --bot2-personality "You are skeptical about AI and raise concerns about risks"
```

**Creative writing:**
```bash
python run_chat.py \
  --bot1-personality "You are a storyteller who loves to start creative stories" \
  --bot2-personality "You build on others' stories with unexpected plot twists"
```
