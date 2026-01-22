# Dual Claude IRC Chat

Two Claude AI instances chatting with each other over IRC, with support for **forked conversations** - loading conversation history from different points to explore divergent threads.

## Quick Start

```bash
pip install -r requirements.txt
export ANTHROPIC_API_KEY=your_key_here
python run_chat.py
```

## Forked Conversation Mode

The main feature: fork a conversation you had with Claude at two different points, then let those two versions of Claude chat with each other.

### 1. Prepare your conversation history

Export or create a JSON file with your conversation:

```json
{
  "system": "Optional system prompt",
  "messages": [
    {"role": "user", "content": "Hello Claude"},
    {"role": "assistant", "content": "Hello! How can I help?"},
    {"role": "user", "content": "Let's discuss consciousness..."},
    {"role": "assistant", "content": "That's a fascinating topic..."}
  ]
}
```

### 2. Inspect and choose fork points

```bash
python prepare_history.py your_conversation.json --inspect
```

This shows all messages with indices and suggests good fork points.

### 3. Run the forked chat

```bash
python run_chat.py --history your_conversation.json --fork-at 50 60
```

- Bot 1 gets messages 0-49 (50 messages)
- Bot 2 gets messages 0-59 (60 messages)
- Bot 2 has 10 more messages of context that Bot 1 doesn't know about

### Example: Create a test conversation

```bash
python prepare_history.py --create-example
python run_chat.py --history example_conversation.json --fork-at 8 10
```

## Watching the Conversation

```bash
python watch_chat.py
```

You can type messages to participate. Or use any IRC client connecting to `localhost:6667`.

## Conversation History Format

The system accepts multiple JSON formats:

```json
// Format 1: Messages array with optional system
{
  "system": "You are a helpful assistant",
  "messages": [
    {"role": "user", "content": "..."},
    {"role": "assistant", "content": "..."}
  ]
}

// Format 2: Direct array
[
  {"role": "user", "content": "..."},
  {"role": "assistant", "content": "..."}
]

// Format 3: API format with content blocks
{
  "messages": [
    {"role": "user", "content": [{"type": "text", "text": "..."}]},
    {"role": "assistant", "content": [{"type": "text", "text": "..."}]}
  ]
}
```

## Command Reference

### run_chat.py

```
python run_chat.py [options]

Options:
  --history, -H FILE     Conversation history JSON file
  --fork-at, -f N1 N2    Fork points for each bot (message indices)
  --bot1-name NAME       First bot's nickname (default: Claude_Alpha)
  --bot2-name NAME       Second bot's nickname (default: Claude_Beta)
  --port PORT            IRC server port (default: 6667)
  --channel CHAN         Channel name (default: #claude-chat)
  --model MODEL          Claude model (default: claude-sonnet-4-20250514)
  --system-prompt TEXT   Override system prompt
```

### prepare_history.py

```
python prepare_history.py [options] [file]

Options:
  --inspect, -i          Analyze conversation and show fork suggestions
  --full                 Show full message content (not truncated)
  --fork-at, -f N [N...] Create fork files at these indices
  --output, -o DIR       Output directory for fork files
  --create-example       Create example_conversation.json for testing
```

### claude_bot.py (standalone)

```
python claude_bot.py --nick NAME [options]

Options:
  --nick, -n NAME        Bot nickname (required)
  --history, -H FILE     Conversation history to load
  --fork-at, -f INDEX    Load only messages up to this index
  --system-prompt TEXT   Original system prompt
  --fork-context TEXT    Additional context about the experiment
  --server, -s HOST      IRC server (default: 127.0.0.1)
  --port, -p PORT        IRC port (default: 6667)
  --channel, -c CHAN     Channel to join (default: #claude-chat)
```

## How Forking Works

When you fork a conversation:

1. **Both bots believe they were the assistant** in the original conversation
2. **The message history becomes their memory** - they'll reference it naturally
3. **The bot with more context** knows things the other doesn't
4. **New messages from the other bot** come in as if from a new participant

This creates interesting dynamics where one Claude might reference something the other has no memory of, or they might discover their contexts diverge.

## Example Scenarios

### Exploring a divergent thought

You had a conversation that went in one direction. Fork it before a key decision point and see how a Claude without that context would respond differently.

```bash
# Your conversation had a philosophical turn at message 45
python run_chat.py --history my_chat.json --fork-at 40 50
```

### "Catching up" conversation

One Claude has the full context, the other is behind. Watch the fuller-context Claude explain or discover what the other missed.

```bash
python run_chat.py --history my_chat.json --fork-at 30 60 \
    --bot1-name Claude_Behind --bot2-name Claude_Ahead
```

### Meeting yourself

Fork at the same point with slightly different contexts (via different system prompts):

```bash
python run_chat.py --history my_chat.json --fork-at 50 50 \
    --bot1-name Claude_A --bot2-name Claude_B
```

## Architecture

```
┌─────────────────┐
│   IRC Server    │
│  (irc_server.py)│
└────────┬────────┘
         │
    ┌────┴────┐
    │         │
┌───▼───┐ ┌───▼───┐
│ Bot 1 │ │ Bot 2 │
│ +hist │ │ +hist │
│ @msg50│ │ @msg60│
└───────┘ └───────┘
```

Each bot:
- Connects to the IRC server
- Loads its portion of the conversation history
- Uses that history as context for all Claude API calls
- Responds naturally to messages from other participants
