# WebSocket Protocol Specification

## Overview
This document describes the WebSocket message protocol for a Scrabble-style game. Messages are JSON-encoded and fall into two categories: client-to-server messages and server-to-client messages.

**Important:** Player numbers are 1-based throughout this protocol. In a 2-player game, players are numbered 1 and 2. In a 4-player game, players are numbered 1, 2, 3, and 4.

---

## Client Messages

These messages are sent from the client to the server.

### 1. Ask Potential Score
Request the potential score for a set of tile placements.

**Command:** `potentialScore`

**Payload:** Array of position-tile pairs
```json
{
  "command": "potentialScore",
  "payload": [
    {
      "pos": {"x": 8, "y": 8},
      "tile": {"letter": "H", "value": 4}
    }
  ]
}
```

### 2. Ask Definition
Request the definition of a word.

**Command:** `askDefinition`

**Payload:** Object with word
```json
{
  "command": "askDefinition",
  "payload": {
    "word": "example"
  }
}
```

### 3. Send Chat Message
Send a chat message to other players.

**Command:** `say`

**Payload:** Object with message text
```json
{
  "command": "say",
  "payload": {
    "message": "Good game!"
  }
}
```

### 4. Board Move
Place tiles on the board.

**Command:** `boardMove`

**Payload:** Array of position-tile pairs
```json
{
  "command": "boardMove",
  "payload": [
    {
      "pos": {"x": 8, "y": 8},
      "tile": {"letter": "H", "value": 4}
    },
    {
      "pos": {"x": 9, "y": 8},
      "tile": {"letter": "I", "value": 1}
    }
  ]
}
```

### 5. Exchange Move
Exchange tiles from your rack.

**Command:** `exchangeMove`

**Payload:** Array of tiles to exchange
```json
{
  "command": "exchangeMove",
  "payload": [
    {"letter": "Q", "value": 10},
    {"letter": "Z", "value": 10}
  ]
}
```

### 6. Pass Move
Pass your turn without making a move.

**Command:** `passMove`

**Payload:** Empty or null
```json
{
  "command": "passMove",
  "payload": null
}
```

---

## Server Messages

These messages are sent from the server to clients, either as direct responses or broadcasts.

**All server messages follow this structure:**
```json
{
  "command": "commandName",
  "payload": { ... }
}
```

### Direct Responses (to client requests)

#### 1. Initialise Game
Sent when a player first connects, containing the full game state.

**Command:** `initialise`

```json
{
  "command": "initialise",
  "payload": {
    "moveCommands": [
      {
        "command": "playerBoardMove",
        "payload": { ... }
      },
      {
        "command": "playerPassMove",
        "payload": { ... }
      }
    ],
    "rack": [
      {"letter": "A", "value": 1},
      {"letter": "E", "value": 1}
    ],  // null for observers
    "players": [
      {
        "name": "Player1",
        "score": 42,
        "endBonus": 0
      }
    ],
    "playerNumber": 1,
    "playerMove": 2,
    "tilesRemaining": 87,
    "connectionStatuses": [
      {
        "playerNumber": 1,
        "active": true,
        "lastSeen": "2024-01-15T10:30:00Z"
      }
    ],
    "appVersion": 1
  }
}
```

**Notes:**
- The `moveCommands` array contains the game history. Each item in this array is a complete server message with its own `command` and `payload` fields, representing moves that have already occurred (e.g., `playerBoardMove`, `playerPassMove`, `playerExchangeMove`, etc.).
- `playerNumber`, `playerMove`, and `connectionStatuses[].playerNumber` are all 1-based (see [Player Number](#player-number) data type).
- The `rack` field will be `null` if the connected user is an observer (not a participant in the game). Observers can view the game but do not have their own tiles.

#### 2. Board Move Success
Confirms a successful board move and returns updated rack.

**Command:** `boardMoveSuccess`

```json
{
  "command": "boardMoveSuccess",
  "payload": {
    "rack": [
      {"letter": "N", "value": 1},
      {"letter": "T", "value": 1}
    ]
  }
}
```

#### 3. Exchange Move Success
Confirms a successful tile exchange and returns new tiles.

**Command:** `exchangeMoveSuccess`

```json
{
  "command": "exchangeMoveSuccess",
  "payload": {
    "rack": [
      {"letter": "A", "value": 1},
      {"letter": "E", "value": 1}
    ]
  }
}
```

#### 4. Pass Move Success
Confirms a successful pass.

**Command:** `passMoveSuccess`

```json
{
  "command": "passMoveSuccess",
  "payload": {}
}
```

#### 5. Chat Success
Confirms a chat message was sent.

**Command:** `chatSuccess`

```json
{
  "command": "chatSuccess",
  "payload": {}
}
```

#### 6. Ask Definition Success
Confirms a definition request was received.

**Command:** `askDefinitionSuccess`

```json
{
  "command": "askDefinitionSuccess",
  "payload": {}
}
```

#### 7. Potential Score
Returns the calculated potential score.

**Command:** `potentialScore`

```json
{
  "command": "potentialScore",
  "payload": {
    "potentialScore": 24
  }
}
```

#### 8. Invalid Command (Error)
Indicates an error occurred processing the request.

**Command:** `error`

```json
{
  "command": "error",
  "payload": {
    "error": "Invalid move: tiles not in a line"
  }
}
```

### Broadcast Messages (sent to all players)

#### 1. Player Board Move
Notifies all players when someone makes a board move.

**Command:** `playerBoardMove`

```json
{
  "command": "playerBoardMove",
  "payload": {
    "moveNumber": 5,
    "placed": [
      {
        "pos": {"x": 8, "y": 8},
        "tile": {"letter": "H", "value": 4}
      }
    ],
    "summary": {
      "type": "board",
      "overallScore": 24,
      "direction": "horizontal",
      "placed": [{"x": 8, "y": 8}],
      "wordsMade": [
        {
          "word": "HELLO",
          "score": 24
        }
      ]
    },
    "players": [...],
    "nowPlaying": 2,
    "tilesRemaining": 85
  }
}
```

#### 2. Player Pass Move
Notifies all players when someone passes.

**Command:** `playerPassMove`

```json
{
  "command": "playerPassMove",
  "payload": {
    "moveNumber": 6,
    "nowPlaying": 1,
    "summary": {
      "type": "pass"
    }
  }
}
```

#### 3. Player Exchange Move
Notifies all players when someone exchanges tiles (without revealing which tiles).

**Command:** `playerExchangeMove`

```json
{
  "command": "playerExchangeMove",
  "payload": {
    "moveNumber": 7,
    "nowPlaying": 2,
    "summary": {
      "type": "exchange"
    }
  }
}
```

#### 4. Game Finished
Notifies all players when the game ends.

**Command:** `gameFinished`

```json
{
  "command": "gameFinished",
  "payload": {
    "moveNumber": 50,
    "placed": [
      {
        "pos": {"x": 11, "y": 11},
        "tile": {"letter": "S", "value": 1}
      }
    ],
    "summary": {
      "type": "gameEnd",
      "players": [...],
      "lastMoveScore": 15,
      "wordsMade": [
        {
          "word": "CATS",
          "score": 15
        }
      ]
    }
  }
}
```

#### 5. Player Chat
Broadcasts a chat message from a player.

**Command:** `playerChat`

```json
{
  "command": "playerChat",
  "payload": {
    "player": "PlayerName",
    "message": "Nice move!",
    "when": "2024-01-15T10:30:00Z",
    "messageNumber": 12
  }
}
```

#### 6. Player Connect
Notifies when a player connects.

**Command:** `playerConnect`

**Note:** `playerNumber` is 1-based (see [Player Number](#player-number) data type).

```json
{
  "command": "playerConnect",
  "payload": {
    "playerNumber": 2,
    "when": "2024-01-15T10:30:00Z"
  }
}
```

#### 7. Player Disconnect
Notifies when a player disconnects.

**Command:** `playerDisconnect`

**Note:** `playerNumber` is 1-based (see [Player Number](#player-number) data type).

```json
{
  "command": "playerDisconnect",
  "payload": {
    "playerNumber": 2,
    "when": "2024-01-15T10:35:00Z"
  }
}
```

#### 8. Word Definitions
Broadcasts word definitions to all players.

**Command:** `wordDefinitions`

```json
{
  "command": "wordDefinitions",
  "payload": {
    "word": "example",
    "when": "2024-01-15T10:30:00Z",
    "definitions": [
      {
        "partOfSpeech": "noun",
        "definition": "a thing characteristic of its kind",
        "example": "it's a good example of how things can go wrong"
      }
    ],
    "definitionNumber": 5
  }
}
```

---

## Data Types

### Timestamp
Timestamps are ISO 8601 formatted strings in UTC. They may include nanosecond precision (up to 9 decimal places for fractional seconds).

Example: `"2026-01-26T20:14:35.303418622Z"`

Fields using timestamps: `when`, `lastSeen`

**Note:** When storing timestamps internally (e.g., in `GameState.players[].lastSeen`), they should be converted to milliseconds since the Unix epoch for consistency.

### Position
```json
{
  "x": 8,
  "y": 8
}
```
Where `x` and `y` are 1-based coordinates within valid board boundaries (1-15 for a standard 15x15 board). The center square is at position `{"x": 8, "y": 8}`.

### Tile
Regular letter:
```json
{
  "letter": "A",
  "value": 1
}
```

Blank tile (unassigned):
```json
{
  "letter": "_",
  "value": 0
}
```

Blank tile (assigned):
```json
{
  "letter": "E",
  "value": 0
}
```

### Square
```json
{
  "tile": {...},  // null if empty, or a Tile object
  "bonus": "N"    // N, DL, TL, DW, or TW
}
```

Bonus types:
- `N` - Normal (no bonus)
- `DL` - Double Letter
- `TL` - Triple Letter
- `DW` - Double Word
- `TW` - Triple Word

### Player Number
Player numbers are **1-based integers**. In a 2-player game, valid player numbers are `1` and `2`. In a 4-player game, valid player numbers are `1`, `2`, `3`, and `4`.

Fields using player numbers:
- `playerNumber` - The player's number in the game
- `playerMove` / `nowPlaying` - Which player's turn it is

### Player
```json
{
  "name": "PlayerName",
  "score": 42,
  "endBonus": 0
}
```

### Move Summary Types
- `"board"` - Board move with tiles placed
- `"pass"` - Player passed their turn
- `"exchange"` - Player exchanged tiles
- `"gameEnd"` - Game has ended
