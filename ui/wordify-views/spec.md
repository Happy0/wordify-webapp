# Wordify Views - Component Specification

This library provides Vue-based view components for the Wordify word game application. Each view can be mounted into a target DOM element and configured via options.

## Installation

```typescript
import { createRound, createCreateGame, createGameLobby, createLogin, createHome } from 'wordify-views'
```

## Views

### Round View (`createRound`)

The main game view displaying the board, tile rack, and game controls.

```typescript
import { createRound, type RoundOptions, type TileValueMap } from 'wordify-views'

// Define tile values for your game variant/locale
const tileValues: TileValueMap = {
  'A': 1, 'B': 3, 'C': 3, 'D': 2, 'E': 1,
  'F': 4, 'G': 2, 'H': 4, 'I': 1, 'J': 8,
  'K': 5, 'L': 1, 'M': 3, 'N': 1, 'O': 1,
  'P': 3, 'Q': 10, 'R': 1, 'S': 1, 'T': 1,
  'U': 1, 'V': 4, 'W': 4, 'X': 8, 'Y': 4,
  'Z': 10
}

// Spanish locale example with multi-character tiles
const spanishTileValues: TileValueMap = {
  'A': 1, 'B': 3, 'C': 3, 'CH': 5, 'D': 2, 'E': 1,
  'F': 4, 'G': 2, 'H': 4, 'I': 1, 'J': 8,
  'L': 1, 'LL': 8, 'M': 3, 'N': 1, 'Ñ': 8, 'O': 1,
  'P': 3, 'Q': 5, 'R': 1, 'RR': 8, 'S': 1, 'T': 1,
  'U': 1, 'V': 4, 'X': 8, 'Y': 4, 'Z': 10
}

const options: RoundOptions = {
  initialState: { /* GameState object */ },
  websocketUrl: 'wss://example.com/game/123',
  isLoggedIn: true,
  tileValues: tileValues  // or spanishTileValues for Spanish games
}

const instance = createRound('#app', options)

// Later, to unmount:
instance.unmount()
```

**Options:**
- `initialState`: The initial game state
- `websocketUrl`: WebSocket URL for real-time game updates
- `isLoggedIn`: Whether the user is logged in (affects navigation)
- `tileValues`: Map of letter strings to their point values (for locale support)

**Locale Support:**

The `tileValues` option enables support for different game locales. The keys of this map determine which letters are available for blank tile assignment. This supports:

- Standard single-character letters (A, B, C, etc.)
- Multi-character tiles used in some languages (CH, RR, LL for Spanish)
- Custom alphabets with locale-specific characters (Ñ for Spanish)

If `tileValues` is not provided, the blank tile selector defaults to the standard English alphabet (A-Z).

---

### Create Game View (`createCreateGame`)

Form for creating a new game with configuration options.

```typescript
import { createCreateGame, type CreateGameOptions } from 'wordify-views'

const options: CreateGameOptions = {
  isLoggedIn: true
}

const instance = createCreateGame('#app', options)
```

**Options:**
- `isLoggedIn`: Whether the user is logged in (affects navigation)

---

### Game Lobby View (`createGameLobby`)

Waiting room view for players to join before a game starts.

```typescript
import { createGameLobby, type GameLobbyOptions } from 'wordify-views'

const options: GameLobbyOptions = {
  gameLobbyId: 'abc123',
  websocketUrl: 'wss://example.com/lobby/abc123',
  isLoggedIn: true
}

const instance = createGameLobby('#app', options)
```

**Options:**
- `gameLobbyId`: Unique identifier for the lobby (used in shareable link)
- `websocketUrl`: WebSocket URL for lobby state updates
- `isLoggedIn`: Whether the user is logged in (affects navigation)

---

### Login View (`createLogin`)

Login/signup prompt view with customizable message.

```typescript
import { createLogin, type LoginOptions } from 'wordify-views'

const options: LoginOptions = {
  message: 'Log in to join this game.'
}

const instance = createLogin('#app', options)
```

**Options:**
- `message`: Custom message explaining why login is needed

---

### Home View (`createHome`)

The home/landing page showing the user's games in progress or prompts to create/join games.

```typescript
import { createHome, type HomeOptions, type GameSummary, type TileValueMap } from 'wordify-views'

// Define tile values for your game variant
const tileValues: TileValueMap = new Map([
  ['A', 1], ['B', 3], ['C', 3], ['D', 2], ['E', 1],
  ['F', 4], ['G', 2], ['H', 4], ['I', 1], ['J', 8],
  ['K', 5], ['L', 1], ['M', 3], ['N', 1], ['O', 1],
  ['P', 3], ['Q', 10], ['R', 1], ['S', 1], ['T', 1],
  ['U', 1], ['V', 4], ['W', 4], ['X', 8], ['Y', 4],
  ['Z', 10]
])

const games: GameSummary[] = [
  {
    gameId: 'game-123',
    boardString: ',,,,,,,H,E,L,L,O,,,,...', // 225 comma-separated values
    players: ['Alice', 'Bob'],
    yourMove: true
  }
]

const options: HomeOptions = {
  games: games,
  isLoggedIn: true,
  tileValues: tileValues
}

const instance = createHome('#app', options)
```

**Options:**
- `games`: Array of `GameSummary` objects for games in progress
- `isLoggedIn`: Whether the user is logged in
- `tileValues`: Map of letter strings to their point values

**GameSummary:**
- `gameId`: Unique game identifier (used for navigation)
- `boardString`: Comma-delimited board state (225 values, column-major order)
- `players`: Array of player names
- `yourMove`: Whether it's the current user's turn

**Behavior:**
- If logged in with games: Shows miniboards for each game in progress
- If logged in without games: Shows "Create Game" button
- If not logged in: Shows login prompt

---

## Exported Types

The library also exports these types for TypeScript users:

```typescript
import type {
  // Game state types
  GameState,
  TileInput,
  BlankTileInput,
  LetterTileInput,
  PlayerSummary,
  MoveSummary,
  ChatMessage,
  SquareType,
  PlacedTile,

  // Service types
  ConnectionState,
  IGameCommandSender,

  // Home view types
  GameSummary,

  // Shared types (used by both home and round views)
  TileValueMap
} from 'wordify-views'
```

## Exported Constants

```typescript
import { BOARD_LAYOUT, BOARD_SIZE } from 'wordify-views'

// BOARD_SIZE = 15 (standard 15x15 board)
// BOARD_LAYOUT = 2D array of SquareType defining bonus squares
```
