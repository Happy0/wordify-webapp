# Wordify Views Library

A library of views and components for the Wordify multiplayer webapp. Each view represents a different page or screen in the application and can be mounted independently into any web project.

## Stack

1. TypeScript
2. npm
3. Vue 3 (latest version)
4. Tailwind CSS
5. PrimeVue component library
6. Vue Pinia for global state management

You are free to introduce other libraries, but please ask permission before installing them.

## Folder Structure

```
src/
├── lib/
│   ├── round.ts            # Round view mounting function
│   ├── create-game.ts      # Create Game view mounting function
│   └── game-lobby.ts       # Game Lobby view mounting function
├── views/
│   ├── GameView.vue        # Round view - game in progress
│   ├── CreateGameView.vue  # Create Game view - game setup
│   └── GameLobbyView.vue   # Game Lobby view - invite players
├── components/
│   ├── round/              # Components specific to the Round view
│   │   ├── GameBoard.vue
│   │   ├── TileRack.vue
│   │   ├── ScoreBoard.vue
│   │   ├── MoveHistory.vue
│   │   ├── GameChat.vue
│   │   ├── MoveControls.vue
│   │   ├── BoardSquare.vue
│   │   ├── GameTile.vue
│   │   ├── BlankTileSelector.vue
│   │   └── PotentialScore.vue
│   └── common/             # Shared components (future)
├── composables/            # Vue composables
├── stores/                 # Pinia stores
├── services/               # Services (websocket, etc.)
├── types/                  # TypeScript type definitions
└── assets/                 # Static assets
```

### Views

The `views/` folder contains top-level view components, each representing a distinct page or screen in the Wordify webapp:

- **Round** (`GameView.vue`): Displays an in-progress game. See `specs/round.md` for detailed specification.
- **Create Game** (`CreateGameView.vue`): Game setup screen for configuring and creating a new game.
- **Game Lobby** (`GameLobbyView.vue`): Lobby screen for sharing invitation links and waiting for players to join.

### Components

The `components/` folder is organized with subfolders for each view:

- **round/**: Components specific to the Round view (game board, tile rack, score board, etc.)
- **common/**: Shared components used across multiple views (to be created as needed)

## Library Entry Points

The library is accessed via the main `lib.ts` file, which exports functions to mount each view.

### `createRound(element, options)`

Mounts the Round view (game in progress) to a DOM element.

**Parameters:**
- `element` - CSS selector string or DOM element
- `options` - Configuration object:
  - `initialState` - A `GameState` object (required)
  - `websocketUrl` - URL to connect to the game websocket (optional)
  - `gameId` - Unique identifier for localStorage scoping (optional, recommended)

**Returns:** `RoundInstance` object with:
- `app` - The Vue app instance
- `unmount()` - Cleanup function
- `updateState(state)` - Update game state
- `controller` - Game controller for sending commands
- `connectionState` - Vue ref with connection state
- `connect(url)` - Connect to websocket
- `disconnect()` - Disconnect from websocket

**Example:**
```typescript
import { createRound } from 'wordify-views'

const round = createRound('#game-container', {
  initialState: gameState,
  websocketUrl: 'wss://example.com/game/123',
  gameId: 'game-123'
})

// Later: cleanup
round.unmount()
```

### `createCreateGame(element, options)`

Mounts the Create Game view (game setup screen) to a DOM element.

**Parameters:**
- `element` - CSS selector string or DOM element
- `options` - Configuration object:
  - `locales` - Map of locale display names to locale values (e.g., `{ "English (US)": "en_us" }`)
  - `isLoggedIn` - Whether the user is currently logged in (optional, defaults to false)

**Returns:** `CreateGameInstance` object with:
- `app` - The Vue app instance
- `unmount()` - Cleanup function

**Example:**
```typescript
import { createCreateGame } from 'wordify-views'

const createGame = createCreateGame('#create-game-container', {
  locales: {
    'English (US)': 'en_us',
    'English (UK)': 'en_gb'
  },
  isLoggedIn: true
})

// Later: cleanup
createGame.unmount()
```

### `createGameLobby(element, options)`

Mounts the Game Lobby view (invitation/waiting screen) to a DOM element.

**Parameters:**
- `element` - CSS selector string or DOM element
- `options` - Configuration object:
  - `gameLobbyId` - The unique identifier for the game lobby (required)
  - `websocketUrl` - WebSocket URL to connect to for lobby state updates (required)
  - `isLoggedIn` - Whether the user is currently logged in (optional, defaults to false)

**Returns:** `GameLobbyInstance` object with:
- `app` - The Vue app instance
- `unmount()` - Cleanup function

**WebSocket Protocol:**
The view listens for websocket messages. When a `startGame` message is received:
```json
{
  "command": "startGame",
  "payload": {
    "gameId": "<gameId>"
  }
}
```
The user is automatically redirected to `/games/{gameId}`.

**Example:**
```typescript
import { createGameLobby } from 'wordify-views'

const lobby = createGameLobby('#lobby-container', {
  gameLobbyId: 'abc123',
  websocketUrl: 'wss://example.com/lobby/abc123',
  isLoggedIn: true
})

// Later: cleanup
lobby.unmount()
```

## Build Output

Running `npm run build` produces:

- `dist/wordify.umd.js` - UMD bundle for script tag usage
- `dist/wordify.es.js` - ES module for bundlers
- `dist/wordify-ui.css` - Bundled styles

## Usage (Plain HTML)

```html
<!DOCTYPE html>
<html>
<head>
  <link rel="stylesheet" href="./dist/wordify-ui.css">
</head>
<body>
  <div id="game-container"></div>
  <script src="./dist/wordify.umd.js"></script>
  <script>
    const round = Wordify.createRound('#game-container', {
      initialState: {
        myPlayerNumber: 0,
        playerToMove: 0,
        players: [...],
        moveHistory: [],
        tilesRemaining: 100,
        potentialScore: null,
        lastMoveReceived: Date.now(),
        chatMessages: [],
        lastChatMessageReceived: 0,
        lastDefinitionReceived: 0,
        rack: [...],
        boardLayout: Wordify.BOARD_LAYOUT,
        placedTiles: [],
        gameEnded: false
      },
      websocketUrl: 'wss://example.com/game/123',
      gameId: 'game-123'
    });
  </script>
</body>
</html>
```

## Exported Constants

- `BOARD_LAYOUT` - Standard 15x15 board layout
- `BOARD_SIZE` - Board dimension (15)

## Exported Types

- `GameState` - Complete game state
- `RoundOptions` - Options for createRound
- `RoundInstance` - Return type of createRound
- `CreateGameOptions` - Options for createCreateGame
- `CreateGameInstance` - Return type of createCreateGame
- `GameLobbyOptions` - Options for createGameLobby
- `GameLobbyInstance` - Return type of createGameLobby
- `TileInput`, `BlankTileInput`, `LetterTileInput` - Tile types
- `PlacedTile` - Tile with position
- `SquareType` - Board square types
- `PlayerSummary` - Player state
- `MoveSummary` - Move history entry
- `ChatMessage` - Chat message types
- `ConnectionState` - WebSocket connection states
- `IGameCommandSender` - Controller interface

## Code Architecture

* Entry point functions initialize Vue with the appropriate view component and return a controller interface
* WebSocket protocol handling is separated from controller logic via interfaces
* Services are transport-agnostic, enabling testing without real socket connections
* Global state is managed via Pinia stores

## View Specifications

Detailed specifications for each view are in separate files:

- [Round View](./round.md) - Game in progress
- [Create Game View](./create-game.md) - Game setup
- [Game Lobby View](./game-lobby.md) - Invitation and waiting room
