# High Level Summary

I'm looking to build a front end of a multiplayer scrabble app. Specifically, we're going to build a component / view that displays an interactive game of scrabble that is in progress - not surrounding scaffolding like starting a game or viewing current games in progress nor the server code for the app as these have already been implemented.

It will be used as part of a multiplayer webapp where 2-4 players can compete against each other, and will be the user's own view of the game including information about the game in progress (Such as the score and move history) as well as the user's private tile rack.

We do not have to concern ourselves with the scrabble logic (around scoring, legal moves, etc) as the server will take care of this. We are concerned only with the display and interactions (such as dragging and dropping tiles on to the board.)

# Stack

1. Typescript
2. npm
3. The latest version of the Vue framework
4. Tailwind CSS
5. The Primevue component library 
6. Use Vue Pinia for a global state store.

You are free to introduce other libraries, etc but please ask permission before installing them.

# Specification

## Game View Components

The game view will have the following top level components which I will specify further in this section:

1. The board with the tiles that have been played
2. A tile rack with the user's current tiles that they can play when it is their turn along with some controls for the tile rack. 
3. Buttons that the user can use to take their turn: submit, exchange and pass
4. A chat box where players can communicate
5. A score board with the player's names and their current scores
6. A move history showing the result of the previous moves that were played (the tiles, words formed, their score.)
7. A 'potential score' label that dynamically updates when the user places tiles to let them know what they could score if the words they play are valid

### High Level Layout

This view should be responsive - it should be pleasant to use on both mobile/tablet devices with small screens as well as PCs/laptops, etc.

#### Mobile / Smaller devices

1. The board and player's tile rack and move controls and potential score should be visible. The board should be large enough to easily view and drag and drop tiles from the rack onto the board easily.
2. The potential score should be displayed at the top of the screen in the mobile toolbar, alongside the navigation buttons for scores, history, and chat.
3. The other components specified above should be viewable by clicking a small button with an appropriate symbol on it to expand them
4. No scrolling left or right or up or down should be required while on the main view but the move history and chat can be scrolled when they're being viewed
5. It should be easy to get back to the main view from the 'expanded view' of the individual components using the back button or a button on the component
6. When any mobile panel (scores, chat, or move history) is open, pressing the browser back button should close the panel and return to the main view instead of navigating to the previous page in browser history. This is achieved by pushing a history state when opening these panels and handling the `popstate` event to close them.
7. It should be possible to drag and drop tiles on the rack and board on a touch screen. Touch-based drag and drop should show a visual clone of the tile being dragged that follows the user's finger.

#### Larger devices

1. The board should be in the middle of the screen and should be as large as possible while still fitting the viewport height (minus space for controls below). It should take up the majority of the available space.
2. The chat should be to the right of the page.
3. The player score board, potential score, and move history should be to the left of the screen (in that order, top to bottom).
4. The player's tile tack, tile rack controls and move controls should be display below thie board
5. All these components should be displayed in the main view without the need to scroll up/down or left/right
6. If the move history or chat would expand beyond the current page, they can be scrolled within in their 'box' without needing a scrollbar for the entire page
7. It should be possible to drag and drop tiles on the rack and board using a mouse
8. The 'potential score' should be displayed under the scoreboard in the left sidebar. It should always be visible (even when no tiles are placed) to prevent layout shift when tiles are placed. When no tiles are placed, it should show placeholder text like "Place tiles to see score". The widget should have a fixed width to prevent size changes when the content changes.

### More Detailed Component Specifications

#### Board

##### Display

The board will be 15 x 15 with the classic bonus tiles.

It should have the following layout where the letters below donate the following sort of squares:

N - normal
TW - Triple Word
DL - Double Letter
TL - Triple Letter
TW - Triple Word

[ ["TW", "N", "N", "DL", "N", "N", "N", "TW", "N", "N", "N", "DL", "N", "N", "TW"],
["N", "DW", "N", "N", "N", "TL", "N", "N", "N", "TL", "N", "N", "N", "DW", "N"],
["N", "N", "DW", "N", "N", "N", "DL", "N", "DL", "N", "N", "N", "DW", "N", "N"],
["DL", "N", "N", "DW", "N", "N", "N", "DL", "N", "N", "N", "DW", "N", "N", "DL"],
["N", "N", "N", "N", "DW", "N", "N", "N", "N", "N", "DW", "N", "N", "N", "N"],
["N", "TL", "N", "N", "N", "TL", "N", "N", "N", "TL", "N", "N", "N", "TL", "N"],
["N", "N", "DL", "N", "N", "N", "DL", "N", "DL", "N", "N", "N", "DL", "N", "N"],
["TW", "N", "N", "DL", "N", "N", "N", "DW", "N", "N", "N", "DL", "N", "N", "TW"],
["N", "N", "DL", "N", "N", "N", "DL", "N", "DL", "N", "N", "N", "DL", "N", "N"],
["N", "TL", "N", "N", "N", "TL", "N", "N", "N", "TL", "N", "N", "N", "TL", "N"],
["N", "N", "N", "N", "DW", "N", "N", "N", "N", "N", "DW", "N", "N", "N", "N"],
["DL", "N", "N", "DW", "N", "N", "N", "DL", "N", "N", "N", "DW", "N", "N", "DL"],
["N", "N", "DW", "N", "N", "N", "DL", "N", "DL", "N", "N", "N", "DW", "N", "N"],
["N", "DW", "N", "N", "N", "TL", "N", "N", "N", "TL", "N", "N", "N", "DW", "N"],
["TW", "N", "N", "DL", "N", "N", "N", "TW", "N", "N", "N", "DL", "N", "N", "TW"]
]

Each square type should have the same colour / styling wherever it is displayed on the board. Their colours should be similar to how they are in the physical board game.

Once a tile is on top of a square the tile should take up the entire width and height of the square, completely filling it and obscuring the colours / design below it.

A tile has a letter on it (or multiple depending on the locale - such as the Spanish LL) as well as the value for the individual tile.

Just like the physical board game, there are 'blank' tiles. When displayed on the board, they have their assigned letter on them. They are styled in a way that calls out they're a 'blank' tile (e.g. dashed border).

##### Tile Display Styles

Tiles are displayed differently depending on their context:

* **On the board**: Tiles fill the entire square with a simple background (e.g. light amber) showing the letter and value. They do not have borders, shadows, or rounded corners - they blend seamlessly into the board grid.
* **On the tile rack**: Tiles have the traditional Scrabble tile appearance with a visible border, subtle shadow, and rounded corners to give them a physical tile-like appearance.
* **Candidate tiles** (tiles placed but not yet submitted): These have a highlight ring to indicate they can still be moved.

##### Interactions

* When a tile is part of a move that's in the history and has already been played (a "permanent tile"), it should not be possible to drag and drop the tile around
* When the tile is one of the user's own tiles that hasn't yet been played AND it is currently the user's move it should be possible to drag and drop the tiles between the squares and the user's tile rack below the board.
* Candidate tiles (tiles placed on the board but not yet submitted) can be picked up and moved to other board squares or dragged back to the tile rack. This allows the user to rearrange their placement before submitting their move.
* When a tile is dropped anywhere other than a valid square on the board (e.g., dropped outside the board area), the tile should automatically return to the player's tile rack.
* Once the user has submitted their move and the server has accepted it, the user should no longer be able to move those tiles.

##### Drag and Drop Behaviour

The following rules apply to drag and drop for both mouse (desktop) and touch (mobile) interactions. The behaviour must be identical regardless of input method.

**Dropping a tile from the rack onto the board:**
* If the destination square is empty: place the tile on the square
* If the destination square has a candidate tile: swap the tiles (the candidate tile returns to the rack, the dragged tile takes its place)
* If the destination square has a permanent tile: cancel the drop (the tile remains on the rack as if the drag never happened)

**Dropping a candidate tile from one board square to another:**
* If the destination square is empty: move the tile to the new square
* If the destination square has another candidate tile: swap the two tiles
* If the destination square has a permanent tile: cancel the drop and return the tile to the rack

**Cancellation behaviour:**
* Dropping a tile outside the board area returns it to the rack
* Tiles must never "disappear" - every drag operation must result in the tile being either placed on a valid square or returned to the rack

**Touch-specific requirements:**
* Touch drag and drop must use isolated state per drag operation to prevent interference between consecutive drags
* The tile being dragged must always correspond to the tile the user touched, regardless of any previous drag operations
* Touch interactions must distinguish between taps and drags using a movement threshold (10 pixels). A touch that ends without exceeding the threshold is treated as a tap, enabling blank tile letter assignment on touch devices. A touch that moves beyond the threshold initiates a drag operation.

#### Score Board

* Displays all players (between 2 - 4) and their current scores as well as an icon displaying if they are active or inactive (are on the game screen.)
* If the game is over, the player's end game bonus (based on how many tiles they have left) or penalty is displayed
* Displays the number of tiles left in the letter bag
* Makes it clear which player is currently the player to move
* For disconnected players, displays when they were last seen (e.g., "5m ago"). This display updates automatically every minute to stay fresh without requiring a page refresh.

#### Move History

* Is a table (but not necessarily literally a HTML table) of all the moves played.
* Moves are displayed in chronological order: the first move appears at the top, and the latest move appears at the bottom.
* Each row contains the overall move score as well as the score of each individual word formed
* When new moves are added, the widget scrolls to the bottom to show the latest move

#### Chat

* Made up of a chat box to display the messages and a box to allow the player to submit messages
* The user's name who sent the message is displayed to the left of the message and is styled differently to the chat message itself to make it clear that's the name of the user who sent the message.
* Can also display definitions of words played when they are requested. Only the first 2 definitions are displayed for each word to avoid flooding the chat.
* Messages and definitions are displayed in chronological order based on their server timestamps. Definitions appear interspersed with chat messages at the correct time they were received.

##### Unread Message Indicator (Mobile Only)

On mobile devices, the chat button in the toolbar should be highlighted when there are unread messages. This helps users know when new messages have arrived without having to open the chat panel.

* The unread indicator compares the `lastChatMessageReceived` from the game state against the last message number the user has viewed
* The last viewed message number is stored in browser localStorage, allowing the unread status to persist across page refreshes and browser sessions
* When the user opens the chat panel on mobile, the last viewed message number is updated to the current `lastChatMessageReceived` value
* The localStorage key is scoped per game using the `gameId` option (e.g., `wordify_lastSeenChat_{gameId}`)
* On desktop, the chat is always visible so there is no need for an unread indicator

##### Interactions
* Can submit messages pressing enter
* On the mobile view should be able to use native keyboard to submit messages
* The user can type "!define <word>" to request a definition of a word
* The chat should always scroll to the bottom to show the most recent messages, including when the chat panel is first opened on mobile
* After sending a message, the chat automatically scrolls to the bottom. This happens both when pressing enter on the keyboard and when pressing the send button on mobile.

#### User Tile Rack

* There is a 'shuffle' button with an icon on it to shuffle the letters on the user's tile rack to help them come up with words. This button is positioned to the right of the tile rack.
* There is a 'recall' button with a down arrow icon on it to help the user quickly bring back all their candidate tiles from the board back on to their rack while they're making their move so they don't have to drag tile by tile on to the rack manually. This button is positioned to the left of the tile rack.

##### Interactions

* When it's the player's own move, they can drag tiles between their rack and the board. This should work with both a touch screen and a mouse
* When a user has a blank tile on their rack or on the board (as a candidate tile), they can tap/click it to bring up a view where they can assign it a letter. The view disappears once they've chosen the letter and the blank tile is assigned that letter. It remains clear it's a blank letter and the user can tap/click it again to assign a new letter. The view should display all the legal values the user can assign the blank tile with each option being in the style of the app's tiles. On touch devices, this tap interaction works during the player's own turn thanks to the movement threshold that distinguishes taps from drags (see Touch-specific requirements under Board Interactions).

##### Move Controls

* There should be 'submit', 'shuffle' and 'pass' buttons that the user can click when it's their move. They should be greyed out when it is not their move

# State

The following 'GameState' model should be used to represent the overall state of the game UI.

The model is updated in the following situations:
* A message is received on the websocket from the server indicating the game state has been updated
* Certain interactions on the UI (such as keeping track of the tiles the user has dragged from their tile on to the board as part of making a move, etc.)

```
type PlayerSummary = {
    // The player's username
    name: string,
    // The player's current score
    score: number

    // The player's end bonus or penalty (as a negative number) if the game has ended
    endBonus: number | undefined

    // True if the user is currently viewing the game
    connected: boolean

    // Undefined if the user is active or has never been active, date + time the user was last seen if known (in milliseconds since the unix epoch)
    lastSeen: number | undefined
}

type MoveSummary = {
    type: "pass"
} | {
    type: "exchange"
} | {
    type: "boardMove",
    // The overall score of the move
    overallScore: number,
    wordsMade: [{
        word: string,
        score: number
    }]
}

type ChatMessage = {
    type: "message",
    user: string,
    message: string
} | {
    type: "definition",
    // Noun, verb, etc
    partOfSpeech: string,
    // One definition of the word
    definition: string,
    // Example of the word being used in a sentence
    example: string
}

type Tile = {
    type: "blank",

    // The letter the blank is assigned (null if unassigned)
    assigned: string | null
} | {
    type: "letter",

    // The letter(s) on the tile
    letter: string,

    // The value of the tile
    value: number
}

type BoardSquare = {
    type: "DoubleWord"
    tile: Tile | undefined
} | {
    type: "TripleWord"
    tile: Tile | undefined
} | {
    type: "TripleLetter"
    tile: Tile | undefined
} |
{
    type: "DoubleLetter"
    tile: Tile | undefined
} | {
    type: "normal",
    tile: Tile | undefined
}

// Represents a tile placed on the board at a specific position
// Used for sparse representation of placed tiles during initialization
type PlacedTile = {
    // Position on the board using 1-based coordinates (1-15 for a standard 15x15 board)
    position: { x: number, y: number }
    // The tile at this position
    tile: Tile
}

type GameState = {
    
    // The player number of the user
    myPlayerNumber: number,

    // The number of the current player to move. Can be compared to 'myPlayerNumber' to determine if it's the user's move
    playerToMove: number,

    // A summary of the state of a player in the game
    players: PlayerSummary[],

    // The history of the moves made so far
    moveHistory: MoveSummary[],

    // The number of tiles left in the bag
    tilesRemaining: number,

    // A calculation of the user's potential score based on the candidate tiles they've placed on the board
    potentialScore: number,

    // The datetime the last move was received from the server in milliseconds since the unix epoch
    lastMoveReceived: number,

    // The chat messages that have been sent
    chatMessages: ChatMessage[],

    // The sequence number of the last chat message received
    lastChatMessageReceived: number,

    // The sequence number of the last definition received
    lastDefinitionReceived: number,

    // The user's tile rack - up to 7 letters, updated as the user shuffles the rack or drags and drops tiles on to the board
    // or when the server sends the latest tile state
    rack: Tile[],

    // The layout of the board defining the type of each square (DoubleWord, TripleLetter, etc.)
    // A 15x15 2D array where each entry is a SquareType ("DoubleWord" | "TripleWord" | "TripleLetter" | "DoubleLetter" | "Normal")
    // Uses the same format as the BOARD_LAYOUT constant
    boardLayout: SquareType[][],

    // The tiles that have been placed on the board. This is a sparse array - it only contains entries for
    // occupied squares, not empty ones. Each entry specifies the position (using 1-based coordinates) and the tile.
    // The initialization code uses this array along with boardLayout to construct the internal board state.
    placedTiles: PlacedTile[]
}
```

# Websocket Protocol 

The UI communicates with the server via a websocket protocol. The following specifies the messages that can be sent to the server from the client, and sent to the client from the server.

The UI should be updated as appropriate when messages are received from the server. Similarly, messages should be sent to the server over the websocket as appropriate when the user interacts with the UI.

The protocol for the websocket messages is defined in websocket-protocol-spec.md

## WebSocket Reconnection

When the websocket disconnects, the client must attempt to reconnect indefinitely until successful. Reconnection attempts should be made every 5 seconds (fixed delay, no exponential backoff). Reconnection only stops when the client explicitly calls disconnect.

When reconnecting, the client must tell the server to only send messages that were missed during the disconnection. This is done by appending query parameters to the websocket URL when reconnecting:

- `chatMessagesSinceMessageNumber`: The sequence number of the last chat message received (from `lastChatMessageReceived` in the game state)
- `definitionsSinceMessageNumber`: The sequence number of the last definition received (from `lastDefinitionReceived` in the game state)

Example reconnection URL:
```
wss://example.com/game/123?chatMessagesSinceMessageNumber=5&definitionsSinceMessageNumber=3
```

This ensures the server only sends chat messages and definitions that were missed during the disconnection, avoiding duplicate messages.

### Client State Preservation on Reconnect

When the websocket reconnects and the server sends a new `initialise` message, the client must preserve certain state rather than resetting it:

- **Chat messages**: The existing chat messages must be preserved. The server will only send new messages since the sequence number provided in the reconnection query parameters.
- **lastChatMessageReceived**: Must be preserved to maintain correct message sequencing.
- **lastDefinitionReceived**: Must be preserved to maintain correct definition sequencing.

This ensures a seamless experience for the user where chat history is not lost during brief disconnections.

# Code Architecture Concerns

* As an entry point, it should be possible to initialise the overall Vue component for the 'game view' with a model of the 'GameState' type as a prop or whatever vue concpet is appropriate here. Assume that there is some server side rendering that will define a HTML document and initialise this vue page with the game state (but don't try to produce this - assume it will happen later.)

* The code to process messages received on the websocket and parse them should be kept seperate from the controller logic that updates the state or UI. The controller logic should be behind an interface that is agnostic of websockets, etc.

* Similarly, the code to send messages on the websocket should be kept separate. It should be behind an interface that doesn't know anything about websockets, etc.

* These interfaces should make it possible to swap out the websockets for another technology or to perform local unit testing without a real socket connection, etc.

* Use Vue Pinia for a global state store.

# Library Packaging & Distribution

The game view must be packaged as a standalone library that can be used in any web project, including non-Vue projects. It should be usable from a plain HTML document via a `<script>` tag without requiring a build system.

## Requirements

* The library must be built as a UMD bundle that exposes a global `Wordify` object when loaded via a script tag
* All dependencies (Vue, Pinia, PrimeVue, etc.) must be bundled into the library so it works standalone
* The library must export a `createWordify(element, options)` function that:
  * Takes a CSS selector string or DOM element as the first argument
  * Takes an options object as the second argument with:
    * `initialState` - A `GameState` object to initialize the game (required). The `GameState` includes:
      * `boardLayout` - A 2D array defining the type of each square (same format as `BOARD_LAYOUT`)
      * `placedTiles` - A sparse array of tiles already on the board, each with a `position` (using 1-based coordinates) and `tile` field
      * The initialization code constructs the internal board state by combining these two fields
    * `websocketUrl` - URL to connect to the game websocket (optional)
    * `gameId` - Unique identifier for the game, used to scope browser localStorage keys for features like unread chat tracking (optional, but recommended)
  * For backwards compatibility, a plain `GameState` object can also be passed as the second argument instead of an options object
  * Returns an object with:
    * `app` - The Vue app instance
    * `unmount()` - Function to unmount and clean up the game (also disconnects websocket)
    * `updateState(state)` - Function to update the game state after initialization
    * `controller` - The game controller for sending commands (null if no websocket connected)
    * `connectionState` - A Vue ref containing the current connection state ('disconnected', 'connecting', 'connected', 'error')
    * `connect(url)` - Function to connect to a websocket URL
    * `disconnect()` - Function to disconnect from the websocket
* CSS must be bundled into a separate file that can be loaded via a `<link>` tag
* The library should also be available as an ES module for use with modern bundlers

## Build Output

Running `npm run build` should produce:
* `dist/wordify.umd.js` - UMD bundle for script tag usage
* `dist/wordify.es.js` - ES module for bundlers
* `dist/wordify-ui.css` - Bundled styles

## Example Usage (Plain HTML)

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
    const initialState = {
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
      // Use the standard board layout (or a custom one)
      boardLayout: Wordify.BOARD_LAYOUT,
      // Sparse array of tiles already placed on the board (positions are 1-based)
      placedTiles: [
        { position: { x: 8, y: 8 }, tile: { type: 'letter', letter: 'H', value: 4 } },
        { position: { x: 9, y: 8 }, tile: { type: 'letter', letter: 'E', value: 1 } },
        { position: { x: 10, y: 8 }, tile: { type: 'letter', letter: 'L', value: 1 } },
        { position: { x: 11, y: 8 }, tile: { type: 'letter', letter: 'L', value: 1 } },
        { position: { x: 12, y: 8 }, tile: { type: 'letter', letter: 'O', value: 1 } }
      ],
      gameEnded: false
    };

    // Option 1: Initialize with websocket connection
    const game = Wordify.createWordify('#game-container', {
      initialState: initialState,
      websocketUrl: 'wss://example.com/game/123',
      gameId: 'game-123'  // Used for localStorage scoping (e.g., unread chat tracking)
    });

    // Option 2: Initialize without websocket (backwards compatible)
    // const game = Wordify.createWordify('#game-container', initialState);

    // Monitor connection state:
    // game.connectionState.value // 'disconnected', 'connecting', 'connected', or 'error'

    // Connect to websocket later (if not provided in options):
    // game.connect('wss://example.com/game/123');

    // Disconnect from websocket:
    // game.disconnect();

    // Update state manually (e.g., for testing without websocket):
    // game.updateState(newState);

    // Clean up when done (also disconnects websocket):
    // game.unmount();
  </script>
</body>
</html>
```

## Additional Exports

The library should also export:
* `BOARD_LAYOUT` - The standard board layout constant
* `BOARD_SIZE` - The board size (15)
* TypeScript types for `GameState`, `TileInput`, `BlankTileInput`, `LetterTileInput`, `PlacedTile`, `SquareType`, `PlayerSummary`, `MoveSummary`, `ChatMessage`, `ConnectionState`, `IGameCommandSender`
