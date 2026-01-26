// Player summary type representing a player's state in the game
export type PlayerSummary = {
  // The player's username
  name: string
  // The player's current score
  score: number
  // The player's end bonus or penalty (as a negative number) if the game has ended
  endBonus: number | undefined
  // True if the user is currently viewing the game
  connected: boolean
  // Undefined if the user is active or has never been active, date + time the user was last seen if known (in milliseconds since the unix epoch)
  lastSeen: number | undefined
}

// Move summary types representing different types of moves
export type MoveSummary = {
  type: 'pass'
  playerIndex: number
} | {
  type: 'exchange'
  playerIndex: number
} | {
  type: 'boardMove'
  playerIndex: number
  // The overall score of the move
  overallScore: number
  wordsMade: {
    word: string
    score: number
  }[]
}

// Chat message types
export type ChatMessage = {
  type: 'message'
  user: string
  message: string
  // Timestamp when the message was received (ISO 8601 string)
  when: string
} | {
  type: 'definition'
  word: string
  // Noun, verb, etc
  partOfSpeech: string
  // One definition of the word
  definition: string
  // Example of the word being used in a sentence
  example: string
  // Timestamp when the definition was received (ISO 8601 string)
  when: string
}

// Input tile types for initialization (id and candidate are internal concerns)
export type BlankTileInput = {
  type: 'blank'
  // The letter the blank is assigned (null if unassigned)
  assigned: string | null
}

export type LetterTileInput = {
  type: 'letter'
  // The letter(s) on the tile
  letter: string
  // The value of the tile
  value: number
}

export type TileInput = BlankTileInput | LetterTileInput

// Internal tile types (with id and candidate tracking)
export type BlankTile = BlankTileInput & {
  // Whether this tile belongs to the user and has not yet been permanently played
  candidate: boolean
  // Unique identifier for drag-drop tracking
  id: string
}

export type LetterTile = LetterTileInput & {
  // Whether this tile belongs to the user and has not yet been permanently played
  candidate: boolean
  // Unique identifier for drag-drop tracking
  id: string
}

export type Tile = BlankTile | LetterTile

// Board square types
export type SquareType = 'DoubleWord' | 'TripleWord' | 'TripleLetter' | 'DoubleLetter' | 'Normal'

export type BoardSquare = {
  type: SquareType
  tile: Tile | null
  row: number
  col: number
}

// Position on the board (0-based for internal use)
export type Position = {
  x: number
  y: number
}

// Represents a tile placed on the board at a specific position
// Used for sparse representation of placed tiles during initialization
// Position uses 1-based coordinates (1-15 for a standard 15x15 board)
export type PlacedTile = {
  position: { x: number; y: number }
  tile: TileInput
}

// Game state representing the entire UI state (input format for initialization)
export type GameState = {
  // The player number of the user (0-indexed)
  myPlayerNumber: number
  // The number of the current player to move
  playerToMove: number
  // A summary of the state of players in the game
  players: PlayerSummary[]
  // The history of the moves made so far
  moveHistory: MoveSummary[]
  // The number of tiles left in the bag
  tilesRemaining: number
  // A calculation of the user's potential score based on the candidate tiles they've placed on the board
  potentialScore: number | null
  // The datetime the last move was received from the server in milliseconds since the unix epoch
  lastMoveReceived: number
  // The chat messages that have been sent
  chatMessages: ChatMessage[]
  // The sequence number of the last chat message received
  lastChatMessageReceived: number
  // The sequence number of the last definition received
  lastDefinitionReceived: number
  // The user's tile rack - up to 7 letters (tile IDs are optional - generated internally if not provided)
  rack: TileInput[]
  // The layout of the board defining the type of each square
  // A 15x15 2D array where each entry is a SquareType
  boardLayout: SquareType[][]
  // The tiles that have been placed on the board (sparse array with 1-based positions)
  placedTiles: PlacedTile[]
  // Whether the game has ended
  gameEnded: boolean
}

// Internal game state used by the store (with constructed board)
export type InternalGameState = Omit<GameState, 'boardLayout' | 'placedTiles'> & {
  // The state of each square on the board (15x15 = 225 squares)
  board: BoardSquare[][]
}

// Board layout constant - the classic Scrabble board layout
export const BOARD_LAYOUT: SquareType[][] = [
  ['TripleWord', 'Normal', 'Normal', 'DoubleLetter', 'Normal', 'Normal', 'Normal', 'TripleWord', 'Normal', 'Normal', 'Normal', 'DoubleLetter', 'Normal', 'Normal', 'TripleWord'],
  ['Normal', 'DoubleWord', 'Normal', 'Normal', 'Normal', 'TripleLetter', 'Normal', 'Normal', 'Normal', 'TripleLetter', 'Normal', 'Normal', 'Normal', 'DoubleWord', 'Normal'],
  ['Normal', 'Normal', 'DoubleWord', 'Normal', 'Normal', 'Normal', 'DoubleLetter', 'Normal', 'DoubleLetter', 'Normal', 'Normal', 'Normal', 'DoubleWord', 'Normal', 'Normal'],
  ['DoubleLetter', 'Normal', 'Normal', 'DoubleWord', 'Normal', 'Normal', 'Normal', 'DoubleLetter', 'Normal', 'Normal', 'Normal', 'DoubleWord', 'Normal', 'Normal', 'DoubleLetter'],
  ['Normal', 'Normal', 'Normal', 'Normal', 'DoubleWord', 'Normal', 'Normal', 'Normal', 'Normal', 'Normal', 'DoubleWord', 'Normal', 'Normal', 'Normal', 'Normal'],
  ['Normal', 'TripleLetter', 'Normal', 'Normal', 'Normal', 'TripleLetter', 'Normal', 'Normal', 'Normal', 'TripleLetter', 'Normal', 'Normal', 'Normal', 'TripleLetter', 'Normal'],
  ['Normal', 'Normal', 'DoubleLetter', 'Normal', 'Normal', 'Normal', 'DoubleLetter', 'Normal', 'DoubleLetter', 'Normal', 'Normal', 'Normal', 'DoubleLetter', 'Normal', 'Normal'],
  ['TripleWord', 'Normal', 'Normal', 'DoubleLetter', 'Normal', 'Normal', 'Normal', 'DoubleWord', 'Normal', 'Normal', 'Normal', 'DoubleLetter', 'Normal', 'Normal', 'TripleWord'],
  ['Normal', 'Normal', 'DoubleLetter', 'Normal', 'Normal', 'Normal', 'DoubleLetter', 'Normal', 'DoubleLetter', 'Normal', 'Normal', 'Normal', 'DoubleLetter', 'Normal', 'Normal'],
  ['Normal', 'TripleLetter', 'Normal', 'Normal', 'Normal', 'TripleLetter', 'Normal', 'Normal', 'Normal', 'TripleLetter', 'Normal', 'Normal', 'Normal', 'TripleLetter', 'Normal'],
  ['Normal', 'Normal', 'Normal', 'Normal', 'DoubleWord', 'Normal', 'Normal', 'Normal', 'Normal', 'Normal', 'DoubleWord', 'Normal', 'Normal', 'Normal', 'Normal'],
  ['DoubleLetter', 'Normal', 'Normal', 'DoubleWord', 'Normal', 'Normal', 'Normal', 'DoubleLetter', 'Normal', 'Normal', 'Normal', 'DoubleWord', 'Normal', 'Normal', 'DoubleLetter'],
  ['Normal', 'Normal', 'DoubleWord', 'Normal', 'Normal', 'Normal', 'DoubleLetter', 'Normal', 'DoubleLetter', 'Normal', 'Normal', 'Normal', 'DoubleWord', 'Normal', 'Normal'],
  ['Normal', 'DoubleWord', 'Normal', 'Normal', 'Normal', 'TripleLetter', 'Normal', 'Normal', 'Normal', 'TripleLetter', 'Normal', 'Normal', 'Normal', 'DoubleWord', 'Normal'],
  ['TripleWord', 'Normal', 'Normal', 'DoubleLetter', 'Normal', 'Normal', 'Normal', 'TripleWord', 'Normal', 'Normal', 'Normal', 'DoubleLetter', 'Normal', 'Normal', 'TripleWord']
]

export const BOARD_SIZE = 15
