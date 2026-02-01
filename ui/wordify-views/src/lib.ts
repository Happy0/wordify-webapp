// Main library entry point
// Re-exports view mounting functions from the lib folder

// Round view
export {
  createRound,
  type RoundOptions,
  type RoundInstance
} from './lib/round'

// Backwards compatibility: createWordify is an alias for createRound
export { createRound as createWordify } from './lib/round'

// Create Game view
export {
  createCreateGame,
  type CreateGameOptions,
  type CreateGameInstance
} from './lib/create-game'

// Re-export types for TypeScript users
export type {
  GameState,
  TileInput,
  BlankTileInput,
  LetterTileInput,
  PlayerSummary,
  MoveSummary,
  ChatMessage,
  SquareType,
  PlacedTile
} from './types/game'

export type { ConnectionState, IGameCommandSender } from './services/interfaces'

// Export constants
export { BOARD_LAYOUT, BOARD_SIZE } from './types/game'
