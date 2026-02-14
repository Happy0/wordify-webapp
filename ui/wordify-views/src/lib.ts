// Main library entry point
// Re-exports view mounting functions from the lib folder

// Round view
export {
  createRound,
  type RoundOptions,
  type RoundInstance
} from './lib/round'

// Create Game view
export {
  createCreateGame,
  type CreateGameOptions,
  type CreateGameInstance
} from './lib/create-game'

// Game Lobby view
export {
  createGameLobby,
  type GameLobbyOptions,
  type GameLobbyInstance
} from './lib/game-lobby'

// Login view
export {
  createLogin,
  type LoginOptions,
  type LoginInstance
} from './lib/login'

// Home view
export {
  createHome,
  type HomeOptions,
  type HomeInstance,
  type GameSummary
} from './lib/home'

// Choose Username view
export {
  createChooseUsername,
  type ChooseUsernameOptions,
  type ChooseUsernameInstance
} from './lib/choose-username'

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

// Export board text representation utilities
export type { TileValueMap } from './common/tile-value-map'
export { fromBoardTextRepresentation } from './common/board-text-presentation'
