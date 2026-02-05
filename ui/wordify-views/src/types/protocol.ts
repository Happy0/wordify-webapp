// WebSocket Protocol Types
// Based on websocket-protocol-spec.md

import type { Position } from './game'

// Wire format for tiles from the server
export type WireTile = {
  letter: string
  value: number
}

export type WirePositionTile = {
  pos: Position
  tile: WireTile
}

// Client -> Server Messages

export type PotentialScoreCommand = {
  command: 'potentialScore'
  payload: WirePositionTile[]
}

export type AskDefinitionCommand = {
  command: 'askDefinition'
  payload: {
    word: string
  }
}

export type SayCommand = {
  command: 'say'
  payload: {
    message: string
  }
}

export type BoardMoveCommand = {
  command: 'boardMove'
  payload: WirePositionTile[]
}

export type ExchangeMoveCommand = {
  command: 'exchangeMove'
  payload: WireTile[]
}

export type PassMoveCommand = {
  command: 'passMove'
  payload: null
}

export type ClientMessage =
  | PotentialScoreCommand
  | AskDefinitionCommand
  | SayCommand
  | BoardMoveCommand
  | ExchangeMoveCommand
  | PassMoveCommand

// Server -> Client Messages

export type WirePlayer = {
  name: string
  score: number
  endBonus?: number
}

export type WireConnectionStatus = {
  playerNumber: number
  active: boolean
  lastSeen: string | null
}

export type WireMoveSummary = {
  type: 'board'
  overallScore: number
  direction: 'horizontal' | 'vertical'
  placed: Position[]
  wordsMade: {
    word: string
    score: number
  }[]
} | {
  type: 'pass'
} | {
  type: 'exchange'
} | {
  type: 'gameEnd'
  players: WirePlayer[]
  // lastMoveScore can be null when game ends due to consecutive passes
  lastMoveScore: number | null
  wordsMade: {
    word: string
    score: number
  }[]
}

export type InitialisePayload = {
  moveCommands: ServerMessage[]
  // The player's tile rack, or null if the player is an observer (not a participant in the game)
  rack: WireTile[] | null
  players: WirePlayer[]
  playerNumber: number
  playerMove: number
  tilesRemaining: number
  connectionStatuses: WireConnectionStatus[]
  appVersion: number
}

export type InitialiseMessage = {
  command: 'initialise'
  payload: InitialisePayload
}

export type BoardMoveSuccessMessage = {
  command: 'boardMoveSuccess'
  payload: {
    rack: WireTile[]
  }
}

export type ExchangeMoveSuccessMessage = {
  command: 'exchangeMoveSuccess'
  payload: {
    rack: WireTile[]
  }
}

export type PassMoveSuccessMessage = {
  command: 'passMoveSuccess'
  payload: Record<string, never>
}

export type ChatSuccessMessage = {
  command: 'chatSuccess'
  payload: Record<string, never>
}

export type AskDefinitionSuccessMessage = {
  command: 'askDefinitionSuccess'
  payload: Record<string, never>
}

export type PotentialScoreMessage = {
  command: 'potentialScore'
  payload: {
    potentialScore: number
  }
}

export type ErrorMessage = {
  command: 'error'
  payload: {
    error: string
  }
}

// Broadcast messages

export type PlayerBoardMovePayload = {
  moveNumber: number
  placed: WirePositionTile[]
  summary: WireMoveSummary & { type: 'board' }
  players: WirePlayer[]
  nowPlaying: number
  tilesRemaining: number
}

export type PlayerBoardMoveMessage = {
  command: 'playerBoardMove'
  payload: PlayerBoardMovePayload
}

export type PlayerPassMoveMessage = {
  command: 'playerPassMove'
  payload: {
    moveNumber: number
    nowPlaying: number
    summary: {
      type: 'pass'
    }
  }
}

export type PlayerExchangeMoveMessage = {
  command: 'playerExchangeMove'
  payload: {
    moveNumber: number
    nowPlaying: number
    summary: {
      type: 'exchange'
    }
  }
}

export type GameFinishedMessage = {
  command: 'gameFinished'
  payload: {
    moveNumber: number
    placed: WirePositionTile[]
    summary: WireMoveSummary & { type: 'gameEnd' }
  }
}

export type PlayerChatMessage = {
  command: 'playerChat'
  payload: {
    player: string
    message: string
    when: string
    messageNumber: number
  }
}

export type PlayerConnectMessage = {
  command: 'playerConnect'
  payload: {
    playerNumber: number
    when: string
  }
}

export type PlayerDisconnectMessage = {
  command: 'playerDisconnect'
  payload: {
    playerNumber: number
    when: string
  }
}

export type WordDefinitionEntry = {
  partOfSpeech: string
  definition: string
  example: string
}

export type WordDefinitionsMessage = {
  command: 'wordDefinitions'
  payload: {
    word: string
    when: string
    definitions: WordDefinitionEntry[]
    definitionNumber: number
  }
}

export type ServerMessage =
  | InitialiseMessage
  | BoardMoveSuccessMessage
  | ExchangeMoveSuccessMessage
  | PassMoveSuccessMessage
  | ChatSuccessMessage
  | AskDefinitionSuccessMessage
  | PotentialScoreMessage
  | ErrorMessage
  | PlayerBoardMoveMessage
  | PlayerPassMoveMessage
  | PlayerExchangeMoveMessage
  | GameFinishedMessage
  | PlayerChatMessage
  | PlayerConnectMessage
  | PlayerDisconnectMessage
  | WordDefinitionsMessage
