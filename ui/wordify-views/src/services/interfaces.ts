// Abstract interfaces for game communication
// These allow swapping out WebSocket for another technology or testing

import type { Position, Tile } from '@/types/game'
import type { WireTile, WirePositionTile } from '@/types/protocol'

// Tile conversion utilities
export function tileToWire(tile: Tile): WireTile {
  if (tile.type === 'blank') {
    return {
      letter: tile.assigned ?? '_',
      value: 0
    }
  }
  return {
    letter: tile.letter,
    value: tile.value
  }
}

export function positionTileToWire(pos: Position, tile: Tile): WirePositionTile {
  return {
    // Convert from 0-based internal coordinates to 1-based wire format
    pos: { x: pos.x + 1, y: pos.y + 1 },
    tile: tileToWire(tile)
  }
}

// Interface for sending game commands to the server
export interface IGameCommandSender {
  // Connection management
  connect(url: string): void
  disconnect(): void

  // Request potential score for placed tiles
  requestPotentialScore(placements: { pos: Position; tile: Tile }[]): void

  // Request a word definition
  requestDefinition(word: string): void

  // Send a chat message
  sendChatMessage(message: string): void

  // Submit a board move
  submitBoardMove(placements: { pos: Position; tile: Tile }[]): void

  // Submit an exchange move
  submitExchangeMove(tiles: Tile[]): void

  // Submit a pass move
  submitPassMove(): void
}

// Interface for handling messages from the server
// The game controller implements this interface
export interface IGameMessageHandler {
  // Called when the game is initialized
  onInitialize(data: {
    playerNumber: number
    playerMove: number
    players: { name: string; score: number; endBonus?: number }[]
    rack: WireTile[]
    tilesRemaining: number
    connectionStatuses: { playerNumber: number; active: boolean; lastSeen: string | null }[]
    moveHistory: unknown[]
  }): void

  // Called when a board move is successful
  onBoardMoveSuccess(rack: WireTile[]): void

  // Called when an exchange move is successful
  onExchangeMoveSuccess(rack: WireTile[]): void

  // Called when a pass move is successful
  onPassMoveSuccess(): void

  // Called when potential score is received
  onPotentialScore(score: number): void

  // Called when an error occurs
  onError(error: string): void

  // Called when a player makes a board move
  onPlayerBoardMove(data: {
    moveNumber: number
    placed: WirePositionTile[]
    summary: {
      type: 'board'
      overallScore: number
      wordsMade: { word: string; score: number }[]
    }
    players: { name: string; score: number; endBonus?: number }[]
    nowPlaying: number
    tilesRemaining: number
  }): void

  // Called when a player passes
  onPlayerPassMove(data: {
    moveNumber: number
    nowPlaying: number
  }): void

  // Called when a player exchanges tiles
  onPlayerExchangeMove(data: {
    moveNumber: number
    nowPlaying: number
  }): void

  // Called when the game ends
  onGameFinished(data: {
    moveNumber: number
    placed: WirePositionTile[]
    summary: {
      type: 'gameEnd'
      players: { name: string; score: number; endBonus?: number }[]
      lastMoveScore: number
      wordsMade: { word: string; score: number }[]
    }
  }): void

  // Called when a chat message is received
  onPlayerChat(data: {
    player: string
    message: string
    when: string
    messageNumber: number
  }): void

  // Called when a player connects
  onPlayerConnect(data: {
    playerNumber: number
    when: string
  }): void

  // Called when a player disconnects
  onPlayerDisconnect(data: {
    playerNumber: number
    when: string
  }): void

  // Called when word definitions are received
  onWordDefinitions(data: {
    word: string
    when: string
    definitions: { partOfSpeech: string; definition: string; example: string }[]
    definitionNumber: number
  }): void
}

// Connection state
export type ConnectionState = 'disconnected' | 'connecting' | 'connected' | 'error'

// Parameters for websocket reconnection
export type ReconnectParams = {
  lastChatMessageReceived: number
  lastDefinitionReceived: number
}

// Interface for the transport layer
export interface IGameTransport {
  connect(url: string): void
  disconnect(): void
  send(message: string): void
  onMessage(handler: (message: string) => void): void
  onStateChange(handler: (state: ConnectionState) => void): void
  getState(): ConnectionState
  // Set a function that returns reconnection parameters (last message numbers)
  setReconnectParamsGetter(getter: () => ReconnectParams): void
}
