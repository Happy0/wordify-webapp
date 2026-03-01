// Game Controller - bridges the transport layer with the game store

import type {
  IGameCommandSender,
  IGameMessageHandler,
  IGameTransport
} from './interfaces'
import { tileToWire, positionTileToWire } from './interfaces'
import type { Position, Tile, PlayerSummary, PlacedTile } from '@/types/game'
import { BOARD_LAYOUT } from '@/types/game'
import { toBoardTextRepresentation } from '@/common/board-text-presentation'
import type {
  ServerMessage,
  WireTile,
  WirePositionTile,
  ClientMessage
} from '@/types/protocol'
import { useGameStore, generateTileId } from '@/stores/gameStore'

// Convert wire tile to internal tile format
function wireTileToTile(wireTile: WireTile, candidate: boolean = false): Tile {
  const isBlank = wireTile.value === 0
  if (isBlank) {
    return {
      type: 'blank',
      assigned: wireTile.letter === '_' ? null : wireTile.letter,
      candidate,
      id: generateTileId()
    }
  }
  return {
    type: 'letter',
    letter: wireTile.letter,
    value: wireTile.value,
    candidate,
    id: generateTileId()
  }
}

// Parse ISO 8601 timestamp to milliseconds since epoch
// Handles timestamps with nanosecond precision (e.g., "2026-01-26T20:14:35.303418622Z")
// by truncating to milliseconds for browser compatibility
function parseIsoTimestamp(isoString: string): number {
  // Truncate fractional seconds to 3 digits (milliseconds) for browser compatibility
  const truncated = isoString.replace(/(\.\d{3})\d+/, '$1')
  return new Date(truncated).getTime()
}

export class GameController implements IGameCommandSender, IGameMessageHandler {
  private transport: IGameTransport

  constructor(transport: IGameTransport) {
    this.transport = transport
    this.setupMessageHandling()
    this.setupReconnectParams()
  }

  private setupMessageHandling(): void {
    this.transport.onMessage((data) => {
      try {
        const message = JSON.parse(data) as ServerMessage
        this.handleServerMessage(message)
      } catch (e) {
        console.error('Failed to parse server message:', e)
      }
    })
  }

  private setupReconnectParams(): void {
    this.transport.setReconnectParamsGetter(() => {
      const store = useGameStore()
      return {
        lastChatMessageReceived: store.lastChatMessageReceived,
        lastDefinitionReceived: store.lastDefinitionReceived
      }
    })
  }

  private handleServerMessage(message: ServerMessage): void {
    switch (message.command) {
      case 'initialise':
        this.onInitialize({
          playerNumber: message.payload.playerNumber,
          playerMove: message.payload.playerMove,
          players: message.payload.players,
          rack: message.payload.rack,
          tilesRemaining: message.payload.tilesRemaining,
          connectionStatuses: message.payload.connectionStatuses,
          moveHistory: message.payload.moveCommands
        })
        break

      case 'boardMoveSuccess':
        this.onBoardMoveSuccess(message.payload.rack)
        break

      case 'exchangeMoveSuccess':
        this.onExchangeMoveSuccess(message.payload.rack)
        break

      case 'passMoveSuccess':
        this.onPassMoveSuccess()
        break

      case 'potentialScore':
        this.onPotentialScore(message.payload.potentialScore)
        break

      case 'error':
        this.onError(message.payload.error)
        break

      case 'playerBoardMove':
        this.onPlayerBoardMove({
          moveNumber: message.payload.moveNumber,
          placed: message.payload.placed,
          summary: message.payload.summary,
          players: message.payload.players,
          nowPlaying: message.payload.nowPlaying,
          tilesRemaining: message.payload.tilesRemaining
        })
        break

      case 'playerPassMove':
        this.onPlayerPassMove({
          moveNumber: message.payload.moveNumber,
          nowPlaying: message.payload.nowPlaying
        })
        break

      case 'playerExchangeMove':
        this.onPlayerExchangeMove({
          moveNumber: message.payload.moveNumber,
          nowPlaying: message.payload.nowPlaying
        })
        break

      case 'gameFinished':
        this.onGameFinished({
          moveNumber: message.payload.moveNumber,
          placed: message.payload.placed,
          summary: message.payload.summary as {
            type: 'gameEnd'
            players: { name: string; score: number; endBonus?: number }[]
            lastMoveScore: number | null
            wordsMade: { word: string; score: number }[]
          }
        })
        break

      case 'playerChat':
        this.onPlayerChat({
          player: message.payload.player,
          message: message.payload.message,
          when: message.payload.when,
          messageNumber: message.payload.messageNumber
        })
        break

      case 'playerConnect':
        this.onPlayerConnect({
          playerNumber: message.payload.playerNumber,
          when: message.payload.when
        })
        break

      case 'playerDisconnect':
        this.onPlayerDisconnect({
          playerNumber: message.payload.playerNumber,
          when: message.payload.when
        })
        break

      case 'wordDefinitions':
        this.onWordDefinitions({
          word: message.payload.word,
          when: message.payload.when,
          definitions: message.payload.definitions,
          definitionNumber: message.payload.definitionNumber
        })
        break

      case 'chatSuccess':
      case 'askDefinitionSuccess':
        // Acknowledgement only, no action needed
        break
    }
  }

  private send(message: ClientMessage): void {
    this.transport.send(JSON.stringify(message))
  }

  // IGameCommandSender implementation

  requestPotentialScore(placements: { pos: Position; tile: Tile }[]): void {
    this.send({
      command: 'potentialScore',
      payload: placements.map(p => positionTileToWire(p.pos, p.tile))
    })
  }

  requestDefinition(word: string): void {
    this.send({
      command: 'askDefinition',
      payload: { word }
    })
  }

  sendChatMessage(message: string): void {
    this.send({
      command: 'say',
      payload: { message }
    })
  }

  submitBoardMove(placements: { pos: Position; tile: Tile }[]): void {
    this.send({
      command: 'boardMove',
      payload: placements.map(p => positionTileToWire(p.pos, p.tile))
    })
  }

  submitExchangeMove(tiles: Tile[]): void {
    this.send({
      command: 'exchangeMove',
      payload: tiles.map(tileToWire)
    })
  }

  submitPassMove(): void {
    this.send({
      command: 'passMove',
      payload: null
    })
  }

  // IGameMessageHandler implementation

  onInitialize(data: {
    playerNumber: number
    playerMove: number
    players: { name: string; score: number; endBonus?: number }[]
    rack: WireTile[] | null
    tilesRemaining: number
    connectionStatuses: { playerNumber: number; active: boolean; lastSeen: string | null }[]
    moveHistory: unknown[]
  }): void {
    const store = useGameStore()

    // Create player summaries with connection status
    // Note: connectionStatuses.playerNumber is 1-based, so we compare with index + 1
    const playerSummaries: PlayerSummary[] = data.players.map((p, index) => {
      const connStatus = data.connectionStatuses.find(cs => cs.playerNumber === index + 1)
      return {
        name: p.name,
        score: p.score,
        endBonus: p.endBonus,
        connected: connStatus?.active ?? false,
        lastSeen: connStatus?.lastSeen ? parseIsoTimestamp(connStatus.lastSeen) : undefined
      }
    })

    // Build placedTiles from move history and check if game has ended
    // Wire format already uses 1-based coordinates, so we keep them as-is for placedTiles
    const placedTiles: PlacedTile[] = []
    let gameEnded = false
    for (const moveCmd of data.moveHistory) {
      const cmd = moveCmd as ServerMessage
      if (cmd.command === 'playerBoardMove') {
        for (const placed of cmd.payload.placed) {
          const tile = wireTileToTile(placed.tile, false)
          placedTiles.push({
            position: { x: placed.pos.x, y: placed.pos.y },
            tile
          })
        }
      } else if (cmd.command === 'gameFinished') {
        gameEnded = true
        // Also place tiles from the final move (if any - placed can be null for pass-ended games)
        if (cmd.payload.placed) {
          for (const placed of cmd.payload.placed) {
            const tile = wireTileToTile(placed.tile, false)
            placedTiles.push({
              position: { x: placed.pos.x, y: placed.pos.y },
              tile
            })
          }
        }
      }
    }

    // Convert rack (null for observers who are not participants in the game)
    const rack = data.rack ? data.rack.map(t => wireTileToTile(t, true)) : null

    // Preserve existing chat state on reconnect - the server only sends new messages
    // since the sequence numbers provided in the reconnection query parameters
    const existingChatMessages = store.chatMessages
    const existingLastChatMessageReceived = store.lastChatMessageReceived
    const existingLastDefinitionReceived = store.lastDefinitionReceived

    store.initializeGame({
      myPlayerNumber: data.playerNumber,
      playerToMove: data.playerMove,
      players: playerSummaries,
      moveHistory: [],
      tilesRemaining: data.tilesRemaining,
      potentialScore: null,
      lastMoveReceived: Date.now(),
      chatMessages: existingChatMessages,
      lastChatMessageReceived: existingLastChatMessageReceived,
      lastDefinitionReceived: existingLastDefinitionReceived,
      rack,
      boardLayout: BOARD_LAYOUT,
      boardString: toBoardTextRepresentation(placedTiles),
      gameEnded
    })

    // Process move history for the move history display
    // Track the last nowPlaying value for gameFinished handling
    let lastNowPlaying = data.playerMove
    for (const moveCmd of data.moveHistory) {
      const cmd = moveCmd as ServerMessage
      if (cmd.command === 'playerBoardMove' || cmd.command === 'playerPassMove' || cmd.command === 'playerExchangeMove') {
        lastNowPlaying = cmd.payload.nowPlaying
      }
      this.processMoveForHistory(cmd, lastNowPlaying, playerSummaries.length)
    }
  }

  private processMoveForHistory(cmd: ServerMessage, lastNowPlaying: number, numPlayers: number): void {
    const store = useGameStore()

    // Calculate which player made this move based on nowPlaying (who plays next)
    // nowPlaying is 1-based, we need 0-based index of the player who just moved
    const getPlayerIndex = (nowPlaying: number) => {
      return (nowPlaying - 2 + numPlayers) % numPlayers
    }

    if (cmd.command === 'playerBoardMove') {
      const playerIndex = getPlayerIndex(cmd.payload.nowPlaying)
      store.addMoveToHistory({
        type: 'boardMove',
        playerIndex,
        overallScore: cmd.payload.summary.overallScore,
        wordsMade: cmd.payload.summary.wordsMade
      })
    } else if (cmd.command === 'playerPassMove') {
      const playerIndex = getPlayerIndex(cmd.payload.nowPlaying)
      store.addMoveToHistory({
        type: 'pass',
        playerIndex
      })
    } else if (cmd.command === 'playerExchangeMove') {
      const playerIndex = getPlayerIndex(cmd.payload.nowPlaying)
      store.addMoveToHistory({
        type: 'exchange',
        playerIndex
      })
    } else if (cmd.command === 'gameFinished' && cmd.payload.placed?.length > 0) {
      // The player who made the final move is the one who was next to play (lastNowPlaying)
      const playerIndex = lastNowPlaying - 1
      store.addMoveToHistory({
        type: 'boardMove',
        playerIndex,
        overallScore: cmd.payload.summary.lastMoveScore ?? 0,
        wordsMade: cmd.payload.summary.wordsMade
      })
    }
  }

  onBoardMoveSuccess(rack: WireTile[]): void {
    const store = useGameStore()
    store.confirmCandidateTiles()
    store.updateRack(rack.map(t => wireTileToTile(t, true)))
    store.updatePotentialScore(null)
  }

  onExchangeMoveSuccess(rack: WireTile[]): void {
    const store = useGameStore()
    store.updateRack(rack.map(t => wireTileToTile(t, true)))
  }

  onPassMoveSuccess(): void {
    // Move confirmed, no additional action needed
  }

  onPotentialScore(score: number): void {
    const store = useGameStore()
    store.updatePotentialScore(score)
  }

  onError(error: string): void {
    const store = useGameStore()
    store.setError(error)
  }

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
  }): void {
    const store = useGameStore()

    // Place tiles on board (convert from 1-based wire format to 0-based)
    for (const placed of data.placed) {
      const tile = wireTileToTile(placed.tile, false)
      store.placeTileFromServerMove(placed.pos.y - 1, placed.pos.x - 1, tile)
    }

    // Update players with new scores
    const playerSummaries: PlayerSummary[] = data.players.map((p, index) => {
      const existingPlayer = store.players[index]
      return {
        name: p.name,
        score: p.score,
        endBonus: p.endBonus,
        connected: existingPlayer?.connected ?? false,
        lastSeen: existingPlayer?.lastSeen
      }
    })
    store.updatePlayers(playerSummaries)

    // Update game state
    store.updatePlayerToMove(data.nowPlaying)
    store.updateTilesRemaining(data.tilesRemaining)

    // Add to move history
    // Calculate which player made this move: nowPlaying is 1-based, we need 0-based index
    const numPlayers = store.players.length
    const playerIndex = (data.nowPlaying - 2 + numPlayers) % numPlayers
    store.addMoveToHistory({
      type: 'boardMove',
      playerIndex,
      overallScore: data.summary.overallScore,
      wordsMade: data.summary.wordsMade
    })
  }

  onPlayerPassMove(data: {
    moveNumber: number
    nowPlaying: number
  }): void {
    const store = useGameStore()
    store.updatePlayerToMove(data.nowPlaying)

    // Calculate which player made this move: nowPlaying is 1-based, we need 0-based index
    const numPlayers = store.players.length
    const playerIndex = (data.nowPlaying - 2 + numPlayers) % numPlayers
    store.addMoveToHistory({
      type: 'pass',
      playerIndex
    })
  }

  onPlayerExchangeMove(data: {
    moveNumber: number
    nowPlaying: number
  }): void {
    const store = useGameStore()
    store.updatePlayerToMove(data.nowPlaying)

    // Calculate which player made this move: nowPlaying is 1-based, we need 0-based index
    const numPlayers = store.players.length
    const playerIndex = (data.nowPlaying - 2 + numPlayers) % numPlayers
    store.addMoveToHistory({
      type: 'exchange',
      playerIndex
    })
  }

  onGameFinished(data: {
    moveNumber: number
    placed: WirePositionTile[] | null
    summary: {
      type: 'gameEnd'
      players: { name: string; score: number; endBonus?: number }[]
      lastMoveScore: number | null
      wordsMade: { word: string; score: number }[]
    }
  }): void {
    const store = useGameStore()

    // Place final tiles if any (convert from 1-based wire format to 0-based)
    // placed can be null when game ends due to consecutive passes
    if (data.placed) {
      for (const placed of data.placed) {
        const tile = wireTileToTile(placed.tile, false)
        store.placeTileFromServerMove(placed.pos.y - 1, placed.pos.x - 1, tile)
      }
    }

    // Update players with final scores and bonuses
    const playerSummaries: PlayerSummary[] = data.summary.players.map((p, index) => {
      const existingPlayer = store.players[index]
      return {
        name: p.name,
        score: p.score,
        endBonus: p.endBonus,
        connected: existingPlayer?.connected ?? false,
        lastSeen: existingPlayer?.lastSeen
      }
    })
    store.updatePlayers(playerSummaries)

    // Add final move to history if there was one
    if ((data.placed?.length ?? 0) > 0) {
      // The player who made the final move is the current playerToMove (1-based),
      // since it hasn't been updated yet. Convert to 0-based index.
      const playerIndex = store.playerToMove - 1
      store.addMoveToHistory({
        type: 'boardMove',
        playerIndex,
        overallScore: data.summary.lastMoveScore ?? 0,
        wordsMade: data.summary.wordsMade
      })
    }

    store.setGameEnded(true)
  }

  onPlayerChat(data: {
    player: string
    message: string
    when: string
    messageNumber: number
  }): void {
    const store = useGameStore()
    store.addChatMessage({
      type: 'text',
      user: data.player,
      text: data.message,
      when: data.when
    })
    store.updateLastChatMessageReceived(data.messageNumber)
  }

  onPlayerConnect(data: {
    playerNumber: number
    when: string
  }): void {
    const store = useGameStore()
    // Convert from 1-based player number to 0-based index
    store.setPlayerConnected(data.playerNumber - 1, true)
  }

  onPlayerDisconnect(data: {
    playerNumber: number
    when: string
  }): void {
    const store = useGameStore()
    const lastSeen = parseIsoTimestamp(data.when)
    // Convert from 1-based player number to 0-based index
    store.setPlayerConnected(data.playerNumber - 1, false, lastSeen)
  }

  onWordDefinitions(data: {
    word: string
    when: string
    definitions: { partOfSpeech: string; definition: string; example: string }[]
    definitionNumber: number
  }): void {
    const store = useGameStore()

    // Add only the first 2 definitions as chat messages to avoid flooding the chat
    const limitedDefinitions = data.definitions.slice(0, 2)
    for (const def of limitedDefinitions) {
      store.addChatMessage({
        type: 'definition',
        word: data.word,
        partOfSpeech: def.partOfSpeech,
        definition: def.definition,
        example: def.example,
        when: data.when
      })
    }

    // Update the last definition number received
    store.updateLastDefinitionReceived(data.definitionNumber)
  }

  // Connection management
  connect(url: string): void {
    this.transport.connect(url)
  }

  disconnect(): void {
    this.transport.disconnect()
  }
}
