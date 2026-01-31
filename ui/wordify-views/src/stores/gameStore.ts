import { defineStore } from 'pinia'
import { ref, computed } from 'vue'
import type {
  GameState,
  PlayerSummary,
  MoveSummary,
  ChatMessage,
  Tile,
  TileInput,
  BoardSquare,
  Position,
  SquareType,
  PlacedTile
} from '@/types/game'
import { BOARD_LAYOUT, BOARD_SIZE } from '@/types/game'

// Generate a unique ID for tiles
let tileIdCounter = 0
export function generateTileId(): string {
  return `tile-${++tileIdCounter}-${Date.now()}`
}

// Create an empty board from a layout
function createBoardFromLayout(layout: SquareType[][]): BoardSquare[][] {
  const board: BoardSquare[][] = []
  for (let row = 0; row < BOARD_SIZE; row++) {
    const rowSquares: BoardSquare[] = []
    for (let col = 0; col < BOARD_SIZE; col++) {
      const layoutType = layout[row]?.[col]
      if (layoutType) {
        rowSquares.push({
          type: layoutType,
          tile: null,
          row,
          col
        })
      }
    }
    board.push(rowSquares)
  }
  return board
}

// Create an empty board with the classic layout (for default initialization)
function createEmptyBoard(): BoardSquare[][] {
  return createBoardFromLayout(BOARD_LAYOUT)
}

// Convert input tile to internal tile with id and candidate
function toInternalTile(input: TileInput, candidate: boolean): Tile {
  return {
    ...input,
    candidate,
    id: generateTileId()
  }
}

// Place tiles on the board from the sparse placedTiles array
// Positions in placedTiles are 1-based and need to be converted to 0-based
function placeTilesOnBoard(board: BoardSquare[][], placedTiles: PlacedTile[]): void {
  for (const placed of placedTiles) {
    // Convert from 1-based to 0-based coordinates
    const row = placed.position.y - 1
    const col = placed.position.x - 1
    const boardRow = board[row]
    if (boardRow) {
      const square = boardRow[col]
      if (square) {
        // Placed tiles are not candidates (already played)
        square.tile = toInternalTile(placed.tile, false)
      }
    }
  }
}

export const useGameStore = defineStore('game', () => {
  // State
  const myPlayerNumber = ref<number>(0)
  const playerToMove = ref<number>(0)
  const players = ref<PlayerSummary[]>([])
  const moveHistory = ref<MoveSummary[]>([])
  const tilesRemaining = ref<number>(100)
  const potentialScore = ref<number | null>(null)
  const lastMoveReceived = ref<number>(Date.now())
  const chatMessages = ref<ChatMessage[]>([])
  const lastChatMessageReceived = ref<number>(0)
  const lastDefinitionReceived = ref<number>(0)
  const rack = ref<Tile[]>([])
  const board = ref<BoardSquare[][]>(createEmptyBoard())
  const gameEnded = ref<boolean>(false)
  const lastError = ref<string | null>(null)
  const gameId = ref<string | null>(null)

  // Getters
  const isMyTurn = computed(() => myPlayerNumber.value === playerToMove.value)

  const myPlayer = computed(() => players.value[myPlayerNumber.value])

  const currentPlayerName = computed(() => {
    const player = players.value[playerToMove.value]
    return player?.name ?? 'Unknown'
  })

  const candidateTilesOnBoard = computed(() => {
    const candidates: { tile: Tile; row: number; col: number }[] = []
    for (let row = 0; row < BOARD_SIZE; row++) {
      for (let col = 0; col < BOARD_SIZE; col++) {
        const square = board.value[row]?.[col]
        if (square?.tile?.candidate) {
          candidates.push({ tile: square.tile, row, col })
        }
      }
    }
    return candidates
  })

  // Actions
  function initializeGame(state: GameState) {
    myPlayerNumber.value = state.myPlayerNumber
    playerToMove.value = state.playerToMove
    players.value = state.players
    moveHistory.value = state.moveHistory
    tilesRemaining.value = state.tilesRemaining
    potentialScore.value = state.potentialScore
    lastMoveReceived.value = state.lastMoveReceived
    chatMessages.value = state.chatMessages
    lastChatMessageReceived.value = state.lastChatMessageReceived
    lastDefinitionReceived.value = state.lastDefinitionReceived
    // Convert input tiles to internal tiles (rack tiles are candidates)
    // Rack will be null for observers who are not participants in the game
    rack.value = state.rack ? state.rack.map(tile => toInternalTile(tile, true)) : []
    gameEnded.value = state.gameEnded

    // Construct the board from boardLayout and placedTiles
    const newBoard = createBoardFromLayout(state.boardLayout)
    placeTilesOnBoard(newBoard, state.placedTiles)
    board.value = newBoard
  }

  function updatePlayers(newPlayers: PlayerSummary[]) {
    players.value = newPlayers
  }

  function updatePlayerToMove(playerIndex: number) {
    playerToMove.value = playerIndex
  }

  function updateTilesRemaining(count: number) {
    tilesRemaining.value = count
  }

  function updateRack(newRack: Tile[]) {
    rack.value = newRack
  }

  function updatePotentialScore(score: number | null) {
    potentialScore.value = score
  }

  function addMoveToHistory(move: MoveSummary) {
    moveHistory.value.push(move)
    lastMoveReceived.value = Date.now()
  }

  function addChatMessage(message: ChatMessage) {
    chatMessages.value.push(message)
  }

  function updateLastChatMessageReceived(num: number) {
    lastChatMessageReceived.value = num
  }

  function updateLastDefinitionReceived(num: number) {
    lastDefinitionReceived.value = num
  }

  function setPlayerConnected(playerIndex: number, connected: boolean, lastSeen?: number) {
    const player = players.value[playerIndex]
    if (player) {
      player.connected = connected
      player.lastSeen = lastSeen
    }
  }

  function placeTileOnBoard(tile: Tile, row: number, col: number) {
    const boardRow = board.value[row]
    if (boardRow) {
      const square = boardRow[col]
      if (square && !square.tile) {
        square.tile = { ...tile, candidate: true }
      }
    }
  }

  function removeTileFromBoard(row: number, col: number): Tile | null {
    const boardRow = board.value[row]
    if (boardRow) {
      const square = boardRow[col]
      if (square?.tile?.candidate) {
        const tile = square.tile
        square.tile = null
        return tile
      }
    }
    return null
  }

  function placeTileFromServerMove(row: number, col: number, tile: Tile) {
    const boardRow = board.value[row]
    if (boardRow) {
      const square = boardRow[col]
      if (square) {
        square.tile = { ...tile, candidate: false }
      }
    }
  }

  function addTileToRack(tile: Tile) {
    if (rack.value.length < 7) {
      rack.value.push(tile)
    }
  }

  function removeTileFromRack(tileId: string): Tile | null {
    const index = rack.value.findIndex(t => t.id === tileId)
    if (index !== -1) {
      const [tile] = rack.value.splice(index, 1)
      return tile ?? null
    }
    return null
  }

  function shuffleRack() {
    const shuffled = [...rack.value]
    for (let i = shuffled.length - 1; i > 0; i--) {
      const j = Math.floor(Math.random() * (i + 1))
      const temp = shuffled[i]
      const temp2 = shuffled[j]
      if (temp && temp2) {
        shuffled[i] = temp2
        shuffled[j] = temp
      }
    }
    rack.value = shuffled
  }

  function recallTilesToRack() {
    // Find all candidate tiles on the board and return them to the rack
    for (let row = 0; row < BOARD_SIZE; row++) {
      for (let col = 0; col < BOARD_SIZE; col++) {
        const square = board.value[row]?.[col]
        if (square?.tile?.candidate) {
          rack.value.push({ ...square.tile, candidate: true })
          square.tile = null
        }
      }
    }
    potentialScore.value = null
  }

  function confirmCandidateTiles() {
    // Mark all candidate tiles as permanent (after successful move)
    for (let row = 0; row < BOARD_SIZE; row++) {
      for (let col = 0; col < BOARD_SIZE; col++) {
        const square = board.value[row]?.[col]
        if (square?.tile?.candidate) {
          square.tile.candidate = false
        }
      }
    }
  }

  function assignBlankTile(tileId: string, letter: string) {
    // Check rack first
    const rackTile = rack.value.find(t => t.id === tileId)
    if (rackTile && rackTile.type === 'blank') {
      rackTile.assigned = letter
      return
    }
    // Check board
    for (let row = 0; row < BOARD_SIZE; row++) {
      for (let col = 0; col < BOARD_SIZE; col++) {
        const square = board.value[row]?.[col]
        if (square?.tile?.id === tileId && square.tile.type === 'blank') {
          square.tile.assigned = letter
          return
        }
      }
    }
  }

  function setGameEnded(ended: boolean) {
    gameEnded.value = ended
  }

  function setError(error: string | null) {
    lastError.value = error
  }

  function clearError() {
    lastError.value = null
  }

  function setGameId(id: string) {
    gameId.value = id
  }

  // Get candidate tiles positioned for a move
  function getCandidateTilePositions(): { pos: Position; tile: Tile }[] {
    const positions: { pos: Position; tile: Tile }[] = []
    for (let row = 0; row < BOARD_SIZE; row++) {
      for (let col = 0; col < BOARD_SIZE; col++) {
        const square = board.value[row]?.[col]
        if (square?.tile?.candidate) {
          positions.push({
            pos: { x: col, y: row },
            tile: square.tile
          })
        }
      }
    }
    return positions
  }

  return {
    // State
    myPlayerNumber,
    playerToMove,
    players,
    moveHistory,
    tilesRemaining,
    potentialScore,
    lastMoveReceived,
    chatMessages,
    lastChatMessageReceived,
    lastDefinitionReceived,
    rack,
    board,
    gameEnded,
    lastError,
    gameId,

    // Getters
    isMyTurn,
    myPlayer,
    currentPlayerName,
    candidateTilesOnBoard,

    // Actions
    initializeGame,
    updatePlayers,
    updatePlayerToMove,
    updateTilesRemaining,
    updateRack,
    updatePotentialScore,
    addMoveToHistory,
    addChatMessage,
    updateLastChatMessageReceived,
    updateLastDefinitionReceived,
    setPlayerConnected,
    placeTileOnBoard,
    removeTileFromBoard,
    placeTileFromServerMove,
    addTileToRack,
    removeTileFromRack,
    shuffleRack,
    recallTilesToRack,
    confirmCandidateTiles,
    assignBlankTile,
    setGameEnded,
    setError,
    clearError,
    setGameId,
    getCandidateTilePositions
  }
})
