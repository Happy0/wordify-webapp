<script setup lang="ts">
import { ref, watch, onMounted, onUnmounted, provide, computed, inject } from 'vue'
import { useGameStore } from '@/stores/gameStore'
import { storeToRefs } from 'pinia'
import { useGameController } from '@/composables/useGameController'
import { useTouchDragDrop } from '@/composables/useTouchDragDrop'
import type { GameState, Tile } from '@/types/game'
import { BOARD_SIZE } from '@/types/game'

// Components
import GameBoard from '@/components/round/GameBoard.vue'
import TileRack from '@/components/round/TileRack.vue'
import MoveControls from '@/components/round/MoveControls.vue'
import ScoreBoard from '@/components/round/ScoreBoard.vue'
import MoveHistory from '@/components/round/MoveHistory.vue'
import GameChat from '@/components/round/GameChat.vue'
import BlankTileSelector from '@/components/round/BlankTileSelector.vue'
import PotentialScore from '@/components/round/PotentialScore.vue'
import Button from 'primevue/button'
import Toast from 'primevue/toast'
import { useToast } from 'primevue/usetoast'
import NavigationButton from '@/components/common/NavigationButton.vue'

// Props for SSR initialization
const props = defineProps<{
  initialState?: GameState
  websocketUrl?: string
}>()

const store = useGameStore()
const { lastError, candidateTilesOnBoard, lastChatMessageReceived, gameId, isMyTurn, gameEnded } = storeToRefs(store)
const { controller, connect, connectionState } = useGameController()
const toast = useToast()

// Inject isLoggedIn from the app-level provider
const isLoggedIn = inject<boolean>('isLoggedIn', false)

// Unread chat message tracking (mobile only)
const lastSeenChatMessage = ref<number>(0)

function getStorageKey(): string | null {
  if (!gameId.value) return null
  return `wordify_lastSeenChat_${gameId.value}`
}

function loadLastSeenChatMessage() {
  const key = getStorageKey()
  if (!key) return
  try {
    const stored = localStorage.getItem(key)
    if (stored) {
      lastSeenChatMessage.value = parseInt(stored, 10) || 0
    }
  } catch {
    // localStorage may not be available
  }
}

function saveLastSeenChatMessage() {
  const key = getStorageKey()
  if (!key) return
  try {
    localStorage.setItem(key, lastChatMessageReceived.value.toString())
    lastSeenChatMessage.value = lastChatMessageReceived.value
  } catch {
    // localStorage may not be available
  }
}

const hasUnreadMessages = computed(() => {
  // Only show unread indicator if we have a gameId for proper tracking
  if (!gameId.value) return false
  return lastChatMessageReceived.value > lastSeenChatMessage.value
})

// Mobile panel state
const activeMobilePanel = ref<'none' | 'scores' | 'history' | 'chat'>('none')

// Blank tile selector state
const showBlankSelector = ref(false)
const selectedBlankTileId = ref<string | null>(null)

// Exchange dialog state (exposed from MoveControls via ref)
const moveControlsRef = ref<InstanceType<typeof MoveControls> | null>(null)

// Drag state for coordinating between rack and board
const draggingTileId = ref<string | null>(null)
const draggingFromBoard = ref<{ row: number; col: number } | null>(null)

// Watch for errors
watch(lastError, (error) => {
  if (error) {
    toast.add({
      severity: 'error',
      summary: 'Error',
      detail: error,
      life: 5000
    })
    store.clearError()
  }
})

// Watch for reconnection success (only show toast after a genuine disconnection, not on initial load)
const hasBeenConnected = ref(false)
const wasDisconnected = ref(false)

watch(connectionState, (newState) => {
  if (newState === 'connected') {
    if (wasDisconnected.value) {
      toast.add({
        severity: 'success',
        summary: 'Connected',
        detail: 'Connection restored',
        life: 3000
      })
    }
    hasBeenConnected.value = true
    wasDisconnected.value = false
  }

  // Only mark as disconnected if we were previously connected
  if (hasBeenConnected.value && (newState === 'disconnected' || newState === 'error')) {
    wasDisconnected.value = true
  }
})

// Initialize with state if provided (SSR scenario)
onMounted(() => {
  if (props.initialState) {
    store.initializeGame(props.initialState)
  }

  if (props.websocketUrl) {
    connect(props.websocketUrl)
  }

  // Load last seen chat message from localStorage
  loadLastSeenChatMessage()
})

// Watch for gameId changes to load the correct last seen value
watch(gameId, () => {
  loadLastSeenChatMessage()
})

// Update document title based on game state
const BASE_TITLE = 'Wordify'
watch([gameEnded, isMyTurn], ([ended, myTurn]) => {
  if (ended) {
    document.title = `${BASE_TITLE} | Game Over`
  } else {
    document.title = myTurn ? `${BASE_TITLE} | Your Move` : BASE_TITLE
  }
}, { immediate: true })

// Push history state when exchange dialog opens (for mobile back button)
watch(
  () => moveControlsRef.value?.exchangeMode,
  (isOpen) => {
    if (isOpen) {
      history.pushState({ dialog: 'exchange' }, '')
    }
  }
)

// Watch for tile placements and request potential score
watch(
  candidateTilesOnBoard,
  (candidates) => {
    if (candidates.length > 0) {
      const placements = candidates.map(c => ({
        pos: { x: c.col, y: c.row },
        tile: c.tile
      }))
      controller.requestPotentialScore(placements)
    }
  },
  { deep: true }
)

// Handlers
function handleTileStartDrag(tileId: string) {
  draggingTileId.value = tileId
}

function handleTileEndDrag() {
  draggingTileId.value = null
  draggingFromBoard.value = null
}

function handleBlankTileClick(tileId: string) {
  selectedBlankTileId.value = tileId
  showBlankSelector.value = true
  history.pushState({ dialog: 'blankSelector' }, '')
}

function handleBlankLetterSelect(letter: string) {
  if (selectedBlankTileId.value) {
    store.assignBlankTile(selectedBlankTileId.value, letter)
    selectedBlankTileId.value = null
  }
}

function handleSubmit() {
  const placements = store.getCandidateTilePositions()
  if (placements.length > 0) {
    controller.submitBoardMove(placements)
  }
}

function handleExchange(tileIds: string[]) {
  const tiles = tileIds
    .map(id => store.rack.find(t => t.id === id))
    .filter((t): t is Tile => t !== undefined)

  if (tiles.length > 0) {
    controller.submitExchangeMove(tiles)
  }
}

function handlePass() {
  controller.submitPassMove()
}

function handleSendMessage(message: string) {
  controller.sendChatMessage(message)
}

function handleRequestDefinition(word: string) {
  controller.requestDefinition(word)
}

// Mobile panel handlers
function openMobilePanel(panel: 'scores' | 'history' | 'chat') {
  activeMobilePanel.value = panel
  // Push history state so browser back button closes the panel
  history.pushState({ mobilePanel: panel }, '')
}

function closeMobilePanel() {
  // Mark chat as read when closing the chat panel (not when opening)
  // This ensures messages received while viewing are marked as read
  if (activeMobilePanel.value === 'chat') {
    saveLastSeenChatMessage()
  }
  activeMobilePanel.value = 'none'
}

// Handle browser back button for mobile panels and dialogs
function handlePopState(_: PopStateEvent) {
  // If the blank tile selector is open, close it
  if (showBlankSelector.value) {
    showBlankSelector.value = false
    return
  }

  // If the exchange dialog is open, close it
  if (moveControlsRef.value?.exchangeMode) {
    moveControlsRef.value.exchangeMode = false
    return
  }

  // If any mobile panel is open, close it instead of navigating back
  if (activeMobilePanel.value !== 'none') {
    closeMobilePanel()
  }
}

onMounted(() => {
  window.addEventListener('popstate', handlePopState)
})

onUnmounted(() => {
  window.removeEventListener('popstate', handlePopState)
})

// Handle board tile drag start
function handleBoardTileDragStart(tileId: string, fromRow: number, fromCol: number) {
  draggingTileId.value = tileId
  draggingFromBoard.value = { row: fromRow, col: fromCol }
}

// Handle board tile drag end
function handleBoardTileDragEnd() {
  // If draggingFromBoard is still set, it means the tile was dropped outside
  // a valid board square - return it to the rack
  if (draggingFromBoard.value && draggingTileId.value) {
    const { row, col } = draggingFromBoard.value
    const tile = store.removeTileFromBoard(row, col)
    if (tile) {
      store.addTileToRack(tile)
    }
  }

  draggingTileId.value = null
  draggingFromBoard.value = null
}

// Handle board tile touch start (for touch drag from board)
function handleBoardTileTouchStart(event: TouchEvent, tileId: string, fromRow: number, fromCol: number) {
  draggingTileId.value = tileId
  draggingFromBoard.value = { row: fromRow, col: fromCol }

  // Use the touch drag composable from TileRack
  // Since we're using the same composable instance, the drop callback will be called
  const { handleTouchStart, setDropCallback, setCancelCallback, setTapCallback } = useTouchDragDrop()

  setDropCallback((row: number, col: number) => {
    handleTouchTileDrop(row, col, tileId)
  })

  setCancelCallback(() => {
    // Tile was dropped outside a valid board square - return it to the rack
    if (draggingFromBoard.value) {
      const { row, col } = draggingFromBoard.value
      const tile = store.removeTileFromBoard(row, col)
      if (tile) {
        store.addTileToRack(tile)
      }
    }
    draggingTileId.value = null
    draggingFromBoard.value = null
  })

  // Handle tap on blank tile on board - open letter selector
  setTapCallback((tappedTileId: string) => {
    // Check if this is a blank tile that can be assigned
    const square = store.board.flat().find(sq => sq.tile?.id === tappedTileId)
    if (square?.tile?.type === 'blank' && square.tile.candidate) {
      handleBlankTileClick(tappedTileId)
    }
    draggingTileId.value = null
    draggingFromBoard.value = null
  })

  handleTouchStart(event, tileId)
}

// Handle dropping tile on board (from mouse drag)
function handleBoardDrop(row: number, col: number) {
  if (!draggingTileId.value) return

  if (draggingFromBoard.value) {
    // Moving from board to board
    const { row: fromRow, col: fromCol } = draggingFromBoard.value

    // Don't do anything if dropping on the same square
    if (fromRow === row && fromCol === col) {
      draggingTileId.value = null
      draggingFromBoard.value = null
      return
    }

    // Check if destination has a candidate tile (swap)
    const destRow = store.board[row]
    const destSquare = destRow?.[col]

    if (destSquare?.tile && !destSquare.tile.candidate) {
      // Square has a permanent tile - can't drop here, return tile to rack
      const tile = store.removeTileFromBoard(fromRow, fromCol)
      if (tile) {
        store.addTileToRack(tile)
      }
    } else if (destSquare?.tile?.candidate) {
      // Swap tiles
      const destTile = store.removeTileFromBoard(row, col)
      const srcTile = store.removeTileFromBoard(fromRow, fromCol)

      if (srcTile) {
        store.placeTileOnBoard(srcTile, row, col)
      }
      if (destTile) {
        store.placeTileOnBoard(destTile, fromRow, fromCol)
      }
    } else {
      // Move to empty square
      const tile = store.removeTileFromBoard(fromRow, fromCol)
      if (tile) {
        store.placeTileOnBoard(tile, row, col)
      }
    }
  } else {
    // Moving from rack to board
    // Check if destination square is available before removing from rack
    const destRow = store.board[row]
    const destSquare = destRow?.[col]

    if (destSquare?.tile && !destSquare.tile.candidate) {
      // Square has a permanent tile - can't drop here, cancel the drop
      draggingTileId.value = null
      draggingFromBoard.value = null
      return
    }

    const tile = store.removeTileFromRack(draggingTileId.value)
    if (tile) {
      if (destSquare?.tile?.candidate) {
        // Swap with existing candidate tile
        const existingTile = store.removeTileFromBoard(row, col)
        store.placeTileOnBoard(tile, row, col)
        if (existingTile) {
          store.addTileToRack(existingTile)
        }
      } else {
        // Place on empty square
        store.placeTileOnBoard(tile, row, col)
      }
    }
  }

  draggingTileId.value = null
  draggingFromBoard.value = null
}

// Handle dropping tile on board (from touch drag via TileRack or Board)
function handleTouchTileDrop(row: number, col: number, tileId: string) {
  if (draggingFromBoard.value) {
    // Moving from board to board
    const { row: fromRow, col: fromCol } = draggingFromBoard.value

    // Don't do anything if dropping on the same square
    if (fromRow === row && fromCol === col) {
      draggingTileId.value = null
      draggingFromBoard.value = null
      return
    }

    // Check if destination has a candidate tile (swap)
    const destRow = store.board[row]
    const destSquare = destRow?.[col]

    if (destSquare?.tile && !destSquare.tile.candidate) {
      // Square has a permanent tile - can't drop here, return tile to rack
      const tile = store.removeTileFromBoard(fromRow, fromCol)
      if (tile) {
        store.addTileToRack(tile)
      }
    } else if (destSquare?.tile?.candidate) {
      // Swap tiles
      const destTile = store.removeTileFromBoard(row, col)
      const srcTile = store.removeTileFromBoard(fromRow, fromCol)

      if (srcTile) {
        store.placeTileOnBoard(srcTile, row, col)
      }
      if (destTile) {
        store.placeTileOnBoard(destTile, fromRow, fromCol)
      }
    } else {
      // Move to empty square
      const tile = store.removeTileFromBoard(fromRow, fromCol)
      if (tile) {
        store.placeTileOnBoard(tile, row, col)
      }
    }
  } else {
    // Moving from rack to board
    // Check if destination square is available before removing from rack
    const destRow = store.board[row]
    const destSquare = destRow?.[col]

    if (destSquare?.tile && !destSquare.tile.candidate) {
      // Square has a permanent tile - can't drop here, cancel the drop
      draggingTileId.value = null
      draggingFromBoard.value = null
      return
    }

    const tile = store.removeTileFromRack(tileId)
    if (tile) {
      if (destSquare?.tile?.candidate) {
        // Swap with existing candidate tile
        const existingTile = store.removeTileFromBoard(row, col)
        store.placeTileOnBoard(tile, row, col)
        if (existingTile) {
          store.addTileToRack(existingTile)
        }
      } else {
        // Place on empty square
        store.placeTileOnBoard(tile, row, col)
      }
    }
  }

  draggingTileId.value = null
  draggingFromBoard.value = null
}

// Handle dropping tile back on rack
function handleRackDrop() {
  if (!draggingTileId.value) return

  if (draggingFromBoard.value) {
    // Moving from board to rack - we know the exact position
    const { row, col } = draggingFromBoard.value
    const tile = store.removeTileFromBoard(row, col)
    if (tile) {
      store.addTileToRack(tile)
    }
  } else {
    // Check if tile is on board (fallback for other cases)
    for (let row = 0; row < BOARD_SIZE; row++) {
      for (let col = 0; col < BOARD_SIZE; col++) {
        const boardRow = store.board[row]
        if (boardRow) {
          const square = boardRow[col]
          if (square?.tile?.id === draggingTileId.value && square.tile.candidate) {
            const tile = store.removeTileFromBoard(row, col)
            if (tile) {
              store.addTileToRack(tile)
            }
            break
          }
        }
      }
    }
  }

  draggingTileId.value = null
  draggingFromBoard.value = null
}

// Provide drag state to child components
provide('draggingTileId', draggingTileId)
provide('onBoardDrop', handleBoardDrop)
provide('onRackDrop', handleRackDrop)
</script>

<template>
  <div class="game-view h-dvh flex flex-col bg-stone-100 overflow-hidden">
    <Toast />
    <!-- Desktop: Fixed FAB navigation -->
    <NavigationButton :is-logged-in="isLoggedIn" class="hidden lg:block" />

    <!-- Connection status banner (only show after initial connection) -->
    <Transition name="banner">
      <div
        v-if="hasBeenConnected && connectionState !== 'connected'"
        class="connection-banner fixed top-0 left-0 right-0 z-50 flex items-center justify-center gap-2 px-4 py-2 text-sm font-medium"
        :class="{
          'bg-yellow-100 text-yellow-800': connectionState === 'connecting',
          'bg-red-100 text-red-800': connectionState === 'disconnected' || connectionState === 'error'
        }"
      >
        <i
          class="pi"
          :class="{
            'pi-spin pi-spinner': connectionState === 'connecting',
            'pi-exclamation-triangle': connectionState === 'disconnected' || connectionState === 'error'
          }"
        />
        <span v-if="connectionState === 'connecting'">Reconnecting...</span>
        <span v-else>Connection lost. Reconnecting...</span>
      </div>
    </Transition>

    <!-- Desktop Layout -->
    <div class="hidden lg:flex flex-1 gap-4 p-4 min-h-0">
      <!-- Left sidebar: Scores + Potential Score + History -->
      <div class="w-72 flex flex-col gap-4 min-h-0">
        <ScoreBoard class="flex-shrink-0" />
        <PotentialScore class="flex-shrink-0" />
        <MoveHistory class="flex-1 min-h-0" />
      </div>

      <!-- Center: Board + Controls -->
      <div class="flex-1 flex flex-col items-center gap-4 min-h-0">
        <!-- Board -->
        <div class="flex-1 flex items-center justify-center min-h-0 w-full">
          <GameBoard
            class="max-h-full"
            @blank-tile-click="handleBlankTileClick"
            @tile-drop="handleBoardDrop"
            @board-tile-drag-start="handleBoardTileDragStart"
            @board-tile-drag-end="handleBoardTileDragEnd"
            @board-tile-touch-start="handleBoardTileTouchStart"
          />
        </div>

        <!-- Tile Rack + Controls -->
        <div class="flex-shrink-0 flex flex-col items-center gap-3">
          <TileRack
            @tile-start-drag="handleTileStartDrag"
            @tile-end-drag="handleTileEndDrag"
            @blank-tile-click="handleBlankTileClick"
            @tile-drop="handleTouchTileDrop"
          />
          <MoveControls
            @submit="handleSubmit"
            @exchange="handleExchange"
            @pass="handlePass"
          />
        </div>
      </div>

      <!-- Right sidebar: Chat -->
      <div class="w-80 min-h-0">
        <GameChat
          class="h-full"
          @send-message="handleSendMessage"
          @request-definition="handleRequestDefinition"
        />
      </div>
    </div>

    <!-- Mobile Layout -->
    <div class="lg:hidden flex flex-col flex-1 min-h-0">
      <!-- Mobile toolbar -->
      <div class="flex justify-between items-center px-2 py-2 bg-white shadow-sm">
        <div class="flex items-center gap-2">
          <NavigationButton :is-logged-in="isLoggedIn" position="inline" />
          <PotentialScore />
        </div>
        <div class="flex gap-2">
          <Button
            icon="pi pi-users"
            severity="secondary"
            size="small"
            rounded
            @click="openMobilePanel('scores')"
          />
          <Button
            icon="pi pi-history"
            severity="secondary"
            size="small"
            rounded
            @click="openMobilePanel('history')"
          />
          <div class="relative">
            <Button
              icon="pi pi-comments"
              :severity="hasUnreadMessages ? 'warn' : 'secondary'"
              size="small"
              rounded
              @click="openMobilePanel('chat')"
            />
            <span
              v-if="hasUnreadMessages"
              class="absolute -top-1 -right-1 w-3 h-3 bg-red-500 rounded-full animate-pulse"
            />
          </div>
        </div>
      </div>

      <!-- Main game area -->
      <div class="flex-1 flex flex-col items-center justify-center p-2 min-h-0 overflow-hidden">
        <GameBoard
          class="max-h-full max-w-full"
          @blank-tile-click="handleBlankTileClick"
          @tile-drop="handleBoardDrop"
          @board-tile-drag-start="handleBoardTileDragStart"
          @board-tile-drag-end="handleBoardTileDragEnd"
          @board-tile-touch-start="handleBoardTileTouchStart"
        />
      </div>

      <!-- Bottom controls -->
      <div class="flex-shrink-0 bg-white shadow-up p-3 space-y-2">
        <TileRack
          @tile-start-drag="handleTileStartDrag"
          @tile-end-drag="handleTileEndDrag"
          @blank-tile-click="handleBlankTileClick"
          @tile-drop="handleTouchTileDrop"
        />
        <MoveControls
          ref="moveControlsRef"
          @submit="handleSubmit"
          @exchange="handleExchange"
          @pass="handlePass"
        />
      </div>

      <!-- Mobile panels (overlays) -->
      <Transition name="slide">
        <div
          v-if="activeMobilePanel !== 'none'"
          class="fixed inset-0 z-50 flex flex-col bg-stone-100"
        >
          <!-- Panel header -->
          <div class="flex items-center justify-between px-4 py-3 bg-white shadow-sm">
            <h2 class="text-lg font-semibold text-gray-800">
              {{ activeMobilePanel === 'scores' ? 'Scores' : activeMobilePanel === 'history' ? 'Move History' : 'Chat' }}
            </h2>
            <Button
              icon="pi pi-times"
              severity="secondary"
              size="small"
              rounded
              @click="closeMobilePanel"
            />
          </div>

          <!-- Panel content -->
          <div class="flex-1 p-4 min-h-0 overflow-auto">
            <ScoreBoard v-if="activeMobilePanel === 'scores'" />
            <MoveHistory v-if="activeMobilePanel === 'history'" class="h-full" />
            <GameChat
              v-if="activeMobilePanel === 'chat'"
              class="h-full"
              @send-message="handleSendMessage"
              @request-definition="handleRequestDefinition"
            />
          </div>
        </div>
      </Transition>
    </div>

    <!-- Blank tile selector dialog -->
    <BlankTileSelector
      v-model:visible="showBlankSelector"
      :tile-id="selectedBlankTileId"
      @select="handleBlankLetterSelect"
    />
  </div>
</template>

<style scoped>
.shadow-up {
  box-shadow: 0 -2px 8px rgba(0, 0, 0, 0.1);
}

.slide-enter-active,
.slide-leave-active {
  transition: transform 0.3s ease;
}

.slide-enter-from,
.slide-leave-to {
  transform: translateY(100%);
}

.banner-enter-active,
.banner-leave-active {
  transition: all 0.3s ease;
}

.banner-enter-from,
.banner-leave-to {
  opacity: 0;
  transform: translateY(-100%);
}
</style>
