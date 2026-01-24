<script setup lang="ts">
import { computed, ref } from 'vue'
import { useGameStore } from '@/stores/gameStore'
import { storeToRefs } from 'pinia'
import BoardSquare from './BoardSquare.vue'
import { BOARD_SIZE } from '@/types/game'

const emit = defineEmits<{
  (e: 'blankTileClick', tileId: string): void
  (e: 'tileDrop', row: number, col: number): void
  (e: 'boardTileDragStart', tileId: string, fromRow: number, fromCol: number): void
  (e: 'boardTileDragEnd'): void
  (e: 'boardTileTouchStart', event: TouchEvent, tileId: string, fromRow: number, fromCol: number): void
}>()

const store = useGameStore()
const { board, isMyTurn } = storeToRefs(store)

// Track the tile being dragged
const draggingTileId = ref<string | null>(null)

// Expose these for external drag-drop coordination with the rack
defineExpose({
  draggingTileId
})

function handleTileClick(row: number, col: number) {
  const boardRow = board.value[row]
  if (boardRow) {
    const square = boardRow[col]
    if (square?.tile?.type === 'blank' && square.tile.candidate) {
      emit('blankTileClick', square.tile.id)
    }
  }
}

function handleSquareDrop(row: number, col: number) {
  emit('tileDrop', row, col)
}

function handleBoardTileDragStart(row: number, col: number, tileId: string) {
  emit('boardTileDragStart', tileId, row, col)
}

function handleBoardTileDragEnd() {
  emit('boardTileDragEnd')
}

function handleBoardTileTouchStart(row: number, col: number, event: TouchEvent, tileId: string) {
  emit('boardTileTouchStart', event, tileId, row, col)
}

// Calculate rows for rendering
const rows = computed(() => {
  return Array.from({ length: BOARD_SIZE }, (_, i) => i)
})

const cols = computed(() => {
  return Array.from({ length: BOARD_SIZE }, (_, i) => i)
})

function getSquare(row: number, col: number) {
  const boardRow = board.value[row]
  if (boardRow) {
    return boardRow[col]
  }
  return undefined
}
</script>

<template>
  <div class="game-board-container w-full mx-auto">
    <div
      class="game-board grid gap-0 bg-amber-800 p-1 rounded shadow-lg"
      :style="{ gridTemplateColumns: `repeat(${BOARD_SIZE}, minmax(0, 1fr))` }"
    >
      <template v-for="row in rows" :key="row">
        <template v-for="col in cols" :key="`${row}-${col}`">
          <BoardSquare
            v-if="getSquare(row, col)"
            :square="getSquare(row, col)!"
            :can-drop="true"
            :is-my-turn="isMyTurn"
            @drop="handleSquareDrop"
            @tile-click="handleTileClick(row, col)"
            @tile-drag-start="(tileId) => handleBoardTileDragStart(row, col, tileId)"
            @tile-drag-end="handleBoardTileDragEnd"
            @tile-touch-start="(event, tileId) => handleBoardTileTouchStart(row, col, event, tileId)"
          />
        </template>
      </template>
    </div>
  </div>
</template>

<style scoped>
.game-board-container {
  touch-action: manipulation;
  max-width: min(100%, calc(100vh - 12rem));
}

.game-board {
  aspect-ratio: 1 / 1;
}

@media (min-width: 1024px) {
  .game-board-container {
    max-width: min(100%, calc(100vh - 10rem));
  }
}
</style>
