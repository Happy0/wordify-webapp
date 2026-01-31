<script setup lang="ts">
import { ref, onMounted, onUnmounted } from 'vue'
import { useGameStore } from '@/stores/gameStore'
import { storeToRefs } from 'pinia'
import GameTile from './GameTile.vue'
import Button from 'primevue/button'
import { useTouchDragDrop, setupGlobalTouchHandlers } from '@/composables/useTouchDragDrop'

const emit = defineEmits<{
  (e: 'tileStartDrag', tileId: string): void
  (e: 'tileEndDrag'): void
  (e: 'blankTileClick', tileId: string): void
  (e: 'tileDrop', row: number, col: number, tileId: string): void
}>()

const store = useGameStore()
const { rack, isMyTurn } = storeToRefs(store)

const draggedTileId = ref<string | null>(null)

const {
  handleTouchStart,
  handleTouchMove,
  handleTouchEnd,
  handleTouchCancel,
  setDropCallback,
  setCancelCallback,
  setTapCallback,
  dragState
} = useTouchDragDrop()

// Set up global touch handlers
let cleanupTouchHandlers: (() => void) | null = null

onMounted(() => {
  cleanupTouchHandlers = setupGlobalTouchHandlers(
    handleTouchMove,
    handleTouchEnd,
    handleTouchCancel
  )

  // Set up tap callback to handle blank tile clicks on touch devices
  setTapCallback((tileId: string) => {
    handleTileClick(tileId)
  })
})

onUnmounted(() => {
  if (cleanupTouchHandlers) {
    cleanupTouchHandlers()
  }
})

function handleDragStart(e: DragEvent, tileId: string) {
  if (!isMyTurn.value) return

  draggedTileId.value = tileId
  emit('tileStartDrag', tileId)

  if (e.dataTransfer) {
    e.dataTransfer.effectAllowed = 'move'
    e.dataTransfer.setData('text/plain', tileId)
  }
}

function handleDragEnd() {
  draggedTileId.value = null
  emit('tileEndDrag')
}

function onTouchStart(e: TouchEvent, tileId: string) {
  if (!isMyTurn.value) return

  // Set up callbacks on each touch start to ensure they haven't been overwritten
  // by other components (e.g., board tile dragging)
  setDropCallback((row: number, col: number) => {
    if (dragState.value.tileId) {
      emit('tileDrop', row, col, dragState.value.tileId)
    }
    draggedTileId.value = null
    emit('tileEndDrag')
  })

  setCancelCallback(() => {
    draggedTileId.value = null
    emit('tileEndDrag')
  })

  draggedTileId.value = tileId
  emit('tileStartDrag', tileId)
  handleTouchStart(e, tileId)
}

function handleTileClick(tileId: string) {
  const tile = rack.value.find(t => t.id === tileId)
  if (tile?.type === 'blank') {
    emit('blankTileClick', tileId)
  }
}

function handleShuffle() {
  store.shuffleRack()
}

function handleRecall() {
  store.recallTilesToRack()
}

// Allow dropping tiles back onto the rack
function handleDragOver(e: DragEvent) {
  if (isMyTurn.value) {
    e.preventDefault()
  }
}

function handleDrop(e: DragEvent) {
  e.preventDefault()
  // The parent component handles the actual tile movement
}
</script>

<template>
  <div class="tile-rack-container flex items-center gap-3">
    <!-- Recall button (left) -->
    <Button
      icon="pi pi-arrow-down"
      severity="secondary"
      size="small"
      :disabled="!isMyTurn"
      title="Recall tiles from board"
      aria-label="Recall tiles from board"
      @click="handleRecall"
    />

    <!-- Tile rack -->
    <div
      class="tile-rack flex gap-1 p-2 bg-amber-700 rounded-lg shadow-inner min-h-[3rem]"
      @dragover="handleDragOver"
      @drop="handleDrop"
    >
      <div
        v-for="tile in rack"
        :key="tile.id"
        class="tile-slot"
        :draggable="isMyTurn"
        @dragstart="(e) => handleDragStart(e, tile.id)"
        @dragend="handleDragEnd"
        @touchstart="(e) => onTouchStart(e, tile.id)"
      >
        <GameTile
          :tile="tile"
          :draggable="isMyTurn"
          size="medium"
          @click="handleTileClick(tile.id)"
        />
      </div>

      <!-- Empty slots -->
      <div
        v-for="i in Math.max(0, 7 - rack.length)"
        :key="`empty-${i}`"
        class="empty-slot w-8 h-8 border-2 border-dashed border-amber-500 rounded-sm opacity-50"
      />
    </div>

    <!-- Shuffle button (right) -->
    <Button
      icon="pi pi-refresh"
      severity="secondary"
      size="small"
      title="Shuffle tiles"
      aria-label="Shuffle tiles"
      @click="handleShuffle"
    />
  </div>
</template>

<style scoped>
.tile-slot {
  touch-action: none;
  -webkit-user-select: none;
  user-select: none;
}

.tile-slot[draggable='true'] {
  cursor: grab;
}

.tile-slot[draggable='true']:active {
  cursor: grabbing;
}
</style>
