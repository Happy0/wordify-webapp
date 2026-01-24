<script setup lang="ts">
import { computed } from 'vue'
import type { BoardSquare } from '@/types/game'
import GameTile from './GameTile.vue'

const props = defineProps<{
  square: BoardSquare
  canDrop?: boolean
  isMyTurn?: boolean
}>()

const emit = defineEmits<{
  (e: 'drop', row: number, col: number): void
  (e: 'dragover'): void
  (e: 'tileClick'): void
  (e: 'tileDragStart', tileId: string): void
  (e: 'tileDragEnd'): void
  (e: 'tileTouchStart', event: TouchEvent, tileId: string): void
}>()

const squareColorClass = computed(() => {
  // Only show square color if no tile is on it
  if (props.square.tile) {
    return 'bg-transparent'
  }

  switch (props.square.type) {
    case 'TripleWord':
      return 'bg-red-500 text-white'
    case 'DoubleWord':
      return 'bg-pink-300 text-pink-800'
    case 'TripleLetter':
      return 'bg-blue-500 text-white'
    case 'DoubleLetter':
      return 'bg-sky-200 text-sky-800'
    default:
      return 'bg-amber-50'
  }
})

const squareLabel = computed(() => {
  if (props.square.tile) return ''

  switch (props.square.type) {
    case 'TripleWord':
      return 'TW'
    case 'DoubleWord':
      // Center star
      if (props.square.row === 7 && props.square.col === 7) {
        return '\u2605'
      }
      return 'DW'
    case 'TripleLetter':
      return 'TL'
    case 'DoubleLetter':
      return 'DL'
    default:
      return ''
  }
})

const canInteract = computed(() => {
  // Can drop on empty squares or squares with candidate tiles (to swap)
  return props.canDrop && props.isMyTurn && (!props.square.tile || props.square.tile.candidate)
})

const canDragTile = computed(() => {
  return props.isMyTurn && props.square.tile?.candidate
})

function handleDragOver(e: DragEvent) {
  if (canInteract.value) {
    e.preventDefault()
    emit('dragover')
  }
}

function handleDrop(e: DragEvent) {
  if (canInteract.value) {
    e.preventDefault()
    emit('drop', props.square.row, props.square.col)
  }
}

function handleTileClick() {
  if (props.square.tile?.type === 'blank' && props.square.tile.candidate) {
    emit('tileClick')
  }
}

function handleTileDragStart(e: DragEvent) {
  if (!canDragTile.value || !props.square.tile) return

  emit('tileDragStart', props.square.tile.id)

  if (e.dataTransfer) {
    e.dataTransfer.effectAllowed = 'move'
    e.dataTransfer.setData('text/plain', props.square.tile.id)
    e.dataTransfer.setData('application/x-board-tile', JSON.stringify({
      tileId: props.square.tile.id,
      fromRow: props.square.row,
      fromCol: props.square.col
    }))
  }
}

function handleTileDragEnd() {
  emit('tileDragEnd')
}

function handleTileTouchStart(e: TouchEvent) {
  if (!canDragTile.value || !props.square.tile) return

  e.preventDefault()
  emit('tileTouchStart', e, props.square.tile.id)
}
</script>

<template>
  <div
    class="board-square relative flex items-center justify-center text-[0.5rem] font-bold aspect-square border border-amber-200"
    :class="[
      squareColorClass,
      {
        'cursor-pointer hover:brightness-110': canInteract
      }
    ]"
    :data-row="square.row"
    :data-col="square.col"
    @dragover="handleDragOver"
    @drop="handleDrop"
  >
    <span v-if="!square.tile" class="opacity-80">{{ squareLabel }}</span>
    <div
      v-if="square.tile"
      class="absolute inset-0 tile-wrapper"
      :class="{ 'touch-action-none': canDragTile }"
      :draggable="canDragTile"
      @dragstart="handleTileDragStart"
      @dragend="handleTileDragEnd"
      @touchstart="handleTileTouchStart"
    >
      <GameTile
        :tile="square.tile"
        :draggable="canDragTile"
        :on-board="true"
        class="w-full h-full"
        @click="handleTileClick"
      />
    </div>
  </div>
</template>

<style scoped>
.board-square {
  min-width: 0;
  min-height: 0;
}

.touch-action-none {
  touch-action: none;
  -webkit-user-select: none;
  user-select: none;
}

.tile-wrapper[draggable='true'] {
  cursor: grab;
}

.tile-wrapper[draggable='true']:active {
  cursor: grabbing;
}
</style>
