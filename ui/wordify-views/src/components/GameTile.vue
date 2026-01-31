<script setup lang="ts">
import { computed } from 'vue'
import type { Tile } from '@/types/game'

const props = defineProps<{
  tile: Tile
  draggable?: boolean
  size?: 'small' | 'medium' | 'large'
  onBoard?: boolean
}>()

const emit = defineEmits<{
  (e: 'click'): void
}>()

const displayLetter = computed(() => {
  if (props.tile.type === 'blank') {
    return props.tile.assigned ?? ''
  }
  return props.tile.letter
})

const displayValue = computed(() => {
  if (props.tile.type === 'blank') {
    return ''
  }
  return props.tile.value
})

const isBlank = computed(() => props.tile.type === 'blank')

const sizeClasses = computed(() => {
  if (props.onBoard) {
    // On board, tile fills the square - use relative sizing
    return 'w-full h-full text-[0.6rem] sm:text-xs lg:text-sm'
  }
  switch (props.size) {
    case 'small':
      return 'w-6 h-6 text-xs'
    case 'large':
      return 'w-10 h-10 text-lg'
    default:
      return 'w-8 h-8 text-sm'
  }
})

function handleClick() {
  emit('click')
}
</script>

<template>
  <div
    class="game-tile relative flex items-center justify-center font-bold select-none"
    :class="[
      sizeClasses,
      {
        // On board: no background, just the letter
        'bg-amber-100 text-amber-900': onBoard && !isBlank,
        'bg-amber-50 text-amber-700 border border-dashed border-amber-300': onBoard && isBlank,
        // On rack: styled tile appearance
        'bg-amber-100 text-amber-900 border border-amber-300 shadow-sm rounded-sm': !onBoard && !isBlank,
        'bg-amber-50 text-amber-700 border border-amber-200 border-dashed shadow-sm rounded-sm': !onBoard && isBlank,
        'cursor-grab active:cursor-grabbing': draggable,
        'ring-2 ring-blue-400 ring-inset': tile.candidate && onBoard,
        'ring-2 ring-blue-400': tile.candidate && !onBoard
      }
    ]"
    :draggable="draggable"
    @click="handleClick"
  >
    <span class="uppercase">{{ displayLetter }}</span>
    <span
      v-if="displayValue"
      class="absolute bottom-0 right-0.5 font-normal"
      :class="onBoard ? 'text-[0.35rem] sm:text-[0.4rem] lg:text-[0.5rem]' : 'text-[0.5em]'"
    >
      {{ displayValue }}
    </span>
  </div>
</template>

<style scoped>
.game-tile {
  transition: transform 0.1s ease, box-shadow 0.1s ease;
}

.game-tile:not([class*="onBoard"]):hover {
  transform: scale(1.05);
}

.game-tile[draggable='true']:active {
  transform: scale(1.1);
  box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
}
</style>
