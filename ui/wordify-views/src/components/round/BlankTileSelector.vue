<script setup lang="ts">
import { computed, inject } from 'vue'
import Dialog from 'primevue/dialog'
import type { TileValueMap } from '@/common/tile-value-map'

const props = defineProps<{
  visible: boolean
  tileId: string | null
}>()

const emit = defineEmits<{
  (e: 'update:visible', value: boolean): void
  (e: 'select', letter: string): void
}>()

// Inject tileValues from the app-level provider (for locale support)
const tileValues = inject<TileValueMap | null>('tileValues', null)

// Default English letters as fallback
const defaultLetters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.split('')

// All letters available for blank tile assignment
// Uses keys from tileValues if provided, otherwise defaults to English A-Z
const letters = computed(() => {
  if (tileValues) {
    // Get sorted keys from the tileValues map
    // Sort alphabetically for consistent display
    return Object.keys(tileValues).sort()
  }
  return defaultLetters
})

// Calculate items per row based on letter count and max width
const itemsPerRow = computed(() => {
  const count = letters.value.length
  // For smaller sets, use fewer columns; for larger sets, use more
  if (count <= 20) return 5
  if (count <= 30) return 6
  return 7
})

// Organize into rows for better display
const letterRows = computed(() => {
  const rows: string[][] = []
  const perRow = itemsPerRow.value
  for (let i = 0; i < letters.value.length; i += perRow) {
    rows.push(letters.value.slice(i, i + perRow))
  }
  return rows
})

function selectLetter(letter: string) {
  emit('select', letter)
  emit('update:visible', false)
}

function handleClose() {
  emit('update:visible', false)
}
</script>

<template>
  <Dialog
    :visible="visible"
    header="Assign Letter to Blank Tile"
    :modal="true"
    :closable="true"
    :draggable="false"
    class="w-80"
    @update:visible="handleClose"
  >
    <p class="text-sm text-gray-600 mb-4">
      Select a letter for your blank tile:
    </p>

    <div class="letter-grid space-y-2">
      <div
        v-for="(row, rowIdx) in letterRows"
        :key="rowIdx"
        class="flex justify-center gap-1"
      >
        <button
          v-for="letter in row"
          :key="letter"
          class="letter-button h-9 flex items-center justify-center
                 bg-amber-100 hover:bg-amber-200 active:bg-amber-300
                 text-amber-900 font-bold rounded-sm
                 border border-amber-300 shadow-sm
                 transition-colors cursor-pointer
                 focus:outline-none focus:ring-2 focus:ring-blue-400"
          :class="letter.length > 1 ? 'px-2 min-w-9 text-sm' : 'w-9 text-lg'"
          @click="selectLetter(letter)"
        >
          {{ letter }}
        </button>
      </div>
    </div>
  </Dialog>
</template>
