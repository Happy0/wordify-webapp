<script setup lang="ts">
import { computed } from 'vue'
import Dialog from 'primevue/dialog'

const props = defineProps<{
  visible: boolean
  tileId: string | null
}>()

const emit = defineEmits<{
  (e: 'update:visible', value: boolean): void
  (e: 'select', letter: string): void
}>()

// All letters available for blank tile assignment
const letters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.split('')

// Organize into rows for better display
const letterRows = computed(() => {
  const rows: string[][] = []
  for (let i = 0; i < letters.length; i += 7) {
    rows.push(letters.slice(i, i + 7))
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
          class="letter-button w-9 h-9 flex items-center justify-center
                 bg-amber-100 hover:bg-amber-200 active:bg-amber-300
                 text-amber-900 font-bold text-lg rounded-sm
                 border border-amber-300 shadow-sm
                 transition-colors cursor-pointer
                 focus:outline-none focus:ring-2 focus:ring-blue-400"
          @click="selectLetter(letter)"
        >
          {{ letter }}
        </button>
      </div>
    </div>
  </Dialog>
</template>
