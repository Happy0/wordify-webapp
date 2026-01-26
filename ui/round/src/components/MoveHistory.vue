<script setup lang="ts">
import { computed, ref, watch, nextTick } from 'vue'
import { useGameStore } from '@/stores/gameStore'
import { storeToRefs } from 'pinia'

const store = useGameStore()
const { moveHistory, players } = storeToRefs(store)

const historyContainer = ref<HTMLElement | null>(null)

// Auto-scroll to bottom when new moves are added
watch(
  () => moveHistory.value.length,
  async () => {
    await nextTick()
    if (historyContainer.value) {
      historyContainer.value.scrollTop = historyContainer.value.scrollHeight
    }
  }
)

function getPlayerName(playerIndex: number): string {
  return players.value[playerIndex]?.name ?? `Player ${playerIndex + 1}`
}

// Add move numbers to the history (first move at top, latest at bottom)
const numberedHistory = computed(() => {
  return moveHistory.value.map((move, idx) => ({
    ...move,
    moveNumber: idx + 1
  }))
})
</script>

<template>
  <div class="move-history bg-white rounded-lg shadow p-4 flex flex-col h-full">
    <h2 class="text-lg font-semibold text-gray-800 mb-3 flex-shrink-0">Move History</h2>

    <div
      ref="historyContainer"
      class="flex-1 overflow-y-auto space-y-2 min-h-0"
    >
      <template v-if="moveHistory.length === 0">
        <p class="text-gray-400 text-sm text-center py-4">No moves yet</p>
      </template>

      <div
        v-for="(move, index) in numberedHistory"
        :key="index"
        class="move-entry p-2 rounded border-l-2 text-sm"
        :class="{
          'border-green-500 bg-green-50': move.type === 'boardMove',
          'border-gray-300 bg-gray-50': move.type === 'pass',
          'border-blue-300 bg-blue-50': move.type === 'exchange'
        }"
      >
        <div class="flex justify-between items-start gap-2">
          <div class="flex-1">
            <span class="font-medium text-gray-700">
              {{ move.moveNumber }}. {{ getPlayerName(move.playerIndex) }}
            </span>

            <template v-if="move.type === 'boardMove'">
              <div class="mt-1 space-y-0.5">
                <div
                  v-for="(word, wIdx) in move.wordsMade"
                  :key="wIdx"
                  class="flex justify-between text-gray-600"
                >
                  <span class="font-mono uppercase">{{ word.word }}</span>
                  <span class="text-gray-500">{{ word.score }}</span>
                </div>
              </div>
            </template>

            <template v-else-if="move.type === 'pass'">
              <span class="text-gray-500 ml-1">passed</span>
            </template>

            <template v-else-if="move.type === 'exchange'">
              <span class="text-gray-500 ml-1">exchanged tiles</span>
            </template>
          </div>

          <!-- Total score for board moves -->
          <div
            v-if="move.type === 'boardMove'"
            class="font-bold text-green-700 text-base"
          >
            +{{ move.overallScore }}
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<style scoped>
.move-history {
  max-height: 100%;
}
</style>
