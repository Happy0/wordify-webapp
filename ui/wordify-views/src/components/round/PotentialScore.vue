<script setup lang="ts">
import { computed } from 'vue'
import { useGameStore } from '@/stores/gameStore'
import { storeToRefs } from 'pinia'

const store = useGameStore()
const { potentialScore, isMyTurn, candidateTilesOnBoard } = storeToRefs(store)

const hasPlacedTiles = computed(() => {
  return isMyTurn.value && candidateTilesOnBoard.value.length > 0
})
</script>

<template>
  <div
    class="potential-score inline-flex items-center gap-1 px-3 py-2 rounded-full shadow w-[120px] justify-center overflow-hidden"
    :class="hasPlacedTiles
      ? 'bg-gradient-to-r from-green-100 to-emerald-100'
      : 'bg-gray-100'"
  >
    <template v-if="hasPlacedTiles">
      <span class="text-xs text-gray-600 whitespace-nowrap">Score:</span>
      <span class="font-bold text-lg text-green-700 min-w-[2rem] text-center">
        {{ potentialScore ?? '...' }}
      </span>
    </template>
    <template v-else>
      <span class="text-xs text-gray-400 truncate">Place tiles to score</span>
    </template>
  </div>
</template>
