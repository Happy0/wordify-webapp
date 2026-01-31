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
    class="potential-score inline-flex items-center gap-2 px-4 py-2 rounded-full shadow w-[200px] justify-center"
    :class="hasPlacedTiles
      ? 'bg-gradient-to-r from-green-100 to-emerald-100'
      : 'bg-gray-100'"
  >
    <template v-if="hasPlacedTiles">
      <span class="text-sm text-gray-600">Potential Score:</span>
      <span class="font-bold text-xl text-green-700 min-w-[2rem] text-center">
        {{ potentialScore ?? '...' }}
      </span>
    </template>
    <template v-else>
      <span class="text-sm text-gray-400">Place tiles to see score</span>
    </template>
  </div>
</template>
