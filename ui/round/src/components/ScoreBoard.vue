<script setup lang="ts">
import { computed, ref, onMounted, onUnmounted } from 'vue'
import { useGameStore } from '@/stores/gameStore'
import { storeToRefs } from 'pinia'

const store = useGameStore()
const { players, playerToMove, tilesRemaining, gameEnded, myPlayerNumber } = storeToRefs(store)

// Reactive timestamp that updates every minute to keep "last seen" times fresh
const currentTime = ref(Date.now())
let updateInterval: ReturnType<typeof setInterval> | null = null

onMounted(() => {
  updateInterval = setInterval(() => {
    currentTime.value = Date.now()
  }, 60000) // Update every minute
})

onUnmounted(() => {
  if (updateInterval) {
    clearInterval(updateInterval)
    updateInterval = null
  }
})

// Sort players by score for display (highest first)
// Note: playerNumber is 1-based (player 1, player 2, etc.) to match the server protocol
const sortedPlayers = computed(() => {
  return [...players.value]
    .map((player, index) => ({ ...player, playerNumber: index + 1 }))
    .sort((a, b) => {
      // Calculate final score including end bonus
      const scoreA = a.score + (a.endBonus ?? 0)
      const scoreB = b.score + (b.endBonus ?? 0)
      return scoreB - scoreA
    })
})

function formatLastSeen(lastSeen: number | undefined): string {
  if (!lastSeen) return ''
  // Use reactive currentTime to ensure re-render when timer updates
  const now = currentTime.value
  const diffMs = now - lastSeen
  const diffMins = Math.floor(diffMs / 60000)

  if (diffMins < 1) return 'just now'
  if (diffMins < 60) return `${diffMins}m ago`

  const diffHours = Math.floor(diffMins / 60)
  if (diffHours < 24) return `${diffHours}h ago`

  const diffDays = Math.floor(diffHours / 24)
  return `${diffDays}d ago`
}
</script>

<template>
  <div class="score-board bg-white rounded-lg shadow p-4">
    <div class="flex justify-between items-center mb-3">
      <h2 class="text-lg font-semibold text-gray-800">Scores</h2>
      <div class="flex items-center gap-1 text-sm text-gray-600">
        <i class="pi pi-inbox text-amber-600"></i>
        <span>{{ tilesRemaining }} tiles left</span>
      </div>
    </div>

    <div class="space-y-2">
      <div
        v-for="player in sortedPlayers"
        :key="player.playerNumber"
        class="player-row flex items-center gap-2 p-2 rounded-md transition-colors"
        :class="{
          'bg-amber-100 border-l-4 border-amber-500': playerToMove === player.playerNumber && !gameEnded,
          'bg-gray-50': playerToMove !== player.playerNumber || gameEnded,
          'ring-2 ring-blue-300': player.playerNumber === myPlayerNumber
        }"
      >
        <!-- Connection status indicator -->
        <div
          class="w-2 h-2 rounded-full flex-shrink-0"
          :class="{
            'bg-green-500': player.connected,
            'bg-gray-300': !player.connected
          }"
          :title="player.connected ? 'Online' : `Last seen: ${formatLastSeen(player.lastSeen)}`"
        />

        <!-- Player name -->
        <div class="flex-1 min-w-0">
          <div class="font-medium text-gray-800 truncate">
            {{ player.name }}
            <span v-if="player.playerNumber === myPlayerNumber" class="text-xs text-blue-500">(you)</span>
          </div>
          <div
            v-if="!player.connected && player.lastSeen"
            class="text-xs text-gray-400"
          >
            {{ formatLastSeen(player.lastSeen) }}
          </div>
        </div>

        <!-- Score -->
        <div class="text-right">
          <div class="font-bold text-lg text-gray-800">
            {{ player.score }}
          </div>
          <div
            v-if="gameEnded && player.endBonus !== undefined"
            class="text-xs"
            :class="{
              'text-green-600': player.endBonus > 0,
              'text-red-600': player.endBonus < 0,
              'text-gray-400': player.endBonus === 0
            }"
          >
            {{ player.endBonus > 0 ? '+' : '' }}{{ player.endBonus }}
          </div>
        </div>

        <!-- Turn indicator -->
        <div
          v-if="playerToMove === player.playerNumber && !gameEnded"
          class="flex-shrink-0"
        >
          <i class="pi pi-arrow-right text-amber-600 animate-pulse"></i>
        </div>
      </div>
    </div>

    <!-- Game over indicator -->
    <div
      v-if="gameEnded"
      class="mt-4 p-2 bg-gradient-to-r from-amber-100 to-yellow-100 rounded text-center"
    >
      <span class="font-semibold text-amber-800">Game Over!</span>
    </div>
  </div>
</template>
