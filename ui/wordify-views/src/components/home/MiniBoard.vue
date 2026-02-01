<script setup lang="ts">
import { computed } from 'vue'
import { BOARD_SIZE, BOARD_LAYOUT, type TileInput, type SquareType } from '@/types/game'
import { fromBoardTextRepresentation, type TileValueMap } from '@/common/board-text-presentation'

const props = defineProps<{
  boardString: string
  players: string[]
  yourMove: boolean
  gameId: string
  tileValues: TileValueMap
}>()

// Parse the board string into placed tiles
const placedTiles = computed(() => {
  return fromBoardTextRepresentation(props.boardString, props.tileValues) ?? []
})

// Create a 2D board representation for rendering
const board = computed(() => {
  const result: (TileInput | null)[][] = []

  // Initialize empty board
  for (let row = 0; row < BOARD_SIZE; row++) {
    result.push(new Array(BOARD_SIZE).fill(null))
  }

  // Place tiles on the board (convert from 1-based to 0-based)
  for (const placed of placedTiles.value) {
    const col = placed.position.x - 1
    const row = placed.position.y - 1
    if (row >= 0 && row < BOARD_SIZE && col >= 0 && col < BOARD_SIZE) {
      result[row]![col] = placed.tile
    }
  }

  return result
})

const rows = computed(() => Array.from({ length: BOARD_SIZE }, (_, i) => i))
const cols = computed(() => Array.from({ length: BOARD_SIZE }, (_, i) => i))

function getSquareType(row: number, col: number): SquareType {
  return BOARD_LAYOUT[row]?.[col] ?? 'Normal'
}

function getTile(row: number, col: number): TileInput | null {
  return board.value[row]?.[col] ?? null
}

function getSquareColorClass(row: number, col: number): string {
  const tile = getTile(row, col)
  if (tile) {
    // Tile present - show tile background
    return tile.type === 'blank' ? 'bg-amber-50' : 'bg-amber-100'
  }

  const type = getSquareType(row, col)
  switch (type) {
    case 'TripleWord':
      return 'bg-red-500'
    case 'DoubleWord':
      return 'bg-pink-300'
    case 'TripleLetter':
      return 'bg-blue-500'
    case 'DoubleLetter':
      return 'bg-sky-200'
    default:
      return 'bg-amber-50'
  }
}

function getTileLetter(row: number, col: number): string {
  const tile = getTile(row, col)
  if (!tile) return ''
  if (tile.type === 'blank') return tile.assigned ?? ''
  return tile.letter
}

// Display other players (exclude empty strings and limit to 2)
const displayPlayers = computed(() => {
  return props.players.filter(p => p.length > 0).slice(0, 2)
})

function navigateToGame() {
  window.location.href = `/games/${props.gameId}`
}
</script>

<template>
  <div
    class="mini-board-card bg-white rounded-lg shadow-md p-3 cursor-pointer hover:shadow-lg transition-shadow"
    @click="navigateToGame"
  >
    <!-- Turn indicator -->
    <div class="flex items-center justify-between mb-2">
      <div
        class="text-xs font-medium px-2 py-1 rounded-full"
        :class="yourMove ? 'bg-green-100 text-green-700' : 'bg-gray-100 text-gray-600'"
      >
        {{ yourMove ? 'Your turn' : 'Waiting...' }}
      </div>
      <div class="text-xs text-gray-500 truncate max-w-24">
        vs {{ displayPlayers.join(', ') }}
      </div>
    </div>

    <!-- Mini board -->
    <div
      class="mini-board grid gap-0 bg-amber-800 p-0.5 rounded aspect-square"
      :style="{ gridTemplateColumns: `repeat(${BOARD_SIZE}, minmax(0, 1fr))` }"
    >
      <template v-for="row in rows" :key="row">
        <template v-for="col in cols" :key="`${row}-${col}`">
          <div
            class="mini-square aspect-square flex items-center justify-center"
            :class="getSquareColorClass(row, col)"
          >
            <span
              v-if="getTileLetter(row, col)"
              class="text-amber-900 uppercase leading-none"
              style="font-size: 0.35rem;"
            >
              {{ getTileLetter(row, col) }}
            </span>
          </div>
        </template>
      </template>
    </div>
  </div>
</template>

<style scoped>
.mini-board-card {
  max-width: 200px;
  width: 100%;
}

.mini-square {
  min-width: 0;
  min-height: 0;
}

@media (min-width: 640px) {
  .mini-board-card {
    max-width: 220px;
  }

  .mini-square span {
    font-size: 0.4rem;
  }
}
</style>
