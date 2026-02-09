<script setup lang="ts">
import { computed, onMounted, onUnmounted, ref } from 'vue'
import { BOARD_SIZE, BOARD_LAYOUT, type TileInput, type SquareType } from '@/types/game'
import { fromBoardTextRepresentation } from '@/common/board-text-presentation'
import type { TileValueMap } from '@/common/tile-value-map'

// Reactive timestamp that updates every minute to keep "last activity" times fresh
const currentTime = ref(Date.now())
let updateInterval: ReturnType<typeof setInterval> | null = null

onMounted(() => {
  updateInterval = setInterval(() => {
    currentTime.value = Date.now()
  }, 60000)
})

onUnmounted(() => {
  if (updateInterval) {
    clearInterval(updateInterval)
    updateInterval = null
  }
})

const props = defineProps<{
  boardString: string
  yourMove: boolean
  lastActivity: string
  gameId: string
  tileValues: TileValueMap
  otherPlayers: { name: string; active: boolean }[]
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

function getTileValue(row: number, col: number): number | null {
  const tile = getTile(row, col)
  if (!tile) return null
  // Blank tiles have no point value
  if (tile.type === 'blank') return null
  return tile.value
}

function getSquareLabel(row: number, col: number): string {
  // Don't show label if there's a tile on the square
  if (getTile(row, col)) return ''

  const type = getSquareType(row, col)
  switch (type) {
    case 'TripleWord':
      return 'TW'
    case 'DoubleWord':
      // Center star
      if (row === 7 && col === 7) {
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
}

function getSquareLabelColorClass(row: number, col: number): string {
  const type = getSquareType(row, col)
  switch (type) {
    case 'TripleWord':
      return 'text-white'
    case 'DoubleWord':
      return 'text-pink-800'
    case 'TripleLetter':
      return 'text-white'
    case 'DoubleLetter':
      return 'text-sky-800'
    default:
      return ''
  }
}

// Format last activity as relative time
const formattedLastActivity = computed(() => {
  const date = new Date(props.lastActivity)
  const now = currentTime.value
  const diffMs = now - date.getTime()
  const diffMins = Math.floor(diffMs / 60000)
  const diffHours = Math.floor(diffMs / 3600000)
  const diffDays = Math.floor(diffMs / 86400000)

  if (diffMins < 1) return 'Just now'
  if (diffMins < 60) return `${diffMins}m ago`
  if (diffHours < 24) return `${diffHours}h ago`
  if (diffDays < 7) return `${diffDays}d ago`
  return date.toLocaleDateString()
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
      <div class="text-xs text-gray-500">
        {{ formattedLastActivity }}
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
            class="mini-square aspect-square flex items-center justify-center relative"
            :class="getSquareColorClass(row, col)"
          >
            <!-- Tile letter and value -->
            <template v-if="getTileLetter(row, col)">
              <span
                class="tile-letter text-amber-900 uppercase leading-none"
              >
                {{ getTileLetter(row, col) }}
              </span>
              <span
                v-if="getTileValue(row, col) !== null"
                class="tile-value absolute text-amber-900"
              >
                {{ getTileValue(row, col) }}
              </span>
            </template>
            <!-- Bonus square label -->
            <span
              v-else-if="getSquareLabel(row, col)"
              class="square-label leading-none font-semibold"
              :class="getSquareLabelColorClass(row, col)"
            >
              {{ getSquareLabel(row, col) }}
            </span>
          </div>
        </template>
      </template>
    </div>

    <!-- Other players -->
    <div v-if="otherPlayers.length" class="mt-2 flex flex-wrap gap-1 justify-center">
      <span
        v-for="player in otherPlayers"
        :key="player.name"
        class="inline-flex items-center gap-1 px-1.5 py-0.5 rounded-full"
        :class="player.active ? 'bg-green-100 text-green-700' : 'bg-stone-100 text-gray-700'"
        :title="player.name"
      >
        <i class="pi pi-user player-icon" />
        <span class="player-name truncate">{{ player.name }}</span>
      </span>
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

.tile-letter {
  font-size: 0.35rem;
}

.tile-value {
  font-size: 0.15rem;
  bottom: 0;
  right: 1px;
  line-height: 1;
}

.square-label {
  font-size: 0.2rem;
}

.player-icon {
  font-size: 0.5rem;
}

.player-name {
  font-size: 0.6rem;
  max-width: 10ch;
}

@media (min-width: 640px) {
  .mini-board-card {
    max-width: 220px;
  }

  .tile-letter {
    font-size: 0.4rem;
  }

  .tile-value {
    font-size: 0.18rem;
  }

  .square-label {
    font-size: 0.25rem;
  }

  .player-icon {
    font-size: 0.55rem;
  }

  .player-name {
    font-size: 0.65rem;
    max-width: 12ch;
  }
}
</style>
