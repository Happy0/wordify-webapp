<script setup lang="ts">
import { computed, onMounted, onUnmounted, ref } from 'vue'
import { BOARD_SIZE, BOARD_LAYOUT, type TileInput } from '@/types/game'
import { fromBoardTextRepresentation } from '@/common/board-text-presentation'
import type { TileValueMap } from '@/common/tile-value-map'

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
  lastActivity: string
  gameId: string
  tileValues: TileValueMap
  players: { name: string; active: boolean }[]
}>()

const placedTiles = computed(() => fromBoardTextRepresentation(props.boardString, props.tileValues) ?? [])

const board = computed(() => {
  const result: (TileInput | null)[][] = []
  for (let row = 0; row < BOARD_SIZE; row++) {
    result.push(new Array(BOARD_SIZE).fill(null))
  }
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

function getTile(row: number, col: number): TileInput | null {
  return board.value[row]?.[col] ?? null
}

function getSquareColorClass(row: number, col: number): string {
  const tile = getTile(row, col)
  if (tile) return tile.type === 'blank' ? 'bg-amber-50' : 'bg-amber-100'
  const type = BOARD_LAYOUT[row]?.[col] ?? 'Normal'
  switch (type) {
    case 'TripleWord':   return 'bg-red-500'
    case 'DoubleWord':   return 'bg-pink-300'
    case 'TripleLetter': return 'bg-blue-500'
    case 'DoubleLetter': return 'bg-sky-200'
    default:             return 'bg-amber-50'
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
  if (!tile || tile.type === 'blank') return null
  return tile.value
}

function getSquareLabel(row: number, col: number): string {
  if (getTile(row, col)) return ''
  const type = BOARD_LAYOUT[row]?.[col] ?? 'Normal'
  switch (type) {
    case 'TripleWord':   return 'TW'
    case 'DoubleWord':   return row === 7 && col === 7 ? '\u2605' : 'DW'
    case 'TripleLetter': return 'TL'
    case 'DoubleLetter': return 'DL'
    default:             return ''
  }
}

function getSquareLabelColorClass(row: number, col: number): string {
  const type = BOARD_LAYOUT[row]?.[col] ?? 'Normal'
  switch (type) {
    case 'TripleWord':   return 'text-white'
    case 'DoubleWord':   return 'text-pink-800'
    case 'TripleLetter': return 'text-white'
    case 'DoubleLetter': return 'text-sky-800'
    default:             return ''
  }
}

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
</script>

<template>
  <a
    :href="`/games/${gameId}`"
    class="tv-mini-board-card block bg-white rounded-lg shadow-md p-3 cursor-pointer hover:shadow-lg transition-shadow no-underline"
  >
    <!-- Header: TV indicator + last activity -->
    <div class="flex items-center justify-between mb-2">
      <div class="text-xs font-medium px-2 py-1 rounded-full bg-gray-100 text-gray-600 flex items-center gap-1">
        📺 TV
      </div>
      <div class="text-xs text-gray-500">{{ formattedLastActivity }}</div>
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
            <template v-if="getTileLetter(row, col)">
              <span class="tile-letter text-amber-900 uppercase leading-none">
                {{ getTileLetter(row, col) }}
              </span>
              <span
                v-if="getTileValue(row, col) !== null"
                class="tile-value absolute text-amber-900"
              >
                {{ getTileValue(row, col) }}
              </span>
            </template>
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

    <!-- Players -->
    <div v-if="players.length" class="mt-2 flex flex-wrap gap-1 justify-center">
      <span
        v-for="player in players"
        :key="player.name"
        class="inline-flex items-center gap-1 px-1.5 py-0.5 rounded-full"
        :class="player.active ? 'bg-green-100 text-green-700' : 'bg-stone-100 text-gray-700'"
        :title="player.name"
      >
        <i class="pi pi-user player-icon" />
        <span class="player-name truncate">{{ player.name }}</span>
      </span>
    </div>
  </a>
</template>

<style scoped>
.tv-mini-board-card {
  width: 100%;
}

.mini-square {
  min-width: 0;
  min-height: 0;
}

.tile-letter {
  font-size: 0.55rem;
}

.tile-value {
  font-size: 0.24rem;
  bottom: 0;
  right: 1px;
  line-height: 1;
}

.square-label {
  font-size: 0.35rem;
}

.player-icon {
  font-size: 0.7rem;
}

.player-name {
  font-size: 0.85rem;
  max-width: 12ch;
}
</style>
