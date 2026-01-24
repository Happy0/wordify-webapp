import { createApp } from 'vue'
import { createPinia } from 'pinia'
import PrimeVue from 'primevue/config'
import ToastService from 'primevue/toastservice'
import Aura from '@primeuix/themes/aura'
import 'primeicons/primeicons.css'
import './style.css'
import App from './App.vue'
import { useGameStore, generateTileId } from './stores/gameStore'
import type { GameState, Tile, PlacedTile } from './types/game'
import { BOARD_LAYOUT } from './types/game'

const app = createApp(App)
const pinia = createPinia()

app.use(pinia)
app.use(PrimeVue, {
  theme: {
    preset: Aura,
    options: {
      darkModeSelector: '.dark-mode'
    }
  }
})
app.use(ToastService)

app.mount('#app')

// Initialize mock game state for development
const store = useGameStore()

// Pre-played tiles on the board (word "HELLO" horizontally starting at center)
// Positions use 1-based coordinates as per the wire protocol
const placedTiles: PlacedTile[] = [
  { position: { x: 6, y: 8 }, tile: { type: 'letter', letter: 'H', value: 4, candidate: false, id: generateTileId() } },
  { position: { x: 7, y: 8 }, tile: { type: 'letter', letter: 'E', value: 1, candidate: false, id: generateTileId() } },
  { position: { x: 8, y: 8 }, tile: { type: 'letter', letter: 'L', value: 1, candidate: false, id: generateTileId() } },
  { position: { x: 9, y: 8 }, tile: { type: 'letter', letter: 'L', value: 1, candidate: false, id: generateTileId() } },
  { position: { x: 10, y: 8 }, tile: { type: 'letter', letter: 'O', value: 1, candidate: false, id: generateTileId() } }
]

// Create mock rack
const rack: Tile[] = [
  { type: 'letter', letter: 'W', value: 4, candidate: true, id: generateTileId() },
  { type: 'letter', letter: 'O', value: 1, candidate: true, id: generateTileId() },
  { type: 'letter', letter: 'R', value: 1, candidate: true, id: generateTileId() },
  { type: 'letter', letter: 'D', value: 2, candidate: true, id: generateTileId() },
  { type: 'blank', assigned: null, candidate: true, id: generateTileId() },
  { type: 'letter', letter: 'I', value: 1, candidate: true, id: generateTileId() },
  { type: 'letter', letter: 'N', value: 1, candidate: true, id: generateTileId() }
]

// Initialize mock game state
const mockState: GameState = {
  myPlayerNumber: 0,
  playerToMove: 0,
  players: [
    { name: 'You', score: 24, endBonus: undefined, connected: true, lastSeen: undefined },
    { name: 'Alice', score: 32, endBonus: undefined, connected: true, lastSeen: undefined },
    { name: 'Bob', score: 18, endBonus: undefined, connected: false, lastSeen: Date.now() - 300000 }
  ],
  moveHistory: [
    {
      type: 'boardMove',
      playerIndex: 1,
      overallScore: 32,
      wordsMade: [{ word: 'HELLO', score: 32 }]
    },
    {
      type: 'pass',
      playerIndex: 2
    }
  ],
  tilesRemaining: 82,
  potentialScore: null,
  lastMoveReceived: Date.now(),
  chatMessages: [
    { type: 'message', user: 'Alice', message: 'Good luck everyone!' },
    { type: 'message', user: 'Bob', message: 'Let\'s have fun!' }
  ],
  lastChatMessageReceived: 2,
  lastDefinitionReceived: 0,
  rack,
  boardLayout: BOARD_LAYOUT,
  placedTiles,
  gameEnded: false
}

store.initializeGame(mockState)
