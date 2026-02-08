import { createApp, type App, type Ref } from 'vue'
import { createPinia } from 'pinia'
import PrimeVue from 'primevue/config'
import ToastService from 'primevue/toastservice'
import Aura from '@primeuix/themes/aura'
import 'primeicons/primeicons.css'
import '../style.css'
import AppComponent from '../App.vue'
import { useGameStore } from '../stores/gameStore'
import { useGameController } from '../composables/useGameController'
import type { GameState } from '../types/game'
import type { ConnectionState, IGameCommandSender } from '../services/interfaces'
export interface RoundOptions {
  initialState: GameState
  websocketUrl?: string
  gameId?: string
  isLoggedIn?: boolean
}

export interface RoundInstance {
  app: App
  unmount: () => void
  updateState: (state: GameState) => void
  controller: IGameCommandSender
  connectionState: Ref<ConnectionState>
  connect: (url: string) => void
  disconnect: () => void
}

export function createRound(
  element: string | HTMLElement,
  options: RoundOptions | GameState
): RoundInstance {
  // Support both old signature (just GameState) and new signature (options object)
  const opts: RoundOptions = 'initialState' in options
    ? options
    : { initialState: options }

  const app = createApp(AppComponent)
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

  // Provide isLoggedIn to all components
  app.provide('isLoggedIn', opts.isLoggedIn ?? false)

  // Provide tileValues for locale-specific letter sets (used by BlankTileSelector)
  app.provide('tileValues', opts.initialState.tileValues ?? null)

  app.mount(element)

  // Initialize the store with the provided state
  const store = useGameStore()
  store.initializeGame(opts.initialState)

  // Set the game ID if provided (used for localStorage scoping)
  if (opts.gameId) {
    store.setGameId(opts.gameId)
  }

  // Use the shared singleton controller from the composable
  const { controller, connectionState, connect, disconnect } = useGameController()

  // Auto-connect if websocketUrl was provided
  if (opts.websocketUrl) {
    connect(opts.websocketUrl)
  }

  return {
    app,
    unmount: () => {
      disconnect()
      app.unmount()
    },
    updateState: (state: GameState) => store.initializeGame(state),
    controller,
    connectionState,
    connect,
    disconnect
  }
}

// Re-export types for TypeScript users
// Note: Only input types are exported (TileInput, etc.) - internal types with id/candidate are not exposed
export type {
  GameState,
  TileInput,
  BlankTileInput,
  LetterTileInput,
  PlayerSummary,
  MoveSummary,
  ChatMessage,
  SquareType,
  PlacedTile
} from '../types/game'

export type { ConnectionState, IGameCommandSender } from '../services/interfaces'

export type { TileValueMap } from '../common/tile-value-map'

// Export constants
export { BOARD_LAYOUT, BOARD_SIZE } from '../types/game'

// Export board text representation utilities
export { fromBoardTextRepresentation } from '../common/board-text-presentation'
