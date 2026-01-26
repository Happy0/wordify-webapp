import { createApp, ref, type App, type Ref } from 'vue'
import { createPinia } from 'pinia'
import PrimeVue from 'primevue/config'
import ToastService from 'primevue/toastservice'
import Aura from '@primeuix/themes/aura'
import 'primeicons/primeicons.css'
import './style.css'
import AppComponent from './App.vue'
import { useGameStore } from './stores/gameStore'
import { GameController } from './services/gameController'
import { WebSocketTransport } from './services/websocketTransport'
import type { GameState } from './types/game'
import type { ConnectionState, IGameCommandSender } from './services/interfaces'

export interface WordifyOptions {
  initialState: GameState
  websocketUrl?: string
}

export interface WordifyInstance {
  app: App
  unmount: () => void
  updateState: (state: GameState) => void
  controller: IGameCommandSender | null
  connectionState: Ref<ConnectionState>
  connect: (url: string) => void
  disconnect: () => void
}

export function createWordify(
  element: string | HTMLElement,
  options: WordifyOptions | GameState
): WordifyInstance {
  // Support both old signature (just GameState) and new signature (options object)
  const opts: WordifyOptions = 'initialState' in options
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

  app.mount(element)

  // Initialize the store with the provided state
  const store = useGameStore()
  store.initializeGame(opts.initialState)

  // Set up websocket connection if URL provided
  const connectionState = ref<ConnectionState>('disconnected')
  let transport: WebSocketTransport | null = null
  let controller: GameController | null = null

  const connect = (url: string) => {
    if (!transport) {
      transport = new WebSocketTransport()
      transport.onStateChange((state) => {
        connectionState.value = state
      })
    }

    if (!controller) {
      controller = new GameController(transport)
    }

    controller.connect(url)
  }

  const disconnect = () => {
    controller?.disconnect()
  }

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
    get controller() {
      return controller
    },
    connectionState,
    connect,
    disconnect
  }
}

// Export types for TypeScript users
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
} from './types/game'

export type { ConnectionState, IGameCommandSender } from './services/interfaces'

// Export constants
export { BOARD_LAYOUT, BOARD_SIZE } from './types/game'
