// Composable for accessing the game controller

import { ref, readonly } from 'vue'
import { GameController, WebSocketTransport } from '@/services'
import type { IGameCommandSender, ConnectionState } from '@/services/interfaces'

// Singleton instances
let controller: GameController | null = null
let transport: WebSocketTransport | null = null

const connectionState = ref<ConnectionState>('disconnected')

export function useGameController(): {
  controller: IGameCommandSender | null
  connectionState: Readonly<typeof connectionState>
  connect: (url: string) => void
  disconnect: () => void
} {
  return {
    controller,
    connectionState: readonly(connectionState),
    connect: (url: string) => {
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
    },
    disconnect: () => {
      controller?.disconnect()
    }
  }
}

// Initialize the game controller for SSR scenarios where initial state is provided
export function initializeWithState(initialState: unknown): void {
  // This would be called by server-side rendering to hydrate initial state
  // The actual implementation would parse the initial state and initialize the store
  console.log('Initializing with server-provided state', initialState)
}
