// Composable for accessing the game controller

import { ref, readonly } from 'vue'
import { GameController, WebSocketTransport } from '@/services'
import type { IGameCommandSender, ConnectionState } from '@/services/interfaces'

// Singleton instances - created immediately so controller is never null
const connectionState = ref<ConnectionState>('disconnected')
const transport = new WebSocketTransport()
const controller: IGameCommandSender = new GameController(transport)

// Set up state change handler
transport.onStateChange((state) => {
  connectionState.value = state
})

export function useGameController(): {
  controller: IGameCommandSender
  connectionState: Readonly<typeof connectionState>
  connect: (url: string) => void
  disconnect: () => void
} {
  return {
    controller,
    connectionState: readonly(connectionState),
    connect: (url: string) => {
      controller.connect(url)
    },
    disconnect: () => {
      controller.disconnect()
    }
  }
}
