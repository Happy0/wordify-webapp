import { ref, readonly, type Ref } from 'vue'
import { WebSocketTransport } from '@/services/websocketTransport'
import type { ConnectionState } from '@/services/interfaces'
import type { GameSummary } from '@/lib/home'

export function useHomeWebSocket(initialGames: GameSummary[]) {
  const games = ref<GameSummary[]>(initialGames)
  const connectionState = ref<ConnectionState>('disconnected')
  const transport = new WebSocketTransport()

  transport.onStateChange((state) => {
    connectionState.value = state
  })

  transport.onMessage((message) => {
    const data = JSON.parse(message)
    if (data.command === 'gamesUpdate') {
      games.value = data.payload
    }
  })

  function connect() {
    const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:'
    const url = `${protocol}//${window.location.host}${window.location.pathname}`
    transport.connect(url)
  }

  function disconnect() {
    transport.disconnect()
  }

  return {
    games: games as Ref<GameSummary[]>,
    connectionState: readonly(connectionState),
    connect,
    disconnect
  }
}
