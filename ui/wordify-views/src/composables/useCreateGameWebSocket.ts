import { ref, readonly } from 'vue'
import { WebSocketTransport } from '@/services/websocketTransport'
import type { ConnectionState } from '@/services/interfaces'

export function useCreateGameWebSocket() {
  const connectionState = ref<ConnectionState>('disconnected')
  const transport = new WebSocketTransport()

  transport.onStateChange((state) => {
    connectionState.value = state
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
    connectionState: readonly(connectionState),
    transport,
    connect,
    disconnect
  }
}
