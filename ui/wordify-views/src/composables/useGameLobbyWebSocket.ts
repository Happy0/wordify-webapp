import { ref, readonly, type Ref } from 'vue'
import { WebSocketTransport } from '@/services/websocketTransport'
import type { ConnectionState } from '@/services/interfaces'

export function useGameLobbyWebSocket(initialPlayers: string[], initialInvitedPlayers: string[]) {
  const players = ref<string[]>([...initialPlayers])
  const invitedPlayers = ref<string[]>([...initialInvitedPlayers])
  const connectionState = ref<ConnectionState>('disconnected')
  const transport = new WebSocketTransport()

  transport.onStateChange((state) => {
    connectionState.value = state
  })

  transport.onMessage((message) => {
    try {
      const data = JSON.parse(message)
      switch (data.command) {
        case 'startGame':
          if (data.payload?.gameId) {
            window.location.href = `/games/${data.payload.gameId}`
          }
          break
        case 'joined':
          if (data.payload?.name) {
            players.value.push(data.payload.name)
            invitedPlayers.value = invitedPlayers.value.filter((p) => p !== data.payload.name)
          }
          break
        case 'playerInvited':
          if (data.payload?.invitedPlayer && !invitedPlayers.value.includes(data.payload.invitedPlayer)) {
            invitedPlayers.value.push(data.payload.invitedPlayer)
          }
          break
      }
    } catch (err) {
      console.error('Failed to parse websocket message:', err)
    }
  })

  function connect(url: string) {
    transport.connect(url)
  }

  function disconnect() {
    transport.disconnect()
  }

  return {
    players: players as Ref<string[]>,
    invitedPlayers: invitedPlayers as Ref<string[]>,
    connectionState: readonly(connectionState),
    transport,
    connect,
    disconnect
  }
}
