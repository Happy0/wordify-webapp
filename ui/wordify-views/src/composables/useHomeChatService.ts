import { ref } from 'vue'
import type { IGameTransport } from '@/services/interfaces'
import type { TextChatMessage } from '@/types/chat'

export function useHomeChatService(transport: IGameTransport) {
  const messages = ref<TextChatMessage[]>([])
  const lastChatMessageReceived = ref(0)

  transport.onMessage((raw) => {
    try {
      const data = JSON.parse(raw)
      if (data.command === 'playerChat') {
        const p = data.payload
        messages.value.push({
          type: 'text',
          user: p.player,
          text: p.message,
          when: p.when
        })
        lastChatMessageReceived.value = p.messageNumber
      }
    } catch {
      // Ignore parse errors
    }
  })

  function sendMessage(text: string): void {
    transport.send(JSON.stringify({ command: 'say', payload: { message: text } }))
  }

  return { messages, lastChatMessageReceived, sendMessage }
}
