import { computed } from 'vue'
import { useGameStore } from '@/stores/gameStore'
import { useGameController } from '@/composables/useGameController'
import type { IGameChatService, GameChatMessage } from '@/services/chat/IGameChatService'
import type { TextChatMessage } from '@/types/chat'

/**
 * Composable that wires up the game store and controller to satisfy IGameChatService.
 *
 * This is the game's concrete implementation of the abstract chat interface —
 * swap this composable out (returning a different IGameChatService) and the
 * entire chat UI works over a different transport with no other changes needed.
 */
export function useGameChatService(): IGameChatService {
  const store = useGameStore()
  const { controller } = useGameController()

  // Core text messages only — satisfies IChatService.messages
  const messages = computed<TextChatMessage[]>(() =>
    store.chatMessages.filter((m): m is TextChatMessage => m.type === 'text')
  )

  // All messages (text + definitions) sorted by timestamp for display
  const allMessages = computed<GameChatMessage[]>(() =>
    [...store.chatMessages].sort(
      (a, b) => new Date(a.when).getTime() - new Date(b.when).getTime()
    )
  )

  return {
    messages,
    allMessages,
    sendMessage: (text: string) => controller.sendChatMessage(text),
    requestDefinition: (word: string) => controller.requestDefinition(word)
  }
}
