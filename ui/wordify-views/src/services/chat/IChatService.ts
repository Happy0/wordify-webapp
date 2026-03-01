import type { Ref } from 'vue'
import type { TextChatMessage } from '@/types/chat'

/**
 * Abstract interface for chat backends.
 *
 * Implementations can use any transport (WebSockets, SSE, HTTP polling, etc.).
 * The ChatBox component and chat composables depend only on this interface so
 * the underlying protocol can be swapped without touching UI code.
 */
export interface IChatService {
  /** Reactive list of received text messages */
  readonly messages: Ref<TextChatMessage[]>

  /** Send a plain text message */
  sendMessage(text: string): void
}
