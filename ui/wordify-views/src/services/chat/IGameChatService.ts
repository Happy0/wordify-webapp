import type { Ref } from 'vue'
import type { IChatService } from './IChatService'
import type { TextChatMessage } from '@/types/chat'
import type { DefinitionMessage } from '@/types/game'

// All message types that can appear in the game chat display
export type GameChatMessage = TextChatMessage | DefinitionMessage

/**
 * Extended chat service for game views.
 *
 * Adds game-specific features (word definitions) on top of the core IChatService.
 * The `allMessages` ref includes both text messages and definitions sorted by
 * timestamp, ready for display in GameChat.
 */
export interface IGameChatService extends IChatService {
  /** All game chat messages (text + definitions) sorted by timestamp */
  readonly allMessages: Ref<GameChatMessage[]>

  /** Request a word definition - game-specific feature */
  requestDefinition(word: string): void
}
