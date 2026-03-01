<script setup lang="ts">
import { useGameStore } from '@/stores/gameStore'
import { storeToRefs } from 'pinia'
import ChatBox from '@/components/chat/ChatBox.vue'
import { useGameChatService } from '@/composables/useGameChatService'
import type { DefinitionMessage } from '@/types/game'

const store = useGameStore()
const { isObserver } = storeToRefs(store)

const chatService = useGameChatService()

function handleSend(text: string) {
  // !define <word> is a game-specific command that triggers a definition lookup
  if (text.toLowerCase().startsWith('!define ')) {
    const word = text.substring(8).trim()
    if (word) {
      chatService.requestDefinition(word)
    }
  } else {
    chatService.sendMessage(text)
  }
}
</script>

<template>
  <ChatBox
    :messages="chatService.allMessages.value"
    :title="isObserver ? 'Observer Chat' : 'Chat'"
    :placeholder="isObserver ? 'Say something to other observers...' : 'Type a message or !define <word>...'"
    @send="handleSend"
  >
    <!-- Game-specific empty state -->
    <template #empty-state>
      <p class="text-gray-400 text-sm text-center py-4">
        <template v-if="isObserver">Private observer only chat</template>
        <template v-else>
          No messages yet. Type
          <code class="bg-gray-100 px-1 rounded">!define word</code>
          to look up definitions.
        </template>
      </p>
    </template>

    <!-- Slot only needed for game-specific message types; text messages fall through to ChatBox's built-in renderer. -->
    <template #message="{ message }">
      <div
        v-if="message.type === 'definition'"
        class="bg-amber-50 border-l-2 border-amber-400 p-2 rounded-r text-sm"
      >
        <div class="flex items-baseline gap-2 mb-1">
          <span class="font-bold text-amber-800 uppercase">{{ (message as DefinitionMessage).word }}</span>
          <span class="text-gray-500 italic text-xs">{{ (message as DefinitionMessage).partOfSpeech }}</span>
        </div>
        <p class="text-gray-700 mb-1">{{ (message as DefinitionMessage).definition }}</p>
        <p v-if="(message as DefinitionMessage).example" class="text-gray-500 italic text-xs">
          "{{ (message as DefinitionMessage).example }}"
        </p>
      </div>
    </template>

  </ChatBox>
</template>
