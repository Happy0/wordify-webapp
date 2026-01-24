<script setup lang="ts">
import { ref, watch, nextTick } from 'vue'
import { useGameStore } from '@/stores/gameStore'
import { storeToRefs } from 'pinia'
import InputText from 'primevue/inputtext'
import Button from 'primevue/button'

const emit = defineEmits<{
  (e: 'sendMessage', message: string): void
  (e: 'requestDefinition', word: string): void
}>()

const store = useGameStore()
const { chatMessages } = storeToRefs(store)

const messageInput = ref('')
const chatContainer = ref<HTMLElement | null>(null)

// Auto-scroll to bottom when new messages arrive
watch(
  () => chatMessages.value.length,
  async () => {
    await nextTick()
    if (chatContainer.value) {
      chatContainer.value.scrollTop = chatContainer.value.scrollHeight
    }
  }
)

function handleSubmit() {
  const trimmed = messageInput.value.trim()
  if (!trimmed) return

  // Check for !define command
  if (trimmed.toLowerCase().startsWith('!define ')) {
    const word = trimmed.substring(8).trim()
    if (word) {
      emit('requestDefinition', word)
    }
  } else {
    emit('sendMessage', trimmed)
  }

  messageInput.value = ''
}

function handleKeyDown(e: KeyboardEvent) {
  if (e.key === 'Enter' && !e.shiftKey) {
    e.preventDefault()
    handleSubmit()
  }
}
</script>

<template>
  <div class="game-chat bg-white rounded-lg shadow p-4 flex flex-col h-full">
    <h2 class="text-lg font-semibold text-gray-800 mb-3 flex-shrink-0">Chat</h2>

    <!-- Messages container -->
    <div
      ref="chatContainer"
      class="flex-1 overflow-y-auto space-y-2 min-h-0 mb-3"
    >
      <template v-if="chatMessages.length === 0">
        <p class="text-gray-400 text-sm text-center py-4">
          No messages yet. Type <code class="bg-gray-100 px-1 rounded">!define word</code> to look up definitions.
        </p>
      </template>

      <div
        v-for="(msg, index) in chatMessages"
        :key="index"
        class="chat-message"
      >
        <!-- Regular chat message -->
        <template v-if="msg.type === 'message'">
          <div class="flex gap-2 text-sm">
            <span class="font-semibold text-blue-600 flex-shrink-0">
              {{ msg.user }}:
            </span>
            <span class="text-gray-700 break-words">
              {{ msg.message }}
            </span>
          </div>
        </template>

        <!-- Definition message -->
        <template v-else-if="msg.type === 'definition'">
          <div class="bg-amber-50 border-l-2 border-amber-400 p-2 rounded-r text-sm">
            <div class="flex items-baseline gap-2 mb-1">
              <span class="font-bold text-amber-800 uppercase">{{ msg.word }}</span>
              <span class="text-gray-500 italic text-xs">{{ msg.partOfSpeech }}</span>
            </div>
            <p class="text-gray-700 mb-1">{{ msg.definition }}</p>
            <p v-if="msg.example" class="text-gray-500 italic text-xs">
              "{{ msg.example }}"
            </p>
          </div>
        </template>
      </div>
    </div>

    <!-- Input area -->
    <div class="flex gap-2 flex-shrink-0">
      <InputText
        v-model="messageInput"
        placeholder="Type a message..."
        class="flex-1"
        @keydown="handleKeyDown"
      />
      <Button
        icon="pi pi-send"
        severity="primary"
        :disabled="!messageInput.trim()"
        @click="handleSubmit"
      />
    </div>
  </div>
</template>

<style scoped>
.game-chat {
  max-height: 100%;
}
</style>
