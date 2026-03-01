<script setup lang="ts" generic="T extends { type: string; when: string }">
import { ref, watch, nextTick, onMounted } from 'vue'
import InputText from 'primevue/inputtext'
import Button from 'primevue/button'

const props = withDefaults(
  defineProps<{
    messages: T[]
    placeholder?: string
    title?: string
    historyHint?: string
  }>(),
  {
    placeholder: 'Type a message...',
    title: 'Chat'
  }
)

const emit = defineEmits<{
  send: [text: string]
}>()

const messageInput = ref('')
const chatContainer = ref<HTMLElement | null>(null)

async function scrollToBottom() {
  await nextTick()
  if (chatContainer.value) {
    chatContainer.value.scrollTop = chatContainer.value.scrollHeight
  }
}

// Scroll to bottom on mount (important for mobile where chat panel is conditionally rendered)
onMounted(scrollToBottom)

// Auto-scroll to bottom when new messages arrive
watch(() => props.messages.length, scrollToBottom)

function handleSubmit() {
  const trimmed = messageInput.value.trim()
  if (!trimmed) return

  emit('send', trimmed)
  messageInput.value = ''

  // Explicitly scroll after sending — the server may not echo immediately,
  // so we can't rely solely on the watch above for outgoing messages.
  scrollToBottom()
}

function handleKeyDown(e: KeyboardEvent) {
  if (e.key === 'Enter' && !e.shiftKey) {
    e.preventDefault()
    handleSubmit()
  }
}
</script>

<template>
  <div class="chat-box bg-white rounded-lg shadow p-4 flex flex-col h-full">
    <h2 class="text-lg font-semibold text-gray-800 mb-3 flex-shrink-0">{{ title }}</h2>

    <!-- Messages container -->
    <div ref="chatContainer" class="flex-1 overflow-y-auto space-y-2 min-h-0 mb-3">
      <!-- History hint sits at the top of the scroll area so it's only visible when scrolled up -->
      <p v-if="props.historyHint" class="text-xs text-gray-400 italic text-center pb-2">{{ props.historyHint }}</p>

      <template v-if="messages.length === 0">
        <!-- Customise empty-state per view -->
        <slot name="empty-state">
          <p class="text-gray-400 text-sm text-center py-4">No messages yet.</p>
        </slot>
      </template>

      <div v-for="(msg, index) in messages" :key="index" class="chat-message">
        <!-- Text messages are always rendered here. -->
        <p v-if="msg.type === 'text'" class="text-base text-gray-700 break-words">
          <span class="font-semibold text-blue-600">{{ (msg as unknown as { user: string }).user }}:</span>
          {{ (msg as unknown as { text: string }).text }}
        </p>
        <!-- Slot for view-specific message types (e.g. definition cards in games). -->
        <slot v-else name="message" :message="msg" />
      </div>
    </div>

    <!-- Input area -->
    <div class="flex gap-2 flex-shrink-0">
      <InputText
        v-model="messageInput"
        :placeholder="placeholder"
        class="flex-1"
        @keydown="handleKeyDown"
      />
      <Button
        icon="pi pi-send"
        severity="primary"
        class="flex-shrink-0"
        :disabled="!messageInput.trim()"
        @click="handleSubmit"
      />
    </div>
  </div>
</template>

<style scoped>
.chat-box {
  max-height: 100%;
}
</style>
