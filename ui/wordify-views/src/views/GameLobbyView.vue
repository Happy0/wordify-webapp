<script setup lang="ts">
import { ref, computed, onMounted, onUnmounted, inject } from 'vue'
import Card from 'primevue/card'
import Button from 'primevue/button'
import NavigationButton from '@/components/common/NavigationButton.vue'

const props = defineProps<{
  gameLobbyId: string
  websocketUrl: string
}>()

const isLoggedIn = inject<boolean>('isLoggedIn', false)

// Websocket state
const ws = ref<WebSocket | null>(null)
const connectionState = ref<'disconnected' | 'connecting' | 'connected' | 'error'>('disconnected')

// UI state
const copied = ref(false)
const copyTimeout = ref<ReturnType<typeof setTimeout> | null>(null)

// Generate the shareable link
const shareLink = computed(() => {
  return `${window.location.origin}/games/${props.gameLobbyId}/lobby`
})

// Handle websocket messages
function handleMessage(event: MessageEvent) {
  try {
    const data = JSON.parse(event.data)

    if (data.command === 'startGame' && data.payload?.gameId) {
      window.location.href = `/games/${data.payload.gameId}`
    }
    // Future: handle other message types like player joins
  } catch (err) {
    console.error('Failed to parse websocket message:', err)
  }
}

// Connect to websocket
function connect() {
  if (ws.value) {
    ws.value.close()
  }

  connectionState.value = 'connecting'

  try {
    ws.value = new WebSocket(props.websocketUrl)

    ws.value.onopen = () => {
      connectionState.value = 'connected'
    }

    ws.value.onmessage = handleMessage

    ws.value.onerror = () => {
      connectionState.value = 'error'
    }

    ws.value.onclose = () => {
      if (connectionState.value !== 'error') {
        connectionState.value = 'disconnected'
      }
      // Attempt to reconnect after 5 seconds
      setTimeout(() => {
        if (connectionState.value === 'disconnected' || connectionState.value === 'error') {
          connect()
        }
      }, 5000)
    }
  } catch {
    connectionState.value = 'error'
  }
}

// Copy link to clipboard
async function copyLink() {
  try {
    await navigator.clipboard.writeText(shareLink.value)
    copied.value = true

    if (copyTimeout.value) {
      clearTimeout(copyTimeout.value)
    }

    copyTimeout.value = setTimeout(() => {
      copied.value = false
    }, 2000)
  } catch (err) {
    console.error('Failed to copy link:', err)
  }
}

onMounted(() => {
  connect()
})

onUnmounted(() => {
  if (ws.value) {
    ws.value.close()
    ws.value = null
  }
  if (copyTimeout.value) {
    clearTimeout(copyTimeout.value)
  }
})
</script>

<template>
  <div class="game-lobby-view min-h-dvh bg-stone-100 flex flex-col">
    <NavigationButton :is-logged-in="isLoggedIn" />

    <div class="flex-1 flex items-center justify-center p-4">
      <Card class="w-full max-w-md">
        <template #title>
          <h1 class="text-2xl font-bold text-center">Game Lobby</h1>
        </template>

        <template #content>
          <div class="flex flex-col gap-6">
            <!-- Instructions -->
            <p class="text-gray-600 text-center">
              Share this link with your friend to invite them to play:
            </p>

            <!-- Link display -->
            <div class="bg-gray-100 rounded-lg p-3 break-all text-sm text-gray-700 font-mono">
              {{ shareLink }}
            </div>

            <!-- Copy button -->
            <Button
              :label="copied ? 'Copied!' : 'Copy Link'"
              :icon="copied ? 'pi pi-check' : 'pi pi-copy'"
              :severity="copied ? 'success' : 'primary'"
              class="w-full"
              @click="copyLink"
            />

            <!-- Connection status -->
            <div class="flex items-center justify-center gap-2 text-sm">
              <span
                class="w-2 h-2 rounded-full"
                :class="{
                  'bg-green-500': connectionState === 'connected',
                  'bg-yellow-500': connectionState === 'connecting',
                  'bg-red-500': connectionState === 'error',
                  'bg-gray-400': connectionState === 'disconnected'
                }"
              ></span>
              <span class="text-gray-600">
                {{ connectionState === 'connected' ? 'Waiting for players...' :
                   connectionState === 'connecting' ? 'Connecting...' :
                   connectionState === 'error' ? 'Connection error, retrying...' :
                   'Disconnected' }}
              </span>
            </div>
          </div>
        </template>
      </Card>
    </div>
  </div>
</template>
