<script setup lang="ts">
import { ref, computed, onMounted, onUnmounted, inject } from 'vue'
import Card from 'primevue/card'
import Button from 'primevue/button'
import NavigationButton from '@/components/common/NavigationButton.vue'
import NotificationMenu from '@/components/common/NotificationMenu.vue'

const props = defineProps<{
  gameLobbyId: string
  websocketUrl: string
  playerCount: number
  joinedPlayers: string[]
  language: string
}>()

const isLoggedIn = inject<boolean>('isLoggedIn', false)

// Websocket state
const ws = ref<WebSocket | null>(null)
const connectionState = ref<'disconnected' | 'connecting' | 'connected' | 'error'>('disconnected')

// Player state - tracks players who have joined (starts with initial joined players)
const players = ref<string[]>([...props.joinedPlayers])

// Computed values for display
const playersNeeded = computed(() => props.playerCount - players.value.length)

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

    switch (data.command) {
      case 'startGame':
        if (data.payload?.gameId) {
          window.location.href = `/games/${data.payload.gameId}`
        }
        break
      case 'joined':
        if (data.payload?.name) {
          players.value.push(data.payload.name)
        }
        break
    }
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
    <NotificationMenu :is-logged-in="isLoggedIn" />

    <div class="flex-1 flex items-center justify-center p-4">
      <Card class="w-full max-w-md">
        <template #title>
          <h1 class="text-2xl font-bold text-center">Game Lobby</h1>
        </template>

        <template #content>
          <div class="flex flex-col gap-6">
            <!-- Game info -->
            <div class="text-center text-gray-600">
              <p class="font-medium">Language: <span class="text-gray-800">{{ language }}</span></p>
            </div>

            <!-- Players section -->
            <div class="bg-gray-50 rounded-lg p-4">
              <h3 class="font-medium text-gray-700 mb-3">Players ({{ players.length }}/{{ playerCount }})</h3>

              <!-- Joined players list -->
              <ul v-if="players.length > 0" class="space-y-2 mb-3">
                <li
                  v-for="(player, index) in players"
                  :key="index"
                  class="flex items-center gap-2 text-gray-700"
                >
                  <span class="w-2 h-2 rounded-full bg-green-500"></span>
                  {{ player }}
                </li>
              </ul>

              <!-- Waiting message -->
              <p v-if="playersNeeded > 0" class="text-sm text-gray-500">
                Waiting for {{ playersNeeded }} more player{{ playersNeeded !== 1 ? 's' : '' }} to join...
              </p>
              <p v-else class="text-sm text-green-600 font-medium">
                All players have joined! Starting game...
              </p>
            </div>

            <!-- Instructions -->
            <p class="text-gray-600 text-center">
              Share this link to invite additional players:
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
