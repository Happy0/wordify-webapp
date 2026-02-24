<script setup lang="ts">
import { ref, onMounted, onUnmounted, inject } from 'vue'
import Card from 'primevue/card'
import Button from 'primevue/button'
import NavigationButton from '@/components/common/NavigationButton.vue'
import NotificationMenu from '@/components/common/NotificationMenu.vue'

const props = defineProps<{
  gameLobbyId: string
  websocketUrl: string
  locale: string
  invitedByUsername: string
}>()

const isLoggedIn = inject<boolean>('isLoggedIn', false)

// Websocket state
const ws = ref<WebSocket | null>(null)
const connectionState = ref<'disconnected' | 'connecting' | 'connected' | 'error'>('disconnected')

// Handle websocket messages
function handleMessage(event: MessageEvent) {
  try {
    JSON.parse(event.data)
    // Message handling to be specified later
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

function acceptInvite() {
  window.location.href = `/games/${props.gameLobbyId}/lobby`
}

function ignoreInvite() {
  window.location.href = '/'
}

onMounted(() => {
  connect()
})

onUnmounted(() => {
  if (ws.value) {
    ws.value.close()
    ws.value = null
  }
})
</script>

<template>
  <div class="game-invite-view min-h-dvh bg-stone-100 flex flex-col">
    <NavigationButton :is-logged-in="isLoggedIn" />
    <NotificationMenu :is-logged-in="isLoggedIn" />

    <div class="flex-1 flex items-center justify-center p-4">
      <Card class="w-full max-w-md">
        <template #title>
          <h1 class="text-2xl font-bold text-center">Game Invite</h1>
        </template>

        <template #content>
          <div class="flex flex-col gap-6">
            <p class="text-center text-gray-700">
              You have been invited to a game by
              <span class="font-semibold text-gray-900">{{ invitedByUsername }}</span>.
              The game language is in
              <span class="font-semibold text-gray-900">{{ locale }}</span>.
            </p>

            <div class="flex flex-col sm:flex-row gap-3">
              <Button
                label="Accept"
                icon="pi pi-check"
                severity="primary"
                class="flex-1"
                @click="acceptInvite"
              />
              <Button
                label="Ignore"
                icon="pi pi-times"
                severity="secondary"
                class="flex-1"
                @click="ignoreInvite"
              />
            </div>
          </div>
        </template>
      </Card>
    </div>
  </div>
</template>
