<script setup lang="ts">
import { ref, computed, onMounted, onUnmounted, inject } from 'vue'
import Card from 'primevue/card'
import Button from 'primevue/button'
import AutoComplete from 'primevue/autocomplete'
import NavigationButton from '@/components/common/NavigationButton.vue'
import NotificationMenu from '@/components/common/NotificationMenu.vue'
import { useGameLobbyWebSocket } from '@/composables/useGameLobbyWebSocket'
import { useNotificationSocketMessages } from '@/composables/useNotificationSocketMessages'

const props = withDefaults(defineProps<{
  gameLobbyId: string
  websocketUrl: string
  playerCount: number
  joinedPlayers: string[]
  invitedPlayers?: string[]
  language: string
}>(), {
  invitedPlayers: () => []
})

const isLoggedIn = inject<boolean>('isLoggedIn', false)

const { players, invitedPlayers, connectionState, transport, connect, disconnect } =
  useGameLobbyWebSocket(props.joinedPlayers, props.invitedPlayers)

useNotificationSocketMessages(transport)

// Computed values for display
const playersNeeded = computed(() => props.playerCount - players.value.length)

// UI state
const copied = ref(false)
const copyTimeout = ref<ReturnType<typeof setTimeout> | null>(null)

// Invite username state
const inviteUsername = ref('')
const usernameSuggestions = ref<string[]>([])
const inviteLoading = ref(false)
const inviteError = ref<string | null>(null)

// Generate the shareable link
const shareLink = computed(() => {
  return `${window.location.origin}/games/${props.gameLobbyId}/lobby`
})

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

// Autocomplete usernames from API
async function searchUsernames(event: { query: string }) {
  try {
    const response = await fetch(`/api/usernames?prefix=${encodeURIComponent(event.query)}`)
    if (response.ok) {
      usernameSuggestions.value = await response.json()
    } else {
      usernameSuggestions.value = []
    }
  } catch {
    usernameSuggestions.value = []
  }
}

// Send invite to a player by username
async function sendInvite() {
  if (!inviteUsername.value) return

  inviteLoading.value = true
  inviteError.value = null

  try {
    const response = await fetch(`/games/${props.gameLobbyId}/lobby`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ inviteTargetUsername: inviteUsername.value })
    })

    if (response.ok) {
      inviteUsername.value = ''
    } else {
      inviteError.value = 'Failed to send invite. Please try again.'
    }
  } catch {
    inviteError.value = 'Failed to send invite. Please try again.'
  } finally {
    inviteLoading.value = false
  }
}

onMounted(() => {
  connect(props.websocketUrl)
})

onUnmounted(() => {
  disconnect()
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

            <!-- Invited players section -->
            <div v-if="invitedPlayers.length > 0" class="bg-gray-50 rounded-lg p-4">
              <h3 class="font-medium text-gray-700 mb-3">Invited</h3>
              <ul class="space-y-2">
                <li
                  v-for="(player, index) in invitedPlayers"
                  :key="index"
                  class="flex items-center gap-2 text-gray-500"
                >
                  <span class="w-2 h-2 rounded-full bg-yellow-400"></span>
                  {{ player }}
                </li>
              </ul>
            </div>

            <!-- Invite by username -->
            <div class="flex flex-col gap-2">
              <h3 class="font-medium text-gray-700">Invite a player</h3>
              <div class="flex gap-2">
                <AutoComplete
                  v-model="inviteUsername"
                  :suggestions="usernameSuggestions"
                  placeholder="Enter username..."
                  class="flex-1"
                  input-class="w-full"
                  @complete="searchUsernames"
                />
                <Button
                  label="Invite"
                  icon="pi pi-user-plus"
                  :loading="inviteLoading"
                  :disabled="!inviteUsername"
                  @click="sendInvite"
                />
              </div>
              <p v-if="inviteError" class="text-sm text-red-600">{{ inviteError }}</p>
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
