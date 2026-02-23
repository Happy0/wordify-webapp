<script setup lang="ts">
import { computed, inject, onMounted, onUnmounted, ref, watch } from 'vue'
import Card from 'primevue/card'
import Button from 'primevue/button'
import Toast from 'primevue/toast'
import { useToast } from 'primevue/usetoast'
import NavigationButton from '@/components/common/NavigationButton.vue'
import NotificationMenu from '@/components/common/NotificationMenu.vue'
import MiniBoard from '@/components/home/MiniBoard.vue'
import { useHomeWebSocket } from '@/composables/useHomeWebSocket'
import type { GameSummary } from '@/lib/home'

const props = defineProps<{
  games: GameSummary[]
}>()

const isLoggedIn = inject<boolean>('isLoggedIn', false)
const toast = useToast()

const { games, connectionState, connect, disconnect } = useHomeWebSocket(props.games)

const hasGames = computed(() => games.value.length > 0)

// Watch for reconnection success (same pattern as GameView.vue)
const hasBeenConnected = ref(false)
const wasDisconnected = ref(false)

watch(connectionState, (newState) => {
  if (newState === 'connected') {
    if (wasDisconnected.value) {
      toast.add({
        severity: 'success',
        summary: 'Connected',
        detail: 'Connection restored',
        life: 3000
      })
    }
    hasBeenConnected.value = true
    wasDisconnected.value = false
  }

  if (hasBeenConnected.value && (newState === 'disconnected' || newState === 'error')) {
    wasDisconnected.value = true
  }
})

onMounted(() => {
  if (isLoggedIn) {
    connect()
  }
})

onUnmounted(() => {
  disconnect()
})

function navigateToCreateGame() {
  window.location.href = '/create-lobby'
}

function navigateToLogin() {
  const returnUrl = encodeURIComponent(window.location.pathname + window.location.search)
  window.location.href = `/login?returnUrl=${returnUrl}`
}
</script>

<template>
  <div class="home-view min-h-dvh bg-stone-100 flex flex-col">
    <Toast />
    <NavigationButton :is-logged-in="isLoggedIn" />
    <NotificationMenu :is-logged-in="isLoggedIn" />

    <div class="flex-1 flex items-center justify-center p-4">
      <!-- Logged in with games -->
      <Card v-if="isLoggedIn && hasGames" class="w-full max-w-4xl">
        <template #title>
          <h1 class="text-2xl font-bold text-center">Your Games</h1>
        </template>

        <template #content>
          <div class="games-grid flex flex-wrap gap-4 justify-center">
            <MiniBoard
              v-for="game in games"
              :key="game.gameId"
              :game-id="game.gameId"
              :board-string="game.boardString"
              :your-move="game.yourMove"
              :last-activity="game.lastActivity"
              :tile-values="game.tileValues"
              :other-players="game.otherPlayers"
            />
          </div>
        </template>
      </Card>

      <!-- Logged in but no games -->
      <Card v-else-if="isLoggedIn && !hasGames" class="w-full max-w-md">
        <template #title>
          <h1 class="text-2xl font-bold text-center">Welcome!</h1>
        </template>

        <template #content>
          <div class="flex flex-col gap-6 text-center">
            <p class="text-gray-600">
              You don't have any games in progress. Start a new game to play with friends!
            </p>

            <Button
              label="Create Game"
              icon="pi pi-plus"
              severity="primary"
              class="w-full"
              @click="navigateToCreateGame"
            />
          </div>
        </template>
      </Card>

      <!-- Not logged in -->
      <Card v-else class="w-full max-w-md">
        <template #title>
          <h1 class="text-2xl font-bold text-center">Welcome to Wordify</h1>
        </template>

        <template #content>
          <div class="flex flex-col gap-6 text-center">
            <p class="text-gray-600">
              Log in or sign up to create a game or join a game.
            </p>

            <Button
              label="Login"
              icon="pi pi-sign-in"
              severity="primary"
              class="w-full"
              @click="navigateToLogin"
            />
          </div>
        </template>
      </Card>
    </div>
  </div>
</template>

<style scoped>
/* Flexbox handles centering naturally */
</style>
