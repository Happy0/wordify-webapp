<script setup lang="ts">
import { computed, inject } from 'vue'
import Card from 'primevue/card'
import Button from 'primevue/button'
import NavigationButton from '@/components/common/NavigationButton.vue'
import MiniBoard from '@/components/home/MiniBoard.vue'
import type { TileValueMap } from '@/common/board-text-presentation'

export interface GameSummary {
  gameId: string
  boardString: string
  yourMove: boolean
  lastActivity: string
}

const props = defineProps<{
  games: GameSummary[]
  tileValues: TileValueMap
}>()

const isLoggedIn = inject<boolean>('isLoggedIn', false)

const hasGames = computed(() => props.games.length > 0)

function navigateToCreateGame() {
  window.location.href = '/create-lobby'
}

function navigateToLogin() {
  window.location.href = '/auth/login'
}
</script>

<template>
  <div class="home-view min-h-dvh bg-stone-100 flex flex-col">
    <NavigationButton :is-logged-in="isLoggedIn" />

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
              :tile-values="tileValues"
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
