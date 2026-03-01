<script setup lang="ts">
import { computed, inject, onMounted, onUnmounted, ref, watch } from 'vue'
import Card from 'primevue/card'
import Button from 'primevue/button'
import Toast from 'primevue/toast'
import { useToast } from 'primevue/usetoast'
import NavigationButton from '@/components/common/NavigationButton.vue'
import NotificationMenu from '@/components/common/NotificationMenu.vue'
import MiniBoard from '@/components/home/MiniBoard.vue'
import ChatBox from '@/components/chat/ChatBox.vue'
import { useHomeWebSocket } from '@/composables/useHomeWebSocket'
import { useNotificationSocketMessages } from '@/composables/useNotificationSocketMessages'
import { useHomeChatService } from '@/composables/useHomeChatService'
import type { GameSummary } from '@/lib/home'

const props = defineProps<{
  games: GameSummary[]
}>()

const isLoggedIn = inject<boolean>('isLoggedIn', false)
const toast = useToast()

const { games, connectionState, transport, connect, disconnect } = useHomeWebSocket(props.games)
useNotificationSocketMessages(transport)
const chatService = useHomeChatService(transport)

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

// Unread chat message tracking
const LAST_SEEN_KEY = 'wordify_lastSeenHomeChat'
const lastSeenChatMessage = ref(0)

function loadLastSeenChatMessage() {
  try {
    const stored = localStorage.getItem(LAST_SEEN_KEY)
    if (stored) {
      lastSeenChatMessage.value = parseInt(stored, 10) || 0
    }
  } catch {
    // localStorage may not be available
  }
}

function saveLastSeenChatMessage() {
  try {
    localStorage.setItem(LAST_SEEN_KEY, chatService.lastChatMessageReceived.value.toString())
    lastSeenChatMessage.value = chatService.lastChatMessageReceived.value
  } catch {
    // localStorage may not be available
  }
}

const hasUnreadMessages = computed(
  () => chatService.lastChatMessageReceived.value > lastSeenChatMessage.value
)

// Mobile chat panel state
const chatPanelOpen = ref(false)
let handlingPopState = false

// Consume stale history entry when panel is closed via UI (not back button)
watch(chatPanelOpen, (newVal, oldVal) => {
  if (oldVal && !newVal && !handlingPopState) {
    history.back()
  }
}, { flush: 'sync' })

function openChat() {
  chatPanelOpen.value = true
  history.pushState({ mobilePanel: 'chat' }, '')
}

function closeChat() {
  saveLastSeenChatMessage()
  chatPanelOpen.value = false
}

function handlePopState(_: PopStateEvent) {
  handlingPopState = true
  if (chatPanelOpen.value) {
    closeChat()
    handlingPopState = false
    return
  }
  handlingPopState = false
}

onMounted(() => {
  if (isLoggedIn) {
    connect()
  }
  loadLastSeenChatMessage()
  window.addEventListener('popstate', handlePopState)
})

onUnmounted(() => {
  disconnect()
  window.removeEventListener('popstate', handlePopState)
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

    <!-- Main layout: content + desktop chat column -->
    <!-- max-w-7xl keeps the chat from drifting too far right on ultra-wide screens -->
    <div class="flex-1 flex max-w-7xl mx-auto w-full">
      <!-- Main content area -->
      <div class="flex-1 flex items-center justify-center p-4">
        <!-- Logged in with games -->
        <!-- Fixed size on desktop (lg:w-[56rem] fits 4 mini-boards; lg:h-[93vh] = 25% taller than chat).   -->
        <!-- On mobile the card is full-width with natural height and no internal scroll.                     -->
        <div
          v-if="isLoggedIn && hasGames"
          class="bg-white rounded-lg shadow p-6
                 w-full lg:w-[56rem]
                 flex flex-col
                 lg:h-[93vh]"
        >
          <h1 class="text-2xl font-bold text-center mb-4 flex-shrink-0">Your Games</h1>
          <div class="flex-1 min-h-0 overflow-y-auto">
            <!-- Mobile: 2-column grid. Desktop: flex-wrap with justify-center so boards
                 grow outward from the middle (1 game = centred, 2 = centred pair, etc.) -->
            <div class="grid grid-cols-2 sm:grid-cols-3 gap-4 p-1
                        lg:flex lg:flex-wrap lg:justify-center">
              <div
                v-for="game in games"
                :key="game.gameId"
                class="lg:w-[200px] lg:flex-none"
              >
                <MiniBoard
                  :game-id="game.gameId"
                  :board-string="game.boardString"
                  :your-move="game.yourMove"
                  :last-activity="game.lastActivity"
                  :tile-values="game.tileValues"
                  :other-players="game.otherPlayers"
                />
              </div>
            </div>
          </div>
        </div>

        <!-- Logged in but no games -->
        <div
          v-else-if="isLoggedIn && !hasGames"
          class="bg-white rounded-lg shadow p-6
                 w-full lg:w-[56rem]
                 flex flex-col items-center justify-center
                 lg:h-[93vh]"
        >
          <div class="flex flex-col gap-6 text-center max-w-md">
            <h1 class="text-2xl font-bold text-center">Welcome!</h1>
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
        </div>

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

      <!-- Desktop chat column (logged in only) -->
      <!-- pt-[12.5vh] positions the chat centred on initial load; sticky top-[12.5vh] keeps it there while scrolling -->
      <div v-if="isLoggedIn" class="hidden lg:flex w-96 flex-shrink-0 justify-center items-start pt-[12.5vh]">
        <div class="sticky top-[12.5vh] w-[22rem]">
          <div class="h-[75vh]">
            <ChatBox
              class="h-full"
              :messages="chatService.messages.value"
              title="Chat"
              placeholder="Say something to everyone..."
              history-hint="Chat history is kept for 2 months"
              @send="chatService.sendMessage"
            />
          </div>
        </div>
      </div>
    </div>

    <!-- Mobile chat FAB (logged in only) -->
    <div v-if="isLoggedIn" class="lg:hidden fixed bottom-6 right-6 z-40">
      <div class="relative">
        <Button
          icon="pi pi-comments"
          :severity="hasUnreadMessages ? 'warn' : 'primary'"
          size="large"
          rounded
          @click="openChat"
        />
        <span
          v-if="hasUnreadMessages"
          class="absolute -top-1 -right-1 w-3 h-3 bg-red-500 rounded-full animate-pulse"
        />
      </div>
    </div>

    <!-- Mobile chat panel overlay -->
    <Transition name="slide">
      <div
        v-if="chatPanelOpen"
        class="fixed inset-0 z-50 flex flex-col bg-stone-100 lg:hidden"
      >
        <div class="flex items-center justify-between px-4 py-3 bg-white shadow-sm">
          <h2 class="text-lg font-semibold text-gray-800">Chat</h2>
          <Button
            icon="pi pi-times"
            severity="secondary"
            size="small"
            rounded
            @click="closeChat"
          />
        </div>
        <div class="flex-1 p-4 min-h-0">
          <ChatBox
            class="h-full"
            :messages="chatService.messages.value"
            title="Chat"
            placeholder="Say something to everyone..."
            history-hint="Chat history is kept for 2 months"
            @send="chatService.sendMessage"
          />
        </div>
      </div>
    </Transition>
  </div>
</template>

<style scoped>
.slide-enter-active,
.slide-leave-active {
  transition: transform 0.3s ease;
}

.slide-enter-from,
.slide-leave-to {
  transform: translateY(100%);
}
</style>
