<script setup lang="ts">
import { ref, inject, computed } from 'vue'
import Button from 'primevue/button'
import Select from 'primevue/select'
import Card from 'primevue/card'
import NavigationButton from '@/components/common/NavigationButton.vue'
import NotificationMenu from '@/components/common/NotificationMenu.vue'

const props = defineProps<{
  locales: Record<string, string>
}>()

const isLoggedIn = inject<boolean>('isLoggedIn', false)

// Form state
const numPlayers = ref(2)
const selectedLocale = ref<string | null>(null)
const isSubmitting = ref(false)
const error = ref<string | null>(null)

// Player count options
const playerOptions = [
  { label: '2 Players', value: 2 },
  { label: '3 Players', value: 3 },
  { label: '4 Players', value: 4 }
]

// Convert locales prop to Select options
const localeOptions = computed(() => {
  return Object.entries(props.locales).map(([label, value]) => ({
    label,
    value
  }))
})

// Set default locale to first option
const firstLocale = localeOptions.value[0]
if (firstLocale && !selectedLocale.value) {
  selectedLocale.value = firstLocale.value
}

async function handleCreate() {
  if (!selectedLocale.value) {
    error.value = 'Please select a locale'
    return
  }

  isSubmitting.value = true
  error.value = null

  try {
    const response = await fetch(`${window.location.origin}/games`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        num_players: numPlayers.value,
        locale: selectedLocale.value
      })
    })

    if (!response.ok) {
      throw new Error(`Failed to create game: ${response.statusText}`)
    }

    const gameId = await response.text()
    window.location.href = `/games/${gameId}/lobby`
  } catch (err) {
    error.value = err instanceof Error ? err.message : 'Failed to create game'
    isSubmitting.value = false
  }
}
</script>

<template>
  <div class="create-game-view min-h-dvh bg-stone-100 flex flex-col">
    <NavigationButton :is-logged-in="isLoggedIn" />
    <NotificationMenu :is-logged-in="isLoggedIn" />

    <div class="flex-1 flex items-center justify-center p-4">
      <Card class="w-full max-w-md">
        <template #title>
          <h1 class="text-2xl font-bold text-center">Create Game</h1>
        </template>

        <template #content>
          <div class="flex flex-col gap-6">
            <!-- Number of Players -->
            <div class="flex flex-col gap-2">
              <label for="num-players" class="font-medium text-gray-700">
                Number of Players
              </label>
              <Select
                id="num-players"
                v-model="numPlayers"
                :options="playerOptions"
                option-label="label"
                option-value="value"
                class="w-full"
              />
            </div>

            <!-- Locale -->
            <div class="flex flex-col gap-2">
              <label for="locale" class="font-medium text-gray-700">
                Language / Dictionary
              </label>
              <Select
                id="locale"
                v-model="selectedLocale"
                :options="localeOptions"
                option-label="label"
                option-value="value"
                placeholder="Select a locale"
                class="w-full"
              />
            </div>

            <!-- Error message -->
            <div v-if="error" class="text-red-600 text-sm">
              {{ error }}
            </div>

            <!-- Create button -->
            <Button
              label="Create Game"
              :loading="isSubmitting"
              :disabled="isSubmitting || !selectedLocale"
              class="w-full"
              @click="handleCreate"
            />
          </div>
        </template>
      </Card>
    </div>
  </div>
</template>
