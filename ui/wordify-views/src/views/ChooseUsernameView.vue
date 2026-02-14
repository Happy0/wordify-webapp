<script setup lang="ts">
import { ref, computed } from 'vue'
import Card from 'primevue/card'
import Button from 'primevue/button'
import InputText from 'primevue/inputtext'
import Message from 'primevue/message'

const props = defineProps<{
  redirectUrl: string
}>()

const username = ref('')
const submitting = ref(false)
const errorMessage = ref<string | null>(null)

const usernamePattern = /^[a-zA-Z0-9_-]+$/

const validationError = computed<string | null>(() => {
  if (username.value.length === 0) return null
  if (username.value.length < 3) return 'Username must be at least 3 characters.'
  if (username.value.length > 20) return 'Username must be 20 characters or fewer.'
  if (!usernamePattern.test(username.value)) return 'Username may only contain letters, numbers, hyphens, and underscores.'
  return null
})

const canSubmit = computed(() => {
  return username.value.length > 0 && validationError.value === null && !submitting.value
})

const displayError = computed(() => validationError.value ?? errorMessage.value)

async function handleSubmit() {
  if (!canSubmit.value) return

  errorMessage.value = null
  submitting.value = true

  try {
    const response = await fetch(`${window.location.origin}/api/username`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ username: username.value })
    })

    if (response.ok) {
      window.location.href = props.redirectUrl
      return
    }

    if (response.status === 409) {
      const body = await response.json()
      errorMessage.value = body.message ?? 'This username is already taken. Please choose another.'
    } else {
      errorMessage.value = 'Something went wrong. Please try again.'
    }
  } catch {
    errorMessage.value = 'Something went wrong. Please try again.'
  } finally {
    submitting.value = false
  }
}
</script>

<template>
  <div class="choose-username-view min-h-dvh bg-stone-100 flex flex-col">
    <div class="flex-1 flex items-center justify-center p-4">
      <Card class="w-full max-w-md">
        <template #title>
          <h1 class="text-2xl font-bold text-center">Choose your username</h1>
        </template>

        <template #content>
          <div class="flex flex-col gap-6">
            <p class="text-gray-600 text-center">
              Pick a username that will be displayed publicly whenever you play games, send messages, or appear on scoreboards. Choose carefully — you won't be able to change it later.
            </p>

            <div class="flex flex-col gap-2">
              <InputText
                v-model="username"
                placeholder="Enter a username"
                class="w-full"
                @keyup.enter="handleSubmit"
              />

              <Message v-if="displayError" severity="error" :closable="false">
                {{ displayError }}
              </Message>
            </div>

            <Button
              label="Claim Username"
              severity="primary"
              class="w-full"
              :disabled="!canSubmit"
              :loading="submitting"
              @click="handleSubmit"
            />
          </div>
        </template>
      </Card>
    </div>
  </div>
</template>
