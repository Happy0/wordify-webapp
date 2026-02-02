<script setup lang="ts">
import Card from 'primevue/card'
import Button from 'primevue/button'

const props = defineProps<{
  message: string
}>()

function handleLogin() {
  const returnUrl = encodeURIComponent(window.location.pathname + window.location.search)
  window.location.href = `/login?returnUrl=${returnUrl}`
}
</script>

<template>
  <div class="login-view min-h-dvh bg-stone-100 flex flex-col">
    <div class="flex-1 flex items-center justify-center p-4">
      <Card class="w-full max-w-md">
        <template #title>
          <h1 class="text-2xl font-bold text-center">Login Required</h1>
        </template>

        <template #content>
          <div class="flex flex-col gap-6">
            <p class="text-gray-600 text-center">
              {{ props.message }}
            </p>

            <Button
              label="Login"
              icon="pi pi-sign-in"
              severity="primary"
              class="w-full"
              @click="handleLogin"
            />
          </div>
        </template>
      </Card>
    </div>
  </div>
</template>
