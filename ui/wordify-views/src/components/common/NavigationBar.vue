<script setup lang="ts">
import { ref, computed } from 'vue'
import Button from 'primevue/button'

const props = withDefaults(defineProps<{
  isLoggedIn: boolean
  variant?: 'full' | 'desktop-only' | 'mobile-only'
}>(), {
  variant: 'full'
})

const mobileMenuOpen = ref(false)

const showDesktop = computed(() => props.variant === 'full' || props.variant === 'desktop-only')
const showMobile = computed(() => props.variant === 'full' || props.variant === 'mobile-only')

function toggleMobileMenu() {
  mobileMenuOpen.value = !mobileMenuOpen.value
}

function closeMobileMenu() {
  mobileMenuOpen.value = false
}
</script>

<template>
  <!-- Desktop Navigation -->
  <nav
    v-if="showDesktop"
    class="items-center justify-between px-6 py-3 bg-white border-b border-gray-200"
    :class="variant === 'desktop-only' ? 'hidden lg:flex' : 'hidden md:flex'"
  >
    <div class="flex items-center gap-6">
      <a href="/" class="text-gray-700 hover:text-gray-900 font-medium">Home</a>
    </div>
    <div>
      <a v-if="!isLoggedIn" href="/auth/login" class="text-gray-700 hover:text-gray-900 font-medium">
        Login
      </a>
      <a v-else href="/auth/logout" class="text-gray-700 hover:text-gray-900 font-medium">
        Logout
      </a>
    </div>
  </nav>

  <!-- Mobile Navigation -->
  <div v-if="showMobile" :class="variant === 'mobile-only' ? 'lg:hidden' : 'md:hidden'">
    <Button
      icon="pi pi-bars"
      severity="secondary"
      text
      rounded
      aria-label="Menu"
      @click="toggleMobileMenu"
    />

    <!-- Mobile Menu Overlay -->
    <div
      v-if="mobileMenuOpen"
      class="fixed inset-0 bg-black/50 z-40"
      @click="closeMobileMenu"
    />

    <!-- Mobile Menu Panel -->
    <div
      v-if="mobileMenuOpen"
      class="fixed top-0 right-0 w-64 h-full bg-white shadow-lg z-50 p-4"
    >
      <div class="flex justify-end mb-4">
        <Button
          icon="pi pi-times"
          severity="secondary"
          text
          rounded
          aria-label="Close menu"
          @click="closeMobileMenu"
        />
      </div>

      <nav class="flex flex-col gap-4">
        <a
          href="/"
          class="text-gray-700 hover:text-gray-900 font-medium py-2 border-b border-gray-100"
          @click="closeMobileMenu"
        >
          Home
        </a>
        <a
          v-if="!isLoggedIn"
          href="/auth/login"
          class="text-gray-700 hover:text-gray-900 font-medium py-2 border-b border-gray-100"
          @click="closeMobileMenu"
        >
          Login
        </a>
        <a
          v-else
          href="/auth/logout"
          class="text-gray-700 hover:text-gray-900 font-medium py-2 border-b border-gray-100"
          @click="closeMobileMenu"
        >
          Logout
        </a>
      </nav>
    </div>
  </div>
</template>
