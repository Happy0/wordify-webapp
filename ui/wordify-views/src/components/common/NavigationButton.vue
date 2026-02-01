<script setup lang="ts">
import { ref, computed } from 'vue'
import Button from 'primevue/button'

const props = withDefaults(defineProps<{
  isLoggedIn: boolean
  position?: 'fixed' | 'inline'
}>(), {
  position: 'fixed'
})

const isExpanded = ref(false)

interface NavItem {
  label: string
  icon: string
  route: string
}

const navItems = computed<NavItem[]>(() => {
  const items: NavItem[] = [
    { label: 'Home', icon: 'pi-home', route: '/' },
    { label: 'Create Game', icon: 'pi-plus', route: '/create-lobby' }
  ]

  if (props.isLoggedIn) {
    items.push({ label: 'Logout', icon: 'pi-sign-out', route: '/auth/logout' })
  } else {
    items.push({ label: 'Login', icon: 'pi-sign-in', route: '/auth/login' })
  }

  return items
})

const isFixed = computed(() => props.position === 'fixed')
const isInline = computed(() => props.position === 'inline')

function toggleMenu() {
  isExpanded.value = !isExpanded.value
}

function navigate(route: string) {
  isExpanded.value = false
  window.location.href = route
}

function handleClickOutside(event: MouseEvent) {
  const target = event.target as HTMLElement
  if (!target.closest('.nav-fab-container')) {
    isExpanded.value = false
  }
}

// Close menu when clicking outside
function onMountedHandler() {
  document.addEventListener('click', handleClickOutside)
}

function onUnmountedHandler() {
  document.removeEventListener('click', handleClickOutside)
}

import { onMounted, onUnmounted } from 'vue'
onMounted(onMountedHandler)
onUnmounted(onUnmountedHandler)
</script>

<template>
  <div
    class="nav-fab-container z-40"
    :class="{
      'fixed bottom-4 left-4': isFixed,
      'relative': isInline
    }"
  >
    <!-- Expanded menu items -->
    <Transition name="fab-menu">
      <div
        v-if="isExpanded"
        class="absolute left-0 flex flex-col gap-2"
        :class="{
          'bottom-14 mb-2': isFixed,
          'top-12 mt-2': isInline
        }"
      >
        <TransitionGroup name="fab-item">
          <div
            v-for="(item, index) in navItems"
            :key="item.route"
            class="flex items-center gap-2"
            :class="{
              'origin-bottom-left': isFixed,
              'origin-top-left': isInline
            }"
            :style="{ transitionDelay: `${index * 50}ms` }"
          >
            <Button
              :icon="`pi ${item.icon}`"
              rounded
              class="nav-item-button shadow-lg"
              :aria-label="item.label"
              @click="navigate(item.route)"
            />
            <span
              class="nav-item-label bg-white px-3 py-1.5 rounded-lg shadow-md text-sm font-medium text-gray-700 whitespace-nowrap cursor-pointer hover:bg-gray-100"
              @click="navigate(item.route)"
            >
              {{ item.label }}
            </span>
          </div>
        </TransitionGroup>
      </div>
    </Transition>

    <!-- Main FAB button -->
    <Button
      :icon="isExpanded ? 'pi pi-times' : 'pi pi-bars'"
      rounded
      severity="secondary"
      class="main-fab transition-transform duration-200"
      :class="[
        { 'rotate-90': isExpanded },
        isFixed ? 'shadow-lg' : ''
      ]"
      aria-label="Navigation menu"
      @click.stop="toggleMenu"
    />
  </div>
</template>

<style scoped>
.main-fab {
  width: 2.5rem;
  height: 2.5rem;
}

.nav-item-button {
  width: 2.5rem;
  height: 2.5rem;
}

/* Menu container animation */
.fab-menu-enter-active,
.fab-menu-leave-active {
  transition: opacity 0.2s ease;
}

.fab-menu-enter-from,
.fab-menu-leave-to {
  opacity: 0;
}

/* Individual item animations */
.fab-item-enter-active {
  transition: all 0.2s ease-out;
}

.fab-item-leave-active {
  transition: all 0.15s ease-in;
}

.fab-item-enter-from {
  opacity: 0;
  transform: scale(0.8) translateY(10px);
}

.fab-item-leave-to {
  opacity: 0;
  transform: scale(0.8) translateY(10px);
}

/* Hover effect for menu items */
.nav-item-button:hover + .nav-item-label {
  background-color: #f3f4f6;
}
</style>
