<script setup lang="ts">
import { ref, computed, onMounted, onUnmounted } from 'vue'
import Button from 'primevue/button'
import { useNotificationStore } from '@/stores/notificationStore'
import { storeToRefs } from 'pinia'

const props = withDefaults(defineProps<{
  isLoggedIn: boolean
  position?: 'fixed' | 'inline'
}>(), {
  position: 'fixed'
})

const notificationStore = useNotificationStore()
const { notifications } = storeToRefs(notificationStore)

const isOpen = ref(false)
const isHovered = ref(false)

const hasUnread = computed(() =>
  notifications.value.some(n => n.notificationReadAt === undefined)
)

const isFixed = computed(() => props.position === 'fixed')

// Match NavigationButton opacity behaviour: semi-transparent when idle and no unread
const bellOpacity = computed(() => {
  if (!isFixed.value) return 1
  if (isOpen.value || isHovered.value || hasUnread.value) return 1
  return 0.4
})

async function markAsRead() {
  const now = new Date()
  try {
    await fetch('/api/notifications', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ beforeDateTimeInclusive: now.toISOString() })
    })
  } catch {
    // Ignore network errors – still update local state
  }
  const readTime = now.getTime()
  notificationStore.updateNotifications(
    notifications.value.map(n => ({
      ...n,
      notificationReadAt: n.notificationReadAt ?? readTime
    }))
  )
}

function closeMenu() {
  if (hasUnread.value) {
    markAsRead()
  }
  isOpen.value = false
}

function toggleOpen() {
  if (isOpen.value) {
    closeMenu()
  } else {
    isOpen.value = true
  }
}

function handleClickOutside(event: MouseEvent) {
  const target = event.target as HTMLElement
  if (!target.closest('.notification-menu-container')) {
    closeMenu()
  }
}

onMounted(() => {
  document.addEventListener('click', handleClickOutside)
})

onUnmounted(() => {
  document.removeEventListener('click', handleClickOutside)
})

function navigate(url: string) {
  isOpen.value = false
  window.location.href = url
}

function getNotificationText(notification: { notificationDetails: { type: string; inviteFromUser?: string } }): string {
  const details = notification.notificationDetails
  if (details.type === 'gameInvite' && details.inviteFromUser) {
    return `${details.inviteFromUser} invited you to a game`
  }
  return 'New notification'
}
</script>

<template>
  <div
    v-if="isLoggedIn"
    class="notification-menu-container z-40"
    :class="{
      'fixed top-4 left-16': isFixed,
      'relative': !isFixed
    }"
  >
    <!-- Dropdown panel -->
    <Transition name="notif-dropdown">
      <div
        v-if="isOpen"
        class="absolute left-0 top-12 mt-2 bg-white rounded-lg shadow-lg border border-gray-200 overflow-hidden min-w-64 max-w-xs"
      >
        <div
          v-if="notifications.length === 0"
          class="px-4 py-3 text-sm text-gray-500 whitespace-nowrap"
        >
          No notifications
        </div>
        <div
          v-for="notification in notifications"
          :key="notification.notificationId"
          class="flex items-start gap-3 px-4 py-3 cursor-pointer transition-colors duration-150 border-l-4"
          :class="{
            'bg-blue-50 hover:bg-blue-100 border-blue-500': !notification.notificationReadAt,
            'bg-white hover:bg-gray-50 border-transparent': !!notification.notificationReadAt
          }"
          @click="navigate(notification.url)"
        >
          <i
            class="pi pi-envelope mt-0.5 flex-shrink-0 text-sm"
            :class="notification.notificationReadAt ? 'text-gray-400' : 'text-blue-500'"
          />
          <span
            class="text-sm leading-snug"
            :class="notification.notificationReadAt ? 'text-gray-500' : 'text-gray-900 font-semibold'"
          >
            {{ getNotificationText(notification) }}
          </span>
          <span
            v-if="!notification.notificationReadAt"
            class="ml-auto flex-shrink-0 w-2 h-2 mt-1 rounded-full bg-blue-500"
          />
        </div>
      </div>
    </Transition>

    <!-- Bell button with unread badge -->
    <div class="relative inline-block">
      <Button
        icon="pi pi-bell"
        rounded
        :severity="hasUnread ? 'warn' : 'secondary'"
        class="notification-bell transition-all duration-200"
        :style="{ opacity: bellOpacity }"
        aria-label="Notifications"
        @click.stop="toggleOpen"
        @mouseenter="isHovered = true"
        @mouseleave="isHovered = false"
      />
      <span
        v-if="hasUnread"
        class="absolute -top-1 -right-1 w-3 h-3 bg-red-500 rounded-full animate-pulse pointer-events-none"
      />
    </div>
  </div>
</template>

<style scoped>
.notification-bell {
  width: 2.5rem;
  height: 2.5rem;
}

.notif-dropdown-enter-active,
.notif-dropdown-leave-active {
  transition: opacity 0.2s ease;
}

.notif-dropdown-enter-from,
.notif-dropdown-leave-to {
  opacity: 0;
}
</style>
