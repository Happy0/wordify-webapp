import { defineStore } from 'pinia'
import { ref } from 'vue'
import type { NotificationItem } from '@/types/notifications'

export const useNotificationStore = defineStore('notifications', () => {
  const notifications = ref<NotificationItem[]>([])

  function updateNotifications(incoming: NotificationItem[]) {
    notifications.value = incoming
  }

  return { notifications, updateNotifications }
})
