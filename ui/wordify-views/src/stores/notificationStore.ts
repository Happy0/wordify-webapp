import { defineStore } from 'pinia'
import { ref } from 'vue'
import type { NotificationItem } from '@/types/notifications'

export const useNotificationStore = defineStore('notifications', () => {
  const notifications = ref<NotificationItem[]>([])

  function updateNotifications(incoming: NotificationItem[]) {
    notifications.value = incoming
  }

  function addNotification(n: NotificationItem) {
    notifications.value = [n, ...notifications.value].slice(0, 5)
  }

  function markNotificationsAsRead(ids: string[]) {
    const idSet = new Set(ids)
    const readAt = Date.now()
    notifications.value = notifications.value.map((n) =>
      idSet.has(n.notificationId) ? { ...n, notificationReadAt: readAt } : n
    )
  }

  return { notifications, updateNotifications, addNotification, markNotificationsAsRead }
})
