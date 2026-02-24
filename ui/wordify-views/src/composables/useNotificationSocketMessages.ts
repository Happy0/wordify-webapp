import type { IGameTransport } from '@/services/interfaces'
import { useNotificationStore } from '@/stores/notificationStore'
import type { NotificationItem } from '@/types/notifications'

export function useNotificationSocketMessages(transport: IGameTransport) {
  const notificationStore = useNotificationStore()

  transport.onMessage((message) => {
    try {
      const data = JSON.parse(message)
      if (data.command === 'notificationAdded') {
        notificationStore.addNotification(data.payload as NotificationItem)
      } else if (data.command === 'notificationsRead') {
        notificationStore.markNotificationsAsRead(data.payload as string[])
      }
    } catch {
      // Ignore parse errors - other handlers will report them
    }
  })
}
