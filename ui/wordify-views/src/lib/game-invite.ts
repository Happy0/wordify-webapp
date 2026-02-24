import { createApp, type App } from 'vue'
import { createPinia } from 'pinia'
import PrimeVue from 'primevue/config'
import Aura from '@primeuix/themes/aura'
import 'primeicons/primeicons.css'
import '../style.css'
import GameInviteView from '../views/GameInviteView.vue'
import { useNotificationStore } from '../stores/notificationStore'
import type { NotificationItem } from '../types/notifications'

export interface GameInviteOptions {
  /**
   * WebSocket URL to connect to
   */
  websocketUrl: string

  /**
   * Whether the user is currently logged in
   * Controls the Login/Logout option in navigation
   */
  isLoggedIn?: boolean

  /**
   * The unique identifier for the game lobby the user has been invited to
   */
  gameLobbyId: string

  /**
   * The locale/language the game is set up with
   */
  locale: string

  /**
   * The username of the person who sent the invite
   */
  invitedByUsername: string

  /**
   * Array of notifications to display in the notification bell menu
   */
  notifications?: NotificationItem[]
}

export interface GameInviteInstance {
  app: App
  unmount: () => void
  updateNotifications: (notifications: NotificationItem[]) => void
}

export function createGameInvite(
  element: string | HTMLElement,
  options: GameInviteOptions
): GameInviteInstance {
  const app = createApp(GameInviteView, {
    websocketUrl: options.websocketUrl,
    gameLobbyId: options.gameLobbyId,
    locale: options.locale,
    invitedByUsername: options.invitedByUsername
  })

  const pinia = createPinia()

  app.use(pinia)
  app.use(PrimeVue, {
    theme: {
      preset: Aura,
      options: {
        darkModeSelector: '.dark-mode'
      }
    }
  })

  // Provide isLoggedIn to all components
  app.provide('isLoggedIn', options.isLoggedIn ?? false)

  app.mount(element)

  // Seed the notification store with any initial notifications
  const notifStore = useNotificationStore()
  notifStore.updateNotifications(options.notifications ?? [])

  return {
    app,
    unmount: () => {
      app.unmount()
    },
    updateNotifications: (notifications: NotificationItem[]) => notifStore.updateNotifications(notifications)
  }
}
