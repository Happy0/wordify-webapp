import { createApp, type App } from 'vue'
import { createPinia } from 'pinia'
import PrimeVue from 'primevue/config'
import Aura from '@primeuix/themes/aura'
import 'primeicons/primeicons.css'
import '../style.css'
import CreateGameView from '../views/CreateGameView.vue'

export interface CreateGameOptions {
  /**
   * Map of locale display names to locale values
   * Key: Display name shown to user (e.g., "English (US)")
   * Value: Locale code sent to server (e.g., "en_us")
   */
  locales: Record<string, string>

  /**
   * Whether the user is currently logged in
   * Controls the Login/Logout option in navigation
   */
  isLoggedIn?: boolean
}

export interface CreateGameInstance {
  app: App
  unmount: () => void
}

export function createCreateGame(
  element: string | HTMLElement,
  options: CreateGameOptions
): CreateGameInstance {
  const app = createApp(CreateGameView, {
    locales: options.locales
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

  return {
    app,
    unmount: () => {
      app.unmount()
    }
  }
}
