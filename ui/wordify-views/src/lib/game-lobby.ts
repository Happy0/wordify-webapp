import { createApp, type App } from 'vue'
import { createPinia } from 'pinia'
import PrimeVue from 'primevue/config'
import Aura from '@primeuix/themes/aura'
import 'primeicons/primeicons.css'
import '../style.css'
import GameLobbyView from '../views/GameLobbyView.vue'

export interface GameLobbyOptions {
  /**
   * The unique identifier for the game lobby
   * Used to construct the shareable link
   */
  gameLobbyId: string

  /**
   * WebSocket URL to connect to for lobby state updates
   */
  websocketUrl: string

  /**
   * Whether the user is currently logged in
   * Controls the Login/Logout option in navigation
   */
  isLoggedIn?: boolean
}

export interface GameLobbyInstance {
  app: App
  unmount: () => void
}

export function createGameLobby(
  element: string | HTMLElement,
  options: GameLobbyOptions
): GameLobbyInstance {
  const app = createApp(GameLobbyView, {
    gameLobbyId: options.gameLobbyId,
    websocketUrl: options.websocketUrl
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
