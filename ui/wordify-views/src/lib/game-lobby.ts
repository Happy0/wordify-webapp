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

  /**
   * The total number of players the game has been set up with
   */
  playerCount: number

  /**
   * Array of display names of players who have already joined the lobby
   */
  joinedPlayers: string[]

  /**
   * The language the game is set up with
   */
  language: string
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
    websocketUrl: options.websocketUrl,
    playerCount: options.playerCount,
    joinedPlayers: options.joinedPlayers,
    language: options.language
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
