import { createApp, type App } from 'vue'
import { createPinia } from 'pinia'
import PrimeVue from 'primevue/config'
import Aura from '@primeuix/themes/aura'
import 'primeicons/primeicons.css'
import '../style.css'
import HomeView from '../views/HomeView.vue'
import type { TileValueMap } from '../common/tile-value-map'

export interface GameSummary {
  /**
   * Unique identifier for the game
   * Used to construct the game URL
   */
  gameId: string

  /**
   * The board state as a comma-delimited string
   * Each element is a letter (e.g., "H"), blank in brackets (e.g., "[I]"), or empty string
   * Ordered by column then row (A1 to O15)
   */
  boardString: string

  /**
   * Whether it's currently the user's turn in this game
   */
  yourMove: boolean

  /**
   * ISO 8601 timestamp of the last activity in the game
   */
  lastActivity: string

  /**
   * A map of letter strings to their point values
   * Required to properly render tiles on the miniboard
   * Different locales can have different tiles with different values
   */
  tileValues: TileValueMap

  /**
   * Names of the other players in this game (not the current user)
   */
  otherPlayers: string[]
}

export interface HomeOptions {
  /**
   * Array of games the user has in progress
   * If empty and logged in, shows "Create Game" button
   */
  games: GameSummary[]

  /**
   * Whether the user is currently logged in
   * Controls whether to show games/create-game or login prompt
   */
  isLoggedIn: boolean
}

export interface HomeInstance {
  app: App
  unmount: () => void
}

export function createHome(
  element: string | HTMLElement,
  options: HomeOptions
): HomeInstance {
  const app = createApp(HomeView, {
    games: options.games
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
  app.provide('isLoggedIn', options.isLoggedIn)

  app.mount(element)

  return {
    app,
    unmount: () => {
      app.unmount()
    }
  }
}
