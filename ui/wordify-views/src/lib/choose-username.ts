import { createApp, type App } from 'vue'
import { createPinia } from 'pinia'
import PrimeVue from 'primevue/config'
import Aura from '@primeuix/themes/aura'
import 'primeicons/primeicons.css'
import '../style.css'
import ChooseUsernameView from '../views/ChooseUsernameView.vue'

export interface ChooseUsernameOptions {
  /**
   * Relative path to navigate to after the username is successfully chosen
   * e.g., "/"
   */
  redirectUrl: string
}

export interface ChooseUsernameInstance {
  app: App
  unmount: () => void
}

export function createChooseUsername(
  element: string | HTMLElement,
  options: ChooseUsernameOptions
): ChooseUsernameInstance {
  const app = createApp(ChooseUsernameView, {
    redirectUrl: options.redirectUrl
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

  app.mount(element)

  return {
    app,
    unmount: () => {
      app.unmount()
    }
  }
}
