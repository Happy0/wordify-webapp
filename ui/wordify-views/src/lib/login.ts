import { createApp, type App } from 'vue'
import { createPinia } from 'pinia'
import PrimeVue from 'primevue/config'
import Aura from '@primeuix/themes/aura'
import 'primeicons/primeicons.css'
import '../style.css'
import LoginView from '../views/LoginView.vue'

export interface LoginOptions {
  /**
   * Message to display explaining why the user needs to log in
   * e.g., "Log in to join this lobby."
   */
  message: string
}

export interface LoginInstance {
  app: App
  unmount: () => void
}

export function createLogin(
  element: string | HTMLElement,
  options: LoginOptions
): LoginInstance {
  const app = createApp(LoginView, {
    message: options.message
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
