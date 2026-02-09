import { ref, onMounted, inject } from 'vue'

const DISMISSED_KEY = 'wordify_push_dismissed'

function urlBase64ToUint8Array(base64String: string): Uint8Array {
  const padding = '='.repeat((4 - (base64String.length % 4)) % 4)
  const base64 = (base64String + padding).replace(/-/g, '+').replace(/_/g, '/')
  const rawData = atob(base64)
  const outputArray = new Uint8Array(rawData.length)
  for (let i = 0; i < rawData.length; ++i) {
    outputArray[i] = rawData.charCodeAt(i)
  }
  return outputArray
}

function arrayBufferToBase64Url(buffer: ArrayBuffer): string {
  const bytes = new Uint8Array(buffer)
  let binary = ''
  for (let i = 0; i < bytes.byteLength; i++) {
    binary += String.fromCharCode(bytes[i]!)
  }
  return btoa(binary).replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '')
}

export function usePushNotifications() {
  const bannerVisible = ref(false)
  const subscribing = ref(false)

  const isLoggedIn = inject<boolean>('isLoggedIn', false)
  const vapidPublicKey = inject<string | null>('vapidPublicKey', null)

  let registration: ServiceWorkerRegistration | null = null

  async function init() {
    if (!isLoggedIn || !vapidPublicKey) return
    if (!('serviceWorker' in navigator) || !('PushManager' in window)) return

    try {
      registration = await navigator.serviceWorker.register('/sw.js')
      await navigator.serviceWorker.ready
    } catch {
      return
    }

    const existingSubscription = await registration.pushManager.getSubscription()
    if (existingSubscription) return

    const permission = Notification.permission
    if (permission === 'denied') return

    if (permission === 'granted') {
      await subscribeAndPost()
      return
    }

    // permission === 'default'
    try {
      const dismissed = localStorage.getItem(DISMISSED_KEY)
      if (dismissed) return
    } catch {
      // localStorage unavailable
    }

    bannerVisible.value = true
  }

  async function subscribeAndPost() {
    if (!registration || !vapidPublicKey) return

    try {
      const subscription = await registration.pushManager.subscribe({
        userVisibleOnly: true,
        applicationServerKey: urlBase64ToUint8Array(vapidPublicKey).buffer as ArrayBuffer
      })

      const key = subscription.getKey('p256dh')
      const auth = subscription.getKey('auth')

      if (!key || !auth) return

      await fetch('/push/subscribe', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          endpoint: subscription.endpoint,
          p256dh: arrayBufferToBase64Url(key),
          auth: arrayBufferToBase64Url(auth),
          expirationTime: subscription.expirationTime ?? null
        })
      })
    } catch {
      // Subscription failed - silently ignore
    }
  }

  async function enableNotifications() {
    subscribing.value = true
    try {
      const permission = await Notification.requestPermission()
      if (permission === 'granted') {
        await subscribeAndPost()
      }
    } finally {
      subscribing.value = false
      bannerVisible.value = false
    }
  }

  function dismissBanner() {
    try {
      localStorage.setItem(DISMISSED_KEY, '1')
    } catch {
      // localStorage unavailable
    }
    bannerVisible.value = false
  }

  onMounted(() => {
    init()
  })

  return { bannerVisible, subscribing, enableNotifications, dismissBanner }
}
