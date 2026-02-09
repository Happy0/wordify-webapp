/// <reference lib="webworker" />

declare const self: ServiceWorkerGlobalScope

self.addEventListener('push', (event: PushEvent) => {
  if (!event.data) return

  const payload = event.data.json() as {
    title?: string
    body?: string
    url?: string
  }

  const title = payload.title ?? 'Wordify'
  const options: NotificationOptions = {
    body: payload.body ?? '',
    icon: '/favicon.ico',
    data: { url: payload.url ?? '/' }
  }

  event.waitUntil(self.registration.showNotification(title, options))
})

self.addEventListener('notificationclick', (event: NotificationEvent) => {
  event.notification.close()

  const url: string = (event.notification.data as { url?: string })?.url ?? '/'

  event.waitUntil(
    self.clients.matchAll({ type: 'window', includeUncontrolled: true }).then((windowClients) => {
      for (const client of windowClients) {
        if (client.url === url && 'focus' in client) {
          return client.focus()
        }
      }
      return self.clients.openWindow(url)
    })
  )
})
