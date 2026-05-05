declare global {
  interface Window {
    Wordify?: Record<string, unknown>
  }
}

export function expose(api: Record<string, unknown>): void {
  window.Wordify = window.Wordify ?? {}
  Object.assign(window.Wordify, api)
}
