// WebSocket transport implementation

import type { IGameTransport, ConnectionState, ReconnectParams } from './interfaces'

export class WebSocketTransport implements IGameTransport {
  private ws: WebSocket | null = null
  private state: ConnectionState = 'disconnected'
  private messageHandlers: Array<(message: string) => void> = []
  private stateChangeHandler: ((state: ConnectionState) => void) | null = null
  private reconnectDelay = 5000 // Fixed 5 second delay between reconnect attempts
  private url: string | null = null
  private reconnectParamsGetter: (() => ReconnectParams) | null = null
  private hasConnectedOnce = false
  private shouldReconnect = true // Flag to control reconnection behavior

  connect(url: string): void {
    this.url = url
    this.hasConnectedOnce = false
    this.shouldReconnect = true
    this.doConnect()
  }

  setReconnectParamsGetter(getter: () => ReconnectParams): void {
    this.reconnectParamsGetter = getter
  }

  private buildReconnectUrl(): string {
    if (!this.url) return ''

    // Only add params on reconnection (not first connection)
    if (!this.hasConnectedOnce || !this.reconnectParamsGetter) {
      return this.url
    }

    const params = this.reconnectParamsGetter()
    const separator = this.url.includes('?') ? '&' : '?'
    return `${this.url}${separator}chatMessagesSinceMessageNumber=${params.lastChatMessageReceived}&definitionsSinceMessageNumber=${params.lastDefinitionReceived}`
  }

  private doConnect(): void {
    if (!this.url) return

    this.setState('connecting')

    try {
      const connectUrl = this.buildReconnectUrl()
      this.ws = new WebSocket(connectUrl)

      this.ws.onopen = () => {
        this.hasConnectedOnce = true
        this.setState('connected')
      }

      this.ws.onmessage = (event) => {
        for (const handler of this.messageHandlers) {
          handler(event.data)
        }
      }

      this.ws.onerror = () => {
        this.setState('error')
      }

      this.ws.onclose = () => {
        this.ws = null
        if (this.state !== 'error') {
          this.setState('disconnected')
        }
        this.attemptReconnect()
      }
    } catch {
      this.setState('error')
    }
  }

  private attemptReconnect(): void {
    if (!this.shouldReconnect) {
      return
    }

    // Fixed 5 second delay between reconnect attempts, retry indefinitely
    setTimeout(() => {
      if (this.shouldReconnect && (this.state === 'disconnected' || this.state === 'error')) {
        this.doConnect()
      }
    }, this.reconnectDelay)
  }

  disconnect(): void {
    this.shouldReconnect = false // Prevent reconnection
    if (this.ws) {
      this.ws.close()
      this.ws = null
    }
    this.setState('disconnected')
  }

  send(message: string): void {
    if (this.ws && this.state === 'connected') {
      this.ws.send(message)
    } else {
      console.warn('Cannot send message: WebSocket not connected')
    }
  }

  onMessage(handler: (message: string) => void): void {
    this.messageHandlers.push(handler)
  }

  onStateChange(handler: (state: ConnectionState) => void): void {
    this.stateChangeHandler = handler
  }

  getState(): ConnectionState {
    return this.state
  }

  private setState(state: ConnectionState): void {
    this.state = state
    if (this.stateChangeHandler) {
      this.stateChangeHandler(state)
    }
  }
}
