// WebSocket transport implementation

import type { IGameTransport, ConnectionState, ReconnectParams } from './interfaces'

export class WebSocketTransport implements IGameTransport {
  private ws: WebSocket | null = null
  private state: ConnectionState = 'disconnected'
  private messageHandler: ((message: string) => void) | null = null
  private stateChangeHandler: ((state: ConnectionState) => void) | null = null
  private reconnectAttempts = 0
  private maxReconnectAttempts = 5
  private reconnectDelay = 1000
  private url: string | null = null
  private reconnectParamsGetter: (() => ReconnectParams) | null = null
  private hasConnectedOnce = false

  connect(url: string): void {
    this.url = url
    this.hasConnectedOnce = false
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
        this.reconnectAttempts = 0
        this.hasConnectedOnce = true
        this.setState('connected')
      }

      this.ws.onmessage = (event) => {
        if (this.messageHandler) {
          this.messageHandler(event.data)
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
    if (this.reconnectAttempts >= this.maxReconnectAttempts) {
      return
    }

    this.reconnectAttempts++
    const delay = this.reconnectDelay * Math.pow(2, this.reconnectAttempts - 1)

    setTimeout(() => {
      if (this.state === 'disconnected' || this.state === 'error') {
        this.doConnect()
      }
    }, delay)
  }

  disconnect(): void {
    this.reconnectAttempts = this.maxReconnectAttempts // Prevent reconnection
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
    this.messageHandler = handler
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
