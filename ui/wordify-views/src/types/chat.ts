// Core chat types shared across all views and transport implementations

// The fundamental text message type - used by IChatService and all chat-aware views
export type TextChatMessage = {
  type: 'text'
  user: string
  text: string
  // ISO 8601 timestamp
  when: string
}

// Minimum shape constraint for items displayable in ChatBox.
// View-specific message types (e.g. DefinitionMessage in games) extend this.
export type ChatDisplayItem = {
  type: string
  when: string
}
