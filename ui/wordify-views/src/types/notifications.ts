export interface GameInviteDetails {
  type: 'gameInvite'
  inviteFromUser: string
  gameLobbyId: string
}

export type NotificationDetails = GameInviteDetails

export interface NotificationItem {
  notificationId: string
  /** milliseconds since epoch */
  notificationCreatedAt: number
  /** undefined if unread */
  notificationReadAt: number | undefined
  notificationDetails: NotificationDetails
  /** The URL to redirect to when the notification is clicked */
  url: string
}
