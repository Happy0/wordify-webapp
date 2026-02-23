# Notification Menu Component

We will create a 'notification menu' component that is shared across different views.

# Architecture

The code for this component should live in the 'src/components/common' folder so that it can be shared across views.

# Model

The component should use an array of 'notifications' in its state. The notifications will have the following rough schema:

type NotificationDetails = {
    type: "gameInvite",
    inviteFromUser: string,
    gameLobbyId: string
}

type Notification = { notificationId :: string,
    // millis since epoch
    notificationCreatedAt :: number,
    // undefined if unread
    notificationReadAt :: number | undefined,
    notificationDetails :: NotificationDetails,
    // The URL to redirect to when the notification is clicked
    url: string
  }

# Design

The notification menu will be a 'bell' icon that lights up when the user has any unread notifications. The styling / colour for the light up should be the same as for the chat icon's colour in the game view on mobile.

When clicked, it opens up the list of the user's notifications above or below the bell depending on its positioning.

It should have a 'fixed' and 'in-line' mode like that navigation bar has.

# Responsive Design

This menu show work well on mobile views also.

# Behaviour

When the notification bell is clicked, the server's /api/notifications endpoint should be called with a POST request with a "beforeDateTimeInclusive" with a value that is deserializable by haskell's UTCTime's default deserialization behaviour. This should also cause the bell to no longer be lit up.

The notifications should light up on mouse over. When a notification is clicked, the 'url' field associated with its model should be followed.

# Configuration

It should be possible to initialise the component with an array of notifications in the schema specified above. There should also be a parameter indicating whether the user is logged in.

# Views

This component should be placed next to the navigation button on all the views it is currently present in. On the round view, the 'place tiles to see score' / 'score' component should be made smaller to make space for it if necessary.

# Example

Please update the round and home example views with this component with some notifications in its state so that I can try it out :)