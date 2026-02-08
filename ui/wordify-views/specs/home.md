# Home View

The home view will display either: 
* The user's games in progress if they are logged in and have games in progress as miniboards
* A 'create game' button linking to the /create-game route if the user is logged in and has no games in progress.
* A 'login' button if the user is not logged in with the message 'log / sign up to create a game or join a game.'

# Responsiveness Concerns

This view should display well on mobile and desktop

# Miniboards

The miniboard should display:
* A mini view of the board of the game in progress
* A UI indication of whether it's the user's move or not in the game
* The last activity time for the game (formatted as relative time, e.g., "5m ago", "2h ago", "3d ago"). These timestamps should refresh every minute so they stay accurate without requiring a page reload.
* The names of the other players in the game (e.g., "vs Alice, Bob")

# Configuration

The view will accept an array of games with the following schema as a 'games' parameter in the view config:

{
    "gameId": string,
    "boardString": string,
    "yourMove": boolean,
    "lastActivity": string (ISO 8601 timestamp),
    "tileValues": Record<string, number> - mapping of letters to point values (per-game to support different locales),
    "otherPlayers": string[] - names of the other players in this game (not the current user)
}

Additionally, the view accepts:
* `isLoggedIn`: boolean - whether the user is logged in

# Websocket

The websocket is served on the same route as the home page (i.e. the client should connect to the same URL it loaded the page from, upgrading to a websocket connection). The server handles the upgrade on the home route handler when the user is logged in.

The websocket sends the following message type to the client:

{
    "command": "gamesUpdate",
    "payload": [{... The same schema as the 'games' parameter outlined in the configuration section ... }]
}

This message is received when:
* A move is made in any of the user's active games
* A new game starts that the user is part of
* An existing game ends

When a game update is received, the full list of active games is sent (not just the changed game). The client should replace its entire games list with the payload.

The websocket should try to reconnect indefinitely every 5 seconds if the connection drops and there should be a 'toast' message for the socket state transitions. Note: no message should be displayed on the first successful connection (unless the connection was not successful.)

It is not a requirement that the websocket should be able to be programmatically disconnected/connected (e.g. we don't need to expose 'connect' and 'disconnect' functions)

# Code Structure Concerns

The existing GameBoard.vue should be reused for the miniboards but with no drag and drop functionality