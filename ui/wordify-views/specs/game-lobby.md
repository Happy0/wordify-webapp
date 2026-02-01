# Game Lobby View

There will be a 'game lobby' view where users can share an invitation link to join their game lobby.

It will instruct the user to copy a link to their clipboard and send it to the friend that they want to play with.

The link will have the route in the following format: /games/{gameLobbyId}/lobby

# Configuration

The view should receive as its configuration a game lobby ID ('gameLobbyId')

It should also receive a websocket URL to connect to fetch messages about the lobby state.

# Responsiveness Concerns

This view should display well on mobile and desktop

# Websocket Protocol

## Start Game Message

When the websocket receives a message with a schema like the following:

{
    "command": "startGame",
    "payload": {
        "gameId": "<gameId here>"
    }
}

The user should be redirected to /games/{gameId}

Note: in the future there will be websocket messages about users joining the lobby so the handler should be written in a way that anticipates different types of moves in the future