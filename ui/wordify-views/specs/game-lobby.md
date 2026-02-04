# Game Lobby View

There will be a 'game lobby' view where users can share an invitation link to join their game lobby.

It will instruct the user to copy a link to their clipboard and share it to invite additional players.

The link will have the route in the following format: /games/{gameLobbyId}/lobby

It will display the players that have already joined the lobby and the number of additional players that need to join before the game states as well as the language the game will be played in.

# Configuration

The view should receive as its configuration:
* A game lobby ID ('gameLobbyId')
* Whether the user is logged in
* The websocket URL
* The number of players that the game has been set up with
* An array of strings with the display names of the players that have already joined the lobby
* The language the game is set up with

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

When the websocket receives a message with a schema like the following:

{
    "command": "joined",
    "payload": {
        "name": "<The name of the player that joined>"
    }
}

The UI should be updated with the new player who has joined and the new number of players that are being waited on joining before the game starts

Note: in the future there will be websocket messages about users joining the lobby so the handler should be written in a way that anticipates different types of moves in the future