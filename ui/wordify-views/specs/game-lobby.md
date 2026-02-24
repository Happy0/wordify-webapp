# Game Lobby View

There will be a 'game lobby' view where users can share an invitation link to join their game lobby.

It will instruct the user to copy a link to their clipboard and share it to invite additional players.

The link will have the route in the following format: /games/{gameLobbyId}/lobby

It will display the players that have already joined the lobby and the number of additional players that need to join before the game states as well as the language the game will be played in.

It will display the players who have been invited to the lobby but haven't yet joined.

# Invite username functionality.

It will have functionality to allow the user to invite other users by username. The /api/usernames endpoint can be used to autocomplete usernames as the user types them.

A POST to the /games/#Text/lobby endpoint can be used to to invite a given player. The request will have a JSON body with an "invited" field with the username of the invited player.

# Configuration

The view should receive as its configuration:
* A game lobby ID ('gameLobbyId')
* Whether the user is logged in
* The websocket URL
* The number of players that the game has been set up with
* An array of strings with the display names of the players that have already joined the lobby
* An array of players who have been invited to the lobby but who haven't yet joined
* The language the game is set up with

It should also receive a websocket URL to connect to fetch messages about the lobby state.

# Responsiveness Concerns

This view should display well on mobile and desktop

# Websocket Protocol

## Server Commands

### Start Game Message

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

When the websocket receives a message with a schema like the following:

{
    "command": "playerInvited",
    "payload": {
        "invitedPlayer": "<The name of the player that was invited>",
        "invitingPlayer": "<The name of the player who sent the invitation>"
    }
}

The UI state should be invited with the updated invitation list