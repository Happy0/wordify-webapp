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
* A UI indiciation of whether it's the user's move or not in the game.
* Some indication of one or more of the other players in the game

# Configuration

The view will accept an array of games with the following schema:

{
    "boardString": string,
    "players": string[],
    "yourMove": boolean,
    "isLoggedIn": boolean
}

# Code Structure Concerns

The existing GameBoard.vue should be reused for the miniboards but with no drag and drop functionality