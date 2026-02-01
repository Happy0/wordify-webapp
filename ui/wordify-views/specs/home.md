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
* The last activity time for the game (formatted as relative time, e.g., "5m ago", "2h ago", "3d ago")

# Configuration

The view will accept an array of games with the following schema:

{
    "gameId": string,
    "boardString": string,
    "yourMove": boolean,
    "lastActivity": string (ISO 8601 timestamp),
    "tileValues": Record<string, number> - mapping of letters to point values (per-game to support different locales)
}

Additionally, the view accepts:
* `isLoggedIn`: boolean - whether the user is logged in

# Code Structure Concerns

The existing GameBoard.vue should be reused for the miniboards but with no drag and drop functionality