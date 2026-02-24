# Game Invite View

We will have a view that will allow users to decide whether to join a lobby that they've been invited to or to ignore it.

# Design

The page will display a prompt saying 'You have been invited to a game by <configured username>. The game language is in <configured locale>'

# Responsiveness

The view should display nicely on mobile also.

# Configuration

The view will be exposed via the top level 'lib.ts' entry point like the other views are.

It will be configured with the following parameters:

* Websocket URL
* isLoggedIn
* The target game lobby ID of the lobby they have been invited to
* The locale that the game is in (as a string parameter)
* The username of the person that invited the person to the lobby

# Behaviour

When the user clicks 'accept' they will be redirected to the /games/{gameLobbyId}/lobby route. When they click 'Ignore' they will be redirected to the '/' route.

# Navigation and Notifications

This view will have the navigation and notification buttons on display like the other views in the app and will surface the necessary configuration options to populate them with that I have missed in the configuration section above.

# Websocket

The view should connect to the configured websocket URL. I will later specify the messages it should process.