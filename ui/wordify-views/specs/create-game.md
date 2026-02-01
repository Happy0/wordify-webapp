# Create Game View

The create view should have the following options:

* Number of players (Allowed values: 2,3 or 4)
* Locale

# Configuration

The component should accept a map of string to string for the allowed locales.

The map key is the locale name as displayed to the user in the dialog and the value is the 'locale string' as sent to the server (see below.)

# Behaviour

When the 'create' button is clicked it should send an ajax request POST request to the window.location.origin + /games route with a JSON payload with the number of players selected and the locale value like this:

{
      "num_players" : 1,
      "locale" : "es_fise"
}

The server will respond with a game ID in the response body as a string (not a JSON object.)

The client should then navigate to: "/games" + "/" + gameId + "/lobby"

# Responsiveness Concerns

This view should display well on mobile and desktop

# Navigation

This view should make use of the components/common/navigation bar to allow the user to navigate the site. See specs/navigation-bar.md for more info.

# Library Export

This should be exported in the 'lib' folder in a 'create-game.ts' file. The top level src/lib.ts file should re-export it.