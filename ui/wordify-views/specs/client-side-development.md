# Client Side Development

We want to create an option to run and develop only the client side of the code against a remote deployment of the server. This gives us a way to try out changes in the UI against real games we have in progress, etc.

# The Approach

## NPM Script

We will add a script to the package.json named 'devWithRemote'

It will accept the base hostname as a command line parameter (e.g. --hostname blah.com )

## Proxying

The script will starts a nodejs expressjs app which:

* Proxies websocket connections to the remote server
* Proxies API requests to the remove server
* Serves HTML pages for each server route that serves HTML pages (Home, Round (game) view, Game Lobby, Create Game View)

### Routes That Serve HTML Pages on the Server

'/' - Serves the home view in views/HomeView.vue . We should just initialise it with an empty game list an let the websocket populate the games when it loads.

'/games/{gameId}' - serves the views/GameView.vue view. We should initialise it with some minimal defaults and let the websocket send the real initial state.

'/create-lobby' - should initialise with just English -> en locale config for now.

'/games/cBrXxH9G/lobby' - Should initialise with sensible defaults and allow the server websocket initial message to populate the real values

We should create these views using the top level lib.ts functions to mount them.

These routes take websocketUrl parameters - these should be passed in as the same path as the view being initiated but with 'ws://' at the front rather than http:// .

### HTML Pages

You should create HTML for each view that the expressjs app serves for the above routes. They should use the library and associated CSS files in the scripts tag to mount the appropriate view.

### Handling Login State

There is a /api/me GET endpoint that returns a JSON payload with a "loggedIn" field that is either true or false. This should be used to populate the appropriate value for the 'isLoggedIn' parameters that are passed in as the config to these views.

In the HTML files we should execute a promise to this endpoint and then mount the view using the result.

## Hot Reloading

When changes are made to the files in the ui folder, the developer should be able to refresh the page to see the changes and not be required to restart the nodejs server.