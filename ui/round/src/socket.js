module.exports = function(opts) {

    var m = require('mithril');

    var send = opts.send;
    var controller = opts.ctrl;

    var handlers = {
        "initialise" : function(data) {
            m.startComputation();

            controller.updateRack(data.rack);
            controller.setPlayers(data.players);
            controller.setPlayerNumber(data.playerNumber);
            controller.setPlayerToMove(data.playerMove);
            controller.setTilesRemaining(data.tilesRemaining);

            var commands = data.moveCommands;

            commands.forEach(parseAndIssueCommand);
            m.endComputation();
        },
        "playerBoardMove" : function(data) {
            controller.scrabbleGroundCtrl.highlight.removeAllHighlightedTiles();

            var moveNumber = data.moveNumber;

            // If we've already been sent this move (e.g. while
            // initialising), ignore it
            if (moveNumber <= controller.data.lastMoveReceived)
                return;

            var placed = data.placed;
            var players = data.players;
            var tilesRemaining = data.tilesRemaining;
            controller.setPlayerToMove(data.nowPlaying);
            controller.boardMoveMade(placed);
            controller.setPlayers(players);
            controller.setTilesRemaining(tilesRemaining);
            controller.addBoardMoveToHistory(data.summary);

            controller.data.lastMoveReceived = moveNumber;

            var highlightLastMove = function () {
              controller.scrabbleGroundCtrl.highlight.highlightMove(data.summary);
              m.redraw();
            }

            if (!document.hasFocus()) {
              $(window).one('focus', function() {
                highlightLastMove();
              });
            }
            else {
              highlightLastMove();
            }
        },
        "playerExchangeMove" : function(data) {
            var moveNumber = data.moveNumber;

            // If we've already been sent this move (e.g. while
            // initialising), ignore it
            if (moveNumber <= controller.data.lastMoveReceived)
                return;

            controller.setPlayerToMove(data.nowPlaying);
            controller.addExchangeMoveToHistory();

            controller.data.lastMoveReceived = moveNumber;
        },
        "playerPassMove" : function(data) {
            var moveNumber = data.moveNumber;

            // If we've already been sent this move (e.g. while
            // initialising), ignore it
            if (moveNumber <= controller.data.lastMoveReceived)
                return;

            controller.setPlayerToMove(data.nowPlaying);
            controller.addPassMoveToHistory();

            controller.data.lastMoveReceived = moveNumber;
        },
        "gameFinished" : function(data) {
            if (data.placed) {
                controller.boardMoveMade(data.placed)
            }

            controller.setPlayers(data.summary.players);

            var boardMoveSummary = {
                "type" : "board",
                "overallScore" : data.summary.lastMoveScore,
                "wordsMade" : data.summary.wordsMade
            };

            controller.addBoardMoveToHistory(boardMoveSummary);
            controller.setPenalties(data.summary.penalties);
        },
        "boardMoveSuccess" : function(data) {
            var rack = data.rack;
            controller.updateRack(rack);
        },
        "exchangeMoveSuccess" : function(data) {
            var rack = data.rack;
            controller.updateRack(rack);
        },
        "playerChat" : function(data) {
            var playerName = data.player;
            var message = data.message;

            controller.addChatMessage(playerName, message);
        },
        "potentialScore" : function(data) {
            var potentialScore = data.potentialScore;
            controller.setPotentialScore(potentialScore);
        },
        "error" : function(data) {
            controller.showErrorMessage(data);
        }
    };

    var parseAndIssueCommand = function(command) {
        var commandName = command.command;

        var handler = handlers[commandName];

        if (handler) {
            handler(command.payload);
        }
        else
        {
            console.info("Unrecognised command: ");
            console.dir(command);
        }

        console.dir(command);
    }

    return {
        parseAndIssueCommand : parseAndIssueCommand
    }
}
