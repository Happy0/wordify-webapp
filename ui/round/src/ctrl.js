var Scrabbleground = require('scrabbleground');
var s = require('./socket');
var d = require('./data');
var m = require('mithril');
var $ = require('jquery');
var merge = require('merge');

module.exports = function(opts) {

    var exports = {};

    var data = d(opts);

    var socketOpts = {
        send : opts.send,
        ctrl : exports
    }

    var socket = s(socketOpts);

    var scrabbleGroundOpts = opts.ground;

    // Our scrabbleground controller for manipulating the state of the game
    var scrabbleGroundCtrl = new Scrabbleground.controller(scrabbleGroundOpts);

    /**
     * A player has made a board move
     */
    var boardMoveMade = function(placed) {
        scrabbleGroundCtrl.move(placed);
    };

    var showErrorMessage = function(errorMessage) {
        // Improve on this later... Can gracefully handed some cases
        alert(errorMessage.error);
    };

    var getServerPlacedModel = function () {
        return scrabbleGroundCtrl.getCandidateTiles().map(function(placed) {
                var x = placed.x;
                var y = placed.y;
                var tile = {
                    letter: placed.tile.letter,
                    value: placed.tile.value
                };

                return {
                    pos : {
                    x: x + 1,
                    y: y + 1
                    },
                    tile: tile
                };
            });
    }

    /**
     * The user has made a move
     */
    var makeBoardMove = function(move) {

        var tilesPlaced = getServerPlacedModel();
        var data = {
            command : "boardMove",
            payload : tilesPlaced
        };

        return socketOpts.send(data);
    };

    var makePassMove = function() {
        var data = {
            command : "passMove",
            payload : {}
        }

        recallTilesToRack();
        return socketOpts.send(data);
    };

    var toggleExchangeMode = function() {
        if (exports.data.exchangeMode)
        {
            // The player has finished selecting the tiles she wants to exchange

            var slotIsSelected = function(slot) {
                return slot.selectedForExchange;
            };

            var getSlotTile = function(slot) {
                return slot.tile;
            };

            var wantsToExchange = data.rack.filter(slotIsSelected).map(getSlotTile);

            if (wantsToExchange.length) {
                var payload = {
                    command : "exchangeMove",
                    payload : wantsToExchange
                };

                return socketOpts.send(payload);
            }
        }

        recallTilesToRack();
        data.exchangeMode = !data.exchangeMode;

        return true;
    };

    var sendChatMessage = function(message) {
        var data = {
            command: "say",
            "payload" : {
                "message" : message
            }
        }

        return socketOpts.send(data);
    }


    var setPlayerToMove = function(playerToMove) {
        m.startComputation();
        data.playerToMove = playerToMove;

        if (data.playerToMove == data.playerNumber)
        {
            scrabbleGroundCtrl.setBoardViewOnly(false);
            document.title="It's your move."

            var notification = new Notification('Wordify', {
              body: "It's your move.",
            });

            notification.onclick = function(){
                window.focus();
                notification.close();
            };

            if (Notification.permission === "granted") {
                setTimeout(notification.close.bind(notification), 5000);
            }
        }
        else
        {
            document.title="Waiting for opponent(s) to move."
            scrabbleGroundCtrl.setBoardViewOnly(true);
            setPotentialScore(0);
        }
        m.endComputation();
    };

    var setPlayerNumber = function(playerNumber) {
        data.playerNumber = playerNumber;
    };

    var setPlayers = function(players) {
        data.players = players;
    };

    var recallTilesToRack = function () {
        var tilesOnBoard = scrabbleGroundCtrl.removeCandidateTiles();
        tilesOnBoard.forEach(putTileOnFirstEmptySlot);
    };

    var setRackTiles = function(rackTiles) {
        if (!rackTiles || rackTiles.length == 0)
            return;

        m.startComputation();

        var emptySlotArray = [{}, {}, {}, {}, {}, {}, {}];
        var numTiles = rackTiles.length;
        var emptySlots = 7 - numTiles;

        var remainingSlots = emptySlots == 0 ? [] : emptySlotArray.slice(0, emptySlots - 1);

        var allTiles = rackTiles.concat(remainingSlots);

        allTiles.forEach(function(tile, slotNumber) {
            tile.isCandidate = true;
            tile.slotNumber = slotNumber;
            data.rack[slotNumber].tile = tile;
        })

        m.endComputation();
    };

    var setMoveHistory = function(moveList) {
        moveList.forEach(function(move) {
            if (move.type == "pass") {
                addPassMoveToHistory(move);
            }
            else if (move.type == "exchange") {
                addExchangeMoveToHistory(move)
            }
            else {
                addBoardMoveToHistory(move);
            }
        });
    };

    var setPotentialScore = function(potentialScore) {
        m.startComputation();
        data.potentialScore = potentialScore;
        m.endComputation();
    };

    var setTilesRemaining = function(tilesRemaining) {
        data.tilesRemaining = tilesRemaining;
    };

    var setChatMessages = function(messages) {
        data.chatMessages = messages;
    };

    var setPenalties = function(penalties) {
        data.penalties = penalties;
    }

    var addBoardMoveToHistory = function(summary) {
        m.startComputation();

        data.moveHistory.push({
            "type" : "board",
            wordsMade: summary.wordsMade,
            overallScore : summary.overallScore
        });

        m.endComputation();
    };

    var addPassMoveToHistory = function(pass) {
        m.startComputation();
        data.moveHistory.push({
            "type" : "pass"
        });

        m.endComputation();
    };

    var addExchangeMoveToHistory = function(exchange) {
        m.startComputation();
        data.moveHistory.push({
            "type" : "exchange"
        });

        m.endComputation();
    }

    var addChatMessage = function(sender, message, when) {
        m.startComputation();
        var messages = data.chatMessages;
        messages.push({sender: sender, message: message, when: when});
        data.lastChatMessageReceived = when;
        m.endComputation();
    }

    /**
     * Given a full new rack, update the old rack without moving
     * any non-played tiles to new locations so as not to confuse
     * the player.
     */
    var updateRack = function(fullNewRack) {
        if (!fullNewRack) return;

        // Just write over the old tiles for now... Will do a diff once main
        // functionality is there
        m.startComputation();

        data.rack.forEach(function(slot, slotNo) {
            if (fullNewRack[slotNo]) {
                slot.tile = fullNewRack[slotNo];
                slot.tile.isCandidate = true;
                slot.tile.slotNumber = slotNo;
            }
            else {
                slot.tile = null;
            }

            slot.selectedForExchange = false;
        });

        m.endComputation();
    };

    var putTileOnFirstEmptySlot = function (tile) {
        data.rack.some(function (rackSlot, i) {
            if (!rackSlot.tile) {
                data.rack[i].tile = tile;
                tile.slotNumber = i;
                return true;
            }
        });
    };

    scrabbleGroundCtrl.setTileDroppedOnSquareListener(function (tile) {
        var fromSlot = tile.slotNumber;
        data.rack[fromSlot].tile = null;
        askPotentialScore();
    });

    var askPotentialScore = function() {
        var placed = getServerPlacedModel();
        socketOpts.send({
            command : "potentialScore",
            payload: placed
        });
    };

    var shuffleRack = function() {
        var shuffle = function(input) {
            for (var i = input.length-1; i >=0; i--) {

                    var randomIndex = Math.floor(Math.random()*(i+1));
                    var itemAtIndex = input[randomIndex];

                    input[randomIndex] = input[i];
                    input[i] = itemAtIndex;
                }
            return input;
        };

        m.startComputation();

        shuffle(data.rack);

        data.rack.forEach(function(slot, i) {
            if (slot.tile) {
                slot.tile.slotNumber = i;
            }
        });

        m.endComputation();
    }

    var putTileBackOnRack = function(tile, tileElement) {
        var fromSlot = tile.slotNumber;

        if (data.rack[fromSlot].tile == tile)
        {
            // Revert to the previous position
            return true;
        }

        if (!data.rack[fromSlot].tile) {
            data.rack[fromSlot].tile = tile;

            if (tileElement) {
                $(tileElement).detach();
                $(data.rack[fromSlot].element).append(tileElement);
                $(tileElement).attr("style", "position: relative; left: 0px; top: 0px; z-index: 10;");
            }
        }
        else
        {
            putTileOnFirstEmptySlot(tile);
        }

        askPotentialScore();
    };

    var setConnections = function(connections) {
        m.startComputation()

        data.connections = connections;

        m.endComputation()
    }

    var playerConnect = function(playerNumber) {
        m.startComputation()

        data.connections[playerNumber - 1] = { active: true }

        m.endComputation()
    }

    var playerDisconnect = function(playerNumber) {
        m.startComputation()

        // TODO: 'last seen' timestamp
        data.connections[playerNumber - 1] = { active: false }

        m.endComputation()
    }

    var getLastChatMessageReceivedSecondsSinceEpoch = function() {
        if (!data.lastChatMessageReceived) {
            return null
        } else {
            return new Date(data.lastChatMessageReceived).getTime() / 1000;
        }
    }

    var sortMessages = function() {
        data.chatMessages.sort(function(a, b) {
            if (a.when < b.when) {
                return -1;
            }
            else if (a.when > b.when) {
                return 1;
            }
            else {
                return Object.hasOwn(a, "sender") ? -1 : 1;
            }
        });
    }

    var addDefinitions = function (definitions) {
        m.startComputation();
        var messages = data.chatMessages;
        if (definitions.definitions.length > 0) {
            var firstDefinition = definitions.definitions[0]
            messages.push({word: definitions.word, definition: firstDefinition, when: definitions.when});
        } else {
            messages.push({word: definitions.word, definition: {definition: "No definitions found."}, when: definitions.when});
        }

        sortMessages();

        data.lastChatMessageReceived = definitions.when;
        
        m.endComputation();
    }
    
    var requestDefinition = function (word) {
        var data = {
            command : "askDefinition",
            payload : {
                word: word
            }
        };

        return socketOpts.send(data);
    }

    scrabbleGroundCtrl.setCustomRevertFunction(putTileBackOnRack);

    var controllerFunctions = {
        data: data,
        boardMoveMade : boardMoveMade,
        makeBoardMove : makeBoardMove,
        makePassMove : makePassMove,
        toggleExchangeMode : toggleExchangeMode,
        sendChatMessage : sendChatMessage,
        showErrorMessage : showErrorMessage,
        setPlayers : setPlayers,
        setPenalties : setPenalties,
        setPlayerToMove : setPlayerToMove,
        setPlayerNumber: setPlayerNumber,
        setRackTiles : setRackTiles,
        setMoveHistory : setMoveHistory,
        setTilesRemaining : setTilesRemaining,
        setPotentialScore : setPotentialScore,
        setChatMessages : setChatMessages,
        shuffleRack : shuffleRack,
        recallTilesToRack : recallTilesToRack,
        addBoardMoveToHistory : addBoardMoveToHistory,
        addPassMoveToHistory: addPassMoveToHistory,
        addExchangeMoveToHistory : addExchangeMoveToHistory,
        addChatMessage : addChatMessage,
        updateRack : updateRack,
        setConnections: setConnections,
        playerConnect: playerConnect,
        playerDisconnect: playerDisconnect,
        addDefinitions: addDefinitions,
        requestDefinition: requestDefinition,
        getLastChatMessageReceivedSecondsSinceEpoch: getLastChatMessageReceivedSecondsSinceEpoch,
        scrabbleGroundCtrl: scrabbleGroundCtrl,
        socket: socket
    };

    merge.recursive(exports, controllerFunctions);

    return exports;
};
