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
    var boardMoveMade = function(move) {
        alert("board move made controller");
    }

    /**
     * The user has made a move
     */
    var makeBoardMove = function(move) {
        var tilesPlaced = scrabbleGroundCtrl.getCandidateTiles().map(function(placed) {
            var x = placed.x;
            var y = placed.y;
            var tile = {
                letter: placed.tile.letter,
                value: placed.tile.value
            };

            return {
                pos : {
                  x: x + 1,
                  y: y + 1,
                },
                tile: tile
            };
        });
        console.dir(tilesPlaced);

        var data = {
            command : "boardMove",
            payload : tilesPlaced
        };

        socketOpts.send(data);
    };

    var setRackTiles = function(rackTiles) {

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

        m.redraw();
    };

    /**
     * Given a full new rack, update the old rack without moving
     * any non-played tiles to new locations so as not to confuse
     * the player.
     */
    var updateRack = function(fullNewRack) {

    };

    var putTileOnFirstEmptySlot = function (tile) {
        //TODO: This won't work. Fix later.
        data.rack.forEach(function (rackSlot, i) {
            if (!rackSlot.tile) {
                data.rack[i].tile = tile;
            }
        });
    };

    scrabbleGroundCtrl.setTileDroppedOnSquareListener(function (tile) {
        var fromSlot = tile.slotNumber;
        data.rack[fromSlot].tile = null;
    });

    var tileDroppedOffBoardFunction = function(tile, tileElement) {
        var fromSlot = tile.slotNumber;

        if (data.rack[fromSlot].tile == tile)
        {
            // Revert to the previous position
            return true;
        }

        if (!data.rack[fromSlot].tile) {
            data.rack[fromSlot].tile = tile;
            $(tileElement).detach();
            $(data.rack[fromSlot].element).append(tileElement);
            $(tileElement).attr("style", "position: relative; left: 0px; top: 0px; z-index: 10;");
        }
        else
        {
            putTileOnFirstEmptySlot(tile);
        }

    };

    scrabbleGroundCtrl.setCustomRevertFunction(tileDroppedOffBoardFunction);

    var controllerFunctions = {
        data: data,
        boardMoveMade : boardMoveMade,
        makeBoardMove : makeBoardMove,
        setRackTiles : setRackTiles,
        updateRack : updateRack,
        scrabbleGroundCtrl: scrabbleGroundCtrl,
        socket: socket
    };

    merge.recursive(exports, controllerFunctions);

    return exports;
};
