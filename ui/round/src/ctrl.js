var Scrabbleground = require('scrabbleground');
var s = require('./socket');
var d = require('./data');
var m = require('mithril');

module.exports = function(opts) {

    var data = d(opts);

    var socketOpts = {
        send : opts.send,
        ctrl : this
    }

    var socket = s(socketOpts);

    var scrabbleGroundOpts = opts.ground;

    // Our scrabbleground controller for manipulating the state of the game
    var scrabbleGroundCtrl = new Scrabbleground.controller(scrabbleGroundOpts);

    var moveMade = function(move) {
        scrabblegroundCtrl.move(move);
    };

    var emptySlotArray = [null, null, null, null, null, null, null];

    var setRackTiles = function(rack) {
        var numTiles = rack.length;
        var emptySlots = 7 - numTiles;

        var remainingSlots = emptySlotArray.slice(emptySlots - 1);

        // Mark the tile as movable and keep a note of the slot that it came from
        rack.forEach(function(tile, slotNumber) {
            tile.isCandidate = true;
            tile.slotNumber = slotNumber;
        });

        data.rack = rack.concat(remainingSlots);

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

        data.rack.forEach(function (rackSlot, i) {
            if (!rackSlot) {
                data.rack[i] = tile;
            }
        });
    };

    scrabbleGroundCtrl.setTileDroppedOnSquareListener(function (tile) {
        var fromSlot = tile.slotNumber;

        if (fromSlot) {
            data.rack[fromSlot] = null;
        }

    });

    var tileDroppedOffBoardFunction = function(tile) {
        var fromSlot = tile.slotNumber;

        if (!data.rack[fromSlot]) {
            data.rack[fromSlot] = tile;
        }
        else
        {
            putTileOnFirstEmptySlot(tile);
        }

    };

    scrabbleGroundCtrl.setCustomRevertFunction(tileDroppedOffBoardFunction);

    return {
        data: data,
        moveMade : moveMade,
        setRackTiles : setRackTiles,
        updateRack : updateRack,
        scrabbleGroundCtrl: scrabbleGroundCtrl,
        socket: socket
    };
};
