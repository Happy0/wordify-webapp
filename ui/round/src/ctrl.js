var Scrabbleground = require('scrabbleground');
var s = require('./socket');
var d = require('./data');
var m = require('mithril');
var $ = require('jquery');

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

    var setRackTiles = function(rackTiles) {
        
        var emptySlotArray = [{}, {}, {}, {}, {}, {}, {}];
        var numTiles = rack.length;
        var emptySlots = 7 - numTiles;

        var remainingSlots = emptySlotArray.slice(0, emptySlots - 1);

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

    return {
        data: data,
        moveMade : moveMade,
        setRackTiles : setRackTiles,
        updateRack : updateRack,
        scrabbleGroundCtrl: scrabbleGroundCtrl,
        socket: socket
    };
};
