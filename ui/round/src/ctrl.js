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

        //TODO: Expose controller method in scrabbleground to make tiles movable instead
        rack.forEach(function(tile) {
            tile.isCandidate = true;
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

    return {
        data: data,
        moveMade : moveMade,
        setRackTiles : setRackTiles,
        updateRack : updateRack,
        scrabbleGroundCtrl: scrabbleGroundCtrl,
        socket: socket
    };
};
