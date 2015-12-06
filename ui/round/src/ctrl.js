var Scrabbleground = require('scrabbleground');
var s = require('./socket');
var d = require('./data');

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

    var setRack = function(rack) {
        data.rack = rack;
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
        setRack : setRack,
        updateRack : updateRack,
        scrabbleGroundCtrl: scrabbleGroundCtrl,
        socket: socket
    };
};
