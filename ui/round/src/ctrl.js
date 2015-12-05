var Scrabbleground = require('scrabbleground');
var s = require('./socket');

module.exports = function(opts) {

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

    return {
        moveMade : moveMade,
        scrabbleGroundCtrl: scrabbleGroundCtrl,
        socket: socket
    };
};
