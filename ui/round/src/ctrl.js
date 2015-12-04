var Scrabbleground = require('scrabbleground');

module.exports = function(opts) {

    var scrabbleGroundOpts = opts.ground;

    // Our scrabbleground controller for manipulating the state of the game
    var scrabbleGroundCtrl = new Scrabbleground.controller(scrabbleGroundOpts);

    var moveMade = function(move) {
        scrabblegroundCtrl.move(move);
    };

    return {
        moveMade : moveMade,
        scrabbleGroundCtrl: scrabbleGroundCtrl
    };
};
