var scrabbleground = require('scrabbleground');
var m = require('mithril');

module.exports = function(ctrl) {

    var renderBoard = function() {
        var attrs = {
            class : ["liscrabble-board-wrap"]
        }

        var scrabblegroundView = scrabbleground.view(ctrl.scrabbleGroundCtrl);

        return m('div', attrs, scrabblegroundView);
    }
    return m('div', {}, renderBoard());
}
