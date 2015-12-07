var scrabbleground = require('scrabbleground');
var m = require('mithril');

module.exports = function(ctrl) {

    var renderTileRack = function() {
        var rack = ctrl.data.rack;

        var putTileOnRack = function(tile) {
            return m("span", {class : "rack-slot"},
                     m("square", {},
                            (function () {
                                if (tile != null ) {
                                    return ctrl.scrabbleGroundCtrl.makeMithrilTile(tile);
                                }
                            })())
                    );
        };

        var renderedSlots = rack.map(putTileOnRack);

        return m("div", {class : "rack", id : "rack" }, renderedSlots);
    }

    var renderBoard = function() {
        var attrs = {
            class : ["liscrabble-board-wrap"]
        }

        var scrabblegroundView = scrabbleground.view(ctrl.scrabbleGroundCtrl);

        return m('div', attrs, scrabblegroundView);
    }

    return m('div', {}, [renderBoard(), renderTileRack()]);
}
