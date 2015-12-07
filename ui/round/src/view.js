var scrabbleground = require('scrabbleground');
var m = require('mithril');

module.exports = function(ctrl) {

    var renderTileRack = function() {
        var rack = ctrl.data.rack;

        var putTileOnRack = function(tile) {
            return m("span", {},
                        "square", {},
                            (function () {
                                if (tile != null ) {
                                    ctrl.scrabbleGroundCtrl.renderTile(tile);
                                }
                            })()
                    );
        };

        var renderedSlots = rack.map(putTileOnRack);

        return m("div", {id : "rack" }, renderedSlots);
    }

    var renderBoard = function() {
        var attrs = {
            class : ["liscrabble-board-wrap"]
        }

        var scrabblegroundView = scrabbleground.view(ctrl.scrabbleGroundCtrl);

        return m('div', attrs, [scrabblegroundView, renderTileRack]);
    }
    return m('div', {}, renderBoard());
}
