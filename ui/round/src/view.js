var scrabbleground = require('scrabbleground');
var m = require('mithril');

module.exports = function(ctrl) {

    var renderTileRack = function() {
        var rack = ctrl.data.rack;

        var renderTile = function (tile, slot) {
                            if (tile != null ) {
                                // We mark the slot that the tile is from so that we can later empty those slots in the
                                // internal model
                                tile.rackSlot = slot;
                                return ctrl.scrabbleGroundCtrl.makeMithrilTile(tile);
                            }
                        };

        var putTileOnRack = function(tile, slot) {
            return m("span", {class : "rack-slot"},
                     m("square", {},
                           renderTile(tile, slot))
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
