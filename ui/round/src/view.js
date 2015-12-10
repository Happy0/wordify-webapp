var scrabbleground = require('scrabbleground');
var m = require('mithril');

module.exports = function(ctrl) {

    var renderButton = function (text, handler) {
        var buttonAttrs = {
            type: "button",
            class: "btn btn-success"
        };

        return m('span', {class : "submit-move-button"},
                        m('button', buttonAttrs, text));
    };

    var renderActionButtons = function () {
        return m('div', {class: 'action-buttons'}, [
                     renderButton("Submit"),
                     renderButton("Exchange"),
                     renderButton("Pass")]);
   };

    var renderTileRack = function() {
        var rack = ctrl.data.rack;

        var renderTile =
            function (tile, slot) {
                if (tile != null ) {
                    // We mark the slot that the tile is from so that we
                    // can later empty those slots in the internal model
                    tile.rackSlot = slot;
                    return ctrl.scrabbleGroundCtrl.makeMithrilTile(tile);
                }
            };

        var putTileOnRack = function(slot, slotNumber) {

            var updateSlotWithElement = function(element, initialised, context) {
                if (initialised) return;

                rack[slotNumber].element = element;
            }

            return m("span", {class : "rack-slot"},
                     m("square", {config: updateSlotWithElement},
                           renderTile(slot.tile, slotNumber))
                    );
        };

        var renderedSlots = rack.map(putTileOnRack);

        return  m('div', {},
              [m("div", {class : "rack", id : "rack" }, renderedSlots),
                  renderActionButtons()
              ]);
    };

    var renderBoard = function() {
        var attrs = {
            class : ["liscrabble-board-wrap"]
        }

        var scrabblegroundView = scrabbleground.view(ctrl.scrabbleGroundCtrl);

        return m('div', attrs, scrabblegroundView);
    }

    return m('div', {}, [renderBoard(), renderTileRack()]);
}
