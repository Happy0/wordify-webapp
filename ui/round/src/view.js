var scrabbleground = require('scrabbleground');
var m = require('mithril');

module.exports = function(ctrl) {

    var renderButton = function (text, buttonClickHandler) {

        var buttonAttrs = {
            onclick: buttonClickHandler,
            type: "button",
            class: "btn btn-success"
        };

        if (ctrl.data.playerNumber != ctrl.data.playerToMove){
            buttonAttrs.disabled = true;
        }

        return m('span', {class : "submit-move-button"},
                        m('button', buttonAttrs, text));
    };

    var renderActionButtons = function () {
        return m('div', {class: 'action-buttons'}, [
                     renderButton("Submit", ctrl.makeBoardMove),
                     renderButton("Exchange"),
                     renderButton("Pass", ctrl.makePassMove)]);
   };

    var renderTileRack = function() {
        var rack = ctrl.data.rack;

        var renderTile =
            function (tile, slot) {
                if (tile != null ) {
                    // We mark the slot that the tile is from so that we
                    // can later empty those slots in the internal model
                    tile.rackSlot = slot;

                    if (ctrl.data.playerToMove == ctrl.data.playerNumber)
                    {
                        tile.isCandidate = true;
                    }
                    else
                    {
                        tile.isCandidate = false;
                    }

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

    var renderScoreBoard = function() {
        var players = ctrl.data.players;

        var renderPlayerRow = function(player) {
            return m('tr', {class: "score-table-border"},
                     [m('td', {class : "score-table-border"}, player.name),
                         m('td', {class: "score-table-border"}, player.score)]);
        }

        return m('table', {class: "score-table-border" }, players.map(renderPlayerRow));
    };


    return m('div', {}, [renderScoreBoard(), renderBoard(), renderTileRack()]);
}
