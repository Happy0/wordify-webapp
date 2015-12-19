var scrabbleground = require('scrabbleground');
var m = require('mithril');

module.exports = function(ctrl) {

    var renderButton = function (text, buttonClickHandler) {

        var buttonAttrs = {
            onclick: buttonClickHandler,
            type: "button",
            class: "btn btn-success"
        };

        if (ctrl.data.playerNumber != ctrl.data.playerToMove || text != "Exchange" && ctrl.data.exchangeMode){
            buttonAttrs.disabled = true;
        }

        return m('span', {class : "submit-move-button"},
                        m('button', buttonAttrs, text));
    };

    var renderActionButtons = function () {
        return m('div', {class: 'action-buttons'}, [
                     renderButton("Submit", ctrl.makeBoardMove),
                     renderButton("Exchange", ctrl.toggleExchangeMode),
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

                    return ctrl.scrabbleGroundCtrl.makeMithrilTile(tile);
                }
            };

        var removeSlotBorder = function(element) {
                $($(element).children()[0]).css('border', '');
        }

        var handleSelectedForExchange = function(element, slot) {
            if (ctrl.data.exchangeMode) {
                var onClick = function () {
                    if (!slot.selectedForExchange)
                    {
                        slot.selectedForExchange = true;
                        $($(element).children()[0]).css('border', 'solid 2px red');
                    }
                    else
                    {
                        
                        slot.selectedForExchange = false;
                        removeSlotBorder(element);
                    }
                }

                $(element).click(onClick);
            }
            else {
                $(element).off("click");
                return;
            }
        }

        var putTileOnRack = function(slot, slotNumber) {

            var configSlot = function(element, initialised, context) {
                rack[slotNumber].element = element;
                handleSelectedForExchange(element, slot);
            }

            return m("span", {class : "rack-slot"},
                     m("square", {config: configSlot},
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
