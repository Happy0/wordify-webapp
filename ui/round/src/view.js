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

        var handleSelectedForExchange = function(element, slot) {
                var onClick = function () {
                    if (!ctrl.data.exchangeMode) {
                        return;
                    }

                    m.startComputation();
                    if (!slot.selectedForExchange)
                    {
                        slot.selectedForExchange = true;
                    }
                    else
                    {
                        
                        slot.selectedForExchange = false;
                    }
                    m.endComputation();
                }

                $(element).click(onClick);
        }

        var putTileOnRack = function(slot, slotNumber) {

            var configSlot = function(element, initialised, context) {
                rack[slotNumber].element = element;

                if (!initialised) {
                    handleSelectedForExchange(element, slot);
                }
            }

            var classes = "rack-slot";
            if (slot.selectedForExchange) {
                classes = classes.concat(" highlighted-slot");
            }

            return m("span", {class : classes},
                     m("square", {config: configSlot},
                           renderTile(slot.tile, slotNumber))
                    );
        };

        var renderedSlots = rack.map(putTileOnRack);

        return  m('div', {},
              m("div", {class : "rack"}, renderedSlots));
    };

    var renderChatBox = function() {

        var renderMessage = function(user, message)
        {
            return m('li', {}, [m('span', {}, user), message]);
        }

        var renderInputBox = function() {
            return m('div', {}, m('input', {class: 'chat-input'}, "test"));
        }

        var testMessages = [];
        var i = 0;
        for (i = 0; i < 50; i++)
        {
            testMessages.push("message " + i);
        }

        return m('span', {class: 'chat-box'},
                 [
                 m('ul', {class: 'chat-messages'}, testMessages.map(function(message) {
                    return renderMessage('testUser', message)
            })),
             renderInputBox()
            ]
        )
    }

    var renderBoard = function() {
        var attrs = {
            class : ["liscrabble-board-wrap"]
        }

        var scrabblegroundView = scrabbleground.view(ctrl.scrabbleGroundCtrl);

        return m('span', attrs, scrabblegroundView);
    }

    var renderScoreBoard = function() {
        var players = ctrl.data.players;

        var renderPlayerRow = function(player) {
            return m('tr', {},
                     [m('td', {class : "score-table-border"}, player.name),
                         m('td', {class: "score-table-border"}, player.score)]);
        }

        return m('table', {class: "score-table" }, players.map(renderPlayerRow));
    };

    return m('div', {class: 'round'}, 
             [
                 m('div', {class: "round-main"}, [renderScoreBoard(), renderBoard(), renderChatBox()]),
                m('div', {class: "below-board"}, [renderTileRack(), renderActionButtons()])
             ]);
}
