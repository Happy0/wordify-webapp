var scrabbleground = require('scrabbleground');
var m = require('mithril');

module.exports = function(ctrl) {

    var scrollToBottomOnRender =  function(element, initialised, context) {
            var scrollHeight = $(element)[0].scrollHeight;

            $(element).scrollTop(scrollHeight);
    };

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

        var handleSelectedForExchange = function(element, slotNumber) {
                var onClick = function () {
                    if (!ctrl.data.exchangeMode) {
                        return;
                    }

                    m.startComputation();
                    if (!rack[slotNumber].selectedForExchange)
                    {
                        rack[slotNumber].selectedForExchange = true;
                    }
                    else
                    {
                        rack[slotNumber].selectedForExchange = false;
                    }
                    m.endComputation();
                }

                $(element).click(onClick);
        }

        var putTileOnRack = function(slot, slotNumber) {

            var configSlot = function(element, initialised, context) {
                rack[slotNumber].element = element;

                if (!initialised) {
                    handleSelectedForExchange(element, slotNumber);
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

        var renderRackButton = function(callback, classes) {
            return m('span', {class: 'rack-button'}, m('button', {onclick: callback, class: "btn btn-success btn-large ".concat(classes)}));
        };

        return  m('div', {class : "rack"}, [
                  renderRackButton(ctrl.recallTilesToRack, 'icon-white glyphicon glyphicon-arrow-down'),
                  m("span", {}, renderedSlots),
                  renderRackButton(ctrl.shuffleRack, 'icon-white glyphicon glyphicon-refresh')
                ]);
    };

    var renderChatBox = function() {

        var renderMessage = function(sender, message)
        {
            return m('li', {class: 'chat-message'}, [m('span', {class: 'chat-user'}, m('b',"<" + sender  + ">")), message]);
        }

        var renderInputBox = function() {
            var listenForEnter = function(element, initialised, context) {
                if (initialised) return;

                $(element).keydown(function(e) {
                    if (e.which == 13 && $(element).val())
                    {
                        ctrl.sendChatMessage($(element).val())
                        $(element).val("");
                    }
                })

            }

            return m('div', {}, m('input', {config: listenForEnter, class: 'chat-input'}, "test"));
        };

        var messagesConfig = function(element, initialised, context) {
            scrollToBottomOnRender(element, initialised, context);
        }

        return m('div', {class: 'chat-box'},
                 [
                     m('ul', {class: 'chat-messages', config: messagesConfig}, ctrl.data.chatMessages.map(function(message) {
                    return renderMessage(message.sender, message.message)
            })),
             renderInputBox()
            ]
        )
    }

    var renderBoard = function() {
        var scrabblegroundView = scrabbleground.view(ctrl.scrabbleGroundCtrl);

        return m('div', {style: "overflow: auto;"}, scrabblegroundView);
    };

    var renderMoveHistory = function() {
        var history = ctrl.data.moveHistory;

        var historyConfig = function(element, initialised, context) {
            scrollToBottomOnRender(element, initialised, context);
        };

        var renderBoardMoveRow = function(boardMove) {
            var wordsAndScore = boardMove.wordsMade.map(function(word) {
                return m('p', {}, word.word + " (" + word.score + ")");
            });

            return m('tr', {}, [
                m('td', {}, wordsAndScore),
                m('td', {}, boardMove.overallScore)
                ]);
        }

        var historyTable = m('table', {}, m('tbody',

            history.map(function(move) {
                if (move.type == "board") {
                    return renderBoardMoveRow(move);
                } else if (move.type == "exchange") {
                    return m('tr', {},
                      [
                        m('td', {}, m('p', {}, "Exchange")),
                        m('td', {}, 0)

                      ]);
                }
                else if (move.type == "pass") {
                    return m('tr', {},
                      [
                        m('td', {}, m('p', {}, "Pass")),
                        m('td', {}, 0)

                      ]);
                }
        })));

        return m('div', {config: historyConfig, class : "round-table"}, historyTable);
    };

    var renderScoreBoard = function() {
        var players = ctrl.data.players;

        var renderPlayerRow = function(player, idx) {
            return m('tr', {},
                     [m('td', {class : ""}, player.name),
                         m('td', {}, !ctrl.data.players[idx].endBonus ? player.score :
                          player.score + ' (' + ctrl.data.players[idx].endBonus + ')')]);
        }

        return m('table', {},
                m('tbody',{}, [players.map(renderPlayerRow),
                  m('tr',
                    m('td', {}, 'Tiles Remaining'),
                    m('td', {}, ctrl.data.tilesRemaining))
                  ]));
    };

    var renderPotentialScore = function()
    {
        return m('div', {class: 'potential-score'}, "potential score: " + ctrl.data.potentialScore) ;
    }

    var renderMiddleColumn = function() {
        return m('div', {}, [
                  m('div', {class: "liscrabble-board-wrap"}, renderBoard()),
                  m('div', {}, [renderTileRack(), renderActionButtons()])]);

    }

    return m('div', {class: 'round'},
        [
            m('div', {class: 'potential-score'}, renderPotentialScore()),
            m('div', {class: 'main row-fluid'},
              m('div', {},
                 [
                        m('span', {class: 'col-md-3'}, [m('div', {class: 'score-board round-table'}, renderScoreBoard()), renderMoveHistory()]),
                        m('span', {class: 'col-md-6'},  [renderMiddleColumn()]),
                        m('span', {class: 'col-md-3'}, [renderChatBox()])
                 ]))
        ]);
}
