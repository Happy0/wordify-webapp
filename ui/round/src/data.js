module.exports = function(opts) {

    var defaults = {
        rack : [{}, {}, {}, {}, {}, {}, {}],
        players : [],
        playerToMove : 1,
        playerNumber : null, // our player number
        exchangeMode : false, // Player is selecting tiles to exchange,
        chatMessages : []
    }

    return defaults;
}
