module.exports = function(opts) {

    var defaults = {
        rack : [{}, {}, {}, {}, {}, {}, {}],
        moveHistory : [],
        players : [],
        playerToMove : 1,
        playerNumber : null, // our player number
        exchangeMode : false, // Player is selecting tiles to exchange,
        chatMessages : [],
        tilesRemaining : 0,
        potentialScore : 0
    }

    return defaults;
}
