module.exports = function(opts) {

    var defaults = {
        rack : [{}, {}, {}, {}, {}, {}, {}],
        moveHistory : [],
        players : [],
        penalties : [0,0,0,0], // Penalties / bonuses for tiles remaining on rack once the game has ended
        playerToMove : 1,
        playerNumber : null, // our player number
        exchangeMode : false, // Player is selecting tiles to exchange,
        chatMessages : [],
        tilesRemaining : 0,
        potentialScore : 0,
        lastMoveReceived: 0
    }

    return defaults;
}
