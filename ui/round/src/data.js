var merge = require('merge');

module.exports = function(opts) {

    var defaults = {
        connections: {},
        rack : [{}, {}, {}, {}, {}, {}, {}],
        moveHistory : [],
        // [{"name": "fulano", "score": 10, "​endBonus": 0}]
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

    if(!opts) return defaults;

    merge.recursive(defaults, opts);

    return defaults;
}
