var merge = require('merge');

module.exports = function(opts) {

    var defaults = {
        //[{"active": true, "lastSeen": "2025-03-24T21:10:32.016620946Z"}]
        connections: [],
        rack : [{}, {}, {}, {}, {}, {}, {}],
        moveHistory : [],
        // [{"name": "fulano", "score": 10, "​endBonus": 0}]
        players : [],
        penalties : [0,0,0,0], // Penalties / bonuses for tiles remaining on rack once the game has ended
        playerToMove : 1,
        playerNumber : null, // our player number
        exchangeMode : false, // Player is selecting tiles to exchange,

        // 2025-03-28T23:21:21.877940745Z | null if none received yet
        lastChatMessageReceived: null,

        // [{sender: sender, message: message} || {word: "word", definition: {"definition": "..", "partOfSpeech": "...", "example": "..." | undefined}}
        chatMessages : [],
        tilesRemaining : 0,
        potentialScore : 0,
        lastMoveReceived: 0
    }

    if(!opts) return defaults;

    merge.recursive(defaults, opts);

    return defaults;
}
