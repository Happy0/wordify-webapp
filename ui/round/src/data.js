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
        playerToMove : 0,
        playerNumber : null, // our player number
        exchangeMode : false, // Player is selecting tiles to exchange,

        // The message number of the last change message received (e.g. 12)
        lastChatMessageReceived: null,

        // The message number of the last definition message received (e.g. 12)
        lastDefinitionReceived: null,

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
