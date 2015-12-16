module.exports = function(opts) {

    var send = opts.send;
    var controller = opts.ctrl;

    var handlers = {
        "playerBoardMove" : function(data) {
            var placed = data.placed;
            var players = data.players;
            controller.boardMoveMade(placed);
            controller.setPlayers(players);
        },
        "boardMoveSuccess" : function(data) {
            var rack = data.rack;
            controller.updateRack(rack);
        }
    };

    var parseAndIssueCommand = function(command) {
        var commandName = command.command;

        var handler = handlers[commandName];

        if (handler) {
            handler(command.payload);
        }
        else
        {
            console.info("Unrecognised command: ");
            console.dir(command);
        }
    }

    return {
        parseAndIssueCommand : parseAndIssueCommand
    }
}
