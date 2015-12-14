module.exports = function(opts) {

    var send = opts.send;
    var controller = opts.ctrl;

    var handlers = {
        "playerBoardMove" : function(data) {
            controller.boardMoveMade(data);
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
