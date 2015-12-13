module.exports = function(opts) {

    var send = opts.send;
    var controller = opts.controller;

    
    var parseAndIssueCommand = function(command) {
        console.dir(command);
    }

    return {
        parseAndIssueCommand : parseAndIssueCommand
    }
}
