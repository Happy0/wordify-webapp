module.exports = function(opts) {

    var send = opts.send;
    var controller = opts.controller;

    
    var parseAndIssueCommand = function(command) {
        console.info("I got a message from the server...arooni");
    }

    return {
        parseAndIssueCommand : parseAndIssueCommand
    }
}
