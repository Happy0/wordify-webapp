module.exports = function(opts) {

    var host = opts.host;
    var handler = opts.handler;

    var socketUrl = "wss://" + host;
    var conn = new Websocket(socketUrl);

    conn.onmessage = function(e) {
        var data = JSON.parse(e.data);
        handler(data);
    };

    conn.onerror = function(e) {
        alert("onerror");
    };

    conn.onclose = function(e) {
        alert("onclose");
    }

    var send = function(obj) {
        var payload = JSON.stringify(obj);
        conn.send(payload);
    };

    return {
        send : send
    }
}
