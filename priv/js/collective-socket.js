/*!
 * @overview  Collective - Introspection UI for distributed Erlang systems
 * @copyright Copyright (c) 2014, Alexander Koeck
 * @license   Licensed under BSD license
 *            See https://raw.github.com/lx7/collective/master/LICENSE
 */

Collective.Socket = function() {
    var ws,
        handlers = {},
        socket = {};

    // Reconnect on connection failure
    var RECONNECT_TIMEOUT = 1000;

    /** Invoke handlers on message */
    socket.onmessage = function(data) {
        Ember.run(function() {
            var msg = $.parseJSON(data);
            if (handlers[msg.type]) {
                handlers[msg.type](msg);
            } else {
                console.log("No handler for message type '" + msg.type + "'");
            }
        });
    }
    
    /** Connect WebSocket to URI */ 
    socket.connect = function(uri) {
        uri = ws_uri(uri);
        ws = new WebSocket(uri);
        
        ws.onopen = function(evt) {
        };
        ws.onclose = function(evt) {
            socket.reconnect(uri);
        };
        ws.onmessage = function(evt) {
            socket.onmessage(evt.data);
        };
    }
    
    /** Reconnect WebSocket after timeout */
    socket.reconnect = function(uri) {
        setTimeout(function () {
            socket.connect(uri);
        }, RECONNECT_TIMEOUT);
    }

    /** Register callback for message type */
    socket.register = function(type, callback) {
        handlers[type] = callback;
    }

    /** Send JSON frame via WebSocket */
    socket.send = function(msg) {
        var json = JSON.stringify(msg);
        ws.send(json); 
    }

    /** Create qualified WebSocket URI from relative path */
    function ws_uri(s) {
        var l = window.location;
        if (!s || s.match(/^ws?:/)) {
            return s;
        } else {
            return ((l.protocol === "https:") ? "wss://" : "ws://") + l.hostname + ":" + l.port + l.pathname + s;
        }
    }

    return socket;
}();

