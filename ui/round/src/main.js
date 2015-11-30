var ctrl = require('./ctrl');
var view = require('./view');
var m = require('mithril');

module.exports = function(opts) {
    var controller = new ctrl(opts);

    var Round = {
        controller : function () {
            return controller;
        },
        view : function (controller) {
            return view(controller);
        }
    };

    m.mount(opts.element, Round);
}

