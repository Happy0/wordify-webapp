var scrabbleground = require('scrabbleground');
var m = require('mithril');

module.exports = function(ctrl) {

    var scrabblegroundView = scrabbleground.view(ctrl.scrabbleGroundCtrl);

    return m('div', {}, scrabblegroundView);
}
