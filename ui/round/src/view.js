var scrabbleground = require('scrabbleground');

module.exports = function(ctrl) {

    var scrabblegroundView = scrabbleground.view(ctrl.scrabblegroundCtrl);

    return m('div', {}, scrabblegroundView);
}
