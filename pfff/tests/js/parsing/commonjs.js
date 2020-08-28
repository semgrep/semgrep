// see eloquent JS book chapter on module to understand the simplicity
// of CommonJS module system. Basically every file is passed 3 entities:
// require, export, and module.

const foo = require('./fs');

exports.myfunc = function() { console.log("myfunc"); }

module.exports = function() { console.log("myfunc"); }

function foo(exports, require) {
    exports.foo = 2;
    require('./fs');
}
