
function f1() {
    // ruleid: test-mvp-either
    var constants = require('crypto');
    var sslOptions = {};
    https.createServer(sslOptions);
}

function f2() {
    // ruleid: test-mvp-either
    var constants = require('constants');
    var sslOptions = {};
    https.createServer(sslOptions);
}

function f1() {
    // OK: test-mvp-either
    var constants = require('foobar');
    var sslOptions = {};
    https.createServer(sslOptions);
}
