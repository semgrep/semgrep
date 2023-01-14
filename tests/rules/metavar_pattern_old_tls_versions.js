
function ok1() {
    // ok: disallow-old-tls-versions2
    var constants = require('crypto');
    var sslOptions = {
    key: fs.readFileSync('/etc/ssl/private/private.key'),
    secureProtocol: 'SSLv23_server_method',
    secureOptions: constants.SSL_OP_NO_SSLv2 | constants.SSL_OP_NO_TLSv1 | constants.SSL_OP_NO_SSLv3
    };
    https.createServer(sslOptions);
}

function ok2() {
    // ok: disallow-old-tls-versions2
    var constants = require('crypto');
    var sslOptions = {
    key: fs.readFileSync('/etc/ssl/private/private.key'),
    secureProtocol: 'SSLv23_server_method',
    secureOptions: constants.SSL_OP_NO_SSLv2 | constants.SSL_OP_NO_SSLv3 | constants.SSL_OP_NO_TLSv1
    };
    https.createServer(sslOptions);
}

function ok3() {
    // ok: disallow-old-tls-versions2
    var constants = require('crypto');
    var sslOptions = {
    key: fs.readFileSync('/etc/ssl/private/private.key'),
    secureProtocol: 'SSLv23_server_method',
    secureOptions: constants.SSL_OP_NO_TLSv1 | constants.SSL_OP_NO_SSLv2 | constants.SSL_OP_NO_SSLv3
    };
    https.createServer(sslOptions);
}

function ok4() {
    // ok: disallow-old-tls-versions2
    var constants = require('constants');
    var sslOptions = {
    key: fs.readFileSync('/etc/ssl/private/private.key'),
    secureProtocol: 'SSLv23_server_method',
    secureOptions: constants.SSL_OP_NO_TLSv1 | constants.SSL_OP_NO_SSLv2 | constants.SSL_OP_NO_SSLv3
    };
    https.createServer(sslOptions);
}

function bad1() {
    // ruleid: disallow-old-tls-versions2
    var constants = require('crypto');
    var sslOptions = {
    key: fs.readFileSync('/etc/ssl/private/private.key'),
    secureProtocol: 'SSLv23_server_method',
    secureOptions: constants.SSL_OP_NO_SSLv2 | constants.SSL_OP_NO_SSLv3
    };
    https.createServer(sslOptions);
}

function bad2() {
    // ruleid: disallow-old-tls-versions2
    var constants = require('crypto');
    var sslOptions = {
    key: fs.readFileSync('/etc/ssl/private/private.key'),
    secureProtocol: 'SSLv23_server_method',
    secureOptions: constants.SSL_OP_NO_SSLv2
    };
    https.createServer(sslOptions);
}

function bad3() {
    // ruleid: disallow-old-tls-versions2
    var constants = require('crypto');
    var sslOptions = {
    key: fs.readFileSync('/etc/ssl/private/private.key'),
    secureProtocol: 'SSLv23_server_method',
    };
    https.createServer(sslOptions);
}

function bad4() {
    // ruleid: disallow-old-tls-versions2
    var constants = require('constants');
    var sslOptions = {
    key: fs.readFileSync('/etc/ssl/private/private.key'),
    secureProtocol: 'SSLv23_server_method',
    };
    https.createServer(sslOptions);
}
