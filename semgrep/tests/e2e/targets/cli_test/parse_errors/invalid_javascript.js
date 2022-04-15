function bad() {
    // ruleid:disallow-old-tls-versions2
    var constants = require('crypto');
    var sslOptions = 
    key: fs.readFileSync('/etc/ssl/private/private.key'),
    secureProtocol: 'SSLv23_server_method',
    secureOptions: constants.SSL_OP_NO_SSLv2 | constants.SSL_OP_NO_SSLv3
    };
    https.createServer(sslOptions);
}
