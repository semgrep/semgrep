//OK:
var sslOptions = { foo: bar, secureOptions: constants.SSL_OP_NO_SSLv2 | constants.SSL_OP_NO_SSLv3 };
//ERROR:
var sslOptions = { foo: bar, secureOptions: constants.SSL_OP_NO_SSLv2 | constants.SSL_OP_NO_SSLv3 | constants.SSL_OP_NO_TLSv1 };
//ERROR:
var sslOptions = { foo: bar, secureOptions: constants.SSL_OP_NO_SSLv2 | constants.SSL_OP_NO_TLSv1 | constants.SSL_OP_NO_SSLv3 };
//ERROR:
var sslOptions = { foo: bar, secureOptions: constants.SSL_OP_NO_SSLv3 | constants.SSL_OP_NO_SSLv2 | constants.SSL_OP_NO_TLSv1 };
//ERROR:
var sslOptions = { foo: bar, secureOptions: constants.SSL_OP_NO_SSLv3 | constants.SSL_OP_NO_TLSv1 | constants.SSL_OP_NO_SSLv2 };
//ERROR:
var sslOptions = { foo: bar, secureOptions: constants.SSL_OP_NO_TLSv1 | constants.SSL_OP_NO_SSLv3 | constants.SSL_OP_NO_SSLv2 };
//ERROR:
var sslOptions = { foo: bar, secureOptions: constants.SSL_OP_NO_TLSv1 | constants.SSL_OP_NO_SSLv2 | constants.SSL_OP_NO_SSLv3 };
