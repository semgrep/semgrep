function test1() {
    var winston = require('winston'),
        express = require('express'),
        expat = require('xml2json');

    var xmlParsingMiddleware = function(req, res, next) {
        var buf = '';
        req.setEncoding('utf8');
        req.on('data', function (chunk) {
            buf += chunk
        });
// ruleid: express-xml2json-xxe-event
        req.on('end', function () {
            req.body = expat.toJson(buf, {coerce: true, object: true});
            next();
        });
    };
}

function test2() {
    const express = require('express')
    const app = express()
    const port = 3000

    app.get('/', (req, res) => {
        var buf = '';
        req.setEncoding('utf8');
        req.on('data', function (chunk) {
            buf += chunk
        });
// ruleid: express-xml2json-xxe-event
        req.on('end', function () {
            req.body = expat.toJson(buf, {coerce: true, object: true});
            next();
        });
    })

    app.listen(port, () => console.log(`Example app listening at http://localhost:${port}`))
}

function okTest() {
    const express = require('express')
    const app = express()
    const port = 3000
    const someEvent = require('some-event')

// ok
    someEvent.on('event', function (err, data) {
        req.body = expat.toJson(data, {coerce: true, object: true});
        next();
    });

    app.listen(port, () => console.log(`Example app listening at http://localhost:${port}`))
}
