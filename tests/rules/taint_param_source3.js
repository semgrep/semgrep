function test() {
    var foobar = function(req, res, next) {
        var buf = '';
        req.on('data', function (chunk) {
            buf += chunk
        });
        // ruleid: test
        sink(buf);
    };
}
