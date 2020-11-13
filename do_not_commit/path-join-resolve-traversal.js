var path = require('path');

function test1() {
    function someFunc(entry) {
        // ruleid:path-join-resolve-traversal
        var extractPath = path.join(opts.path, entry.path);
        return extractFile(extractPath);
    }
    someFunc();
}

function test2() {
    function someFunc(val) {
        createFile({
            // ruleid:path-join-resolve-traversal
            filePath: path.resolve(opts.path, val)
        })
        return true
    }
    someFunc()
}

function test3(req,res) {
    // ruleid:path-join-resolve-traversal
    let somePath = req.body.path;
    return path.join(opts.path, somePath);
}

function okTest1() {
    // ok
    function someFunc() {
        createFile({
            filePath: pth.join(opts.path, 'val')
        })
        return true
    }
    someFunc()
}