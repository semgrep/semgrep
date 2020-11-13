const Sandbox = require('sandbox');

function test1(userInput, cb) {
// ruleid: sandbox-code-injection
    const s = new Sandbox();
    s.run('lol('+userInput+')', cb);
}

function test2(userInput, cb) {
// ruleid: sandbox-code-injection
    const s = new Sandbox();
    var code = 'lol('+userInput+')'
    s.run(code, cb);
}

function test3(userInput, cb) {
// ruleid: sandbox-code-injection
    const s = new Sandbox();
    s.run(`lol(${userInput})`, cb);
}

function okTest1(cb) {
// ok
    const s = new Sandbox();
    s.run('lol("hi")', cb);
}

function okTest2(cb) {
// ok
    const s = new Sandbox();
    var code = 'lol("hi")'
    s.run(code, cb);
}

function okTest3(cb) {
// ok
    const s = new Sandbox();
    s.run(`lol("hi")`, cb);
}