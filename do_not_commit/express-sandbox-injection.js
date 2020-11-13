const Sandbox = require('sandbox');
const express = require('express');
const app = express();
const port = 3000;

const cb = () => {
    console.log('ok')
}

app.get('/', (req, res) => res.send('Hello World!'))

app.get('/test1', function (req, res) {
// ruleid:express-sandbox-code-injection
    const s = new Sandbox();
    s.run('lol('+req.query.userInput+')', cb);
    res.send('Hello world');
})

app.get('/test2', function (req, res) {
// ruleid:express-sandbox-code-injection
    const s = new Sandbox();
    var code = 'lol('+req.query.userInput+')'
    s.run(code, cb);
    res.send('Hello world');
})

app.get('/test3', function (req, res) {
// ruleid:express-sandbox-code-injection
    const s = new Sandbox();
    s.run(`lol(${req.query.userInput})`, cb);
    res.send('Hello world');
})

app.get('/ok-test1', function (req, res) {
// ok
    const s = new Sandbox();
    s.run('lol("hi")', cb);
    res.send('Hello world');
})

app.get('/ok-test2', function (req, res) {
// ok
    const s = new Sandbox();
    var code = 'lol("hi")'
    s.run(code, cb);
    res.send('Hello world');
})

app.get('/test1', function (req, res) {
// ok
    const s = new Sandbox();
    s.run(`lol("hi")`, cb);
    res.send('Hello world');
})

app.listen(port, () => console.log(`Example app listening at http://localhost:${port}`))