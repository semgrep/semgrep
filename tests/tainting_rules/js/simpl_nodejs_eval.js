const Sandbox = require('sandbox');
const express = require('express');
const app = express();

const cb = () => {
    console.log('ok')
}
app.get('/test1', function (req, res) {
    const s = new Sandbox();
    //ruleid: tainting
    s.run('lol(' + req.query.userInput + ')', cb);
    res.send('Hello world');
})

app.get('/test2', function (req, res) {
    const s = new Sandbox();
    var code = 'lol(' + req.query.userInput + ')'
    //ruleid: tainting
    s.run(code, cb);
    res.send('Hello world');
})

app.get('/test3', function (req, res) {
    const s = new Sandbox();
    //ruleid: tainting
    s.run(`lol(${req.query.userInput})`, cb);
    res.send('Hello world');
})
