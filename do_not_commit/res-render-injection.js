const express = require('express')
const app = express()
const port = 3000

const hardcodedPath = 'lib/layout'

function testController1(req, res) {
// ruleid: res-render-injection
    return res.render(`tpl.${req.query.path}`, {foo: bar})
};

app.get('/test1', testController1)

app.get('/test2', (req, res) => {
// ruleid: res-render-injection
    return res.render('tpl.' + req.query.path + '.smth-else', {foo: bar})
})

app.get('/ok-test', (req, res) => {
// ok
    return res.render(hardcodedPath, {foo: bar})
})

app.listen(port, () => console.log(`Example app listening at http://localhost:${port}`))