function test1() {
    const express = require('express')
    const xml2json = require('xml2json')
    const app = express()
    const port = 3000

    app.get('/', (req, res) => {
// ruleid: express-xml2json-xxe
        const xml = req.query.xml
        const content = xml2json.toJson(xml, {coerce: true, object: true});
        res.send(content)
    })

    app.listen(port, () => console.log(`Example app listening at http://localhost:${port}`))
}

function test2() {
    const express = require('express')
    const xml2json = require('xml2json')
    const app = express()
    const port = 3000

    app.get('/', (req, res) => {
// ruleid: express-xml2json-xxe
        const content = xml2json.toJson(req.body, {coerce: true, object: true});
        res.send(content)
    })

    app.listen(port, () => console.log(`Example app listening at http://localhost:${port}`))
}

function okTest() {
    const express = require('express')
    const xml2json = require('xml2json')
    const app = express()
    const port = 3000

    app.get('/', (req, res) => {
// ok
        const content = expat.toJson(someVerifiedData(), {coerce: true, object: true});
        res.send(content)
    })

    app.listen(port, () => console.log(`Example app listening at http://localhost:${port}`))
}