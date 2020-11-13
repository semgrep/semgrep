const express = require('express')
const app = express()
const port = 3000
const wkhtmltopdf = require('wkhtmltopdf')
const wkhtmltoimage = require('wkhtmltoimage')

app.get('/', async (req, res) => {
// ruleid: express-wkhtmltopdf-injection
    const pdf = wkhtmltopdf(req.query.q, { output: 'vuln.pdf' })
    res.send(pdf)
})

app.post('/ok', async (req, res) => {
// ok
    const pdf = wkhtmltopdf('<html></html>', { output: 'vuln.pdf' })
    res.send(pdf)
})

app.post('/test', async (req, res) => {
// ruleid: express-wkhtmltoimage-injection
    const img = wkhtmltoimage.generate(req.body, { output: 'vuln.pdf' })
    res.send(img)
})

app.post('/test-ok', async (req, res) => {
// ok
    const data = '<html></html>'
    const img = wkhtmltoimage.generate(data, { output: 'vuln.pdf' })
    res.send(img)
})

app.listen(port, () => console.log(`Example app listening at http://localhost:${port}`))
