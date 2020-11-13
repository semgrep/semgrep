const express = require('express')
const app = express()
const port = 3000
const puppeteer = require('puppeteer')

app.get('/', async (req, res) => {
    const browser = await puppeteer.launch()
    const page = await browser.newPage()
// ruleid: express-puppeteer-injection
    const url = `https://${req.query.name}`
    await page.goto(url)

    await page.screenshot({path: 'example.png'})
    await browser.close()

    res.send('Hello World!')
})

app.post('/test', async (req, res) => {
    const browser = await puppeteer.launch()
    const page = await browser.newPage()
// ruleid: express-puppeteer-injection
    await page.setContent(`${req.body.foo}`)

    await page.screenshot({path: 'example.png'})
    await browser.close()

    res.send('Hello World!')
})

const controller = async (req, res) => {
    const browser = await puppeteer.launch();
    const page = await browser.newPage();
// ruleid: express-puppeteer-injection
    const body = req.body.foo;
    await page.setContent('<html>' + body + '</html>');

    await page.screenshot({path: 'example.png'});
    await browser.close();

    res.send('Hello World!');
}

app.post('/test2', async (req, res) => {
    const browser = await puppeteer.launch()
    const page = await browser.newPage()
// ruleid: express-puppeteer-injection
    await page.evaluateOnNewDocument(`${req.body.foo}`)

    await page.screenshot({path: 'example.png'})
    await browser.close()

    res.send('Hello World!')
})

const controller2 = async (req, res) => {
    const browser = await puppeteer.launch();
    const page = await browser.newPage();
// ruleid: express-puppeteer-injection
    const body = req.body.foo;
    await page.evaluate('alert(' + body + ')');

    await page.screenshot({path: 'example.png'});
    await browser.close();

    res.send('Hello World!');
}

app.post('/test2', controller)

app.post('/ok-test', async (req, res) => {
    const browser = await puppeteer.launch();
    const page = await browser.newPage();
// ok
    await page.goto('https://example.com');

    await page.screenshot({path: 'example.png'});
    await browser.close();

    res.send('Hello World!');
})

const controller = async (req, res) => {
    const browser = await puppeteer.launch();
    const page = await browser.newPage();
// ok
    const body = '<div>123</div>';
    await page.setContent('<html>' + body + '</html>');

    await page.screenshot({path: 'example.png'});
    await browser.close();

    res.send('Hello World!');
}

app.post('/ok-test2', controller)

app.listen(port, () => console.log(`Example app listening at http://localhost:${port}`))