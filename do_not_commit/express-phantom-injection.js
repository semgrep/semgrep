const express = require('express')
const app = express()
const port = 3000
const phantom = require('phantom');

app.get('/test', async (req, res) => {
    const instance = await phantom.create();
    const page = await instance.createPage();
    await page.on('onResourceRequested', function(requestData) {
        console.info('Requesting', requestData.url);
    });

// ruleid: express-phantom-injection
    const status = await page.property('content', req.get('name'));

// ruleid: express-phantom-injection
    await page.setContent(req.query.q);

    res.send('Hello World!')
})

app.post('/test2', async (req, res) => {
    const instance = await phantom.create();
    const page = await instance.createPage();
    await page.on('onResourceRequested', function(requestData) {
        console.info('Requesting', requestData.url);
    });

// ruleid: express-phantom-injection
    const status = await page.property('content', req.query.q);

// ruleid: express-phantom-injection
    await page.setContent(req.body);

// ok
    var html = '<html>123</html>'
    const status = await page.property('content', html);

    const content = await page.property('content');
    console.log(content);
    
    await instance.exit();

    res.send('Hello World!')
})

app.post('/test3', async (req, res) => {
    const instance = await phantom.create();
    const page = await instance.createPage();
    await page.on('onResourceRequested', function(requestData) {
        console.info('Requesting', requestData.url);
    });

// ruleid: express-phantom-injection
    const status = await page.openUrl(req.params.url, {}, {});

// ruleid: express-phantom-injection
    await page.evaluateJavaScript(req.body.script);

// ok
    var url = 'https://stackoverflow.com/'
    const status = await page.openUrl(url, {}, {});

    const content = await page.property('content');
    console.log(content);
    
    await instance.exit();

    res.send('Hello World!')
})


app.listen(port, () => console.log(`Example app listening at http://localhost:${port}`))