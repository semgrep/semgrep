const CDP = require('chrome-remote-interface');

async function example(userInput) {
    let client;
    try {
        client = await CDP();
        const {Network, Page} = client;
        Network.requestWillBeSent((params) => {
            console.log(params.request.url);
        });
        await Network.enable();
        await Page.enable();
        // ok
        await Page.navigate({url: 'https://github.com'});
        // ruleid:chrome-remote-interface-navigate-injection
        await Page.navigate({url: userInput});
        await Page.loadEventFired();
    } catch (err) {
        console.error(err);
    } finally {
        if (client) {
            await client.close();
        }
    }
}

function example2(userInput) {
    CDP(async (client) => {
        const {Page} = client;
        try {
            const {frameId} = await Page.navigate({url: 'about:blank'});
            const html = '<html>test</html>';
            // ok
            await Page.setDocumentContent({frameId, html});
            // ruleid:chrome-remote-interface-setdocumentcontent-injection
            await Page.setDocumentContent({frameId, html: userInput});
        } catch (err) {
            console.error(err);
            client.close();
        }
    }).on('error', (err) => {
        console.error(err);
    });
}

async function example3(userInput) {
    let client;
    try {
        client = await CDP();
        const {Runtime} = client;
        const script1 = "document.querySelector('p').textContent"
        // ok
        const result = await Runtime.evaluate({expression: script1});
        // ruleid:chrome-remote-interface-evaluate-injection
        const result2 = await Runtime.evaluate({expression: userInput});
        // ruleid:chrome-remote-interface-evaluate-injection
        const result3 = await Runtime.evaluate({expression: 'var x = 123;' + userInput});
    } catch (err) {
        console.error(err);
    } finally {
        if (client) {
            await client.close();
        }
    }
}

async function example4(userInput) {
    let client;
    try {
        client = await CDP();
        const {Runtime} = client;
        const script1 = "document.querySelector('p').textContent"
        // ok
        const result = await Runtime.compileScript({expression: script1, sourceURL:"", persistScript:false, executionContextId:1});
        // ruleid:chrome-remote-interface-compilescript-injection
        const result2 = await Runtime.compileScript({expression: userInput, sourceURL:"", persistScript:false, executionContextId:1});
        // ruleid:chrome-remote-interface-compilescript-injection
        const result3 = await Runtime.compileScript({expression: 'var x = 123;' + userInput, sourceURL:"", persistScript:false, executionContextId:1});
    } catch (err) {
        console.error(err);
    } finally {
        if (client) {
            await client.close();
        }
    }
}

function example5(userInput) {

    CDP(async (client) => {
        const {Page} = client;
        try {
            await Page.enable();
            await Page.navigate({url: 'https://github.com'});
            await Page.loadEventFired();
            // ok
            const result = await Page.printToPDF({landscape: true, printBackground: true, headerTemplate: '<h1>Title</h1>'});
            // ruleid:chrome-remote-interface-printtopdf-injection
            const result2 = await Page.printToPDF({landscape: true, printBackground: true, footerTemplate: userInput});
            // ruleid:chrome-remote-interface-printtopdf-injection
            const result3 = await Page.printToPDF({landscape: true, printBackground: true, headerTemplate: '<h1>' + userInput + '</h1>'});
            fs.writeFileSync('page.pdf', Buffer.from(data, 'base64'));
        } catch (err) {
            console.error(err);
        } finally {
            await client.close();
        }
    }).on('error', (err) => {
        console.error(err);
    });

}