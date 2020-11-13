const phantom = require('phantom');

(async function() {
  const instance = await phantom.create();
  const page = await instance.createPage();
  await page.on('onResourceRequested', function(requestData) {
    console.info('Requesting', requestData.url);
  });

// ruleid: phantom-injection
  const status = await page.open(input());

// ok
  const status = await page.open('https://stackoverflow.com/');

  const content = await page.property('content');
  console.log(content);

  await instance.exit();
})();

(async function(userInput) {
  const instance = await phantom.create();
  const page = await instance.createPage();
  await page.on('onResourceRequested', function(requestData) {
    console.info('Requesting', requestData.url);
  });

// ruleid: phantom-injection
  const status = await page.property('content', input());

// ruleid: phantom-injection
  await page.setContent(userInput);

// ok
  var html = '<html>123</html>'
  const status = await page.property('content', html);

  const content = await page.property('content');
  console.log(content);

  await instance.exit();
})();

(async function(userInput) {
  const instance = await phantom.create();
  const page = await instance.createPage();
  await page.on('onResourceRequested', function(requestData) {
    console.info('Requesting', requestData.url);
  });

// ruleid: phantom-injection
  const status = await page.openUrl(input(), {}, {});

// ruleid: phantom-injection
  await page.evaluateJavaScript(userInput);

// ok
  var url = 'https://stackoverflow.com/'
  const status = await page.openUrl(url, {}, {});

  const content = await page.property('content');
  console.log(content);

  await instance.exit();
})();
