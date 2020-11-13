const { chromium } = require('playwright');

const testFunc = async (userInput) => {
  const browser = await chromium.launch();
  const page = await browser.newPage();
  let url = 'https://hardcoded.url.com'
// ok
  await page.goto('https://example.com');

// ok
  await page.goto(url);

// ruleid:playwright-goto-injection
  await page.goto(unverifiedInput());

  const newUrl = userInput;
// ruleid:playwright-goto-injection
  await page.goto(newUrl);

  await page.screenshot({path: 'example.png'});
  await browser.close();
};

(async () => {
  const browser = await chromium.launch();
  const page = await browser.newPage();

// ok
  await page.setContent('<html></html>');

// ruleid:playwright-setcontent-injection
  await page.setContent(unverifiedInput());

  await page.screenshot({path: 'example.png'});
  await browser.close();
})();

async function test2(userInput) {

  const browser = await chromium.launch();
  const page = await browser.newPage();

// ok
  await page.evaluate(x => console.log(x), 5);

// ruleid:playwright-evaluate-code-injection
  await page.evaluate(`fetch(${userInput})`);

  await page.screenshot({path: 'example.png'});
  await browser.close();
}

async function test3(userInput) {

  const browser = await chromium.launch();
  const page = await browser.newPage();

// ok
  await page.evaluate(x => console.log(x), 5);

// ruleid:playwright-evaluate-arg-injection
  await page.evaluate(x => fetch(x), userInput);

  await page.screenshot({path: 'example.png'});
  await browser.close();
}

async function test4(userInput) {

  const browser = await chromium.launch();
  const page = await browser.newPage();
  const context = await browser.newContext();

// ok
  await context.addInitScript(x => console.log(x), 5);

// ruleid:playwright-addinitscript-code-injection
  await context.addInitScript(`fetch(${userInput})`);

  await page.screenshot({path: 'example.png'});
  await browser.close();
}
