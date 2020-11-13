const { chromium } = require('playwright');

(async () => {
  // ruleid:playwright-exposed-chrome-devtools
  const browser = await chromium.launch({args:['--remote-debugging-address=123','--somethin-else']});
  const page = await browser.newPage();
  await page.goto('https://example.com');
  await browser.close();
})();

(async () => {
  var port = 9222;  
  // ruleid:playwright-exposed-chrome-devtools
  const browser = await chromium.launch({args:[`--remote-debugging-port=${port}`,'--somethin-else']});
  const page = await browser.newPage();
  await page.goto('https://example.com');
  await browser.close();
})();

(async () => {
  // ok
  const browser = await chromium.launch({args:['--somethin-else', '--more-examples']});
  const page = await browser.newPage();
  await page.goto('https://example.com');
  await browser.close();
})();