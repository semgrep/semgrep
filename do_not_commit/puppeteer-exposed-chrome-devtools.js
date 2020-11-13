const puppeteer = require('puppeteer');

(async () => {
  // ruleid:puppeteer-exposed-chrome-devtools
  const browser = await puppeteer.launch({args:['--remote-debugging-address=123','--somethin-else']});
  const page = await browser.newPage();
  await page.goto('https://example.com');
  await browser.close();
})();

(async () => {
  var port = 9222;  
  // ruleid:puppeteer-exposed-chrome-devtools
  const browser = await puppeteer.launch({args:[`--remote-debugging-port=${port}`,'--somethin-else']});
  const page = await browser.newPage();
  await page.goto('https://example.com');
  await browser.close();
})();

(async () => {
  // ok
  const browser = await puppeteer.launch({args:['--somethin-else', '--more-examples']});
  const page = await browser.newPage();
  await page.goto('https://example.com');
  await browser.close();
})();