var wkhtmltoimage = require('wkhtmltoimage')

const html = '<html></html>'
// ok
wkhtmltoimage.generate(html, { output: 'vuln.jpg' })

// ruleid: wkhtmltoimage-injection
wkhtmltoimage.generate(input(), { output: 'vuln.jpg' })

function test(userInput) {
// ruleid: wkhtmltoimage-injection
    wkhtmltoimage.generate(userInput, { output: 'vuln.jpg' })
}