
const express = require('express')
const router = express.Router()

// cf. juice-shop
exports.promotionVideo = () => {
    return (req, res) => {
      fs.readFile('views/promotionVideo.pug', function (err, buf) {
        if (err) throw err
        let template = buf.toString()
        const subs = getSubsFromFile()

        utils.solveIf(challenges.videoXssChallenge, () => { return utils.contains(subs, '</script><script>alert(`xss`)</script>') })

        const theme = themes[config.get('application.theme')]
        template = template.replace(/_title_/g, config.get('application.name'))
        template = template.replace(/_favicon_/g, favicon())
        template = template.replace(/_bgColor_/g, theme.bgColor)
        template = template.replace(/_textColor_/g, theme.textColor)
        template = template.replace(/_navColor_/g, theme.navColor)
        template = template.replace(/_primLight_/g, theme.primLight)
        template = template.replace(/_primDark_/g, theme.primDark)
        const fn = pug.compile(template)
        let compiledTemplate = fn()
        compiledTemplate = compiledTemplate.replace('<script id="subtitle"></script>', '<script id="subtitle" type="text/vtt" data-label="English" data-lang="en">' + subs + '</script>')
        // ruleid: direct-response-write
        res.send(compiledTemplate)
      })
    }
    function favicon () {
      return utils.extractFilename(config.get('application.favicon'))
    }
  }


router.get('/greeting', (req, res) => {
    const { name } = req.query;
    // ruleid: direct-response-write
    res.send('<h1> Hello :' + name + "</h1>")
})

//template handle escaping
router.get('/greet-template', (req, res) => {
    name = req.query.name
    // ok: direct-response-write
    res.render('index', { user_name: name });
})

module.exports = router


app.get('/', function (req, res) {
    var user = req.query.name;

    msg = "Hi " + user
    // ruleid: direct-response-write
    res.send('Response</br>' + msg);
});


var msg = '';
app.get('/3', function (req, res) {
    var user = req.query.name;

    msg = "Hi " + user
    // ruleid: direct-response-write
    res.send('Response</br>' + msg);
});

app.get('/2', function (req, res) {
    var user = { user: req.query.name };
    // ruleid: direct-response-write
    res.send('Response</br>' + user.name);
});

app.get('/1', function (req, res) {
    var user = req.query.name;
    var msg = [];
    msg.push(user);
    // ruleid: direct-response-write
    res.send('Response</br>' + msg[0]);
});

app.get('/4', function (req, res) {
    var user = req.query.name;
    var header = "<html>";
    var msg = 'Hi ' + user;
    var footer = "</html>";
    var output = header + msg + footer;
    // ruleid: direct-response-write
    res.send(output);
});





var express = require('express');
var app = express();
app.get('/', function (req, res) {
    var resp = req.query.name;
    // ruleid: direct-response-write
    res.send('Response</br>' + resp);
});
app.get('/3', function (req, res) {
    var resp = req.query.name;
    // ruleid: direct-response-write
    res.write('Response</br>' + resp);
});

app.get('/3', function (req, res) {
    var resp = req.foo;
    var x = 1;
    // ruleid: direct-response-write
    res.write('Response</br>' + resp);
});

app.get('/xss', function (req, res) {
    var html = "ASadad" + req.query.name + "Asdadads"
    // ruleid: direct-response-write
    res.write('Response</br>' + html);
});
app.get('/xss', function (req, res) {
    // ruleid: direct-response-write
    res.write('Response</br>' + req.query('doo'));
});
app.get('/xss', function (req, res) {
    // ruleid: direct-response-write
    res.write('Response</br>' + req.query.name);
});

app.get('/noxss', function (req, res) {
    var resp = req.query.name;
    // ok: direct-response-write
    res.write('Response</br>');
});

app.get('/noxs2s', function (req, res) {
    var resp = req.query.name;
    // ruleid: direct-response-write
    res.write('Response</br>' + foo);
});

app.get('/xss', function (req, res) {
    var resp = req.query.name;
    var html = "ASadad" + resp + "Asdadads"
    // ruleid: direct-response-write
    res.write('Response</br>' + html);
});
app.listen(8000);
