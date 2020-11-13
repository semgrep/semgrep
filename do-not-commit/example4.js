// ruleid: hardcoded-jwt-secret
const $jwt = require('jsonwebtoken');

const cert = 'hardcoded-secret';

module.exports = (app) => {
  app.post('/api/login', (req, res) => {
    app.login(req.body.username, req.body.password).then((out) => {
      out.token = $jwt.sign(out, cert, {expiresIn: '1d'});
      res.send(out);
    }, (err) => {
      console.error(err);
      res.status(400).send(err);
    });
  });
};
