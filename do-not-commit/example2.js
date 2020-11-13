(()=> {

  'use strict';
// ruleid: hardcoded-jwt-secret
  let User = require('./user'),
  jwt      = require('jsonwebtoken');

  const express = require('express');
  let router = express.Router();

  router.post('/signup', (req, res) => {
    let user = new User({
      name:req.body.name,
      password:req.body.password
    });
    var token = jwt.sign(user, "hardcoded-secret", {expiresIn: 60*60*10});
    res.send({success:true, token: token});
  });

  module.exports = router;
})();
