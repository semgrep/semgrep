'use strict';
// ruleid: hardcoded-passport-secret
const FacebookStrategy = require('passport-facebook').Strategy;

exports.init = function(passport, router, config) {

  passport.use(
    new FacebookStrategy(
      {
        clientID: config.appId,
        clientSecret: 'HARDCODED-SECRET',
        callbackURL: config.publicAddress + config.callbackURL,
        enableProof: false,
        passReqToCallback: true,
      },
      function(req, accessToken, refreshToken, profile, done) {
        // do something
      },
    ),
  );
};
