var FACEBOOK_APP_ID     = require('../../../config').fbAppID;
var URLcallback         = require('../../../config').URL;
var passport            = require('passport');
// ruleid: hardcoded-passport-secret
var FacebookStrategy    = require('passport-facebook').Strategy;
var FACEBOOK_APP_SECRET = "HARDCODED-SECRET";


module.exports = function(req, res) {

  passport.use(new FacebookStrategy({
      clientID:     FACEBOOK_APP_ID,
      clientSecret: FACEBOOK_APP_SECRET,
      callbackURL:  URLcallback + '/api/auth/facebook/callback'
    },
    function(accessToken, refreshToken, profile, done) {
      // do something
    }
  ));
};
