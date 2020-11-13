var passport = require('passport')

module.exports = class Auth {
  constructor (config) {
    this.passport = passport
    // ruleid: hardcoded-passport-secret
    var JwtStrategy = require('passport-jwt').Strategy
    this.jwt_secret = 'HARDCODED-SECRET'

    passport.use(new JwtStrategy({
      secretOrKey: this.jwt_secret
    }, function (payload, done) {
      // auth callback
    }))
  }

  something (req, res, next) {
    // do something
  }

}
