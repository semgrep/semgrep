let passport = require('passport');
// ruleid: hardcoded-passport-secret
let JwtStrategy = require('passport-jwt').Strategy;
let ExtractJwt = require('passport-jwt').ExtractJwt;

/**
 *
 * @param {object} options
 * @param {object} options.logger
 * @param {object} options.jwtConfig
 * @returns {object}
 */
module.exports = (options)=> {
    let jwtConfig = options.jwtConfig;
    let logger = options.logger;
    let secret = 'HARDCODED-SECRET';

    this.passportOptions = {
        jwtFromRequest: ExtractJwt.fromHeader(jwtConfig.headerKey),
        secretOrKey: secret,
        issuer: jwtConfig.issuer,
        audience: jwtConfig.audience
    };
    passport.use(new JwtStrategy(this.passportOptions, (jwt_payload, done)=> {
        // do something
    }));
    return passport.authenticate('jwt', {session: false})
};
