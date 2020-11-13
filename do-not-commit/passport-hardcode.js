// ruleid: hardcoded-passport-secret
const Strat = require("passport-jwt").Strategy;
console.log("words");
var x = new Strat({secretOrKey: "secret"});
