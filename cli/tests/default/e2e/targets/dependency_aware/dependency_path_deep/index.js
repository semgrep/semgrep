// Exercises the reachable SCA path: a code match on the transitively-introduced
// "victim" package, so the dependency-path gate is tested for reachable findings
// (not just the unreachable/depends-on-only path).
const victim = require("victim");

victim.run();
