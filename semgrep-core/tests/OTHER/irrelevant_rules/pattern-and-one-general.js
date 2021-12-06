// https://raw.githubusercontent.com/lodash/lodash/master/random.js

// This file does not need to be parsed because "child_process" is
// required for the rule to match but not in this file. Tests also
// that "child" is insufficient

require("child")

/** Built-in method references without a dependency on `root`. */
const freeParseFloat = parseFloat;


function do_something() {
  exec("process" + freeParseFloat)
}

export default random;
