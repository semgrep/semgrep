
// MATCH:
var x = function foo(req, res) { return a };

// OK:
var x = function foo(req) { return a };

// MATCH:
const y = function bar(req, res) { return a }

// OK:
function y(req, res) {
  return a;
}

// MATCH:
const z = (req, res) => { return a }
