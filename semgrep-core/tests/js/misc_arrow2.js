function a() { return 1; }

//ERROR: match, this is now parsed as a Lambda
const a = () => { return 1; }

//ERROR: match
var b = () => { return 2; };
//ERROR: match
b = () => { return 3; };



// from https://semgrep.dev/s/q6WR/

//ERROR: match
var k = (a,...rest) => { return 10; }
//ERROR: match
var j = (...rest) => { return 12; }
//ERROR: match
var l = (a,b) => rest[0]; 
//ERROR: match
var i = (a) => { return 10; }

//ERROR: match
j = (a) => { return 10; }
//ERROR: match
l = (a,...rest) => { return 10; }
//ERROR: match
m = (...rest) => { return 12; }

//ERROR: match
let n = (a) => { return 10; }
