const { execSync } = require('child_process');
//ERROR: match
let ls = execSync("ls");

const { execSync: es } = require('child_process');
// MATCH:
es("ls");

const cp = require('child_process');
// MATCH:
cp.execSync("ls");

// TODO:
require('child_process').execSync("ls");
