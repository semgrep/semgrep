// MATCH:
try foo;

// MATCH:
for x in foo {}

// MATCH:
var x = foo
// MATCH:
var foo = x 

// MATCH:
let x = foo
// MATCH:
let foo = x 