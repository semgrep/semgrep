var x = 1;
var y = 2;
var z = 3;

// x = y = z is parsed as (x = (y = z))
console.log(x = y = z);
// => 3
