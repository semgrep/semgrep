// ruleid:useless-assignment
var x = 1;

// ruleid:useless-assignment
x = 2;
x = 3;
console.log(x);


// ok
y = [1, 2];
y = y.map(function(e) { return e * 2; });
console.log(y);

// ok
z = [1, 2];
z = z.map(e => e * 2);
console.log(z);

// ok
a = "Hi ";
a += "Mom";
