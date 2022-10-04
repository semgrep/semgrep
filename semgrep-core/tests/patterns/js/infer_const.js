// https://github.com/returntocorp/semgrep/issues/554

var string1 = 'test1'
//ERROR:
console.log(`Insert a string here: ${string1}`);
//ERROR:
console.log(`Insert a string here:` + string1);

let string2 = 'test2'
//ERROR:
console.log(`Insert a string here: ${string2}`);
//ERROR:
console.log(`Insert a string here:` + string2);
