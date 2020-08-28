function foo() {
    var b = 1;
}

var b = 2;
// this was initially badly tagged as a Local by the AST JS builder
console.log(b);
