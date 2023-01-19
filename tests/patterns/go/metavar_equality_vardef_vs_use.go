package Foo

// match
var foo = "bar"
// not allowed in Go
// console.log(foo);

func test() {
    //ERROR: match
    var foo2 = "bar"
    console.log(foo2)
}
