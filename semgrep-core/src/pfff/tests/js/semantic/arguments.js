console.log(arguments.length);
console.log(arguments);

foo(42, "bar", "more stuff");

function foo(a,b) {
    console.log(arguments);
}
