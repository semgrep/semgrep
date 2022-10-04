foo1 = "1st undeclared variable"
//ERROR:
console.log(foo1)

function bar()
{
    foo2 = "2nd undeclared variable" // This is a global too!
    //ERROR:
    console.log(foo2)
}
