
function test1() {
    myArray = tainted;
    myArray.forEach((x) => {
        foobar()
        //ruleid: test
        sink(x)
     })
}

function test2() {
    myArray = [tainted];
    myArray.forEach((x) => {
        foobar()
        //ruleid: test
        sink(x)
     })
}

function test() {
    const myArray = [tainted, 'ok', 'ok'];
    const iterator = myArray.values();
    iterator.forEach((x) => {
        foobar()
        //ruleid: test
        sink(x)
     })
}