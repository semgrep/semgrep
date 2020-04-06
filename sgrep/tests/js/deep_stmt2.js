
var bar = 1

//ERROR: match
var foo = "bar"

function check() {
    if (foo === 'bar') {
        console.log(foo)   
    }
    //TOFIX: without this call, this does not work because js_to_generic
    // will not use a Block for the body but a single If.
    bar();
}