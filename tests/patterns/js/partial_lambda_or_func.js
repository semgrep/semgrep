//ERROR: match
function test(foo) {
    console.log(foo)
}

//ERROR: match
tryItOut(function named(foo) {
    console.log(foo)
})

//ERROR: match
return function(foo) {
    console.log(foo)
}
