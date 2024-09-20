function test1(x) {
    var a = x
    //ruleid: tainting
    return o[a]
}

function test2() {
    var b = baz(0)
    //ruleid: tainting
    var z = o[b]
    return z+1
}

function test3(x) {
    var c
    if (z)
        c = x
    else
        c = 1
    //ruleid: tainting
    return o[c]
}

function test4(x) {
    var d
    if (x)
        d = 1
    else
        d = 2
    //OK:
    return o[d]
}
