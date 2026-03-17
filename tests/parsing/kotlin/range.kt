fun foo() {
    // .. is inclusive of the end value
    for (i in 1..10) {
        println(i)
    }

    // until is exclusive of the end value
    for (i in 1..<10) {
        println(i)
    }
}
