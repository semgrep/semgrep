open class B {
}

interface C {
}

//ERROR: match
class ShouldMatch : C {
}

class ShouldntMatch : B() {
}

// In Kotlin the order after ':' does not matter.
// There is no 'implements' keyword like in Java, so whatever
// is not the extended class (with the ()), is an implement.
//ERROR: match
class AlsoShouldMatch : B(), C {
}
