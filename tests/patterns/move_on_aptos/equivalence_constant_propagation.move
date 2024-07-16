
module 0xcafe::equivalence_constant_propagation {
    fun test_equivalence_constant_propagation() {
        let str = b"secret";

        // ERROR: match
        bar(str);
    }
}
