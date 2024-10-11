
module 0xcafe::metavar_anno {
    // ERROR: match
    #[test(key = b"value"), randomness]
    public entry fun test_metavar_anno() {

    }
}
