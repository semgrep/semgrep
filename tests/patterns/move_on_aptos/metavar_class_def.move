
module 0xcafe::metavar_class_def {

    // ERROR: match
    #[test]
    struct $X has drop, copy {
        field1: u64,
    }


    fun test_metavar_class_def() {

    }
}
