module move::func_parsing_test {
    // ok: pattern-move-func-signature
    #[attr(key = b"value", key2 = b"value2")]
    public fun test_func(signer: &signer, addr: address) {
        // Do nothing
    }

    // ruleid: pattern-move-func-signature
    #[attr(key = b"value", key2 = b"value2")]
    public(friend) fun test_func2(signer: &signer, addr: address) {
        // Do nothing
    }

    // ok: pattern-move-func-signature
    #[attr(key = b"value2")]
    fun test_func3(signer: &signer, object: Object<Hello>) {
        // Do nothing
    }

    // ruleid: pattern-move-func-signature
    #[attr2 = value3]
    fun test_func4(signer: &signer, addr: address) {
        // Do nothing
    }
}

script {
    use some::some_module::test_func;

    const ADDRESS: address = 0xabcd;

    // ruleid: pattern-move-func-signature
    #[attr(key = b"value", inner(key2 = b"value2"))]
    entry fun do_something() {
        test_func(&signer, ADDRESS);
    }
}