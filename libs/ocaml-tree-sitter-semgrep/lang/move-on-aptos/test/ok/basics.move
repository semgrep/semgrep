module 0x42::test {
    use 0x42::test::S;
    struct S has copy, drop { f: u64 }

    /// /*
    /// Core data structures for holding tokens
    ///
    struct Token has store {
        id: TokenId,
        /// the amount of tokens. Only property_version = 0 can have a value bigger than 1.
        amount: u64,
        /// The properties with this token.
        /// when property_version = 0, the token_properties are the same as default_properties in TokenData, we don't store it.
        /// when the property_map mutates, a new property_version is assigned to the token.
        token_properties: PropertyMap,
    }

    fun t(cond: bool, s1: S, s2: S) {
        // (if (cond) s1 else s2).f
        let _: u64 = if (cond) { s1 } else { s2 }.f;

        // (if (cond) s1).f else (s2.f)
        // so parsing error
        if (cond) s1.f else s2.f;

        assert!(get_used_cap(local_chain_id) == amount1 + amount3 + amount5 + amount7, 0);

        while(cond) { s1 }.f
    }

    /***/
    /*****/
    /**/

    /*
    /**/
    /***/
    */

    // fun!!
    fun xx() : u64 {
        /// doc1 /*
        //// regular /*
        // regular 2 /*
        let x = b"/*" /** ??? */;
        table.target_bucket_size = max(1024 /* free_write_quota */ / estimated_entry_size, 1);
        300
        / // this is not a doc comment
        3 + if (true) { 1; 22 } else { 25 }
    }

    spec {
        aborts_if xx() == 100 with yy();
    }

}

script{
    #[test, test_helpers]
    fun script_fun(): u32 {
        let x = 1;
        let y = 2;
        x + y
    }
}
