module 0x42::test {
    use 0x42::test::{ S, R as V, T };
    struct S<phantom V, T> has copy, drop { f: u64 }

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

    public entry fun update_lockup<CoinType>(
        sponsor: &signer, recipient: address, new_unlock_time_secs: u64) acquires Locks {
        let sponsor_address = signer::address_of(sponsor);
        assert!(exists<Locks<CoinType>>(sponsor_address), error::not_found(ESPONSOR_ACCOUNT_NOT_INITIALIZED));
        let locks = borrow_global_mut<Locks<CoinType>>(sponsor_address);
        assert!(table::contains(&locks.locks, recipient), error::not_found(ELOCK_NOT_FOUND));
    }

    spec {
        aborts_if false;
        ensures left == right<S<V, T>>();
    }
}

script{
    use 0x42::test::Token;

    #[test]
    const C: Token = Token { id: 1, amount: 100, token_properties: PropertyMap::new() };
    
    #[test, test_helpers]
    fun script_fun(): u32 {
        let 0x42::test::Token { id: z, amount: f } = C;
        let Token { id } = C;

        call(id);

        let tmp = C {
            id, amount: 100, token_properties: PropertyMap::new(),
        };

        let x = 1;
        let y = 2;
        x + y + z / f + use_some(id)
    }
}
