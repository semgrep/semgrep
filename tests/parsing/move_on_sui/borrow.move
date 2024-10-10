// Copyright (c) Mysten Labs, Inc.
// SPDX-License-Identifier: Apache-2.0

/// A simple library that enables hot-potato-locked borrow mechanics.
///
/// With Programmable transactions, it is possible to borrow a value within
/// a transaction, use it and put back in the end. Hot-potato `Borrow` makes
/// sure the object is returned and was not swapped for another one.
module sui::borrow {
    use sui::object::{Self, ID};
    use std::option::{Self, Option};
    use sui::tx_context::{Self, TxContext};

    /// The `Borrow` does not match the `Referent`.
    const EWrongBorrow: u64 = 0;
    /// An attempt to swap the `Referent.value` with another object of the same type.
    const EWrongValue: u64 = 1;

    /// An object wrapping a `T` and providing the borrow API.
    struct Referent<T: key + store> has store {
        id: address,
        value: Option<T>
    }

    /// A hot potato making sure the object is put back once borrowed.
    struct Borrow { ref: address, obj: ID }

    /// Create a new `Referent` struct
    public fun new<T: key + store>(value: T, ctx: &mut TxContext): Referent<T> {
        Referent {
            id: tx_context::fresh_object_address(ctx),
            value: option::some(value)
        }
    }

    /// Borrow the `T` from the `Referent` receiving the `T` and a `Borrow`
    /// hot potato.
    public fun borrow<T: key + store>(self: &mut Referent<T>): (T, Borrow) {
        let value = option::extract(&mut self.value);
        let id = object::id(&value);

        (value, Borrow {
            ref: self.id,
            obj: id
        })
    }

    /// Put an object and the `Borrow` hot potato back.
    public fun put_back<T: key + store>(self: &mut Referent<T>, value: T, borrow: Borrow) {
        let Borrow { ref, obj } = borrow;

        assert!(object::id(&value) == obj, EWrongValue);
        assert!(self.id == ref, EWrongBorrow);
        option::fill(&mut self.value, value);
    }

    /// Unpack the `Referent` struct and return the value.
    public fun destroy<T: key + store>(self: Referent<T>): T {
        let Referent { id: _, value } = self;
        option::destroy_some(value)
    }

    #[test_only]
    struct Test has key, store {
        id: object::UID
    }

    #[test]
    fun test_borrow() {
        let ctx = &mut sui::tx_context::dummy();
        let ref = new(Test { id: object::new(ctx) }, ctx);

        let (value, borrow) = borrow(&mut ref);
        put_back(&mut ref, value, borrow);

        let Test { id } = destroy(ref);
        object::delete(id);
    }

    #[test]
    #[expected_failure(abort_code = EWrongValue)]
    /// The `value` is swapped with another instance of the type `T`.
    fun test_object_swap() {
        let ctx = &mut sui::tx_context::dummy();
        let ref_1 = new(Test { id: object::new(ctx) }, ctx);
        let ref_2 = new(Test { id: object::new(ctx) }, ctx);

        let (v_1, b_1) = borrow(&mut ref_1);
        let (v_2, b_2) = borrow(&mut ref_2);

        put_back(&mut ref_1, v_2, b_1);
        put_back(&mut ref_2, v_1, b_2);

        let Test { id } = destroy(ref_1);
        object::delete(id);

        let Test { id } = destroy(ref_2);
        object::delete(id);
    }

    #[test]
    #[expected_failure(abort_code = EWrongBorrow)]
    /// The both `borrow` and `value` are swapped with another `Referent`.
    fun test_borrow_fail() {
        let ctx = &mut sui::tx_context::dummy();
        let ref_1 = new(Test { id: object::new(ctx) }, ctx);
        let ref_2 = new(Test { id: object::new(ctx) }, ctx);

        let (v_1, b_1) = borrow(&mut ref_1);
        let (v_2, b_2) = borrow(&mut ref_2);

        put_back(&mut ref_1, v_2, b_2);
        put_back(&mut ref_2, v_1, b_1);

        let Test { id } = destroy(ref_1);
        object::delete(id);

        let Test { id } = destroy(ref_2);
        object::delete(id);
    }
}
