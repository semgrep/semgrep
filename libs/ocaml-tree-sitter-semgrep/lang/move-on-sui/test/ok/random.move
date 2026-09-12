// Copyright (c) Mysten Labs, Inc.
// SPDX-License-Identifier: Apache-2.0

#[allow(unused_use)]
// This module provides functionality for generating and using secure randomness.
//
// Randomness is currently write-only, until user-facing API is implemented.
module sui::random {
    use std::vector;
    use sui::object::{Self, UID};
    use sui::transfer;
    use sui::tx_context::{Self, TxContext};
    use sui::versioned::{Self, Versioned};

    // Sender is not @0x0 the system address.
    const ENotSystemAddress: u64 = 0;
    const EWrongInnerVersion: u64 = 1;
    const EInvalidRandomnessUpdate: u64 = 2;

    const CURRENT_VERSION: u64 = 1;

    /// Singleton shared object which stores the global randomness state.
    /// The actual state is stored in a versioned inner field.
    struct Random has key {
        id: UID,
        inner: Versioned,
    }

    struct RandomInner has store {
        version: u64,

        epoch: u64,
        randomness_round: u64,
        random_bytes: vector<u8>,
    }

    #[allow(unused_function)]
    /// Create and share the Random object. This function is called exactly once, when
    /// the Random object is first created.
    /// Can only be called by genesis or change_epoch transactions.
    fun create(ctx: &mut TxContext) {
        assert!(tx_context::sender(ctx) == @0x0, ENotSystemAddress);

        let version = CURRENT_VERSION;

        let inner = RandomInner {
            version,
            epoch: tx_context::epoch(ctx),
            randomness_round: 0,
            random_bytes: vector[],
        };

        let self = Random {
            id: object::randomness_state(),
            inner: versioned::create(version, inner, ctx),
        };
        transfer::share_object(self);
    }

    #[test_only]
    public fun create_for_testing(ctx: &mut TxContext) {
        create(ctx);
    }

    fun load_inner_mut(
        self: &mut Random,
    ): &mut RandomInner {
        let version = versioned::version(&self.inner);

        // Replace this with a lazy update function when we add a new version of the inner object.
        assert!(version == CURRENT_VERSION, EWrongInnerVersion);
        let inner: &mut RandomInner = versioned::load_value_mut(&mut self.inner);
        assert!(inner.version == version, EWrongInnerVersion);
        inner
    }

    #[allow(unused_function)] // TODO: remove annotation after implementing user-facing API
    fun load_inner(
        self: &Random,
    ): &RandomInner {
        let version = versioned::version(&self.inner);

        // Replace this with a lazy update function when we add a new version of the inner object.
        assert!(version == CURRENT_VERSION, EWrongInnerVersion);
        let inner: &RandomInner = versioned::load_value(&self.inner);
        assert!(inner.version == version, EWrongInnerVersion);
        inner
    }

    #[allow(unused_function)]
    /// Record new randomness. Called when executing the RandomnessStateUpdate system
    /// transaction.
    fun update_randomness_state(
        self: &mut Random,
        new_round: u64,
        new_bytes: vector<u8>,
        ctx: &TxContext,
    ) {
        // Validator will make a special system call with sender set as 0x0.
        assert!(tx_context::sender(ctx) == @0x0, ENotSystemAddress);

        // Randomness should only be incremented.
        let epoch = tx_context::epoch(ctx);
        let inner = load_inner_mut(self);
        assert!(
            (epoch == inner.epoch + 1 && inner.randomness_round == 0) ||
                (new_round == inner.randomness_round + 1),
            EInvalidRandomnessUpdate
        );

        inner.epoch = tx_context::epoch(ctx);
        inner.randomness_round = new_round;
        inner.random_bytes = new_bytes;
    }

    #[test_only]
    public fun update_randomness_state_for_testing(
        self: &mut Random,
        new_round: u64,
        new_bytes: vector<u8>,
        ctx: &TxContext,
    ) {
        update_randomness_state(self, new_round, new_bytes, ctx);
    }
}
