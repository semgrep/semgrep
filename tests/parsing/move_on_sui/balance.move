// Copyright (c) Mysten Labs, Inc.
// SPDX-License-Identifier: Apache-2.0

/// A storable handler for Balances in general. Is used in the `Coin`
/// module to allow balance operations and can be used to implement
/// custom coins with `Supply` and `Balance`s.
module sui::balance {
    use sui::tx_context::{Self, TxContext};

    friend sui::sui;

    /// For when trying to destroy a non-zero balance.
    const ENonZero: u64 = 0;

    /// For when an overflow is happening on Supply operations.
    const EOverflow: u64 = 1;

    /// For when trying to withdraw more than there is.
    const ENotEnough: u64 = 2;

    /// Sender is not @0x0 the system address.
    const ENotSystemAddress: u64 = 3;

    /// A Supply of T. Used for minting and burning.
    /// Wrapped into a `TreasuryCap` in the `Coin` module.
    struct Supply<phantom T> has store {
        value: u64
    }

    /// Storable balance - an inner struct of a Coin type.
    /// Can be used to store coins which don't need the key ability.
    struct Balance<phantom T> has store {
        value: u64
    }

    /// Get the amount stored in a `Balance`.
    public fun value<T>(self: &Balance<T>): u64 {
        self.value
    }

    /// Get the `Supply` value.
    public fun supply_value<T>(supply: &Supply<T>): u64 {
        supply.value
    }

    /// Create a new supply for type T.
    public fun create_supply<T: drop>(_: T): Supply<T> {
        Supply { value: 0 }
    }

    /// Increase supply by `value` and create a new `Balance<T>` with this value.
    public fun increase_supply<T>(self: &mut Supply<T>, value: u64): Balance<T> {
        assert!(value < (18446744073709551615u64 - self.value), EOverflow);
        self.value = self.value + value;
        Balance { value }
    }

    /// Burn a Balance<T> and decrease Supply<T>.
    public fun decrease_supply<T>(self: &mut Supply<T>, balance: Balance<T>): u64 {
        let Balance { value } = balance;
        assert!(self.value >= value, EOverflow);
        self.value = self.value - value;
        value
    }

    /// Create a zero `Balance` for type `T`.
    public fun zero<T>(): Balance<T> {
        Balance { value: 0 }
    }

    spec zero {
        aborts_if false;
        ensures result.value == 0;
    }

    /// Join two balances together.
    public fun join<T>(self: &mut Balance<T>, balance: Balance<T>): u64 {
        let Balance { value } = balance;
        self.value = self.value + value;
        self.value
    }

    spec join {
        ensures self.value == old(self.value) + balance.value;
        ensures result == self.value;
    }

    /// Split a `Balance` and take a sub balance from it.
    public fun split<T>(self: &mut Balance<T>, value: u64): Balance<T> {
        assert!(self.value >= value, ENotEnough);
        self.value = self.value - value;
        Balance { value }
    }

    spec split {
        aborts_if self.value < value with ENotEnough;
        ensures self.value == old(self.value) - value;
        ensures result.value == value;
    }

    /// Withdraw all balance. After this the remaining balance must be 0.
    public fun withdraw_all<T>(self: &mut Balance<T>): Balance<T> {
        let value = self.value;
        split(self, value)
    }

    spec withdraw_all {
        ensures self.value == 0;
    }

    /// Destroy a zero `Balance`.
    public fun destroy_zero<T>(balance: Balance<T>) {
        assert!(balance.value == 0, ENonZero);
        let Balance { value: _ } = balance;
    }

    spec destroy_zero {
        aborts_if balance.value != 0 with ENonZero;
    }

    #[allow(unused_function)]
    /// CAUTION: this function creates a `Balance` without increasing the supply.
    /// It should only be called by the epoch change system txn to create staking rewards,
    /// and nowhere else.
    fun create_staking_rewards<T>(value: u64, ctx: &TxContext): Balance<T> {
        assert!(tx_context::sender(ctx) == @0x0, ENotSystemAddress);
        Balance { value }
    }

    #[allow(unused_function)]
    /// CAUTION: this function destroys a `Balance` without decreasing the supply.
    /// It should only be called by the epoch change system txn to destroy storage rebates,
    /// and nowhere else.
    fun destroy_storage_rebates<T>(self: Balance<T>, ctx: &TxContext) {
        assert!(tx_context::sender(ctx) == @0x0, ENotSystemAddress);
        let Balance { value: _ } = self;
    }

    /// Destroy a `Supply` preventing any further minting and burning.
    public(friend) fun destroy_supply<T>(self: Supply<T>): u64 {
        let Supply { value } = self;
        value
    }

    #[test_only]
    /// Create a `Balance` of any coin for testing purposes.
    public fun create_for_testing<T>(value: u64): Balance<T> {
        Balance { value }
    }

    #[test_only]
    /// Destroy a `Balance` of any coin for testing purposes.
    public fun destroy_for_testing<T>(self: Balance<T>): u64 {
        let Balance { value } = self;
        value
    }

    #[test_only]
    /// Create a `Supply` of any coin for testing purposes.
    public fun create_supply_for_testing<T>(): Supply<T> {
        Supply { value: 0 }
    }
}

#[test_only]
module sui::balance_tests {
    use sui::balance;
    use sui::sui::SUI;
    use sui::test_utils;

    #[test]
    fun test_balance() {
        let balance = balance::zero<SUI>();
        let another = balance::create_for_testing(1000);

        balance::join(&mut balance, another);

        assert!(balance::value(&balance) == 1000, 0);

        let balance1 = balance::split(&mut balance, 333);
        let balance2 = balance::split(&mut balance, 333);
        let balance3 = balance::split(&mut balance, 334);

        balance::destroy_zero(balance);

        assert!(balance::value(&balance1) == 333, 1);
        assert!(balance::value(&balance2) == 333, 2);
        assert!(balance::value(&balance3) == 334, 3);

        test_utils::destroy(balance1);
        test_utils::destroy(balance2);
        test_utils::destroy(balance3);
    }
}
