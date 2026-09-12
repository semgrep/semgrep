// Copyright (c) Mysten Labs, Inc.
// SPDX-License-Identifier: Apache-2.0

/// This module implements BCS (de)serialization in Move.
/// Full specification can be found here: https://github.com/diem/bcs
///
/// Short summary (for Move-supported types):
///
/// - address - sequence of X bytes
/// - bool - byte with 0 or 1
/// - u8 - a single u8 byte
/// - u64 / u128 - LE bytes
/// - vector - ULEB128 length + LEN elements
/// - option - first byte bool: None (0) or Some (1), then value
///
/// Usage example:
/// ```
/// /// This function reads u8 and u64 value from the input
/// /// and returns the rest of the bytes.
/// fun deserialize(bytes: vector<u8>): (u8, u64, vector<u8>) {
///     use sui::bcs::{Self, BCS};
///
///     let prepared: BCS = bcs::new(bytes);
///     let (u8_value, u64_value) = (
///         bcs::peel_u8(&mut prepared),
///         bcs::peel_u64(&mut prepared)
///     );
///
///     // unpack bcs struct
///     let leftovers = bcs::into_remainder_bytes(prepared);
///
///     (u8_value, u64_value, leftovers)
/// }
/// ```
module sui::bcs {
    use std::option::{Self, Option};
    use std::vector as v;
    use sui::address;
    use std::bcs;

    /// For when bytes length is less than required for deserialization.
    const EOutOfRange: u64 = 0;

    /// For when the boolean value different than `0` or `1`.
    const ENotBool: u64 = 1;

    /// For when ULEB byte is out of range (or not found).
    const ELenOutOfRange: u64 = 2;

    /// A helper struct that saves resources on operations. For better
    /// vector performance, it stores reversed bytes of the BCS and
    /// enables use of `vector::pop_back`.
    struct BCS has store, copy, drop {
        bytes: vector<u8>
    }

    /// Get BCS serialized bytes for any value.
    /// Re-exports stdlib `bcs::to_bytes`.
    public fun to_bytes<T>(value: &T): vector<u8> {
        bcs::to_bytes(value)
    }

    /// Creates a new instance of BCS wrapper that holds inversed
    /// bytes for better performance.
    public fun new(bytes: vector<u8>): BCS {
        v::reverse(&mut bytes);
        BCS { bytes }
    }

    /// Unpack the `BCS` struct returning the leftover bytes.
    /// Useful for passing the data further after partial deserialization.
    public fun into_remainder_bytes(bcs: BCS): vector<u8> {
        let BCS { bytes } = bcs;
        v::reverse(&mut bytes);
        bytes
    }

    /// Read address from the bcs-serialized bytes.
    public fun peel_address(bcs: &mut BCS): address {
        assert!(v::length(&bcs.bytes) >= address::length(), EOutOfRange);
        let (addr_bytes, i) = (v::empty(), 0);
        while (i < address::length()) {
            v::push_back(&mut addr_bytes, v::pop_back(&mut bcs.bytes));
            i = i + 1;
        };
        address::from_bytes(addr_bytes)
    }

    /// Read a `bool` value from bcs-serialized bytes.
    public fun peel_bool(bcs: &mut BCS): bool {
        let value = peel_u8(bcs);
        if (value == 0) {
            false
        } else if (value == 1) {
            true
        } else {
            abort ENotBool
        }
    }

    /// Read `u8` value from bcs-serialized bytes.
    public fun peel_u8(bcs: &mut BCS): u8 {
        assert!(v::length(&bcs.bytes) >= 1, EOutOfRange);
        v::pop_back(&mut bcs.bytes)
    }

    /// Read `u64` value from bcs-serialized bytes.
    public fun peel_u64(bcs: &mut BCS): u64 {
        assert!(v::length(&bcs.bytes) >= 8, EOutOfRange);

        let (value, i) = (0u64, 0u8);
        while (i < 64) {
            let byte = (v::pop_back(&mut bcs.bytes) as u64);
            value = value + (byte << i);
            i = i + 8;
        };

        value
    }

    /// Read `u128` value from bcs-serialized bytes.
    public fun peel_u128(bcs: &mut BCS): u128 {
        assert!(v::length(&bcs.bytes) >= 16, EOutOfRange);

        let (value, i) = (0u128, 0u8);
        while (i < 128) {
            let byte = (v::pop_back(&mut bcs.bytes) as u128);
            value = value + (byte << i);
            i = i + 8;
        };

        value
    }

    // === Vector<T> ===

    /// Read ULEB bytes expecting a vector length. Result should
    /// then be used to perform `peel_*` operation LEN times.
    ///
    /// In BCS `vector` length is implemented with ULEB128;
    /// See more here: https://en.wikipedia.org/wiki/LEB128
    public fun peel_vec_length(bcs: &mut BCS): u64 {
        let (total, shift, len) = (0u64, 0, 0);
        while (true) {
            assert!(len <= 4, ELenOutOfRange);
            let byte = (v::pop_back(&mut bcs.bytes) as u64);
            len = len + 1;
            total = total | ((byte & 0x7f) << shift);
            if ((byte & 0x80) == 0) {
                break
            };
            shift = shift + 7;
        };
        total
    }

    /// Peel a vector of `address` from serialized bytes.
    public fun peel_vec_address(bcs: &mut BCS): vector<address> {
        let (len, i, res) = (peel_vec_length(bcs), 0, vector[]);
        while (i < len) {
            v::push_back(&mut res, peel_address(bcs));
            i = i + 1;
        };
        res
    }

    /// Peel a vector of `address` from serialized bytes.
    public fun peel_vec_bool(bcs: &mut BCS): vector<bool> {
        let (len, i, res) = (peel_vec_length(bcs), 0, vector[]);
        while (i < len) {
            v::push_back(&mut res, peel_bool(bcs));
            i = i + 1;
        };
        res
    }

    /// Peel a vector of `u8` (eg string) from serialized bytes.
    public fun peel_vec_u8(bcs: &mut BCS): vector<u8> {
        let (len, i, res) = (peel_vec_length(bcs), 0, vector[]);
        while (i < len) {
            v::push_back(&mut res, peel_u8(bcs));
            i = i + 1;
        };
        res
    }

    /// Peel a `vector<vector<u8>>` (eg vec of string) from serialized bytes.
    public fun peel_vec_vec_u8(bcs: &mut BCS): vector<vector<u8>> {
        let (len, i, res) = (peel_vec_length(bcs), 0, vector[]);
        while (i < len) {
            v::push_back(&mut res, peel_vec_u8(bcs));
            i = i + 1;
        };
        res
    }

    /// Peel a vector of `u64` from serialized bytes.
    public fun peel_vec_u64(bcs: &mut BCS): vector<u64> {
        let (len, i, res) = (peel_vec_length(bcs), 0, vector[]);
        while (i < len) {
            v::push_back(&mut res, peel_u64(bcs));
            i = i + 1;
        };
        res
    }

    /// Peel a vector of `u128` from serialized bytes.
    public fun peel_vec_u128(bcs: &mut BCS): vector<u128> {
        let (len, i, res) = (peel_vec_length(bcs), 0, vector[]);
        while (i < len) {
            v::push_back(&mut res, peel_u128(bcs));
            i = i + 1;
        };
        res
    }

    // === Option<T> ===

    /// Peel `Option<address>` from serialized bytes.
    public fun peel_option_address(bcs: &mut BCS): Option<address> {
        if (peel_bool(bcs)) {
            option::some(peel_address(bcs))
        } else {
            option::none()
        }
    }

    /// Peel `Option<bool>` from serialized bytes.
    public fun peel_option_bool(bcs: &mut BCS): Option<bool> {
        if (peel_bool(bcs)) {
            option::some(peel_bool(bcs))
        } else {
            option::none()
        }
    }

    /// Peel `Option<u8>` from serialized bytes.
    public fun peel_option_u8(bcs: &mut BCS): Option<u8> {
        if (peel_bool(bcs)) {
            option::some(peel_u8(bcs))
        } else {
            option::none()
        }
    }

    /// Peel `Option<u64>` from serialized bytes.
    public fun peel_option_u64(bcs: &mut BCS): Option<u64> {
        if (peel_bool(bcs)) {
            option::some(peel_u64(bcs))
        } else {
            option::none()
        }
    }

    /// Peel `Option<u128>` from serialized bytes.
    public fun peel_option_u128(bcs: &mut BCS): Option<u128> {
        if (peel_bool(bcs)) {
            option::some(peel_u128(bcs))
        } else {
            option::none()
        }
    }

    // TODO: re-enable once bit-wise operators in peel_vec_length are supported in the prover
    spec module { pragma verify = false; }

    // === Tests ===

    #[test_only]
    struct Info has drop { a: bool, b: u8, c: u64, d: u128, k: vector<bool>, s: address }

    #[test]
    #[expected_failure(abort_code = ELenOutOfRange)]
    fun test_uleb_len_fail() {
        let value = vector[0xff, 0xff, 0xff, 0xff, 0xff];
        let bytes = new(to_bytes(&value));
        let _fail = peel_vec_length(&mut bytes);
        abort 2 // TODO: make this test fail
    }

    #[test]
    #[expected_failure(abort_code = ENotBool)]
    fun test_bool_fail() {
        let bytes = new(to_bytes(&10u8));
        let _fail = peel_bool(&mut bytes);
    }

    #[test]
    fun test_option() {
        {
            let value = option::some(true);
            let bytes = new(to_bytes(&value));
            assert!(value == peel_option_bool(&mut bytes), 0);
        };

        {
            let value = option::some(10u8);
            let bytes = new(to_bytes(&value));
            assert!(value == peel_option_u8(&mut bytes), 0);
        };

        {
            let value = option::some(10000u64);
            let bytes = new(to_bytes(&value));
            assert!(value == peel_option_u64(&mut bytes), 0);
        };

        {
            let value = option::some(10000999999u128);
            let bytes = new(to_bytes(&value));
            assert!(value == peel_option_u128(&mut bytes), 0);
        };

        {
            let value = option::some(@0xC0FFEE);
            let bytes = new(to_bytes(&value));
            assert!(value == peel_option_address(&mut bytes), 0);
        };

        {
            let value: Option<bool> = option::none();
            let bytes = new(to_bytes(&value));
            assert!(value == peel_option_bool(&mut bytes), 0);
        };
    }

    #[test]
    fun test_bcs() {
        {
            let value = @0xC0FFEE;
            let bytes = new(to_bytes(&value));
            assert!(value == peel_address(&mut bytes), 0);
        };

        { // boolean: true
            let value = true;
            let bytes = new(to_bytes(&value));
            assert!(value == peel_bool(&mut bytes), 0);
        };

        { // boolean: false
            let value = false;
            let bytes = new(to_bytes(&value));
            assert!(value == peel_bool(&mut bytes), 0);
        };

        { // u8
            let value = 100u8;
            let bytes = new(to_bytes(&value));
            assert!(value == peel_u8(&mut bytes), 0);
        };

        { // u64 (4 bytes)
            let value = 1000100u64;
            let bytes = new(to_bytes(&value));
            assert!(value == peel_u64(&mut bytes), 0);
        };

        { // u64 (8 bytes)
            let value = 100000000000000u64;
            let bytes = new(to_bytes(&value));
            assert!(value == peel_u64(&mut bytes), 0);
        };

        { // u128 (16 bytes)
            let value = 100000000000000000000000000u128;
            let bytes = new(to_bytes(&value));
            assert!(value == peel_u128(&mut bytes), 0);
        };

        { // vector length
            let value = vector[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0];
            let bytes = new(to_bytes(&value));
            assert!(v::length(&value) == peel_vec_length(&mut bytes), 0);
        };

        { // vector length (more data)
            let value = vector[
                0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            ];

            let bytes = new(to_bytes(&value));
            assert!(v::length(&value) == peel_vec_length(&mut bytes), 0);
        };

        { // full deserialization test (ordering)
            let info = Info { a: true, b: 100, c: 9999, d: 112333, k: vector[true, false, true, false], s: @0xAAAAAAAAAAA };
            let bytes = new(to_bytes(&info));

            assert!(info.a == peel_bool(&mut bytes), 0);
            assert!(info.b == peel_u8(&mut bytes), 0);
            assert!(info.c == peel_u64(&mut bytes), 0);
            assert!(info.d == peel_u128(&mut bytes), 0);

            let len = peel_vec_length(&mut bytes);

            assert!(v::length(&info.k) == len, 0);

            let i = 0;
            while (i < v::length(&info.k)) {
                assert!(*v::borrow(&info.k, i) == peel_bool(&mut bytes), 0);
                i = i + 1;
            };

            assert!(info.s == peel_address(&mut bytes), 0);
        };

        { // read vector of bytes directly
            let value = vector[
                vector[1,2,3,4,5],
                vector[1,2,3,4,5],
                vector[1,2,3,4,5]
            ];
            let bytes = new(to_bytes(&value));
            assert!(value == peel_vec_vec_u8(&mut bytes), 0);
        };

        { // read vector of bytes directly
            let value = vector[1,2,3,4,5];
            let bytes = new(to_bytes(&value));
            assert!(value == peel_vec_u8(&mut bytes), 0);
        };

        { // read vector of bytes directly
            let value = vector[1,2,3,4,5];
            let bytes = new(to_bytes(&value));
            assert!(value == peel_vec_u64(&mut bytes), 0);
        };

        { // read vector of bytes directly
            let value = vector[1,2,3,4,5];
            let bytes = new(to_bytes(&value));
            assert!(value == peel_vec_u128(&mut bytes), 0);
        };

        { // read vector of bytes directly
            let value = vector[true, false, true, false];
            let bytes = new(to_bytes(&value));
            assert!(value == peel_vec_bool(&mut bytes), 0);
        };

        { // read vector of address directly
            let value = vector[@0x0, @0x1, @0x2, @0x3];
            let bytes = new(to_bytes(&value));
            assert!(value == peel_vec_address(&mut bytes), 0);
        };
    }
}
