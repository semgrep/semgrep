// Copyright (c) Mysten Labs, Inc.
// SPDX-License-Identifier: Apache-2.0

/// Functionality for converting Move types into values. Use with care!
module std::type_name {
    use std::ascii::{Self, String};
    use std::address;
    use std::vector;

    /// ASCII Character code for the `:` (colon) symbol.
    const ASCII_COLON: u8 = 58;

    struct TypeName has copy, drop, store {
        /// String representation of the type. All types are represented
        /// using their source syntax:
        /// "u8", "u64", "u128", "bool", "address", "vector", "signer" for ground types.
        /// Struct types are represented as fully qualified type names; e.g.
        /// `00000000000000000000000000000001::string::String` or
        /// `0000000000000000000000000000000a::module_name1::type_name1<0000000000000000000000000000000a::module_name2::type_name2<u64>>`
        /// Addresses are hex-encoded lowercase values of length ADDRESS_LENGTH (16, 20, or 32 depending on the Move platform)
        name: String
    }

    /// Return a value representation of the type `T`.  Package IDs
    /// that appear in fully qualified type names in the output from
    /// this function are defining IDs (the ID of the package in
    /// storage that first introduced the type).
    public native fun get<T>(): TypeName;
    spec get {
        pragma opaque;
    }

    /// Return a value representation of the type `T`.  Package IDs
    /// that appear in fully qualified type names in the output from
    /// this function are original IDs (the ID of the first version of
    /// the package, even if the type in question was introduced in a
    /// later upgrade).
    public native fun get_with_original_ids<T>(): TypeName;
    spec get_with_original_ids {
        pragma opaque;
    }

    /// Get the String representation of `self`
    public fun borrow_string(self: &TypeName): &String {
        &self.name
    }

    /// Get Address string (Base16 encoded), first part of the TypeName.
    public fun get_address(self: &TypeName): String {
        // Base16 (string) representation of an address has 2 symbols per byte.
        let len = address::length() * 2;
        let str_bytes = ascii::as_bytes(&self.name);
        let addr_bytes = vector[];
        let i = 0;

        // Read `len` bytes from the type name and push them to addr_bytes.
        while (i < len) {
            vector::push_back(
                &mut addr_bytes,
                *vector::borrow(str_bytes, i)
            );
            i = i + 1;
        };

        ascii::string(addr_bytes)
    }

    /// Get name of the module.
    public fun get_module(self: &TypeName): String {
        // Starts after address and a double colon: `<addr as HEX>::`
        let i = address::length() * 2 + 2;
        let str_bytes = ascii::as_bytes(&self.name);
        let module_name = vector[];

        loop {
            let char = vector::borrow(str_bytes, i);
            if (char != &ASCII_COLON) {
                vector::push_back(&mut module_name, *char);
                i = i + 1;
            } else {
                break
            }
        };

        ascii::string(module_name)
    }

    /// Convert `self` into its inner String
    public fun into_string(self: TypeName): String {
        self.name
    }
}
