// Copyright (c) Mysten Labs, Inc.
// SPDX-License-Identifier: Apache-2.0

/// Sui types helpers and utilities
module sui::types {
    // === one-time witness ===

    /// Tests if the argument type is a one-time witness, that is a type with only one instantiation
    /// across the entire code base.
    public native fun is_one_time_witness<T: drop>(_: &T): bool;

    spec is_one_time_witness {
        pragma opaque;
        // TODO: stub to be replaced by actual abort conditions if any
        aborts_if [abstract] true;
        // TODO: specify actual function behavior
    }
}
