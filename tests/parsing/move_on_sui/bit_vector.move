// Copyright (c) Mysten Labs, Inc.
// SPDX-License-Identifier: Apache-2.0

module std::bit_vector {
    use std::vector;

    /// The provided index is out of bounds
    const EINDEX: u64 = 0x20000;
    /// An invalid length of bitvector was given
    const ELENGTH: u64 = 0x20001;

    const WORD_SIZE: u64 = 1;
    /// The maximum allowed bitvector size
    const MAX_SIZE: u64 = 1024;

    struct BitVector has copy, drop, store {
        length: u64,
        bit_field: vector<bool>,
    }

    public fun new(length: u64): BitVector {
        assert!(length > 0, ELENGTH);
        assert!(length < MAX_SIZE, ELENGTH);
        let counter = 0;
        let bit_field = vector::empty();
        while ({spec {
            invariant counter <= length;
            invariant len(bit_field) == counter;
        };
            (counter < length)}) {
            vector::push_back(&mut bit_field, false);
            counter = counter + 1;
        };
        spec {
            assert counter == length;
            assert len(bit_field) == length;
        };

        BitVector {
            length,
            bit_field,
        }
    }
    spec new {
        include NewAbortsIf;
        ensures result.length == length;
        ensures len(result.bit_field) == length;
    }
    spec schema NewAbortsIf {
        length: u64;
        aborts_if length <= 0 with ELENGTH;
        aborts_if length >= MAX_SIZE with ELENGTH;
    }

    /// Set the bit at `bit_index` in the `bitvector` regardless of its previous state.
    public fun set(bitvector: &mut BitVector, bit_index: u64) {
        assert!(bit_index < vector::length(&bitvector.bit_field), EINDEX);
        let x = vector::borrow_mut(&mut bitvector.bit_field, bit_index);
        *x = true;
    }
    spec set {
        include SetAbortsIf;
        ensures bitvector.bit_field[bit_index];
    }
    spec schema SetAbortsIf {
        bitvector: BitVector;
        bit_index: u64;
        aborts_if bit_index >= length(bitvector) with EINDEX;
    }

    /// Unset the bit at `bit_index` in the `bitvector` regardless of its previous state.
    public fun unset(bitvector: &mut BitVector, bit_index: u64) {
        assert!(bit_index < vector::length(&bitvector.bit_field), EINDEX);
        let x = vector::borrow_mut(&mut bitvector.bit_field, bit_index);
        *x = false;
    }
    spec set {
        include UnsetAbortsIf;
        ensures bitvector.bit_field[bit_index];
    }
    spec schema UnsetAbortsIf {
        bitvector: BitVector;
        bit_index: u64;
        aborts_if bit_index >= length(bitvector) with EINDEX;
    }

    /// Shift the `bitvector` left by `amount`. If `amount` is greater than the
    /// bitvector's length the bitvector will be zeroed out.
    public fun shift_left(bitvector: &mut BitVector, amount: u64) {
        if (amount >= bitvector.length) {
           let len = vector::length(&bitvector.bit_field);
           let i = 0;
           while (i < len) {
               let elem = vector::borrow_mut(&mut bitvector.bit_field, i);
               *elem = false;
               i = i + 1;
           };
        } else {
            let i = amount;

            while (i < bitvector.length) {
                if (is_index_set(bitvector, i)) set(bitvector, i - amount)
                else unset(bitvector, i - amount);
                i = i + 1;
            };

            i = bitvector.length - amount;

            while (i < bitvector.length) {
                unset(bitvector, i);
                i = i + 1;
            };
        }
    }

    /// Return the value of the bit at `bit_index` in the `bitvector`. `true`
    /// represents "1" and `false` represents a 0
    public fun is_index_set(bitvector: &BitVector, bit_index: u64): bool {
        assert!(bit_index < vector::length(&bitvector.bit_field), EINDEX);
        *vector::borrow(&bitvector.bit_field, bit_index)
    }
    spec is_index_set {
        include IsIndexSetAbortsIf;
        ensures result == bitvector.bit_field[bit_index];
    }
    spec schema IsIndexSetAbortsIf {
        bitvector: BitVector;
        bit_index: u64;
        aborts_if bit_index >= length(bitvector) with EINDEX;
    }
    spec fun spec_is_index_set(bitvector: BitVector, bit_index: u64): bool {
        if (bit_index >= length(bitvector)) {
            false
        } else {
            bitvector.bit_field[bit_index]
        }
    }

    /// Return the length (number of usable bits) of this bitvector
    public fun length(bitvector: &BitVector): u64 {
        vector::length(&bitvector.bit_field)
    }

    /// Returns the length of the longest sequence of set bits starting at (and
    /// including) `start_index` in the `bitvector`. If there is no such
    /// sequence, then `0` is returned.
    public fun longest_set_sequence_starting_at(bitvector: &BitVector, start_index: u64): u64 {
        assert!(start_index < bitvector.length, EINDEX);
        let index = start_index;

        // Find the greatest index in the vector such that all indices less than it are set.
        while (index < bitvector.length) {
            if (!is_index_set(bitvector, index)) break;
            index = index + 1;
        };

        index - start_index
    }

    #[test_only]
    public fun word_size(): u64 {
        WORD_SIZE
    }
}
