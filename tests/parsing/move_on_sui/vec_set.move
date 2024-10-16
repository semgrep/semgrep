// Copyright (c) Mysten Labs, Inc.
// SPDX-License-Identifier: Apache-2.0

module sui::vec_set {
    use std::option::{Self, Option};
    use std::vector;

    /// This key already exists in the map
    const EKeyAlreadyExists: u64 = 0;

    /// This key does not exist in the map
    const EKeyDoesNotExist: u64 = 1;

    /// A set data structure backed by a vector. The set is guaranteed not to
    /// contain duplicate keys. All operations are O(N) in the size of the set
    /// - the intention of this data structure is only to provide the convenience
    /// of programming against a set API. Sets that need sorted iteration rather
    /// than insertion order iteration should be handwritten.
    struct VecSet<K: copy + drop> has copy, drop, store {
        contents: vector<K>,
    }

    /// Create an empty `VecSet`
    public fun empty<K: copy + drop>(): VecSet<K> {
        VecSet { contents: vector::empty() }
    }

    /// Create a singleton `VecSet` that only contains one element.
    public fun singleton<K: copy + drop>(key: K): VecSet<K> {
        VecSet { contents: vector::singleton(key) }
    }

    /// Insert a `key` into self.
    /// Aborts if `key` is already present in `self`.
    public fun insert<K: copy + drop>(self: &mut VecSet<K>, key: K) {
        assert!(!contains(self, &key), EKeyAlreadyExists);
        vector::push_back(&mut self.contents, key)
    }

    /// Remove the entry `key` from self. Aborts if `key` is not present in `self`.
    public fun remove<K: copy + drop>(self: &mut VecSet<K>, key: &K) {
        let idx = get_idx(self, key);
        vector::remove(&mut self.contents, idx);
    }

    /// Return true if `self` contains an entry for `key`, false otherwise
    public fun contains<K: copy + drop>(self: &VecSet<K>, key: &K): bool {
        option::is_some(&get_idx_opt(self, key))
    }

    /// Return the number of entries in `self`
    public fun size<K: copy + drop>(self: &VecSet<K>): u64 {
        vector::length(&self.contents)
    }

    /// Return true if `self` has 0 elements, false otherwise
    public fun is_empty<K: copy + drop>(self: &VecSet<K>): bool {
        size(self) == 0
    }

    /// Unpack `self` into vectors of keys.
    /// The output keys are stored in insertion order, *not* sorted.
    public fun into_keys<K: copy + drop>(self: VecSet<K>): vector<K> {
        let VecSet { contents } = self;
        contents
    }

    /// Borrow the `contents` of the `VecSet` to access content by index
    /// without unpacking. The contents are stored in insertion order,
    /// *not* sorted.
    public fun keys<K: copy + drop>(self: &VecSet<K>): &vector<K> {
        &self.contents
    }

    // == Helper functions ==

    /// Find the index of `key` in `self`. Return `None` if `key` is not in `self`.
    /// Note that keys are stored in insertion order, *not* sorted.
    fun get_idx_opt<K: copy + drop>(self: &VecSet<K>, key: &K): Option<u64> {
        let i = 0;
        let n = size(self);
        while (i < n) {
            if (vector::borrow(&self.contents, i) == key) {
                return option::some(i)
            };
            i = i + 1;
        };
        option::none()
    }

    /// Find the index of `key` in `self`. Aborts if `key` is not in `self`.
    /// Note that map entries are stored in insertion order, *not* sorted.
    fun get_idx<K: copy + drop>(self: &VecSet<K>, key: &K): u64 {
        let idx_opt = get_idx_opt(self, key);
        assert!(option::is_some(&idx_opt), EKeyDoesNotExist);
        option::destroy_some(idx_opt)
    }
}
