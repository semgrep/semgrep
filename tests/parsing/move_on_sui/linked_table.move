// Copyright (c) Mysten Labs, Inc.
// SPDX-License-Identifier: Apache-2.0

/// Similar to `sui::table` but the values are linked together, allowing for ordered insertion and
/// removal
module sui::linked_table {
    use std::option::{Self, Option};
    use sui::object::{Self, UID};
    use sui::dynamic_field as field;
    use sui::tx_context::TxContext;

    // Attempted to destroy a non-empty table
    const ETableNotEmpty: u64 = 0;
    // Attempted to remove the front or back of an empty table
    const ETableIsEmpty: u64 = 1;

    struct LinkedTable<K: copy + drop + store, phantom V: store> has key, store {
        /// the ID of this table
        id: UID,
        /// the number of key-value pairs in the table
        size: u64,
        /// the front of the table, i.e. the key of the first entry
        head: Option<K>,
        /// the back of the table, i.e. the key of the last entry
        tail: Option<K>,
    }

    struct Node<K: copy + drop + store, V: store> has store {
        /// the previous key
        prev: Option<K>,
        /// the next key
        next: Option<K>,
        /// the value being stored
        value: V
    }

    /// Creates a new, empty table
    public fun new<K: copy + drop + store, V: store>(ctx: &mut TxContext): LinkedTable<K, V> {
        LinkedTable {
            id: object::new(ctx),
            size: 0,
            head: option::none(),
            tail: option::none(),
        }
    }

    /// Returns the key for the first element in the table, or None if the table is empty
    public fun front<K: copy + drop + store, V: store>(table: &LinkedTable<K, V>): &Option<K> {
        &table.head
    }

    /// Returns the key for the last element in the table, or None if the table is empty
    public fun back<K: copy + drop + store, V: store>(table: &LinkedTable<K, V>): &Option<K> {
        &table.tail
    }

    /// Inserts a key-value pair at the front of the table, i.e. the newly inserted pair will be
    /// the first element in the table
    /// Aborts with `sui::dynamic_field::EFieldAlreadyExists` if the table already has an entry with
    /// that key `k: K`.
    public fun push_front<K: copy + drop + store, V: store>(
        table: &mut LinkedTable<K, V>,
        k: K,
        value: V,
    ) {
        let old_head = option::swap_or_fill(&mut table.head, k);
        if (option::is_none(&table.tail)) option::fill(&mut table.tail, k);
        let prev = option::none();
        let next = if (option::is_some(&old_head)) {
            let old_head_k = option::destroy_some(old_head);
            field::borrow_mut<K, Node<K, V>>(&mut table.id, old_head_k).prev = option::some(k);
            option::some(old_head_k)
        } else {
            option::none()
        };
        field::add(&mut table.id, k, Node { prev, next, value });
        table.size = table.size + 1;
    }

    /// Inserts a key-value pair at the back of the table, i.e. the newly inserted pair will be
    /// the last element in the table
    /// Aborts with `sui::dynamic_field::EFieldAlreadyExists` if the table already has an entry with
    /// that key `k: K`.
    public fun push_back<K: copy + drop + store, V: store>(
        table: &mut LinkedTable<K, V>,
        k: K,
        value: V,
    ) {
        if (option::is_none(&table.head)) option::fill(&mut table.head, k);
        let old_tail = option::swap_or_fill(&mut table.tail, k);
        let prev = if (option::is_some(&old_tail)) {
            let old_tail_k = option::destroy_some(old_tail);
            field::borrow_mut<K, Node<K, V>>(&mut table.id, old_tail_k).next = option::some(k);
            option::some(old_tail_k)
        } else {
            option::none()
        };
        let next = option::none();
        field::add(&mut table.id, k, Node { prev, next, value });
        table.size = table.size + 1;
    }

    /// Immutable borrows the value associated with the key in the table `table: &LinkedTable<K, V>`.
    /// Aborts with `sui::dynamic_field::EFieldDoesNotExist` if the table does not have an entry with
    /// that key `k: K`.
    public fun borrow<K: copy + drop + store, V: store>(table: &LinkedTable<K, V>, k: K): &V {
        &field::borrow<K, Node<K, V>>(&table.id, k).value
    }

    /// Mutably borrows the value associated with the key in the table `table: &mut LinkedTable<K, V>`.
    /// Aborts with `sui::dynamic_field::EFieldDoesNotExist` if the table does not have an entry with
    /// that key `k: K`.
    public fun borrow_mut<K: copy + drop + store, V: store>(
        table: &mut LinkedTable<K, V>,
        k: K,
    ): &mut V {
        &mut field::borrow_mut<K, Node<K, V>>(&mut table.id, k).value
    }

    /// Borrows the key for the previous entry of the specified key `k: K` in the table
    /// `table: &LinkedTable<K, V>`. Returns None if the entry does not have a predecessor.
    /// Aborts with `sui::dynamic_field::EFieldDoesNotExist` if the table does not have an entry with
    /// that key `k: K`
    public fun prev<K: copy + drop + store, V: store>(table: &LinkedTable<K, V>, k: K): &Option<K> {
        &field::borrow<K, Node<K, V>>(&table.id, k).prev
    }

    /// Borrows the key for the next entry of the specified key `k: K` in the table
    /// `table: &LinkedTable<K, V>`. Returns None if the entry does not have a predecessor.
    /// Aborts with `sui::dynamic_field::EFieldDoesNotExist` if the table does not have an entry with
    /// that key `k: K`
    public fun next<K: copy + drop + store, V: store>(table: &LinkedTable<K, V>, k: K): &Option<K> {
        &field::borrow<K, Node<K, V>>(&table.id, k).next
    }

    /// Removes the key-value pair in the table `table: &mut LinkedTable<K, V>` and returns the value.
    /// This splices the element out of the ordering.
    /// Aborts with `sui::dynamic_field::EFieldDoesNotExist` if the table does not have an entry with
    /// that key `k: K`. Note: this is also what happens when the table is empty.
    public fun remove<K: copy + drop + store, V: store>(table: &mut LinkedTable<K, V>, k: K): V {
        let Node<K, V> { prev, next, value } = field::remove(&mut table.id, k);
        table.size = table.size - 1;
        if (option::is_some(&prev)) {
            field::borrow_mut<K, Node<K, V>>(&mut table.id, *option::borrow(&prev)).next = next
        };
        if (option::is_some(&next)) {
            field::borrow_mut<K, Node<K, V>>(&mut table.id, *option::borrow(&next)).prev = prev
        };
        if (option::borrow(&table.head) == &k) table.head = next;
        if (option::borrow(&table.tail) == &k) table.tail = prev;
        value
    }

    /// Removes the front of the table `table: &mut LinkedTable<K, V>` and returns the value.
    /// Aborts with `ETableIsEmpty` if the table is empty
    public fun pop_front<K: copy + drop + store, V: store>(table: &mut LinkedTable<K, V>): (K, V) {
        assert!(option::is_some(&table.head), ETableIsEmpty);
        let head = *option::borrow(&table.head);
        (head, remove(table, head))
    }

    /// Removes the back of the table `table: &mut LinkedTable<K, V>` and returns the value.
    /// Aborts with `ETableIsEmpty` if the table is empty
    public fun pop_back<K: copy + drop + store, V: store>(table: &mut LinkedTable<K, V>): (K, V) {
        assert!(option::is_some(&table.tail), ETableIsEmpty);
        let tail = *option::borrow(&table.tail);
        (tail, remove(table, tail))
    }

    /// Returns true iff there is a value associated with the key `k: K` in table
    /// `table: &LinkedTable<K, V>`
    public fun contains<K: copy + drop + store, V: store>(table: &LinkedTable<K, V>, k: K): bool {
        field::exists_with_type<K, Node<K, V>>(&table.id, k)
    }

    /// Returns the size of the table, the number of key-value pairs
    public fun length<K: copy + drop + store, V: store>(table: &LinkedTable<K, V>): u64 {
        table.size
    }

    /// Returns true iff the table is empty (if `length` returns `0`)
    public fun is_empty<K: copy + drop + store, V: store>(table: &LinkedTable<K, V>): bool {
        table.size == 0
    }

    /// Destroys an empty table
    /// Aborts with `ETableNotEmpty` if the table still contains values
    public fun destroy_empty<K: copy + drop + store, V: store>(table: LinkedTable<K, V>) {
        let LinkedTable { id, size, head: _, tail: _ } = table;
        assert!(size == 0, ETableNotEmpty);
        object::delete(id)
    }

    /// Drop a possibly non-empty table.
    /// Usable only if the value type `V` has the `drop` ability
    public fun drop<K: copy + drop + store, V: drop + store>(table: LinkedTable<K, V>) {
        let LinkedTable { id, size: _, head: _, tail: _ } = table;
        object::delete(id)
    }
}
