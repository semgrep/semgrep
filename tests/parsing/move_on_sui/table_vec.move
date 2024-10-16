// Copyright (c) Mysten Labs, Inc.
// SPDX-License-Identifier: Apache-2.0

/// A basic scalable vector library implemented using `Table`.
module sui::table_vec {
    use sui::table::{Self, Table};
    use sui::tx_context::TxContext;

    struct TableVec<phantom Element: store> has store {
        /// The contents of the table vector.
        contents: Table<u64, Element>,
    }

    const EIndexOutOfBound: u64 = 0;
    const ETableNonEmpty: u64 = 1;

    /// Create an empty TableVec.
    public fun empty<Element: store>(ctx: &mut TxContext): TableVec<Element> {
        TableVec {
            contents: table::new(ctx)
        }
    }

    /// Return a TableVec of size one containing element `e`.
    public fun singleton<Element: store>(e: Element, ctx: &mut TxContext): TableVec<Element> {
        let mut t = empty(ctx);
        push_back(&mut t, e);
        t
    }

    /// Return the length of the TableVec.
    public fun length<Element: store>(t: &TableVec<Element>): u64 {
        table::length(&t.contents)
    }

    /// Return if the TableVec is empty or not.
    public fun is_empty<Element: store>(t: &TableVec<Element>): bool {
        length(t) == 0
    }

    /// Acquire an immutable reference to the `i`th element of the TableVec `t`.
    /// Aborts if `i` is out of bounds.
    public fun borrow<Element: store>(t: &TableVec<Element>, i: u64): &Element {
        assert!(length(t) > i, EIndexOutOfBound);
        table::borrow(&t.contents, i)
    }

    /// Add element `e` to the end of the TableVec `t`.
    public fun push_back<Element: store>(t: &mut TableVec<Element>, e: Element) {
        let key = length(t);
        table::add(&mut t.contents, key, e);
    }

    /// Return a mutable reference to the `i`th element in the TableVec `t`.
    /// Aborts if `i` is out of bounds.
    public fun borrow_mut<Element: store>(t: &mut TableVec<Element>, i: u64): &mut Element {
        assert!(length(t) > i, EIndexOutOfBound);
        table::borrow_mut(&mut t.contents, i)
    }

    /// Pop an element from the end of TableVec `t`.
    /// Aborts if `t` is empty.
    public fun pop_back<Element: store>(t: &mut TableVec<Element>): Element {
        let length = length(t);
        assert!(length > 0, EIndexOutOfBound);
        table::remove(&mut t.contents, length - 1)
    }

    /// Destroy the TableVec `t`.
    /// Aborts if `t` is not empty.
    public fun destroy_empty<Element: store>(t: TableVec<Element>) {
        assert!(length(&t) == 0, ETableNonEmpty);
        let TableVec { contents } = t;
        table::destroy_empty(contents);
    }

    /// Drop a possibly non-empty TableVec `t`.
    /// Usable only if the value type `Element` has the `drop` ability
    public fun drop<Element: drop + store>(t: TableVec<Element>) {
        let TableVec { contents } = t;
        table::drop(contents)
    }

    /// Swaps the elements at the `i`th and `j`th indices in the TableVec `t`.
    /// Aborts if `i` or `j` is out of bounds.
    public fun swap<Element: store>(t: &mut TableVec<Element>, i: u64, j: u64) {
        assert!(length(t) > i, EIndexOutOfBound);
        assert!(length(t) > j, EIndexOutOfBound);
        if (i == j) { return };
        let element_i = table::remove(&mut t.contents, i);
        let element_j = table::remove(&mut t.contents, j);
        table::add(&mut t.contents, j, element_i);
        table::add(&mut t.contents, i, element_j);
    }

    /// Swap the `i`th element of the TableVec `t` with the last element and then pop the TableVec.
    /// This is O(1), but does not preserve ordering of elements in the TableVec.
    /// Aborts if `i` is out of bounds.
    public fun swap_remove<Element: store>(t: &mut TableVec<Element>, i: u64): Element {
        assert!(length(t) > i, EIndexOutOfBound);
        let last_idx = length(t) - 1;
        swap(t, i, last_idx);
        pop_back(t)
    }
}
