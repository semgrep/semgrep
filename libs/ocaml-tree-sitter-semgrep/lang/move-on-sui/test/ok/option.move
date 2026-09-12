// Copyright (c) Mysten Labs, Inc.
// SPDX-License-Identifier: Apache-2.0

/// This module defines the Option type and its methods to represent and handle an optional value.
module std::option {
    use std::vector;

    /// Abstraction of a value that may or may not be present. Implemented with a vector of size
    /// zero or one because Move bytecode does not have ADTs.
    struct Option<Element> has copy, drop, store {
        vec: vector<Element>
    }
    spec Option {
        // The size of vector is always less than equal to 1
        // because it's 0 for "none" or 1 for "some".
        //  TODO: disable temporarily to avoid an error triggered after
        //  https://github.com/move-language/move/pull/820 was implemented
        //  (change the comment above to doc comment once this id fixed)
        //  invariant len(vec) <= 1;
    }

    /// The `Option` is in an invalid state for the operation attempted.
    /// The `Option` is `Some` while it should be `None`.
    const EOPTION_IS_SET: u64 = 0x40000;
    /// The `Option` is in an invalid state for the operation attempted.
    /// The `Option` is `None` while it should be `Some`.
    const EOPTION_NOT_SET: u64 = 0x40001;

    /// Return an empty `Option`
    public fun none<Element>(): Option<Element> {
        Option { vec: vector::empty() }
    }
    spec none {
        pragma opaque;
        aborts_if false;
        ensures result == spec_none<Element>();
    }
    spec fun spec_none<Element>(): Option<Element> {
        Option{ vec: vec() }
    }

    /// Return an `Option` containing `e`
    public fun some<Element>(e: Element): Option<Element> {
        Option { vec: vector::singleton(e) }
    }
    spec some {
        pragma opaque;
        aborts_if false;
        ensures result == spec_some(e);
    }
    spec fun spec_some<Element>(e: Element): Option<Element> {
        Option{ vec: vec(e) }
    }

    /// Return true if `t` does not hold a value
    public fun is_none<Element>(t: &Option<Element>): bool {
        vector::is_empty(&t.vec)
    }
    spec is_none {
        pragma opaque;
        aborts_if false;
        ensures result == is_none(t);
    }

    /// Return true if `t` holds a value
    public fun is_some<Element>(t: &Option<Element>): bool {
        !vector::is_empty(&t.vec)
    }
    spec is_some {
        pragma opaque;
        aborts_if false;
        ensures result == is_some(t);
    }

    /// Return true if the value in `t` is equal to `e_ref`
    /// Always returns `false` if `t` does not hold a value
    public fun contains<Element>(t: &Option<Element>, e_ref: &Element): bool {
        vector::contains(&t.vec, e_ref)
    }
    spec contains {
        pragma opaque;
        aborts_if false;
        ensures result == spec_contains(t, e_ref);
    }
    spec fun spec_contains<Element>(t: Option<Element>, e: Element): bool {
        is_some(t) && borrow(t) == e
    }

    /// Return an immutable reference to the value inside `t`
    /// Aborts if `t` does not hold a value
    public fun borrow<Element>(t: &Option<Element>): &Element {
        assert!(is_some(t), EOPTION_NOT_SET);
        vector::borrow(&t.vec, 0)
    }
    spec borrow {
        pragma opaque;
        include AbortsIfNone<Element>;
        ensures result == borrow(t);
    }

    /// Return a reference to the value inside `t` if it holds one
    /// Return `default_ref` if `t` does not hold a value
    public fun borrow_with_default<Element>(t: &Option<Element>, default_ref: &Element): &Element {
        let vec_ref = &t.vec;
        if (vector::is_empty(vec_ref)) default_ref
        else vector::borrow(vec_ref, 0)
    }
    spec borrow_with_default {
        pragma opaque;
        aborts_if false;
        ensures result == (if (is_some(t)) borrow(t) else default_ref);
    }

    /// Return the value inside `t` if it holds one
    /// Return `default` if `t` does not hold a value
    public fun get_with_default<Element: copy + drop>(
        t: &Option<Element>,
        default: Element,
    ): Element {
        let vec_ref = &t.vec;
        if (vector::is_empty(vec_ref)) default
        else *vector::borrow(vec_ref, 0)
    }
    spec get_with_default {
        pragma opaque;
        aborts_if false;
        ensures result == (if (is_some(t)) borrow(t) else default);
    }

    /// Convert the none option `t` to a some option by adding `e`.
    /// Aborts if `t` already holds a value
    public fun fill<Element>(t: &mut Option<Element>, e: Element) {
        let vec_ref = &mut t.vec;
        if (vector::is_empty(vec_ref)) vector::push_back(vec_ref, e)
        else abort EOPTION_IS_SET
    }
    spec fill {
        pragma opaque;
        aborts_if is_some(t) with EOPTION_IS_SET;
        ensures is_some(t);
        ensures borrow(t) == e;
    }

    /// Convert a `some` option to a `none` by removing and returning the value stored inside `t`
    /// Aborts if `t` does not hold a value
    public fun extract<Element>(t: &mut Option<Element>): Element {
        assert!(is_some(t), EOPTION_NOT_SET);
        vector::pop_back(&mut t.vec)
    }
    spec extract {
        pragma opaque;
        include AbortsIfNone<Element>;
        ensures result == borrow(old(t));
        ensures is_none(t);
    }

    /// Return a mutable reference to the value inside `t`
    /// Aborts if `t` does not hold a value
    public fun borrow_mut<Element>(t: &mut Option<Element>): &mut Element {
        assert!(is_some(t), EOPTION_NOT_SET);
        vector::borrow_mut(&mut t.vec, 0)
    }
    spec borrow_mut {
        pragma opaque;
        include AbortsIfNone<Element>;
        ensures result == borrow(t);
    }

    /// Swap the old value inside `t` with `e` and return the old value
    /// Aborts if `t` does not hold a value
    public fun swap<Element>(t: &mut Option<Element>, e: Element): Element {
        assert!(is_some(t), EOPTION_NOT_SET);
        let vec_ref = &mut t.vec;
        let old_value = vector::pop_back(vec_ref);
        vector::push_back(vec_ref, e);
        old_value
    }
    spec swap {
        pragma opaque;
        include AbortsIfNone<Element>;
        ensures result == borrow(old(t));
        ensures is_some(t);
        ensures borrow(t) == e;
    }

    /// Swap the old value inside `t` with `e` and return the old value;
    /// or if there is no old value, fill it with `e`.
    /// Different from swap(), swap_or_fill() allows for `t` not holding a value.
    public fun swap_or_fill<Element>(t: &mut Option<Element>, e: Element): Option<Element> {
        let vec_ref = &mut t.vec;
        let old_value = if (vector::is_empty(vec_ref)) none()
            else some(vector::pop_back(vec_ref));
        vector::push_back(vec_ref, e);
        old_value
    }
    spec swap_or_fill {
        pragma opaque;
        ensures result == old(t);
        ensures borrow(t) == e;
    }

    /// Destroys `t.` If `t` holds a value, return it. Returns `default` otherwise
    public fun destroy_with_default<Element: drop>(t: Option<Element>, default: Element): Element {
        let Option { vec } = t;
        if (vector::is_empty(&mut vec)) default
        else vector::pop_back(&mut vec)
    }
    spec destroy_with_default {
        pragma opaque;
        aborts_if false;
        ensures result == (if (is_some(t)) borrow(t) else default);
    }

    /// Unpack `t` and return its contents
    /// Aborts if `t` does not hold a value
    public fun destroy_some<Element>(t: Option<Element>): Element {
        assert!(is_some(&t), EOPTION_NOT_SET);
        let Option { vec } = t;
        let elem = vector::pop_back(&mut vec);
        vector::destroy_empty(vec);
        elem
    }
    spec destroy_some {
        pragma opaque;
        include AbortsIfNone<Element>;
        ensures result == borrow(t);
    }

    /// Unpack `t`
    /// Aborts if `t` holds a value
    public fun destroy_none<Element>(t: Option<Element>) {
        assert!(is_none(&t), EOPTION_IS_SET);
        let Option { vec } = t;
        vector::destroy_empty(vec)
    }
    spec destroy_none {
        pragma opaque;
        aborts_if is_some(t) with EOPTION_IS_SET;
    }

    /// Convert `t` into a vector of length 1 if it is `Some`,
    /// and an empty vector otherwise
    public fun to_vec<Element>(t: Option<Element>): vector<Element> {
        let Option { vec } = t;
        vec
    }
    spec to_vec {
        pragma opaque;
        aborts_if false;
        ensures result == t.vec;
    }

    spec module {} // switch documentation context back to module level

    spec module {
        pragma aborts_if_is_strict;
    }

    /// # Helper Schema

    spec schema AbortsIfNone<Element> {
        t: Option<Element>;
        aborts_if is_none(t) with EOPTION_NOT_SET;
    }
}
