// Copyright (c) Mysten Labs, Inc.
// SPDX-License-Identifier: Apache-2.0

/// Similar to `sui::dynamic_field`, this module allows for the access of dynamic fields. But
/// unlike, `sui::dynamic_field` the values bound to these dynamic fields _must_ be objects
/// themselves. This allows for the objects to still exist within in storage, which may be important
/// for external tools. The difference is otherwise not observable from within Move.
module sui::dynamic_object_field {
    use std::option::{Self, Option};
    use sui::object::{Self, UID, ID};
    use sui::dynamic_field::{
        Self as field,
        add_child_object,
        borrow_child_object,
        borrow_child_object_mut,
        remove_child_object,
    };

    // Internal object used for storing the field and the name associated with the value
    // The separate type is necessary to prevent key collision with direct usage of dynamic_field
    struct Wrapper<Name> has copy, drop, store {
        name: Name,
    }

    /// Adds a dynamic object field to the object `object: &mut UID` at field specified by `name: Name`.
    /// Aborts with `EFieldAlreadyExists` if the object already has that field with that name.
    public fun add<Name: copy + drop + store, Value: key + store>(
        // we use &mut UID in several spots for access control
        object: &mut UID,
        name: Name,
        value: Value,
    ) {
        let key = Wrapper { name };
        let id = object::id(&value);
        field::add(object, key, id);
        let (field, _) = field::field_info<Wrapper<Name>>(object, key);
        add_child_object(object::uid_to_address(field), value);
    }

    /// Immutably borrows the `object`s dynamic object field with the name specified by `name: Name`.
    /// Aborts with `EFieldDoesNotExist` if the object does not have a field with that name.
    /// Aborts with `EFieldTypeMismatch` if the field exists, but the value object does not have the
    /// specified type.
    public fun borrow<Name: copy + drop + store, Value: key + store>(
        object: &UID,
        name: Name,
    ): &Value {
        let key = Wrapper { name };
        let (field, value_id) = field::field_info<Wrapper<Name>>(object, key);
        borrow_child_object<Value>(field, value_id)
    }

    /// Mutably borrows the `object`s dynamic object field with the name specified by `name: Name`.
    /// Aborts with `EFieldDoesNotExist` if the object does not have a field with that name.
    /// Aborts with `EFieldTypeMismatch` if the field exists, but the value object does not have the
    /// specified type.
    public fun borrow_mut<Name: copy + drop + store, Value: key + store>(
        object: &mut UID,
        name: Name,
    ): &mut Value {
        let key = Wrapper { name };
        let (field, value_id) = field::field_info_mut<Wrapper<Name>>(object, key);
        borrow_child_object_mut<Value>(field, value_id)
    }

    /// Removes the `object`s dynamic object field with the name specified by `name: Name` and returns
    /// the bound object.
    /// Aborts with `EFieldDoesNotExist` if the object does not have a field with that name.
    /// Aborts with `EFieldTypeMismatch` if the field exists, but the value object does not have the
    /// specified type.
    public fun remove<Name: copy + drop + store, Value: key + store>(
        object: &mut UID,
        name: Name,
    ): Value {
        let key = Wrapper { name };
        let (field, value_id) = field::field_info<Wrapper<Name>>(object, key);
        let value = remove_child_object<Value>(object::uid_to_address(field), value_id);
        field::remove<Wrapper<Name>, ID>(object, key);
        value
    }

    /// Returns true if and only if the `object` has a dynamic object field with the name specified by
    /// `name: Name`.
    public fun exists_<Name: copy + drop + store>(
        object: &UID,
        name: Name,
    ): bool {
        let key = Wrapper { name };
        field::exists_with_type<Wrapper<Name>, ID>(object, key)
    }

    /// Returns true if and only if the `object` has a dynamic field with the name specified by
    /// `name: Name` with an assigned value of type `Value`.
    public fun exists_with_type<Name: copy + drop + store, Value: key + store>(
        object: &UID,
        name: Name,
    ): bool {
        let key = Wrapper { name };
        if (!field::exists_with_type<Wrapper<Name>, ID>(object, key)) return false;
        let (field, value_id) = field::field_info<Wrapper<Name>>(object, key);
        field::has_child_object_with_ty<Value>(object::uid_to_address(field), value_id)
    }

    /// Returns the ID of the object associated with the dynamic object field
    /// Returns none otherwise
    public fun id<Name: copy + drop + store>(
        object: &UID,
        name: Name,
    ): Option<ID> {
        let key = Wrapper { name };
        if (!field::exists_with_type<Wrapper<Name>, ID>(object, key)) return option::none();
        let (_field, value_id) = field::field_info<Wrapper<Name>>(object, key);
        option::some(object::id_from_address(value_id))
    }
}
