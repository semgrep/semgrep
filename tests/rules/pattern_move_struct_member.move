address 0xdeadbeef {
    module example {
        use aptos_std::object::{Self, Object};

        // ruleid: pattern_move_struct_signature
        struct Subscription has store, drop, key {
            end_subscription: u64,
        }

        // ruleid: pattern_move_struct_member
        struct Something has key, store {
            some_number: u64,
            obj: Object<Subscription>,
        }

        // ruleid: pattern_move_struct_member
        struct Something2 has key {
            obj2: Object<Subscription>,
        }

        fun address_function(subscription: &Subscription): address {
            let subscription = subscription;
            let subscription_address = address_of(subscription);
            subscription_address
        }
    }
}