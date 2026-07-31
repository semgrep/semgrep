fn test_weird_import() {
    use hashconsing::{HConsed, hashing::HConsedBuilder};
    // ruleid: ssc-rust-limitation-5
    let h = HConsed::new();
    // ruleid: ssc-rust-limitation-5
    let y = HConsedBuilder::new();
}

// Two-segment scope prefix before the braces: `alpha::beta` must be
// prepended (not appended) to the nested `gamma::Gadget` path.
fn test_two_segment_scope() {
    use alpha::beta::{Widget, gamma::Gadget};
    // ruleid: ssc-rust-limitation-5
    let w = Widget::new();
    // ruleid: ssc-rust-limitation-5
    let g = Gadget::new();
}

// Nested group inside a group: both `outer` and `inner` scopes must be
// prepended in order.
fn test_nested_group() {
    use outer::{inner::{Thing}};
    // ruleid: ssc-rust-limitation-5
    let t = Thing::new();
}

// Several nested paths in one group; each keeps its own scope in order.
fn test_multi_nested() {
    use root::{one::First, two::Second};
    // ruleid: ssc-rust-limitation-5
    let a = First::new();
    // ruleid: ssc-rust-limitation-5
    let b = Second::new();
}

// Aliased import with a nested path under a scope: the alias target's FQN
// must still resolve to the correctly-ordered path.
fn test_aliased_nested() {
    use pkg::{sub::Real as Aliased};
    // ruleid: ssc-rust-limitation-5
    let r = Aliased::new();
}

// Deep nesting: multi-segment scope AND multi-segment nested path.
fn test_deep_nesting() {
    use deep::a::b::{c::d::Buried};
    // ruleid: ssc-rust-limitation-5
    let x = Buried::new();
}

// Control: import from a different crate. Correct FQN is
// `hashconsing2::HConsed`, so the `hashconsing::HConsed` pattern must NOT fire.
fn test_control_wrong_crate() {
    use hashconsing2::{HConsed};
    // ok: ssc-rust-limitation-5
    let h = HConsed::new();
}

// Control: reversed scope order would only match under the old buggy
// `modname @ scope` behavior. Correct FQN is `pkgx::mid::Gizmo`, so the
// `mid::pkgx::Gizmo` trap pattern must NOT fire.
fn test_control_order() {
    use pkgx::{mid::Gizmo};
    // ok: ssc-rust-limitation-5
    let g = Gizmo::new();
}
