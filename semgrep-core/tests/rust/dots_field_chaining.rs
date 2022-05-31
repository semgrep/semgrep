struct A {
    x: B,
}

struct B {
    y: C,
}

struct C {
    z: (),
}

fn main() {
    let s = A { x: B { y: C { z: () } } };
    s.x;
    s.x.y;
    // ERROR: match
    s.x.y.z;
    // ERROR: match
    let () = s.x.y.z;
}
