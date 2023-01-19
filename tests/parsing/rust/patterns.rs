/*
 * =================================
 * Tuple struct patterns
 * =================================
 */

fn main() {
  match x {
    Some(x) => "some",
    std::None() => "none"
  }
}

/*
 * =================================
 * Reference patterns
 * =================================
 */

fn main() {
  match x {
    A(ref x) => x.0,
    ref mut y => y,
    & mut  z => z,
  }
}

/*
 * =================================
 * Struct patterns
 * =================================
 */

fn main() {
  match x {
    Person{name, age} if age < 5 => ("toddler", name),
    Person{name: adult_name, age: _} => ("adult", adult_name),
  }
}

/*
 * =================================
 * Ignored patterns
 * =================================
 */

fn main() {
  match x {
    (a, ..) => a,
    B(..) => c,
    D::E{f: g, ..} => g
  }
}

/*
 * =================================
 * Captured patterns
 * =================================
 */

fn main() {
  match x {
    a @ A(_) | b @ B(..) => a,
    a @ 1 ... 5 => a,
    Some(1 ... 5) => a,
    a @ b...c => a,
    a @ b..=c => a,
  }
}
