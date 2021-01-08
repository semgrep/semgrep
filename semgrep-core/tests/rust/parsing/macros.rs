/*
 * ============================================
 * Macro invocation - no arguments
 * ============================================
 */

fn main() {
  a!();
  b![];
  c!{};
  d::e!();
  f::g::h!{};
}

a!()

/*
 * ============================================
 * Macro invocation - arbitrary tokens
 * ============================================
 */

fn main() {
  a!(* a *);
  a!(& a &);
  a!(- a -);
  a!(b + c + +);
  a!('a'..='z');
  a!('\u{0}'..='\u{2}');
  a!('lifetime)
}

/*
 * ============================================
 * Macro definition
 * ============================================
 */

macro_rules! say_hello {
    () => (
        println!("Hello!");
    )
}

macro_rules! four {
    () => {1 + 3};
}

macro_rules! foo {
    (x => $e:expr) => (println!("mode X: {}", $e));
    (y => $e:expr) => (println!("mode Y: {}", $e))
}

macro_rules! o_O {
    (
      $($x:expr; [ $( $y:expr ),* ]);*
    ) => {
      $($($x + $e),*),*
    }
}

macro_rules! zero_or_one {
    ($($e:expr),?) => {
        $($e),?
    };
}
