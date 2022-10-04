/*
 * =====================
 * The unit type
 * =====================
 */

type A = ();

/*
 * =====================
 * Tuple types
 * =====================
 */

type A = (i32, String);

/*
 * =====================
 * Reference types
 * =====================
 */

type A = &B;
type C = &'a str;
type D = &'a mut str;

/*
 * =====================
 * Raw pointer types
 * =====================
 */

type A = *mut B;
type C = *const str;

/*
 * =====================
 * Generic types
 * =====================
 */

type A = B<C>;
type D = E<F, str>;
type G = H<'a, I>;
type J = H<K=L>;

/*
 * =====================
 * Scoped types
 * =====================
 */

type A = B::C;
type D = E::F::G;
type H = I::J<K>;
type L = M<N>::O;

/*
 * =====================
 * Array types
 * =====================
 */

type A = [B; 4];
type C = &[D];

/*
 * ============================
 * Function types
 * ============================
 */

fn high_order1(value: i32, f: fn(i32)) -> i32 {}

fn high_order2(value: i32, f: fn(i32) -> i32) -> i32 {
    f(value)
}

fn high_order3(value: i32, f: &FnOnce(i32) -> i32) -> i32 {
    f(value)
}

type F = for<'a, 'b> fn(x: &'a A, y: &'a mut B<'i, 't>,) -> C;

/*
 * =================================
 * Unsafe and extern function types
 * =================================
 */

type a = extern "C" fn(*mut c_void);
type b = unsafe extern "C" fn() -> *mut c_void;

/*
 * ===================================
 * Trait objects
 * ===================================
 */

type a = Box<Something + 'a>;
type b = Rc<dyn Something>;
type c = A<&dyn Fn(&B) -> C>;

/*
 * ====================================
 * Type cast expressions with generics
 * ====================================
 */

fn main() {
  a as B<C>;
  d as *mut E<<F as E>::G>;
}
