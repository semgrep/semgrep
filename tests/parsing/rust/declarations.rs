/*
 * ============================================
 * Outer Attributes
 * ============================================
 */

#![allow(clippy::useless_transmute)]

/*
 * ============================================
 * Modules
 * ============================================
 */

mod english;

mod english {}

mod english {
    mod greetings {}
    mod farewells {}
}

pub mod english;

/*
 * ============================================
 * Extern crate declarations
 * ============================================
 */

extern crate std;
extern crate std as ruststd;
pub extern crate futures;

/*
 * ============================================
 * Function declarations
 * ============================================
 */

fn main() {}

fn add(x: i32, y: i32) -> i32 {
    return x + y;
}

fn takes_slice(slice: &str) {
    println!("Got: {}", slice);
}

fn foo() -> [u32; 2] {
    return [1, 2];
}

fn foo() -> (u32, u16) {
    return (1, 2);
}

fn foo() {
    return
}

fn foo(x: impl FnOnce() -> result::Result<T, E>) {}

fn foo(#[attr] x: i32, #[attr] x: i64) {}

fn accumulate(self) -> Machine<{State::Accumulate}> {}

/*
 * ============================================
 * Const function declarations
 * ============================================
 */

const fn main() {}

/*
 * ============================================
 * Functions with abstract return types
 * ============================================
 */

fn triples(a: impl B) -> impl Iterator<Item=(usize)> {
}

/*
 * ============================================
 * Diverging functions
 * ============================================
 */

fn aborts() -> ! {
}

/*
 * ============================================
 * Extern function declarations
 * ============================================
 */

extern "C" fn foo() {}
extern "C" fn printf(
    *const c_char,
    ...,
) {}

/*
 * ============================================
 * Use declarations
 * ============================================
 */

use abc;
use phrases::japanese;
use sayings::english::greetings;
use sayings::english::greetings as en_greetings ;
use phrases::english::{greetings,farewells};
use sayings::japanese::farewells::*;
pub use self::greetings::hello;
use sayings::english::{self, greetings as en_greetings, farewells as en_farewells};
use three::{ dot::{one, four} };
use my::{ some::* };
use my::{*};

/*
 * ============================================
 * Variable bindings
 * ============================================
 */

fn main() {
  let x;
  let x = 42;
  let x: i32;
  let x: i8 = 42;
  let mut x = 5;
  let y: bool = false;
  let bool: bool = false;
  let u32: str = "";
}

/*
 * ============================================
 * Structs
 * ============================================
 */

struct Proton;
struct Electron {}
struct Person {pub name: String, pub age: u32}
struct Point {
  x: i32,

  #[attribute1]
  y: i32,
}
struct Color(pub i32, i32, i32);
struct Inches(i32);

/*
 * ============================================
 * Unions
 * ============================================
 */

pub union in6_addr__bindgen_ty_1 {
    pub __u6_addr8: [__uint8_t; 16usize],
    pub __u6_addr16: [__uint16_t; 8usize],
    pub __u6_addr32: [__uint32_t; 4usize],
    _bindgen_union_align: [u32; 4usize],
}

/*
 * ============================================
 * Generic structs
 * ============================================
 */

struct A<B> {}
struct C<'a, 'b> {}
struct C<'a,> {}
struct D<const SIZE: usize> {}

/*
 * ============================================
 * Enums
 * ============================================
 */

pub enum Option<T> {
    None,
    Some(T),
}

pub enum Node<T: Item> {
    Internal {
        children: Vec<Tree<T>>,
        height: u16
    },
    #[attribute1]
    #[attribute2]
    Leaf {
        value: T
    }
}

/*
 * ============================================
 * Enums with values specified
 * ============================================
 */

pub enum c_style_enum {
    val1 = 1,
    val2 = 2
}

/*
 * ============================================
 * Generic functions
 * ============================================
 */

pub fn splice<T: Into<Text>>(&mut self, old_range: Range<usize>, new_text: T) {
}
pub fn uninit_array<const LEN: usize>() -> [Self; LEN] {}

/*
 * ============================================
 * Functions with mutable parameters
 * ============================================
 */

fn foo(mut x : u32) {
}

/*
 * ============================================
 * Functions with destructured parameters
 * ============================================
 */

fn f1([x, y]: [u32; 2]) {}
fn f2(&x: &Y) {}
fn f3((x, y): (T, U)) {}

/*
 * ============================================
 * Functions with custom types for self
 * ============================================
 */

trait Callback {
    fn call(self: Box<Self>);
}

/*
 * ============================================
 * Constant items
 * ============================================
 */

const N: i32 = 5;

trait Foo {
    const X: u8;
}

/*
 * ============================================
 * Static items
 * ============================================
 */

static N: i32 = 5;
static mut __progname: *mut ::c_char;

/*
 * ============================================
 * Static 'ref' items using lazy_static
 * ============================================
 */

static ref ONE: usize = 0;

/*
 * ============================================
 * Type aliases
 * ============================================
 */

type Inch = u64;
type Name<T> = Vec<T>;

/*
 * ============================================
 * Empty statements
 * ============================================
 */

fn main() {
    ;
}

/*
 * ============================================
 * Attributes
 * ============================================
 */

#[test]
fn test_foo() {}

#[derive(Debug)]
struct Baz;

#[derive(Debug, Eq,)]
struct Foo;

#[cfg(target_os = "macos")]
mod macos_only {}

#[clippy::cyclomatic_complexity = "100"]
struct Bar;

/*
 * ============================================
 * Inner attributes
 * ============================================
 */

mod macos_only {
  #![cfg(target_os = "macos")]
}

/*
 * ============================================
 * Attributes and Expressions
 * ============================================
 */

fn foo() {
   bar(x,
       #[cfg(foo = "bar")]
       y);
   let z = [#[hello] 2, 7, 8];
   let t = (#[hello] 2, 7, 8);
}

/*
 * ===========================================
 * Inherent Impls
 * ===========================================
 */

impl Person {
  const leg_count : u32 = 2;

  fn walk(self) {}
  fn walk_mut(mut self) {}
  fn talk(& self) {}
  fn talk_mut(&'a mut self) {}
}

impl Machine<{State::Init}> {}

/*
 * ===========================================
 * Trait impls
 * ===========================================
 */

impl<'a> iter::Iterator for Self::Iter<'a> {
}

impl ConvertTo<i64> for i32 {
    fn convert(&self) -> i64 { *self as i64 }
}

/*
 * ===========================================
 * Unsafe impls
 * ===========================================
 */

unsafe impl Foo {
}

/*
 * ===========================================
 * Impls with default functions
 * ===========================================
 */

impl Foo {
  const default fn bar() -> i32 {
    // Make 'default' still works as an identifier
    default.bar();
  }
}

/*
 * ============================================
 * Trait declarations
 * ============================================
 */

pub trait Item: Clone + Eq + fmt::Debug {
    fn summarize(&self) -> Self::Summary;
}

unsafe trait Foo { }

/*
 * ============================================
 * Trait declarations with optional type parameters
 * ============================================
 */

trait Add<RHS=Self> {
    type Output;
    fn add(self, rhs: RHS) -> Self::Output;
}

/*
 * ============================================
 * Unsized types in trait bounds
 * ============================================
 */

trait Foo<T: ?Sized> {
}

/*
 * ============================================
 * Macro invocations inside trait declarations
 * ============================================
 */


pub trait A: B + C + D {
    private_decl!{}
    fn f(&self);
}

/*
 * ============================================
 * Associated Types
 * ============================================
 */

pub trait Graph {
    type N: fmt::Display;
    type E;
}

/*
 * =====================
 * Higher-ranked types
 * =====================
 */

trait T: for<'a> AddAssign<&'a usize> {
}

/*
 * =====================
 * Visibility modifiers
 * =====================
 */

pub fn a() {}
pub(super) fn b() {}
pub(self) fn c() {}
pub(crate) fn c() {}
pub(in crate::d) fn e() {}

/*
 * ========================================================
 * Function parameter names that match built-in type names
 * ========================================================
 */

fn foo(str: *const c_char) {}
fn bar(bool: bool) {}

/*
 * =====================
 * Where clauses
 * =====================
 */

fn walk<F>(&self, it: &mut F) -> bool
    where F: FnMut(&Pat) -> bool
{
  return false
}

impl<'a, T: 'a + Item> Iterator for Iter<'a, T> where Self: 'a {
}

impl<T> A for B<T>
    where C<T>: D,
          T: 'c,
          'c: 'b,
{
}

impl<'a, E> Read
where &'a E: Read,
{
}

impl<T> A for B<T> where (T, T, T): C, {}

impl<T> A for B<T>
    where for<'a> D<T>: E<'a>,
{
}

pub trait A<B> where B: C,
{
}

fn foo<A>() where A: B + As<f64>, f64: As<A> {}

impl<A> Default for B<A> where *mut A: C + D {}

/*
 * ===================================
 * External Modules
 * ===================================
 */

pub extern {
  pub fn napi_module_register(mod_: *mut napi_module);
}

extern "C" {
  static TYPE_INFO_VTABLE: *const u8;
}

/*
 * ===================================
 * Crate visibility
 * ===================================
 */

crate mod foo;
crate struct Foo(crate crate::Bar);
crate fn foo() { }
crate const X: u32 = 0;
