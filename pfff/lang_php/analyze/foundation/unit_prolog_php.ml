open Common

open OUnit

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * What to put here? Should we duplicate things from Unit_static_analysis_php
 * as many results from static analysis are now translated into Prolog facts?
 * No, no need to duplicate! Just copy here the basic versions
 * of some tests, e.g. for the callgraph just the basic function and
 * method calls for instance.
 *
 * todo: port most of unit_analyze_db_php.ml here using also the abstract
 * interpreter for more "demanding" callgraph/datagraph unit tests.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let normalize_whitespace (w: string) : string =
  Str.global_replace (Str.regexp ",\\s") "," w

let prolog_query ?header ~file query =
  let source_file = Parse_php.tmp_php_file_from_string ?header file in
  Database_prolog_php.prolog_query ~verbose:false ~source_file ~query
  |> List.map normalize_whitespace

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let unittest =
 "prolog" >::: ([

(*****************************************************************************)
(* Entities *)
(*****************************************************************************)
   "kinds" >:: (fun () ->
      let file = "
function foo() { }
const BAR = 1;
class A {
  public $fld = 0;
  public function bar() { }
  const CST = 1;
}
interface I { }
trait T { }
" in
      (* quite similar to the unit test for tags in unit_foundation_php.ml *)
      assert_equal
        ["function"]  (prolog_query ~file "kind('foo', X), writeln(X)");
      assert_equal
        ["constant"]  (prolog_query ~file "kind('BAR', X), writeln(X)");
      assert_equal
        ["class"]     (prolog_query ~file "kind('A', X), writeln(X)");
      assert_equal
        ["method"]     (prolog_query ~file "kind(('A','bar'), X), writeln(X)");
      assert_equal
        ["field"]     (prolog_query ~file "kind(('A','fld'), X), writeln(X)");
      assert_equal
        ["constant"]  (prolog_query ~file "kind(('A','CST'), X), writeln(X)");
      assert_equal
        ["interface"] (prolog_query ~file "kind('I', X), writeln(X)");
      assert_equal
        ["trait"]     (prolog_query ~file "kind('T', X), writeln(X)");
    );

   "types" >:: (fun () ->
     let file ="
class A {
private int $x;
private $y;
}" in
     assert_equal
       ["int"] (prolog_query ~file "type(('A','x'), X), writeln(X)");
     assert_equal
       ["!unknown!"] (prolog_query ~file "type(('A','y'), X), writeln(X)");
   );

(*****************************************************************************)
(* Class/Traits *)
(*****************************************************************************)
    (*-----------------------------------------------------------------------*)
    (* Inheritance *)
    (*-----------------------------------------------------------------------*)

    "inheritance" >:: (fun () ->
      let file = "
class A { }
class B extends A { }
class C extends B { }
"
      in
      let xs = prolog_query ~file "children(X, 'A'), writeln(X)" in
      assert_equal ~msg:"it should find all children of a class"
        (sort ["B";"C"])
        (sort xs)
    );


    "inheritance and traits" >:: (fun () ->
      let file = "
interface I { }
trait T implements I { }
class A { use T; }
"
      in
      let xs = prolog_query ~file "children(X, 'I'), writeln(X)" in
      assert_equal ~msg:"it should find all children of an interface"
        (sort ["A";"T"])
        (sort xs)
    );
    (*-----------------------------------------------------------------------*)
    (* Traits *)
    (*-----------------------------------------------------------------------*)

    "traits" >:: (fun () ->
      let file = "
trait T {
  public function trait1() { }
}
class A {
  use T;
  public function a() { }
}
"
      in
      let xs = prolog_query ~file "method('A', (_Class, X)), writeln(X)"
      in
      assert_equal ~msg:"it should find all methods of a class using traits"
        (sort ["a";"trait1"])
        (sort xs);

    let file = "
trait T1 { public function foo() { } }
trait T2 { public function bar() { } }
trait TComp { use T1, T2; }
class A { use TComp; }
"
    in
    let xs = prolog_query ~file "method('A', (_Class, X)), writeln(X), fail"
    in
    assert_equal
      ~msg:"it should find all methods of a class using multiple traits"
        (sort ["foo";"bar"])
        (sort xs)
    );

    (*-----------------------------------------------------------------------*)
    (* Privacy and inheritance *)
    (*-----------------------------------------------------------------------*)
    (* todo: tricky when traits *)

    (*-----------------------------------------------------------------------*)
    (* Override *)
    (*-----------------------------------------------------------------------*)

    "overrides" >:: (fun () ->
      let file = "
class A {
   public function foo() { }
   public function bar() { }
}
class B extends A { public function foo() { } }
"
      in
      let xs = prolog_query ~file
        "overrides(Class, Method), writeln(Method), fail" in
      assert_equal ~msg:"it should detect overriden methods"
        (sort ["foo"])
        (sort xs);

    );
(*****************************************************************************)
(* Callgraph *)
(*****************************************************************************)

  (*-------------------------------------------------------------------------*)
  (* Callgraph and functions *)
  (*-------------------------------------------------------------------------*)
    "basic callgraph for functions" >:: (fun () ->
      let file = "
function foo() { }
function bar() { foo(); }
" in
      let xs = prolog_query ~file
        "docall(X, 'foo', function), writeln(X), fail" in
      assert_equal ~msg:"it should find basic callers to a function"
        ["bar"]
        (sort xs);
    );

  (*-------------------------------------------------------------------------*)
  (* Callgraph and methods *)
  (*-------------------------------------------------------------------------*)
    "basic (imprecise) callgraph for methods" >:: (fun () ->
      (* cannot resolve the class of a method calls, but at least can index
       * that there was a method call with a specific name
       *)
      let file ="
class A { }
function bar() {
  $o = new A();
  $y = $o->foo();
} " in
      let xs = prolog_query ~file
        "docall('bar', X, method), writeln(X), fail" in
      assert_equal ~msg:"it should find basic callers to a method name"
        ["foo"]
        (sort xs);

      let xs = prolog_query ~file
        "docall('bar', X, class), writeln(X), fail" in
      assert_equal ~msg:"it should find basic use of new"
        ["A"]
        (sort xs);
    );

    "handling new PHP syntax (new X)->" >:: (fun () ->
      (* we used to use id() for that *)
      let file ="
class A { }
class B { }
function bar1() {
  $o = (new A())->foo();
}
function bar2() {
  $o = new B;
  $o = (new A);
} " in
      let xs = prolog_query ~file
        "docall('bar1', X, class), writeln(X), fail" in
      assert_equal ~msg:"it should find nested use of new"
        ["A"]
        (sort xs);
      let xs = prolog_query ~file
        "docall('bar2', X, class), writeln(X), fail" in
      assert_equal ~msg:"it should find nested use of new"
        ["A";"B"]
        (sort xs);
    );

    "callgraph for static methods" >:: (fun () ->
      let file ="
class A {
  static function foo() { }
  static function foobar() { }
  static public function a() {
    self::foobar();
  }
}
function bar() {
  $y = A::foo();
}
" in
      let xs = prolog_query ~file
        "docall('bar', X, method), writeln(X), fail" in
      assert_equal ~msg:"it should find basic callers to a static method"
        ["A,foo"; "foo"]
        (sort xs);

      let xs = prolog_query ~file
        "docall(('A','a'), X, method), writeln(X), fail" in
      assert_equal ~msg:"it should unsugar self"
        ["A,foobar"; "foobar"]
        (sort xs);
    );

    "callgraph for higher order functions" >:: (fun () ->
      let file ="
function newv($string) { return new $string(); }
class A { }
function bar() {
  $o = newv('A');
}
" in
      let xs = prolog_query ~file
        "docall('bar', X, special), writeln(X), fail" in
      assert_equal ~msg:"it should index classnames passed as strings"
        ["newv,A"]
        (sort xs);
    );

    "advanced callgraph analysis for methods" >:: (fun () ->
      (* this one requires more sophisticated analysis, with
       * append_callgraph_to_prolog_db
       *)
      let file ="
class A { function foo() { } }
class B extends A { }
function bar() {
  $o = new B();
  $y = $o->foo();
} " in
      let xs = prolog_query ~file
        "docall2('bar', (X,Y), method), writeln((X,Y)), fail" in
      assert_equal ~msg:"it should find basic callers to a function"
        ["A,foo"]
        (sort xs);
    );

(*****************************************************************************)
(* Exceptions *)
(*****************************************************************************)
    "exceptions" >:: (fun () ->
      let file = "
class Exception { }
class ViolationException extends Exception { }
class ForgotExtendsException { }
class UnrelatedClass { }

function foo() {
  throw new Exception();
}
function bar() {
  try {
    throw new ViolationException();
  } catch (ViolationException $e) {
  }
}
function bad() {
  throw new ForgotExtendsException();
}
" in
      let xs = prolog_query ~file "throw('foo', X), writeln(X)" in
      assert_equal ~msg:"it should find basic throw"
        ["Exception"]
        xs;
      let xs = prolog_query ~file "catch('bar', X), writeln(X)" in
      assert_equal ~msg:"it should find basic catch"
        ["ViolationException"]
        xs;
      let xs = prolog_query ~file
        ("throw(_, X), not(children(X, 'Exception')), X \\= 'Exception', " ^
        "writeln(X)") in
      assert_equal ~msg:"it should find exceptions not deriving from Exception"
        ["ForgotExtendsException"]
        xs;
    );

(*****************************************************************************)
(* Data graph *)
(*****************************************************************************)
    "arrays used as records" >:: (fun () ->
      let file = "
function foo($x) {
  echo $x['bar'];
}
function foo2($x) {
  $x['bar'] = 1;
}
" in
    let xs = prolog_query ~file "use(X, 'bar', array, read), writeln(X)" in
    assert_equal ~msg:"it should find read accesses to a record field"
      ["foo"] (xs);
    let xs = prolog_query ~file "use(X, 'bar', array, write), writeln(X)" in
    assert_equal ~msg:"it should find write accesses to a record field"
      ["foo2"] (xs);
    );

    "fields use" >:: (fun () ->
      let file = "
class A {
 public $bar = 0;
}
function foo(A $o) {
  echo $o->bar;
}
function foo2(A $o) {
  $o->bar = 1;
}
function foo3(A $o) {
  echo $o->array_fld['x'];
}
" in
    let xs = prolog_query ~file "use(X, 'bar', field, write), writeln(X)" in
    assert_equal ~msg:"it should find write accesses to an object field"
      ["foo2"] (xs);

    let xs = prolog_query ~file "use(X, 'array_fld', field, read),writeln(X)" in
    assert_equal ~msg:"it should find accesses to an array field"
      ["foo3"] (xs);
    );

    "class constant use" >:: (fun () ->
      let file = "
class A {
 const CST = 1;
}
function foo() {
  echo A::CST;
}
" in
    let xs = prolog_query ~file "use('foo', X , constant, read), writeln(X)" in
    assert_equal ~msg:"it should find basic access to a class constant"
      ["A,CST"] (xs);
    );

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
    "types" >:: (fun () ->
      let file = "
function foo(int $x): int {
}"in
      let xs = prolog_query ~file "return('foo', X), writeln(X)" in
      assert_equal ~msg:"it should index return types"
        (["int"])
        xs
    );

(*****************************************************************************)
(* XHP *)
(*****************************************************************************)
    "xhp" >:: (fun () ->
      let file = "
class :x:frag {
 attribute string template;
}
"in
      let xs = prolog_query ~file "field(':x:frag', (_, X)), writeln(X)" in
      assert_equal ~msg:"it should understand xhp attributes"
        (["template"])
        xs
    );
    (* todo: handle also children, inherit, etc *)

(*****************************************************************************)
(* Hack *)
(*****************************************************************************)
    "hack" >:: (fun () ->
      let file = "
function foo(): int {
  return 1;
}
"in
      let xs = prolog_query ~header:"<?hh\n" ~file "hh(X,_), writeln(X)" in
      assert_bool ~msg:"it should recognize hh files"
        (match xs with
        | [f] -> f =~ "/tmp/.*"
        | _ -> false
        );

      let xs = prolog_query ~header:"<?php\n" ~file "hh(X,_), writeln(X)" in
      assert_equal ~msg:"it should not recognize php files"
        [] xs;

      let xs =
        prolog_query ~header:"<?hh //decl\n" ~file "hh(_,X), writeln(X)"
      in
      assert_equal ~msg:"it should recognize hh modes"  ["decl"] xs;

      let xs =
        prolog_query ~header:"<?hh //     strict\n" ~file "hh(_,X), writeln(X)"
      in
      assert_equal ~msg:"it should recognize hh modes"  ["strict"] xs;


      let xs =
        prolog_query ~header:"<?hh // partial\n" ~file "hh(_,X), writeln(X)"
      in
      assert_equal ~msg:"it should recognize hh modes"  ["default"] xs;

      let xs =
        prolog_query ~header:"<?hh // not a mode\n" ~file "hh(_,X), writeln(X)"
      in
      assert_equal ~msg:"it should recognize hh modes"  ["default"] xs;

    );


(*****************************************************************************)
(* Generator *)
(*****************************************************************************)
    "generator" >:: (fun () ->
      let file = "
async function foo() { }
class A {
  async function bar() { }
}
function not_async() { }
"in
      let xs = prolog_query ~file "async(X), writeln(X), fail" in
      assert_equal ~msg:"it should understand async functions/methods"
        (["foo"; "A,bar"])
        xs
    );

(*****************************************************************************)
  ]
  )
