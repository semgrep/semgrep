open Common
open OUnit

open Env_interpreter_php
module Env = Env_interpreter_php
module Interp = Abstract_interpreter_php.Interp (Tainting_fake_php.Taint)
module Db = Database_juju_php
module CG = Callgraph_php2

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* See also tests/php/ia/*.php *)

(*****************************************************************************)
(* Run analysis *)
(*****************************************************************************)

let prepare content =
  let tmp_file = 
    Parse_php.tmp_php_file_from_string content in
  let db = 
    Db.code_database_of_juju_db  (Db.juju_db_of_files [tmp_file]) in
  let env = 
    Env.empty_env db tmp_file in
  let ast = 
    Ast_php_build.program (Parse_php.parse_program tmp_file) in
  env, ast

let heap_of_program_at_checkpoint content =
  let (env, ast) = prepare content in
  Common.save_excursion Abstract_interpreter_php.extract_paths false (fun()->
  Common.save_excursion Abstract_interpreter_php.strict true (fun()->
    let _heap = Interp.program env Env.empty_heap ast in
    match !Abstract_interpreter_php._checkpoint_heap with
    | None -> failwith "use checkpoint() in your unit test"
    | Some x -> x
  ))

(* less: use Callgraph_php_build.create_graph *)
let callgraph_generation content =
  let (env, ast) = prepare content in
  Common.save_excursion Abstract_interpreter_php.extract_paths true (fun()->
  Common.save_excursion Abstract_interpreter_php.strict true (fun()->
    Abstract_interpreter_php.graph := Map_.empty;
    let _heap = Interp.program env Env.empty_heap ast in
    !(Abstract_interpreter_php.graph)
  ))

(*****************************************************************************)
(* Examine value *)
(*****************************************************************************)

let rec chain_ptrs heap v =
  match v with
  | Vptr n ->
      Vptr n::(chain_ptrs heap (IMap.find n heap.ptrs))
  | Vref aset ->
      let n = ISet.choose aset in
      Vref aset::(chain_ptrs heap (Vptr n))
  | x -> [x]

let value_of_var s vars heap =
  let v = SMap.find s vars in
  match v with
  | Vptr _n ->
      chain_ptrs heap v
  | _ -> assert_failure "variable is not a Vptr"

let info heap v = 
  Env.string_of_value heap (List.hd v)

(*****************************************************************************)
(* Assert helpers *)
(*****************************************************************************)

let assert_value_at_checkpoint var file fpattern =
  let (heap, vars) = heap_of_program_at_checkpoint file in
  let v = value_of_var var vars heap in
  if fpattern v
  then ()
  else assert_failure (spf "wrong value for %s: %s " var (info heap v))

let assert_final_value_at_checkpoint var file v =
  assert_value_at_checkpoint var file (function
  | [Vptr _n1; Vptr _n2; x] -> x =*= v
  | _ -> false
  )

(* todo: a pathdown, pathup specialization? *)
let assert_graph file xs = 
  let g = callgraph_generation file in
  let _nb_nodes = List.length xs in
  xs |> List.iter (fun (s, expected) ->
    try
      let n = CG.node_of_string s in
      let actual_child = 
        Map_.find n g 
        |> Set_.elements 
        |> List.map CG.string_of_node 
      in
      assert_equal
        ~msg:"it should have the expected callees"
        (sort expected)
        (sort actual_child)
    with Not_found ->
      assert_failure (spf "could not find callees for %s" s)
  );
  (* todo? assert all the nodes are there *)
  ()

(* sugar to make a graph by adjacent list *)
let (-->) a b = (a, b)

(*****************************************************************************)
(* Abstract interpreter *)
(*****************************************************************************)
let abstract_interpreter_unittest =
  "abstract interpreter" >::: [

  (*-------------------------------------------------------------------------*)
  (* Basic types and dataflow *)
  (*-------------------------------------------------------------------------*)
    "basic" >:: (fun () ->
      let file ="
$x = 42;
checkpoint(); // x:42
" in
      (* note: I don't use assert_final_value_at_checkpoint for teaching
       * purpose here *)
      let (heap, vars) = heap_of_program_at_checkpoint file in
      match value_of_var "$x" vars heap with
      (* variables in PHP are pointers to a pointer to a value ... *)
      | [Vptr _n1; Vptr _n2; Vint 42] -> ()
      | v -> assert_failure ("wrong value for $x: " ^ info heap v)
    );

    "unsugaring" >:: (fun () ->
      let file ="
$x = <<<END
hello
END;
checkpoint(); // x:'hello'
" in
      assert_value_at_checkpoint "$x" file (function
      (* todo? it should maybe be "hello" without the newline *)
      | [Vptr _n1; Vptr _n2; Vstring "hello\n"] -> true | _ -> false)
    );

    "aliasing" >:: (fun () ->
      let file ="
$x = 42;
$y =& $x;
checkpoint();
" in
      let (heap, vars) = heap_of_program_at_checkpoint file in
      let x = value_of_var "$x" vars heap in
      let y = value_of_var "$y" vars heap in
      match x, y with
      | [Vptr ix1; Vref _set; Vptr ix2; Vint 42],
        [Vptr iy1; Vref _set2; Vptr iy2; Vint 42]
        ->
          assert_equal
            ~msg:"it should share the second pointer"
            ix2 iy2;
          assert_bool 
            "variables should have different original pointers"
            (ix1 <> iy1)

      | _ -> assert_failure (spf "wrong value for $x: %s, $y = %s "
                               (info heap x) (info heap y))
    );

    "abstraction when if" >:: (fun () ->
      let file ="
$x = 1;
$y = true; // path sensitivity would detect it's always $x = 2 ...
if($y) { $x = 2;} else { $x = 3; }
checkpoint(); // x: int
" in
      (* there is no range, we go from a very precise value to a
       * very general abstraction (the type) very quickly.
       * If forget the initial $x = 1; then $x will be instead
       * a 'choice(null,int)'.
       *)
      assert_final_value_at_checkpoint "$x" file (Vabstr Tint); 
    );

    "union types" >:: (fun () ->
      let file ="
$x = null;
$y = true;
if($y) { $x = 2;} else { $x = 3; }
checkpoint(); // x: null | int
" in
      assert_final_value_at_checkpoint "$x" file (Vsum [Vnull; Vabstr Tint]);
    );

    "simple dataflow" >:: (fun () ->
      let file ="
$x = 2;
$x = 3;
$y = $x;
checkpoint(); // y:int
" in
      assert_final_value_at_checkpoint "$y" file (Vabstr Tint);
    );

    "constants" >:: (fun () ->
      let file ="
const CST = 2;
$x = CST;
checkpoint(); // x:int
" in
      assert_final_value_at_checkpoint "$x" file (Vint 2);
    );

  (*-------------------------------------------------------------------------*)
  (* Error handling *)
  (*-------------------------------------------------------------------------*)

    "use of undefined" >:: (fun () ->
      let file ="
const CST = 2;
$x = ANOTHER_CST;
checkpoint(); // x:int
" in
      try 
        let _ = heap_of_program_at_checkpoint file in
        assert_failure 
          "it should raise exns in strict mode on undefined entities"
      with Abstract_interpreter_php.UnknownConstant "ANOTHER_CST" -> ()
    );

  (*-------------------------------------------------------------------------*)
  (* Fixpoint *)
  (*-------------------------------------------------------------------------*)

  (* TODO while loop, dowhile, recursion, iterate 2 times is enough?
   * Because of the abstraction we've chosen (no int range for instance),
   * we achieve the fixpoint in one step so probably has
   * no fixpoint issues.
   *)

  (*-------------------------------------------------------------------------*)
  (* Interprocedural dataflow *)
  (*-------------------------------------------------------------------------*)

    "interprocedural dataflow" >:: (fun () ->
      let file ="
$x = 2;
function foo($a) { return $a + 1; }
$y = foo($x);
checkpoint(); // y: int
" in
      assert_final_value_at_checkpoint "$y" file (Vabstr Tint);
    );

    "interprocedural dataflow with static methods" >:: (fun () ->

      let file ="
class A {
  static function foo() { return self::bar(); }
  static function bar() { return 1+1; }
}

class B extends A {
 static function bar() { return false || false; }
  }
$x = B::foo();
$y = B::bar();
checkpoint(); // x: int, y: bool
" in
      assert_final_value_at_checkpoint "$x" file (Vabstr Tint);
      assert_final_value_at_checkpoint "$y" file (Vabstr Tbool);
    );

    "interprocedural dataflow with normal methods" >:: (fun () ->
      let file ="
$x = 2;
class A { function foo($a) { return $a + 1; } }
class B extends A { }
$o = new B();
$y = $o->foo($x);
checkpoint(); // y: int
" in
      assert_final_value_at_checkpoint "$y" file (Vabstr Tint);
    );

(*****************************************************************************)
(* Callgraph *)
(*****************************************************************************)

  (* less: move in unit_callgraph_php.ml *)
  
  (*-------------------------------------------------------------------------*)
  (* Callgraph and functions *)
  (*-------------------------------------------------------------------------*)

    "basic callgraph for direct functions" >:: (fun () ->
      let file = "
function foo() { }
function bar() { foo(); }
" in
      (* note: I don't use assert_graph for teaching purpose here *)
      let g = callgraph_generation file in
      let xs = Map_.find (CG.Function "bar") g |> Set_.elements in
      assert_equal
        ~msg:"it should handle simple direct calls:"
        [CG.Function "foo"]
        xs;

      let file = "
function bar() { foo(); }
" in
      try 
        let _ = callgraph_generation file in
        assert_failure "it should throw an exception for unknown function"
      with (Abstract_interpreter_php.UnknownFunction "foo") -> ()
    );


  (* todo: call_user_func, id wrapper preserve graph, ?? *)

  (*-------------------------------------------------------------------------*)
  (* Callgraph and static methods *)
  (*-------------------------------------------------------------------------*)

    "simple static method call" >:: (fun () ->
      let file = "
class A { static function a() { } }
function b() { A::a(); }
" in
      assert_graph file ["b" --> ["A::a"]];

      let file = "
class A { static function a() { } }
function b() { A::unknown(); }
" in
      try 
        let _ = callgraph_generation file in
        assert_failure "it should throw an exception for unknown static method"
      with (Abstract_interpreter_php.UnknownMember ("unknown", "A", _)) -> ()
    );

    (* In PHP it is ok to call B::foo() even if B does not define
     * a static method 'foo' provided that B inherits from a class
     * that defines such a foo.
     *)
    "lookup even for static method call" >:: (fun () ->
      let file ="
class A { static function a() { } }
class B extends A { }
function b() { B::a(); }
" in
      assert_graph file ["b" --> ["A::a"]]
    );

    "static lookup self in parent" >:: (fun () ->
      let file ="
class A {
  static function foo() { self::bar(); }
  static function bar() { }
}
class B extends A {
 static function bar() { }
}
function b() { B::foo(); }
function c() { B::bar(); }
" in
      assert_graph file 
        ["b" --> ["A::foo"];"c" --> ["B::bar"];"A::foo" --> ["A::bar"]]
    );

    "static method call with self:: and parent::" >:: (fun () ->
      let file = "
class A {
 static function a() { }
 static function a2() { self::a(); }
}
class B extends A {
 function b() { parent::a(); }
}" in
        assert_graph file ["A::a2" --> ["A::a"]; "B::b" --> ["A::a"]]
      );

    (* PHP is very permissive regarding static method calls as one can
     * do $this->foo() even if foo is a static method. PHP does not
     * impose the X::foo() syntax, which IMHO is just wrong.
     *)
    "static method call and $this" >:: (fun () ->
      let file = "
class A {
  static function a() { }
  function a2() { $this->a(); }
} " in
      assert_graph file ["A::a2" --> ["A::a"]];
    );

  (*-------------------------------------------------------------------------*)
  (* Callgraph and normal methods *)
  (*-------------------------------------------------------------------------*)

    "lookup normal method" >:: (fun () ->
      let file ="
class A { function foo() { } }
class B extends A { }
function b() {
  $o = new B();
  $y = $o->foo();
}
" in
      assert_graph file ["b" --> ["A::foo"]];

      let file = "
class A { function foo() { } }
class B extends A { }
function b() {
  $o = new B();
  $y = $o->unknown();
}
" in
      try 
        let _ = callgraph_generation file in
        assert_failure "it should throw an exception for unknown method"
      with (Abstract_interpreter_php.UnknownMember ("unknown", _, _)) -> ()
    );

    (* I used to have a very simple method analysis that did some gross over
     * approximation. With a call like $x->foo(), the analysis considered
     * any method foo in any class as a viable candidate. We can now
     * do better thx to the abstract interpreter.
     *)
    "method call no over approximation" >:: (fun () ->
        let file = "
class A { function foo() { } }
class B { function foo() { } }
function c() { $a = new A(); $a->foo(); }
" in
        (* no B::foo, no over approximation! *)
        assert_graph file ["c" --> ["A::foo"]];
      );

    "XHP method call" >:: (fun () ->
      let file = "
class :x:frag { public function foo() { } }
function bar() { $x = <x:frag></x:frag>; $x->foo(); }
" in
      assert_graph file ["bar" --> [":x:frag::foo"]];
    );

    (* todo: example of current limitations of the analysis *)

  ]

(*****************************************************************************)
(* Tainting analysis *)
(*****************************************************************************)

(*****************************************************************************)
(* Final suite *)
(*****************************************************************************)
let unittest =
  "static_analysis_php" >::: [
    abstract_interpreter_unittest;
  ]
