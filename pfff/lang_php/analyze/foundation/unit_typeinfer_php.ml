open OUnit

module Ast = Ast_php
module Env = Env_typing_php
module Infer  = Typing_php
module InferH = Typing_helpers_php
module Builtins = Builtins_typed_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* See also tests/php/typing/*.php *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let normalize t =
  let env = Env_typing_php.make_env () in
  InferH.Normalize.normalize env t

let get_signature file =
  let ast = 
    Parse_php.tmp_php_file_from_string file
    |> Parse_php.parse_program
    |> Ast_php_build.program 
  in
  let env = { (Env_typing_php.make_env ()) with Env_typing_php.
    verbose = false;
    strict = true;
  } in
  Builtins.make env;

  Infer.add_defs_code_database_and_update_dependencies env ast;

  match Common2.list_last ast with
  | Ast.FuncDef fd ->
      Infer.func_def env fd;
      normalize (InferH.GEnv.get_fun env (Ast.unwrap fd.Ast.f_name))
  | Ast.ClassDef cd ->
      Infer.class_def env cd;
      normalize (InferH.GEnv.get_class env (Ast.unwrap cd.Ast.c_name))
  | _ -> failwith "last entity in file must be a class or function"

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)
let unittest =
  let n = normalize in
  "type_inference" >::: ([

  (*-------------------------------------------------------------------------*)
  (* Basic *)
  (*-------------------------------------------------------------------------*)

   "basic" >:: (fun () ->
     let t sig_ def = assert_equal sig_ (get_signature def) in

     t (n Env.(fun_ [int] int)) "function f($x) { return $x + 1; }";
     t (n Env.(fun_ [bool] bool)) "function f($x) { return $x === true; }";
   );
    
    "trait" >:: (fun () ->
      let file = "
trait T1 { public function foo() { return 0; } }
trait T2 { public function bar() { return 0; } }
class A { use T1, T2; }
function f() { $a = new A(); return $a->foo(); }
    " in
      assert_equal (n Env.(fun_ [] int)) (get_signature file)
    );

  (*-------------------------------------------------------------------------*)
  (* Error handling *)
  (*-------------------------------------------------------------------------*)

    "use of undefined" >:: (fun () ->

      let file = "
function foo() {
  undefined();
}" in
      try 
      Common.save_excursion Flag_analyze_php.show_errors false (fun () ->
        let _ = get_signature file in
        assert_failure 
          "it should raise exns in strict mode on undefined entities"
      )
      with Infer.UnknownEntity ("undefined") -> ()
    );
  ])
