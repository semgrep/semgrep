(*s: unit_parsing_php.ml *)
open Common

open Cst_php
module Ast = Cst_php
module Flag = Flag_parsing

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* old:
 * Back when the PHP parser was quite fragile we used to do some error
 * recovery in case of a parse error, and instead of failing hard we
 * were returning a NotParsedCorrectly toplevel element. Now
 * we fail hard because the PHP parser is better. So the function below
 * is not useful anymore:
 *
 * let assert_no_parser_error ast =
 *  assert_bool "bad: have a NotParsedCorrectly"
 *  (List.for_all (function NotParsedCorrectly _ -> false | _ -> true) ast);
 *  ()
*)

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let tests =
  Testutil.pack_tests "parsing_php" [

    (*-----------------------------------------------------------------------*)
    (* Lexing *)
    (*-----------------------------------------------------------------------*)

    "lexing regular code", (fun () ->
      let toks = Parse_php.tokens_of_string "echo 1+2;" in
      Alcotest.(check bool)
        "it should have a Echo token"
        true
        (toks |> List.exists (function
             Parser_php.T_ECHO _ -> true | _ -> false));
    );

    "lexing and case sensitivity", (fun () ->
      let toks = Parse_php.tokens_of_string
          "function foo() { echo __function__; }" in
      Alcotest.(check bool)
        "it should have a __FUNCTION__ token"
        true
        (toks |> List.exists (function
             Parser_php.T_FUNC_C _ -> true | _ -> false));
    );

    (*-----------------------------------------------------------------------*)
    (* Parsing *)
    (*-----------------------------------------------------------------------*)

    "parsing regular code", (fun () ->
      let _ast = Parse_php.program_of_string "echo 1+2;" in
      ()
    );
    (* had such a bug one day ... *)
    "parsing empty comments", (fun () ->
      let _ast = Parse_php.program_of_string "$a/**/ =1;" in
      ()
    );

    "rejecting bad code", (fun () ->
      Flag.show_parsing_error := false;
      try
        let _ = Parse_php.program_of_string "echo 1+" in
        Alcotest.fail "it should have thrown a Parse_error exception"
      with
        Parse_info.Parsing_error _ ->
          ()
          (* old:
           * The PHP parser does not return an exception when a PHP file contains
           * an error, to allow some form of error recovery by not stopping
           * at the first mistake. Instead it returns a NotParsedCorrectly
           * AST toplevel element for parts of the code that were not parsed.
           * Here we check that correctly formed code do not contain such
           * NotParsedCorrectly element.
           *
           *  assert_bool "bad: should have a NotParsedCorrectly"
           * (List.exists (function NotParsedCorrectly _ -> true | _ -> false) ast)
          *)
    );

    "rejecting variadic param with default", (fun () ->
      Flag.show_parsing_error := false;
      try
        let _ = Parse_php.program_of_string "function foo($x, ...$rest=123) {}" in
        Alcotest.fail "it should have thrown a Parse_error exception"
      with
        Parse_info.Parsing_error _ ->
          ()
    );

    "rejecting multiple variadic params", (fun () ->
      Flag.show_parsing_error := false;
      try
        let _ = Parse_php.program_of_string "function foo($x, ...$rest, ...$another) {}" in
        Alcotest.fail "it should have thrown a Parse_error exception"
      with
        Parse_info.Parsing_error _ ->
          ()
    );
    "rejecting non-tail variadic param without variable name", (fun () ->
      Flag.show_parsing_error := false;
      try
        let _ = Parse_php.program_of_string "function foo($x, ..., ...$rest) {}" in
        Alcotest.fail "it should have thrown a Parse_error exception"
      with
        Parse_info.Parsing_error _ ->
          ()
    );

    "rejecting ellipsis with optional constructs", (fun () ->
      Flag.show_parsing_error := false;
      try
        let _ = Parse_php.program_of_string "function foo(int ...) {}" in
        Alcotest.fail "it should have thrown a Parse_error exception"
      with
        Parse_info.Parsing_error _ ->
          ()
    );

    "regression files", (fun () ->
      let dir = Config_pfff.tests_path "php/parsing" in
      let files = Common2.glob (spf "%s/*.php" dir) in
      files |> List.iter (fun file ->
        try
          let _ = Parse_php.parse_program file in
          ()
        with Parse_info.Parsing_error _ ->
          Alcotest.failf "it should correctly parse %s" file
      )
    );

    (*-----------------------------------------------------------------------*)
    (* Types *)
    (*-----------------------------------------------------------------------*)

    "sphp", (fun () ->
      let t x =
        try
          let _ = Parse_php.program_of_string x in
          ()
        with Parse_info.Parsing_error _ ->
          Alcotest.failf "it should correctly parse %s" x
      in

      t "class A<T> { }";
      t "class A<T1, T2> { }";
      t "trait A<T1, T2> { }";
      t "interface A<T1, T2> { }";
      t "class A<T> extends B<int> { }";
      t "interface A extends B<int>, C {}";
      t "class A { use B<int>; }";
      t "function foo(): int { }";
      t "class A { public function foo(): int { }}";
      t "function foo(mixed $x): int { }";
      t "function foo(): void { }";
      t "function id<T>(T $x): T { return $x; }";
      t "function id((A, B) $x): T { return $x; }";
      t "function id(?(A, B) $x): ?int { return $x; }";
      t "function id( (function(?A) : int) $x): int { return $x; }";
      t "function id( (function() : int) $x): int { }";
      t "function test(int $x) { return 0; }";
      t "class A { private ?(int, int) $x; }";
      t "class A { const ?A<T1, T2> X = 0; }";
      t "$x = function(): ?int { return null; };";
      t "function foo(A<A<int>> $x): ?int { return null; };";
      t "class A { public static function foo<T>(): ?int { } }";
    );

    (*-----------------------------------------------------------------------*)
    (* Misc *)
    (*-----------------------------------------------------------------------*)

    (* Check that the visitor implementation correctly visit all AST
     * subelements, even when they are deep inside the AST tree (e.g.
     * sub-sub expressions inside parenthesis).
    *)
(*
    "visitor", (fun () ->
      let ast = Parse_php.program_of_string "echo 1+2+(3+4);" in

      let cnt = ref 0 in
      (* This is very tricky. See docs/manual/Parsing_php.pdf section
       * 2.1.2 for a tutorial on visitors in OCaml. *)
      let hooks = { Visitor_php.default_visitor with
        Visitor_php.kexpr = (fun (k, _) e ->
          match e with
          | Sc _ -> incr cnt
          | _ -> k e
        )
      }
      in
      let visitor = Visitor_php.mk_visitor hooks in
      visitor (Program ast);
      assert_equal 4 !cnt ;
    );
*)
    "checking column numbers", (fun () ->

      (* See bug reported by dreiss, because the lexer had a few todos
       * regarding objects. *)
      let e = Parse_php.expr_of_string "$o->foo" in
      match e with
      | ObjGet (_v, _tok, Id name) ->
          let info = Ast.info_of_name name in
          Alcotest.(check int) "same values" 4 (Parse_info.col_of_info info)
      | _ ->
          Alcotest.fail "not good AST"
    );

    (*-----------------------------------------------------------------------*)
    (* Sgrep *)
    (*-----------------------------------------------------------------------*)
(*
    "parsing sgrep expressions", (fun () ->

      let _e = Parse_php.any_of_string "debug_rlog(1)" in
      assert_bool "it should not generate an error" true;
      let _e = Parse_php.any_of_string "debug_rlog(X)" in
      assert_bool "it should not generate an error" true;
      let _e = Parse_php.any_of_string "debug_rlog(X, 0)" in
      assert_bool "it should not generate an error" true;

      (try
        let _e =
          Common.save_excursion Flag.show_parsing_error false (fun () ->
            Parse_php.any_of_string "debug_rlog(X, 0"
          )
        in
        Alcotest.fail "it should generate an error"
      with _exn ->
        ()
      );
    );

    "parsing sgrep patterns", (fun () ->
      let any = Parse_php.any_of_string "foo();" in
      let ok = match any with
       | Toplevel(StmtList[ExprStmt( _)]) -> true
       | _ -> false
      in
      assert_bool "it should be the AST of a statement" ok;
      let any = Parse_php.any_of_string "foo()" in
      let ok = match any with Expr(_) -> true | _ -> false in
      assert_bool "it should be the AST of an expression" ok;
      let any = Parse_php.any_of_string "<x:frag>x</x:frag>" in
      let ok = match any with Expr(_) -> true | _ -> false in
      assert_bool "it should be the AST of an expression" ok;

    );
*)
    (* todo:
     *  - ? sexp and json output
     *  - ? correctness of Ast (too many cases)
    *)
  ]
(*e: unit_parsing_php.ml *)
