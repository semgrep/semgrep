open Fpath_.Operators
module Ast = Cst_php
module Flag = Flag_parsing

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let t = Testo.create

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(* ran from the root of the semgrep repository *)
let tests_path = Fpath.v "tests"

let tests =
  let is_syn_err = function
    | Parsing_error.Syntax_error _ -> true
    | _ -> false
  in
  let assert_invalid p =
    Hook.with_hook_set Flag.show_parsing_error false (fun () ->
        Alcotest.match_raises __LOC__ is_syn_err (fun () ->
            ignore (Parse_php.program_of_string p)))
  in

  Testo.categorize "parsing_php"
    [
      (* Parsing *)
      (*-----------------------------------------------------------------------*)
      t "parsing regular code" (fun () ->
          let _ast = Parse_php.program_of_string "echo 1+2;" in
          ());
      (* had such a bug one day ... *)
      t "parsing empty comments" (fun () ->
          let _ast = Parse_php.program_of_string "$a/**/ =1;" in
          ());
      t "rejecting bad code" (fun () -> assert_invalid "echo 1+");
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
      t "rejecting variadic param with default" (fun () ->
          assert_invalid "function foo($x, ...$rest=123) {}");
      t "rejecting multiple variadic params" (fun () ->
          assert_invalid "function foo($x, ...$rest, ...$another) {}");
      t "rejecting non-tail variadic param without variable name" (fun () ->
          assert_invalid "function foo($x, ..., ...$rest) {}");
      t "rejecting ellipsis with optional constructs" (fun () ->
          assert_invalid "function foo(int ...) {}");
      t "regression files" (fun () ->
          let dir = tests_path / "php" / "parsing" in
          let files = Common2.glob (dir / "*.php") in
          files
          |> List.iter (fun file ->
                 try
                   let _ = Parse_php.parse_program file in
                   ()
                 with
                 | Parsing_error.Syntax_error _ ->
                     Alcotest.failf "it should correctly parse %a" Fpath.pp file));
      (*-----------------------------------------------------------------------*)
      (* Types *)
      (*-----------------------------------------------------------------------*)
      t "sphp" (fun () ->
          let t x =
            try
              let _ = Parse_php.program_of_string x in
              ()
            with
            | Parsing_error.Syntax_error _ ->
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
          t "class A { public static function foo<T>(): ?int { } }");
    ]
