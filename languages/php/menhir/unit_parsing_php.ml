open Common
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
let tests_path = "tests"

let tests =
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
      t "rejecting bad code" (fun () ->
          Flag.show_parsing_error := false;
          try
            let _ = Parse_php.program_of_string "echo 1+" in
            Alcotest.fail "it should have thrown a Parse_error exception"
          with
          | Parsing_error.Syntax_error _ -> ()
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
           *));
      t "rejecting variadic param with default" (fun () ->
          Flag.show_parsing_error := false;
          try
            let _ =
              Parse_php.program_of_string "function foo($x, ...$rest=123) {}"
            in
            Alcotest.fail "it should have thrown a Parse_error exception"
          with
          | Parsing_error.Syntax_error _ -> ());
      t "rejecting multiple variadic params" (fun () ->
          Flag.show_parsing_error := false;
          try
            let _ =
              Parse_php.program_of_string
                "function foo($x, ...$rest, ...$another) {}"
            in
            Alcotest.fail "it should have thrown a Parse_error exception"
          with
          | Parsing_error.Syntax_error _ -> ());
      t "rejecting non-tail variadic param without variable name" (fun () ->
          Flag.show_parsing_error := false;
          try
            let _ =
              Parse_php.program_of_string "function foo($x, ..., ...$rest) {}"
            in
            Alcotest.fail "it should have thrown a Parse_error exception"
          with
          | Parsing_error.Syntax_error _ -> ());
      t "rejecting ellipsis with optional constructs" (fun () ->
          Flag.show_parsing_error := false;
          try
            let _ = Parse_php.program_of_string "function foo(int ...) {}" in
            Alcotest.fail "it should have thrown a Parse_error exception"
          with
          | Parsing_error.Syntax_error _ -> ());
      t "regression files" (fun () ->
          let dir = Filename.concat tests_path "php/parsing" in
          let files = Common2.glob (spf "%s/*.php" dir) in
          files
          |> List.iter (fun file ->
                 try
                   let _ = Parse_php.parse_program (Fpath.v file) in
                   ()
                 with
                 | Parsing_error.Syntax_error _ ->
                     Alcotest.failf "it should correctly parse %s" file));
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
