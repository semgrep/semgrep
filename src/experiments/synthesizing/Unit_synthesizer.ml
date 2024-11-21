(*s: semgrep/matching/Unit_matcher.ml *)
open Common
open Fpath_.Operators
module G = AST_generic
module PPG = Pretty_print_AST

let t = Testo.create

(*****************************************************************************)
(* Semgrep Unit tests *)
(*****************************************************************************)
(* ran from the root of the semgrep repository *)
let test_path = Fpath.v "tests/synthesizing"

(* Format: file, range of code to infer, expected patterns *)
let python_tests =
  [
    ( "arrays_and_funcs.py",
      "3:3-3:23",
      [
        ("exact match", "a.bar(f(x), y == f(x))");
        ("dots", "a.bar(...)");
        ("metavars", "a.bar($X, $Y, ...)");
        ("exact metavars", "a.bar($X, $Y)");
        ("deep metavars", "a.bar(f($X), $Y == f($X))");
      ] );
    ( "arrays_and_funcs.py",
      "4:9-4:29",
      [
        ("exact match", "metrics.send(\"my-report-id\")");
        ("dots", "metrics.send(...)");
        ("metavars", "metrics.send(\"...\", ...)");
        ("exact metavars", "metrics.send(\"...\")");
      ] );
    (* TODO: adjust range, this changed after we added parens for tuples
       ("arrays_and_funcs.py", "5:4-5:11", [ ("exact match", "(hi, my)") ]);
       ("arrays_and_funcs.py", "6:1-6:15", [ ("exact match", "(hi, my, bye)") ]);
    *)
    ("arrays_and_funcs.py", "7:3-7:7", [ ("exact match", "A[1]") ]);
    ("arrays_and_funcs.py", "8:3-8:8", [ ("exact match", "A[-(1)]") ]);
    ("arrays_and_funcs.py", "9:3-9:9", [ ("exact match", "A[1:4]") ]);
    ("arrays_and_funcs.py", "10:3-10:12", [ ("exact match", "A[1:4:-(1)]") ]);
    ("arrays_and_funcs.py", "11:3-11:10", [ ("exact match", "A[::-(1)]") ]);
    ("arrays_and_funcs.py", "12:3-12:8", [ ("exact match", "A[1:]") ]);
    ( "arrays_and_funcs.py",
      "13:3-13:14",
      [ ("exact match", "1 == 1"); ("exact metavars", "$X == $X") ] );
    ( "arrays_and_funcs.py",
      "14:3-14:7",
      [ ("exact match", "true"); ("metavar", "$X") ] );
    ( "arrays_and_funcs.py",
      "15:3-15:6",
      [ ("exact match", "3.3"); ("metavar", "$X") ] );
    ( "arrays_and_funcs.py",
      "16:3-16:12",
      [ ("exact match", "self.data"); ("metavar", "$X") ] );
    ( "arrays_and_funcs.py",
      "17:3-17:36",
      [ ("exact match", "\"nice\" if is_nice else \"not nice\"") ] );
    ( "arrays_and_funcs.py",
      "18:3-18:34",
      [
        ("exact match", "f(a, b(g(a, k)), c, c(k), a, c)");
        ("dots", "f(...)");
        ("metavars", "f($X, $Y, $Z, $A, $X, $Z, ...)");
        ("exact metavars", "f($X, $Y, $Z, $A, $X, $Z)");
        ("deep metavars", "f($X, b(g($X, $Y)), $Z, c($Y), $X, $Z)");
      ] );
    ( "arrays_and_funcs.py",
      "19:6-19:27",
      [ ("exact match", "node.id == node.id"); ("exact metavars", "$X == $X") ]
    );
    ( "set_cookie.py",
      "5:10-7:35",
      [
        ( "exact match",
          "flask.response.set_cookie(\"sessionid\", \
           generate_cookie_value(\"RANDOM-UUID\"), secure=True)" );
        ("dots", "flask.response.set_cookie(...)");
        ("metavars", "flask.response.set_cookie(\"...\", $Y, secure=$Z, ...)");
        ("exact metavars", "flask.response.set_cookie(\"...\", $Y, secure=$Z)");
        ( "deep metavars",
          "flask.response.set_cookie(\"...\", generate_cookie_value(\"...\"), \
           secure=$Z)" );
      ] );
    ( "set_cookie.py",
      "8:3-8:31",
      [
        ("exact match", "a = set_cookie(1234, b, 123)");
        ("dots", "a = ...");
        ("metavars", "$X = $Y");
        ("righthand dots", "$X = set_cookie(...)");
        ("righthand metavars", "$X = set_cookie($Y, $Z, $A, ...)");
        ("righthand exact metavars", "$X = set_cookie($Y, $Z, $A)");
      ] );
    ( "import_as.py",
      "2:0-2:25",
      [
        ("exact match", "exec(user_input)");
        ("dots", "exec(...)");
        ("metavars", "exec($X, ...)");
        ("exact metavars", "exec($X)");
      ] );
  ]

let java_tests =
  [
    ( "typed_funcs.java",
      "6:8-6:14",
      [
        ("exact match", "foo(a);");
        ("dots", "foo(...);");
        ("metavars", "foo($X, ...);");
        ("exact metavars", "foo($X);");
        ("typed metavars", "foo((int $X));");
      ] );
    ( "typed_funcs.java",
      "7:8-7:42",
      [
        ("exact match", "foo(bar(car(a)), b, foo(b, c), d);");
        ("dots", "foo(...);");
        ("metavars", "foo($X, $Y, $Z, $A, ...);");
        ("exact metavars", "foo($X, $Y, $Z, $A);");
        ( "typed metavars",
          "foo(bar(car((int $X))), (String $Y), foo((String $Y), (bool $Z)), \
           $A);" );
        ("deep metavars", "foo(bar(car($X)), $Y, foo($Y, $Z), $A);");
      ] );
    ( "typed_funcs.java",
      "8:8-8:26",
      [
        ("exact match", "foo(foo(a, b), c);");
        ("dots", "foo(...);");
        ("metavars", "foo($X, $Y, ...);");
        ("exact metavars", "foo($X, $Y);");
        ("typed metavars", "foo(foo((int $X), (String $Y)), (bool $Z));");
        ("deep metavars", "foo(foo($X, $Y), $Z);");
      ] );
    ( "typed_funcs.java",
      "6:12-6:14",
      [ ("exact match", "a"); ("metavar", "$X"); ("typed metavar", "(int $X)") ]
    );
    ( "typed_funcs.java",
      "10:8-10:30",
      [
        ("exact match", "System.out.print(\"A\");");
        ("dots", "System.out.print(...);");
        ("metavars", "System.out.print(\"...\", ...);");
        ("exact metavars", "System.out.print(\"...\");");
      ] );
    ( "typed_funcs.java",
      "11:20-11:47",
      [ ("exact match", "new Scanner(new File(), 1)") ] );
    ( "typed_funcs.java",
      "12:10-12:38",
      [
        ("exact match", "foo(bar(a)) == foo(bar(a))");
        ("exact metavars", "$X == $X");
        ("typed metavars", "foo(bar((int $X))) == foo(bar((int $X)))");
        ("deep metavars", "foo(bar($X)) == foo(bar($X))");
      ] )
    (* "exact_match.java", "4:15-4:42",
       ["exact match", "foo(bar(a)) == foo(bar(a))";
       "exact metavars", "$X == $X";
       "typed metavars", "foo(bar((int $X))) == foo(bar((int $X)))";
       "deep metavars", "foo(bar($X)) == foo(bar($X))";
       ]; *);
  ]

(* Cases splits up the test cases by language.
 * For each item in tests, this expects a filename, range, and solutions.
 * Patterns will be inferred from the file at filename in the given range.
 * The list of patterns will be checked against the expected solutions,
 * which they should match exactly.
 * They will then be matched against the code at the given range to make
 * sure semgrep actually correctly matches the pattern to the code.
 * Place test files in tests/synthesizing
 *)

let tests =
  [
    t "pattern inference features" (fun () ->
        let cases = [ (Lang.Python, python_tests); (Lang.Java, java_tests) ] in
        cases
        |> List.iter (fun (lang, tests) ->
               tests
               |> List.iter (fun (filename, range, sols) ->
                      let file = test_path / filename in
                      let config = Rule_options.default in

                      (* pattern candidates (as strings) *)
                      let pats =
                        Synthesizer.synthesize_patterns config range file
                      in
                      (* the code *)
                      let ast =
                        Parse_target.parse_and_resolve_name_fail_if_partial lang
                          file
                      in
                      (* BUG? resolve again? already done above *)
                      Naming_AST.resolve lang ast;

                      let r = Range.range_of_linecol_spec range file in

                      let check_pats (str, pat) =
                        try
                          (* the pattern AST *)
                          let pattern =
                            match Parse_pattern.parse_pattern lang pat with
                            | Ok pat -> pat
                            | Error s ->
                                failwith (spf "problem parsing %s: %s" pat s)
                          in

                          (* extracting the code at the range *)
                          let e_opt = Range_to_AST.any_at_range_first r ast in
                          match e_opt with
                          | Some any ->
                              let code =
                                match (pattern, any) with
                                | G.E _, G.S { G.s = G.ExprStmt (e, _); _ } ->
                                    G.E e
                                | _, x -> x
                              in
                              let matches_with_env =
                                let env =
                                  Matching_generic.environment_of_any lang
                                    Rule_options.default code
                                in
                                Match_patterns.match_any_any pattern code env
                              in
                              (* Debugging note: uses pattern_to_string for convenience,
                               * but really should match the code in the given file at
                               * the given range *)
                              if matches_with_env =*= [] then (
                                UCommon.pr2 str;
                                UCommon.pr2 (AST_generic.show_any pattern);
                                UCommon.pr2 (AST_generic.show_any code));
                              Alcotest.(check bool)
                                (spf "pattern:|%s| should match |%s" pat
                                   (Pretty_print_pattern.pattern_to_string lang
                                      code))
                                true (matches_with_env <> [])
                          | None ->
                              failwith
                                (spf "Couldn't find range %s in %s" range !!file)
                        with
                        | Parsing.Parse_error ->
                            failwith (spf "problem parsing %s" pat)
                      in
                      pats |> List.iter check_pats;
                      let pats_str =
                        List.fold_left
                          (fun s (s1, s2) -> s ^ s1 ^ ": " ^ s2 ^ "\n")
                          "" pats
                      in
                      Alcotest.(check bool)
                        ("Patterns do not match solution, where inferred \
                          patterns are:\n" ^ pats_str)
                        true (pats =*= sols))));
  ]
