(*s: semgrep/matching/Unit_matcher.ml *)
open OUnit

(*****************************************************************************)
(* Semgrep Unit tests *)
(*****************************************************************************)
let test_path = "../../../tests/SYNTHESIZING/"

let python_tests = [
  "arrays_and_funcs.py", "3:3-3:23",
  ["exact match", "a.bar(f(x), y == f(x))";
   "dots", "a.bar(...)";
   "metavars", "a.bar($X, $Y, ...)";
   "exact metavars", "a.bar($X, $Y)";
   "deep metavars", "a.bar(f($X), $Y == f($X))"
  ];

  "arrays_and_funcs.py", "4:9-4:29",
  ["exact match", "metrics.send('my-report-id')";
   "dots", "metrics.send(...)";
   "metavars", "metrics.send($X, ...)";
   "exact metavars", "metrics.send($X)"
  ];

  "arrays_and_funcs.py", "5:4-5:11",
  ["exact match", "(hi, my)"];

  "arrays_and_funcs.py", "6:1-6:14",
  ["exact match", "(hi, my, bye)"];

  "arrays_and_funcs.py", "7:3-7:7",
  ["exact match", "A[1]"];

  "arrays_and_funcs.py", "8:3-8:8",
  ["exact match", "A[-(1)]"];

  "arrays_and_funcs.py", "9:3-9:9",
  ["exact match", "A[1:4]"];

  "arrays_and_funcs.py", "10:3-10:12",
  ["exact match", "A[1:4:-(1)]"];

  "arrays_and_funcs.py", "11:3-11:10",
  ["exact match", "A[::-(1)]"];

  "arrays_and_funcs.py", "12:3-12:8",
  ["exact match", "A[1:]"];

  "arrays_and_funcs.py", "13:3-13:14",
  ["exact match", "1 == 1";
   "exact metavars", "$X == $X"];

  "arrays_and_funcs.py", "14:3-14:7",
  ["exact match", "true";
   "metavar", "$X"];

  "arrays_and_funcs.py", "15:3-15:6",
  ["exact match", "3.3";
   "metavar", "$X"];

  "arrays_and_funcs.py", "16:3-16:12",
  ["exact match", "self.data";
   "metavar", "$X"];

  "arrays_and_funcs.py", "17:3-17:36",
  ["exact match", "'nice' if is_nice else 'not nice'"];

  "arrays_and_funcs.py", "18:3-18:34",
  ["exact match", "f(a, b(g(a, k)), c, c(k), a, c)";
   "dots", "f(...)";
   "metavars", "f($X, $Y, $Z, $A, $X, $Z, ...)";
   "exact metavars", "f($X, $Y, $Z, $A, $X, $Z)";
   "deep metavars", "f($X, b(g($X, $Y)), $Z, c($Y), $X, $Z)"
  ];

  "arrays_and_funcs.py", "19:3-19:32",
  ["exact match", "node.id == node.id";
   "exact metavars", "$X == $X"];
]

let unittest =
  "pattern inference features" >:: (fun () ->
    let cases = python_tests
    in
    cases |> List.iter (fun (file, range, sols) ->
        let pats = Synthesizer.synthesize_patterns range (test_path ^ file) in
        let check_pats = () (* todo check that they parse *) in
        check_pats;
        assert(pats = sols)
    )
  )
(*
(*s: function [[Unit_matcher.unittest]] *)
let unittest ~any_gen_of_string =
  "sgrep(generic) features" >:: (fun () ->

    (* spec: pattern string, code string, should_match boolean *)
    let cases = [
          "ex1.py", "4:9-4:29"
    ]
    in
    let check_pattern pat (file, range) =
      (* TODO: Semgrep_generic.match_any_file_range is a hypothetical function that
      checks whether pattern matches the file at the specified range *)
      let matches_with_env = Semgrep_generic.match_any_file_range pattern (file, range) in
        assert_bool (spf "pattern:|%s| should match |%s" spattern scode)
          (matches_with_env <> [])
    in
    cases |> List.iter (fun (file, range) ->
      let patterns = Synthesizer.synthesize_pattern range file in
      List.map (fun (label, pat) -> check_pattern pat (file, range)) patterns
  ) *)
(*e: function [[Unit_matcher.unittest]] *)
(*e: semgrep/matching/Unit_matcher.ml *)
