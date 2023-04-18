(*
   Unit tests for aliengrep matching a parsed pattern (AST) against target
   code (string).
*)

(* Disable warning 37 [unused-constructor]): constructor Capture is never
   used to build values.
*)
[@@@warning "-37"]

open Printf
open Match

type expectation =
  | Num_matches of int
  | Match
  | Match_value of string
  | Capture of Pat_compile.metavariable
  | Capture_value of Pat_compile.metavariable * string
  | Not of expectation

let match_subs (x : Match.match_) = x.match_loc.substring
let _subs (loc : Match.loc) = loc.substring
let check_num_matches n matches = List.length matches = n
let check_match matches = matches <> []

let check_match_value str matches =
  List.exists (fun x -> match_subs x = str) matches

let check_capture metavariable_name matches =
  List.exists
    (fun match_ ->
      List.exists (fun (name, _) -> name = metavariable_name) match_.captures)
    matches

let check_capture_value metavariable_name metavariable_value matches =
  List.exists
    (fun match_ ->
      List.exists
        (fun (name, loc) ->
          name = metavariable_name && loc.substring = metavariable_value)
        match_.captures)
    matches

let rec check_expectation expectation matches =
  match expectation with
  | Num_matches n -> check_num_matches n matches
  | Match -> check_match matches
  | Match_value str -> check_match_value str matches
  | Capture name -> check_capture name matches
  | Capture_value (name, value) -> check_capture_value name value matches
  | Not x -> not (check_expectation x matches)

(*
   Main function used to run a pattern against a target string and check
   our expectations.
*)
let check conf pat_string target_string expectations =
  printf "--- pattern ---\n%s\n------\n%!" pat_string;
  let pat = Pat_compile.from_string conf pat_string in
  printf "--- compiled pattern ---\n%s\n------\n" (Pat_compile.show pat);
  printf "--- target ---\n%s\n------\n%!" target_string;
  let matches = Match.search pat target_string in
  printf "--- matches ---\n%s\n%!" (Match.show_matches matches);
  List.iter
    (fun expectation ->
      if not (check_expectation expectation matches) then
        failwith "unexpected test result")
    expectations

let uconf = Conf.default_uniline_conf
let mconf = Conf.default_multiline_conf

let test_word () =
  check uconf {|a|} {|a b c|} [ Num_matches 1; Match_value "a" ];
  check uconf {|ab|} {|ab abc|} [ Num_matches 1; Match_value "ab" ];
  check uconf {|ab|} {|ab c ab|} [ Num_matches 2; Match_value "ab" ];
  check uconf {|ab|} {|xabx|} [ Num_matches 0 ]

let test_whitespace () =
  check uconf {|a b|} {|a  b|} [ Num_matches 1; Match_value "a  b" ];
  check uconf {|a  b|} {|a b|} [ Num_matches 1; Match_value "a b" ];
  check uconf "a\nb" "a b" [ Num_matches 0 ];
  check uconf "a b" "a\nb" [ Num_matches 0 ];
  check mconf "a\nb" "a b" [ Num_matches 1; Match_value "a b" ];
  check mconf "a b" "a\nb" [ Num_matches 1; Match_value "a\nb" ]

let test_ellipsis () =
  check uconf {|a...b|} {|a b|} [ Num_matches 1; Match_value "a b" ];
  check uconf {|a...b|} {|a/b|} [ Num_matches 1; Match_value "a/b" ];
  check uconf {|a...b|} {|a x y b|} [ Num_matches 1; Match_value "a x y b" ];
  check uconf {|a...b|} {|a b b|} [ Num_matches 1; Match_value "a b" ]

let tests =
  [
    ("word", test_word);
    ("whitespace", test_whitespace);
    ("ellipsis", test_ellipsis);
  ]
