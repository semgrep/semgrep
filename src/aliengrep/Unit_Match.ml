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

let t = Testo.create

type expectation =
  | Num_matches of int
  | Match_value of string
  | Capture of Pat_compile.metavariable
  | Capture_value of Pat_compile.metavariable * string
  | Not of expectation
[@@deriving show]

let mv kind bare_name : Pat_compile.metavariable = { kind; bare_name }
let match_subs (x : Match.match_) = x.match_loc.substring
let check_num_matches n matches = List.length matches = n

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
        failwith ("failed expectation: " ^ show_expectation expectation))
    expectations

let slconf = Conf.default_singleline_conf
let mlconf = Conf.default_multiline_conf

let test_word () =
  check slconf {|a|} {|a b c|} [ Num_matches 1; Match_value "a" ];
  check slconf {|ab|} {|ab abc|} [ Num_matches 1; Match_value "ab" ];
  check slconf {|ab|} {|ab c ab|} [ Num_matches 2; Match_value "ab" ];
  check slconf {|ab|} {|xabx|} [ Num_matches 0 ]

let test_whitespace () =
  check slconf {|a b|} {|a  b|} [ Num_matches 1; Match_value "a  b" ];
  check slconf {|a  b|} {|a b|} [ Num_matches 1; Match_value "a b" ];
  check slconf "a\nb" "a b" [ Num_matches 0 ];
  check slconf "a b" "a\nb" [ Num_matches 0 ];
  check mlconf "a\nb" "a b" [ Num_matches 1; Match_value "a b" ];
  check mlconf "a b" "a\nb" [ Num_matches 1; Match_value "a\nb" ]

let test_ellipsis () =
  (* test behavior shared with long ellipsis *)
  check slconf {|a...b|} {|a b|} [ Num_matches 1; Match_value "a b" ];
  check slconf {|a...b|} {|a/b|} [ Num_matches 1; Match_value "a/b" ];
  check slconf {|a...b|} {|a x y b|} [ Num_matches 1; Match_value "a x y b" ];
  check slconf {|a...b|} {|a b b|} [ Num_matches 1; Match_value "a b" ];
  check slconf {|a...b b c|} {|a b b b c|}
    [ Num_matches 1; Match_value "a b b b c" ];
  (* test behavior specific to regular ellipsis *)
  check slconf {|a...b|} "a\nb" [ Num_matches 0 ];
  (* in multiline mode, a regular ellipsis matches newlines *)
  check mlconf {|a...b|} "a\nx\nx\nb"
    [ Num_matches 1; Match_value "a\nx\nx\nb" ]

let test_long_ellipsis () =
  (* test behavior shared with regular ellipsis *)
  check slconf {|a....b|} {|a b|} [ Num_matches 1; Match_value "a b" ];
  check slconf {|a....b|} {|a/b|} [ Num_matches 1; Match_value "a/b" ];
  check slconf {|a....b|} {|a x y b|} [ Num_matches 1; Match_value "a x y b" ];
  check slconf {|a....b|} {|a b b|} [ Num_matches 1; Match_value "a b" ];
  check slconf {|a....b b c|} {|a b b b c|}
    [ Num_matches 1; Match_value "a b b b c" ];
  (* test behavior specific to long ellipsis *)
  check slconf {|a....b|} "a\nb" [ Num_matches 1; Match_value "a\nb" ];
  check mlconf {|a....b|} "a\nx\nx\nb"
    [ Num_matches 1; Match_value "a\nx\nx\nb" ]

let test_metavariables () =
  check slconf {|a $X b|} {|a xy b|}
    [
      Num_matches 1;
      Match_value "a xy b";
      Capture (mv Metavariable "X");
      Capture_value (mv Metavariable "X", "xy");
    ];
  check slconf {|a $AB_4! b|} {|a xy! b|}
    [
      Num_matches 1;
      Match_value "a xy! b";
      Capture_value (mv Metavariable "AB_4", "xy");
    ];
  check slconf {|$ X|} {|$ X|}
    [
      Num_matches 1;
      Match_value "$ X";
      Not (Capture (mv Metavariable "X"));
      Not (Capture (mv Metavariable ""));
      Not (Capture (mv Metavariable "$"));
    ];
  check slconf {|$A $B|} {|1 2 3 4|}
    [
      (* at the moment, matches don't overlap -> no match on "2 3" *)
      Num_matches 2;
      (* first match *)
      Match_value "1 2";
      Capture_value (mv Metavariable "A", "1");
      Capture_value (mv Metavariable "B", "2");
      (* other match *)
      Match_value "3 4";
      Capture_value (mv Metavariable "A", "3");
      Capture_value (mv Metavariable "B", "4");
    ]

let test_ellipsis_brackets () =
  check slconf {|x...x|} {|x [x] x|} [ Num_matches 1; Match_value {|x [x] x|} ];
  (* unexpected closing parenthesis is treated as an ordinary character *)
  check slconf {|x...x|} {|x ([)x])x|}
    [ Num_matches 1; Match_value {|x ([)x])x|} ];
  (* nested parentheses *)
  check slconf {|f(...)|} {|f(((x)))|}
    [ Num_matches 1; Match_value {|f(((x)))|} ];
  (* quotes *)
  check slconf {|"..."|} {|"("")"|} [ Num_matches 1; Match_value {|"("")"|} ];
  check slconf {|(...)|} {|(")")|} [ Num_matches 1; Match_value {|(")")|} ];
  check slconf {|x...x|} {|x "'x' x" x x|}
    [ Num_matches 1; Match_value {|x "'x' x" x|} ];
  (* default multiline config doesn't treat quotes as brackets *)
  check mlconf {|(...)|} {|(")")|} [ Num_matches 1; Match_value {|(")|} ]

let test_explicit_brackets () =
  check slconf {|(...)|} {|())|} [ Num_matches 1; Match_value {|()|} ];
  (* Since parentheses are defined as brackets, we're not allowed to reject
     a closing parenthesis that matches the opening parenthesis. *)
  check slconf {|(... x)|} {|()x)|} [ Num_matches 0 ];
  check slconf {|(...)|} {|([])|} [ Num_matches 1; Match_value {|([])|} ];
  (* The behavior of the following test cases is subject to change.
     There's only so much we can do when braces are mismatched. *)
  check slconf {|(...)|} {|([)|} [ Num_matches 1; Match_value {|([)|} ];
  check slconf {|(...)|} {|(])|} [ Num_matches 1; Match_value {|(])|} ];
  check slconf {|(...)|} {|([)]|} [ Num_matches 0 ];
  check slconf {|(...)|} {|[([)]|} [ Num_matches 0 ]

let test_custom_brackets () =
  check mlconf {|(...)|} "((x))" [ Num_matches 1; Match_value "((x))" ];
  check mlconf {|(...)|} "((x ))" [ Num_matches 1; Match_value "((x ))" ];
  check
    { mlconf with brackets = [ ('<', '>') ] }
    {|<...>|} {|<<x>>|}
    [ Num_matches 1; Match_value "<<x>>" ];
  check
    { mlconf with brackets = [ ('<', '>') ] }
    {|<...>|} "<< x >>"
    [ Num_matches 1; Match_value "<< x >>" ];
  check
    { mlconf with brackets = [ ('(', ')'); ('<', '>') ] }
    {|<...>|} {|<(<x>)>|}
    [ Num_matches 1; Match_value "<(<x>)>" ]

let test_backreferences () =
  check slconf {|$A ... $A|} {|a, b, c, a, d|}
    [
      Num_matches 1;
      Match_value {|a, b, c, a|};
      Capture_value (mv Metavariable "A", "a");
    ];
  (* no overlaps -> only 2 matches *)
  check slconf {|$A ... $A ... $A|} {|a x x x a x x x a x x x x a|}
    [
      Num_matches 2;
      Match_value {|a x x x a x x x a|};
      Match_value {|x x x|};
      Capture_value (mv Metavariable "A", "a");
      Capture_value (mv Metavariable "A", "x");
    ];
  (* back-references should not end in the middle of a word *)
  check slconf {|$A ... $A|} {|ab abc|} [ Num_matches 0 ];
  (* word matches should not start in the middle of a word *)
  check slconf {|$A ... $A|} {|abc bc|} [ Num_matches 0 ];
  (* ellipsis extremities that are not words may touch words *)
  check slconf {|... $...A : $...A ...|} {|x+ : +x|}
    [ Num_matches 1; Capture_value (mv Metavariable_ellipsis "A", "+") ];
  (* ellipsis extremities that are not words may touch [specific] words *)
  check slconf {|x $...A : $...A x|} {|x+ : +x|}
    [ Num_matches 1; Match_value {|x+ : +x|} ];
  (* ellipsis extremities that are words may not touch words *)
  check slconf {|... $...A : $...A ...|} {|xy : yx|}
    [ Num_matches 1; Capture_value (mv Metavariable_ellipsis "A", "") ];
  (* ellipsis extremities that are words may not touch [specific] words *)
  check slconf {|x $...A : $...A x|} {|x+ : +x|}
    [
      Num_matches 1;
      Match_value {|x+ : +x|};
      Capture_value (mv Metavariable_ellipsis "A", "+");
    ];
  (* word extremities of ellipsis back-references may not touch words *)
  check slconf {|... $...A : $...A ...|} {|x : xx|}
    [ Num_matches 1; Capture_value (mv Metavariable_ellipsis "A", "") ];
  (* nonword extremities of ellipsis back-references may touch words *)
  check slconf {|... $...A : $...A ...|} {|+ : ++|}
    [ Num_matches 1; Capture_value (mv Metavariable_ellipsis "A", "+") ]

let test_ellipsis_metavariable () =
  check slconf {|[$...ITEMS]|} {|a, [ b, c ], d|}
    [
      Num_matches 1;
      Match_value {|[ b, c ]|};
      Capture_value (mv Metavariable_ellipsis "ITEMS", {|b, c|});
    ];
  (* regular vs. long ellipsis in single-line mode *)
  check slconf {|[$...ITEMS]|} "a, [ b,\nc ], d" [ Num_matches 0 ];
  check slconf {|[$....ITEMS]|} "a, [ b,\nc ], d"
    [
      Num_matches 1;
      Match_value "[ b,\nc ]";
      Capture_value (mv Metavariable_ellipsis "ITEMS", "b,\nc");
    ];
  (* backtracking and back-references *)
  check slconf {|[$...A $...A]|} "[a b a b]"
    [
      Num_matches 1;
      Match_value "[a b a b]";
      Capture_value (mv Metavariable_ellipsis "A", "a b");
    ];
  (* back-references require exact whitespace match, unfortunately *)
  check slconf {|[$...A $...A]|} "[a b a  b]" [ Num_matches 0 ]

(* Demonstrate the use of long ellipsis to match multiple lines
   in single-line mode. *)
let test_skip_lines () =
  check slconf "\na\nb\n" "\na\nb\n" [ Num_matches 1; Match_value "\na\nb\n" ];
  let pat = {|
var $ORIG = ...;
....
var $COPY = $ORIG;
|} in
  let target =
    {|
/* sample code */
var a = 17;
var b = 42;
var c = 77;
var d = b;
var e = "xx";
|}
  in
  check slconf pat target
    [
      Num_matches 1;
      Capture_value (mv Metavariable "ORIG", "b");
      Capture_value (mv Metavariable "COPY", "d");
    ]

let test_left_anchored_ellipses () =
  check slconf "... $A" "!!!\n!!!hello world"
    [ Num_matches 1; Match_value "!!!hello" ];
  check slconf ".... $A" "!!!\n!!!hello world"
    [ Num_matches 1; Match_value "!!!\n!!!hello" ];
  check mlconf "... $A" "!!!\n!!!hello world"
    [ Num_matches 1; Match_value "!!!\n!!!hello" ]

let test_right_anchored_ellipses () =
  check slconf "$A ..." "hello!!!\n!!!"
    [ Num_matches 1; Match_value "hello!!!" ];
  check slconf "$A ...." "hello!!!\n!!!"
    [ Num_matches 1; Match_value "hello!!!\n!!!" ];
  check mlconf "$A ..." "hello!!!\n!!!"
    [ Num_matches 1; Match_value "hello!!!\n!!!" ];
  check mlconf "$A ..." "hello!!!\n!!!"
    [ Num_matches 1; Match_value "hello!!!\n!!!" ]

let test_pure_ellipsis () =
  check slconf "..." "hello\nworld"
    [ Num_matches 2; Match_value "hello"; Match_value "world" ];
  check slconf "...." "hello\nworld"
    [ Num_matches 1; Match_value "hello\nworld" ];
  check mlconf "..." "hello\nworld"
    [ Num_matches 1; Match_value "hello\nworld" ];
  check slconf "...\n..." "a\nb" [ Num_matches 1; Match_value "a\nb" ];
  check slconf "...\n..." "a\nb\nc\n"
    [ Num_matches 2; Match_value "a\nb"; Match_value "c\n" ]

let test_caseless () =
  check mlconf "hello" "HeLLo, world" [ Num_matches 0 ];
  check
    { mlconf with caseless = true }
    "hello" "HeLLo, world"
    [ Num_matches 1; Match_value "HeLLo" ]

let tests =
  Testo.categorize "matching"
    [
      t "word" test_word;
      t "whitespace" test_whitespace;
      t "ellipsis" test_ellipsis;
      t "long ellipsis" test_long_ellipsis;
      t "metavariables" test_metavariables;
      t "ellipsis brackets" test_ellipsis_brackets;
      t "explicit brackets" test_explicit_brackets;
      t "custom brackets" test_custom_brackets;
      t "backreferences" test_backreferences;
      t "ellipsis metavariable" test_ellipsis_metavariable;
      t "skip lines" test_skip_lines;
      t "left-anchored ellipses" test_left_anchored_ellipses;
      t "right-anchored ellipses" test_right_anchored_ellipses;
      t "pure ellipsis" test_pure_ellipsis;
      t "caseless" test_caseless;
    ]
