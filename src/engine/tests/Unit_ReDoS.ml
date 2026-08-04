(*
   Copyright (c) 2022-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Unit tests for ReDoS
*)

open Printf

let t = Testo.create

let escaped_strings =
  [
    (* input, expected output *)
    ({||}, {||});
    ({|a|}, {|a|});
    ({|'|}, {|'|});
    ({|"|}, {|"|});
    ({|\'|}, {|'|});
    ({|\"|}, {|"|});
    ({|\\|}, {|\|});
    ({|ab|}, {|ab|});
    ({|\\\|}, {|\\|});
    ({|"\\\'|}, {|"\'|});
    ({|''|}, {|''|});
    ({|""|}, {|""|});
    ({|'ab'|}, {|'ab'|});
    ({|'a"b'|}, {|'a"b'|});
    ({|"a'b"|}, {|"a'b"|});
  ]

let test_unescape () =
  escaped_strings
  |> List.iter (fun (input, expected_output) ->
      let output = String_literal.approximate_unescape input in
      Alcotest.(check string)
        ("String_literal.approximate_unescape " ^ input)
        expected_output output)

type result = Succeeds | Blows_up

let string_of_result = function
  | Succeeds -> "succeeds"
  | Blows_up -> "blows up"

(* Test inputs *)
let aa = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa!"
let ab = "abababababababababababababababababababababababab!"

(*
   See
   https://owasp.org/www-community/attacks/Regular_expression_Denial_of_Service_-_ReDoS

   These are the expectations using PCRE2 (with our wrapper's match/depth
   limits; see Pcre2_.match_limit and Pcre2_.depth_limit). Other regexp
   libraries differ in where they succeed or blow up. To test a regexp with
   NodeJS, you can do this:

   $ js
   > console.log(/^(a+)*$/.test('aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa!'));
   ^C
*)
let pattern_expectations =
  [
    (* pattern, input, PCRE2 result, NodeJS result, prediction *)
    ("", aa, Succeeds, Succeeds, Succeeds);
    ("a+$", aa, Succeeds, Succeeds, Succeeds);
    ("(a+)+", aa, Succeeds, Succeeds, Succeeds);
    ("(a?)*$", aa, Succeeds, Succeeds, Succeeds);
    ("(a|b)*$", aa, Succeeds, Succeeds, Succeeds);
    ("(a|ab)*$", aa, Succeeds, Succeeds, Succeeds);
    ("(a|ab)*$", ab, Succeeds, Succeeds, Succeeds);
    ("(a|aa)*$", aa, Blows_up, Blows_up, Succeeds) (* TODO *);
    ("(a|a)*$", aa, Blows_up, Blows_up, Succeeds) (* TODO: not useful *);
    ("([ab]|[ac])*$", aa, Blows_up, Blows_up, Succeeds) (* TODO: hard *);
    ("(aa?)*$", aa, Blows_up, Blows_up, Succeeds) (* TODO *);
    ("(a{1,2})*$", aa, Blows_up, Blows_up, Blows_up);
    ("(a*)*$", aa, Blows_up, Blows_up, Blows_up);
    ("(a*?)*$", aa, Blows_up, Blows_up, Blows_up);
    ("(a*)*?$", aa, Blows_up, Blows_up, Blows_up);
    ("(a+)*$", aa, Blows_up, Blows_up, Blows_up);
    ("(a+)+$", aa, Blows_up, Blows_up, Blows_up);
    ("(a++)+$", aa, Succeeds, Succeeds (* unsupported by js *), Succeeds);
    ("(a+)++$", aa, Succeeds, Succeeds (* unsupported by js *), Succeeds);
    ("(a|a?)+$", aa, Blows_up, Blows_up, Succeeds) (* TODO *);
    ("(.*a){10}$", aa, Blows_up, Blows_up, Blows_up);
    ("(a*a){20}$", aa, Blows_up, Blows_up, Blows_up);
    ( "(.*a)(.*a)(.*a)(.*a)(.*a)(.*a)(.*a)(.*a)(.*a)(.*a)$",
      aa,
      Blows_up,
      Blows_up,
      Succeeds )
    (* TODO: not useful *);
    ("(aa+)*$", aa, Blows_up, Blows_up, Blows_up);
    ("(a+a)*$", aa, Blows_up, Blows_up, Blows_up);
    ("(aa+a)*$", aa, Blows_up, Blows_up, Blows_up);
    ("(aa*)*$", aa, Blows_up, Blows_up, Blows_up);
    ("(a*a)*$", aa, Blows_up, Blows_up, Blows_up);
    ("(aa*a)*$", aa, Blows_up, Blows_up, Blows_up);
    ("([ab]|ab|ba)+$", ab, Blows_up, Blows_up, Succeeds) (* TODO: hard *);
    (* shortened email validation regexp from
       https://regexlib.com/REDetails.aspx?regexp_id=1757&AspxAutoDetectCookieSupport=1
       blows up with NodeJS but not with PCRE. <shrug>
    *)
    ( "^([a-zA-Z0-9])(([-.]|[_]+)?([a-zA-Z0-9]+))*@$",
      aa,
      Succeeds,
      Blows_up,
      Blows_up );
    (* Fixed-up version using possessive quantifiers *)
    ( "^([a-zA-Z0-9])(([-.]|[_]++)?+([a-zA-Z0-9]++))*+@$",
      aa,
      Succeeds,
      Succeeds (* unsupported by js *),
      Succeeds );
  ]

let worse_of a b =
  match (a, b) with
  | Succeeds, Succeeds -> Succeeds
  | _ -> Blows_up

let print_expectations () =
  (* confusion matrix for the prediction of "blows up" *)
  let tp = ref 0 in
  let fp = ref 0 in
  let fn = ref 0 in
  let tn = ref 0 in
  pattern_expectations
  |> List.iter (fun (pat, _input, pcre2_res, js_res, prediction) ->
      let actual = worse_of pcre2_res js_res in
      let count =
        match (actual, prediction) with
        | Blows_up, Blows_up -> tp
        | Succeeds, Blows_up -> fp
        | Blows_up, Succeeds -> fn
        | Succeeds, Succeeds -> tn
      in
      incr count;
      printf "%s : %s, prediction %s\n" pat (string_of_result actual)
        (if actual = prediction then "agrees" else "disagrees"));
  printf "Confusion matrix:\n  TP: %i\n  FP: %i\n  FN: %i\n  TN: %i\n%!" !tp !fp
    !fn !tn

let test_pcre2_pattern_explosion ~pat ~input expected =
  let rex = Pcre2_.regexp pat in
  let res =
    match Pcre2_.pmatch ~rex input with
    | Ok _ -> Succeeds
    | Error MatchLimit
    | Error DepthLimit ->
        Blows_up
    | Error err ->
        Alcotest.failf "unexpected PCRE2 error on %S: %s" pat
          (Pcre2_.show_error err)
  in
  printf "pattern: '%s' %s\n%!" pat (string_of_result res);
  Alcotest.(check string)
    "equal"
    (string_of_result expected)
    (string_of_result res)

let test_pcre2_pattern_explosions () =
  print_expectations ();
  List.iter
    (fun (pat, input, pcre2_expected, _, _) ->
      test_pcre2_pattern_explosion ~pat ~input pcre2_expected)
    pattern_expectations

let test_vulnerability_prediction () =
  print_expectations ();
  List.iter
    (fun (pat, _, _, _, expected_prediction) ->
      let prediction =
        match ReDoS.find_vulnerable_subpatterns pat with
        | Error () -> Alcotest.fail (sprintf "cannot parse regexp '%s'" pat)
        | Ok [] -> Succeeds
        | Ok subpatterns ->
            subpatterns
            |> List.iter (fun pat ->
                printf "found vulnerable subpattern: %s\n" pat);
            Blows_up
      in
      printf "pattern: %s\n  expected prediction: %s\n  prediction: %s\n%!" pat
        (string_of_result expected_prediction)
        (string_of_result prediction);
      Alcotest.(check string)
        "equal"
        (string_of_result expected_prediction)
        (string_of_result prediction))
    pattern_expectations

let tests =
  [
    t "unescape" test_unescape;
    t "pcre2 pattern explosion" test_pcre2_pattern_explosions;
    t "vulnerability prediction" test_vulnerability_prediction;
  ]
