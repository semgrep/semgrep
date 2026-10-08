(*
   Copyright (c) 2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Unit tests for Regex
*)

let t = Testo.create

let test_match_limit_ok () =
  match Pcre2_.compile "(a+)+$" with
  | Error e -> Alcotest.fail ("unexpected error: " ^ Pcre2.show_compile_error e)
  | Ok re -> (
      (* This small input must not trip a PCRE2 error (e.g. a match limit); the
         pattern itself does not match, so [Ok false] is expected and fine. *)
      match Pcre2_.is_match re "aaaaaaaaaaaaaaaaa!" with
      | Ok _ -> ()
      | Error e ->
          Alcotest.fail ("unexpected error: " ^ Pcre2.show_match_error e))

let test_compile_failure () =
  match Pcre2_.compile "???" with
  | Error Pcre2.{ code = QUANTIFIER_INVALID; _ } -> ()
  | Ok _ -> Alcotest.fail "should have failed to compile the regexp"
  | Error e -> Alcotest.fail ("wrong error: " ^ Pcre2.show_compile_error e)

let test_quote () =
  Alcotest.(check string)
    "metacharacters are escaped" {|a\.b\[c\]\\d\-\$|}
    (Pcre2_.quote {|a.b[c]\d-$|});
  let plain = Bytes.of_string "literal/path_123" |> Bytes.to_string in
  let quoted = Pcre2_.quote plain in
  Alcotest.(check string) "plain string is unchanged" plain quoted;
  Alcotest.(check bool)
    "plain string is returned without allocation" true (plain == quoted)

let test_replace () =
  let rex = Pcre2_.compile_exn {|(x+)-(y+)?|} in
  let replace ?limit template subject =
    match Pcre2_.replace ?limit rex ~template subject with
    | Ok result -> result
    | Error e -> Alcotest.fail (Pcre2.show_match_error e)
  in
  Alcotest.(check string)
    "capture reference syntaxes" {|<xx|xx|xx-|$|||$>|}
    (replace {|<\1|$1|\0|$$|\2|$9|$>|} "xx-");
  Alcotest.(check string)
    "replace all" "<x|y> <xx|>"
    (replace {|<$1|$2>|} "x-y xx-");
  Alcotest.(check string)
    "limit" "<x|y> xx-"
    (replace ~limit:1 {|<$1|$2>|} "x-y xx-");
  Alcotest.(check string)
    "no match leaves the subject unchanged" "unchanged"
    (replace "replacement" "unchanged");
  Alcotest.(check string)
    "nonempty alternative after an empty match" "_a__b_"
    (let rex = Pcre2_.compile_exn " *" in
     match Pcre2_.replace rex ~template:"_" "a  b" with
     | Ok result -> result
     | Error e -> Alcotest.fail (Pcre2.show_match_error e))

let test_replace_fn () =
  let replace ?options ?subject_offset ?limit rex f subject =
    match Pcre2_.replace_fn ?options ?subject_offset ?limit rex f subject with
    | Ok result -> result
    | Error e -> Alcotest.fail (Pcre2.show_match_error e)
  in
  let rex = Pcre2_.compile_exn "x+" in
  Alcotest.(check string)
    "all matches" "a<xx> <x>"
    (replace rex (Printf.sprintf "<%s>") "axx x");
  Alcotest.(check string)
    "limit" "a<xx> x"
    (replace ~limit:1 rex (Printf.sprintf "<%s>") "axx x");
  Alcotest.(check string)
    "subject offset preserves prefix" "x-<x>"
    (replace ~subject_offset:2 rex (Printf.sprintf "<%s>") "x-x");
  Alcotest.(check string)
    "no match leaves the subject unchanged" "abc"
    (replace rex String.uppercase_ascii "abc");
  Alcotest.(check string)
    "zero-width matches" "_x_x"
    (replace (Pcre2_.compile_exn {|(?=x)|}) (fun _ -> "_") "xx");
  Alcotest.(check string)
    "match options are forwarded" "x"
    (replace ~options:[ `NOTBOL ] (Pcre2_.compile_exn "^x")
       String.uppercase_ascii "x");
  Alcotest.(check string)
    "zero limit does not evaluate matching" "x"
    (replace ~subject_offset:2 ~limit:0 rex String.uppercase_ascii "x");
  match Pcre2_.replace_fn ~subject_offset:2 rex String.uppercase_ascii "x" with
  | Error Pcre2.BADOFFSET -> ()
  | Error e -> Alcotest.failf "unexpected error: %s" (Pcre2.show_match_error e)
  | Ok result -> Alcotest.failf "expected BADOFFSET, got %S" result

let tests =
  Testo.categorize "pcre2 settings"
    [
      t "match limit ok" test_match_limit_ok;
      t "compile failure" test_compile_failure;
      t "quote" test_quote;
      t "replace" test_replace;
      t "replace function" test_replace_fn;
    ]
