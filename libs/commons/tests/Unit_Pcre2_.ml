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

let test_jit_stack_fallback () =
  let rex = Pcre2_.compile_exn {|(?<LETTER>a|b)*c|} in
  let long_match = String.make 3_000 'a' ^ "c" in
  let subject = "ac " ^ long_match ^ " ac" in
  Alcotest.(check bool)
    "long input matches" true
    (match Pcre2_.is_match_valid_utf8 rex long_match with
    | Ok matched -> matched
    | Error e -> Alcotest.fail (Pcre2.show_match_error e));
  let matches =
    Pcre2_.captures_iter_valid_utf8 rex subject
    |> List.of_seq
    |> List.map (function
      | Ok captures ->
          Alcotest.(check (option string))
            "named capture survives fallback" (Some "a")
            (Pcre2_.named_substring_of_captures captures "LETTER");
          Pcre2_.substring_of_captures captures 0
      | Error e -> Alcotest.fail (Pcre2.show_match_error e))
  in
  Alcotest.(check (list (option string)))
    "all matches survive fallback"
    [ Some "ac"; Some long_match; Some "ac" ]
    matches

let test_shared_jit_across_domains () =
  let rex = Pcre2_.compile_exn {|(?<KEY>key[a-z]+)|} in
  let subject = "keyabc keyxyz" in
  let check () =
    let rec loop remaining =
      if remaining = 0 then true
      else
        let is_match = Pcre2_.is_match_valid_utf8 rex subject = Ok true in
        let captures =
          match Pcre2_.captures_iter_valid_utf8 rex subject |> List.of_seq with
          | [ Ok first; Ok second ] ->
              Pcre2_.named_substring_of_captures first "KEY" = Some "keyabc"
              && Pcre2_.named_substring_of_captures second "KEY" = Some "keyxyz"
          | _ -> false
        in
        is_match && captures && loop (remaining - 1)
    in
    loop 100
  in
  let domains = List.init 4 (fun _ -> Domain.spawn check) in
  List.iter
    (fun domain ->
      Alcotest.(check bool) "shared regexp matches" true (Domain.join domain))
    domains

let test_literal_byte_search () =
  let module Search_pattern = Base.String.Search_pattern in
  let compile case_sensitive needle =
    match Pcre2_.matching_exact_string_bytes ~case_sensitive needle with
    | Ok re -> re
    | Error error -> Alcotest.fail (Pcre2_.show_compile_error error)
  in
  let check case_sensitive needle content =
    let expected =
      Search_pattern.matches
        (Search_pattern.create ~case_sensitive needle)
        content
    in
    let re = compile case_sensitive needle in
    let got =
      match Pcre2_.is_match re content with
      | Ok matches -> matches
      | Error _ -> Alcotest.fail "literal byte search failed"
    in
    if (expected && not got) || (case_sensitive && expected <> got) then
      Alcotest.failf "literal %S in %S: expected %b, got %b" needle content
        expected got
  in
  List.iter
    (fun (needle, content) ->
      check true needle content;
      check false needle content)
    [
      ("", "");
      ("a.b", "xxa.by");
      ("A\\B", "a\\b");
      ("a\000b", "xa\000by");
      ("é", "xéy");
      ("\xff", "x\xffy");
      ("\000", "x\000y");
      ("[a]+", "x[a]+y");
    ];
  for byte = 0 to 255 do
    let needle = String.make 1 (Char.chr byte) in
    let re = compile false needle in
    let pattern = Search_pattern.create ~case_sensitive:false needle in
    for haystack_byte = 0 to 255 do
      let content = String.make 1 (Char.chr haystack_byte) in
      if Search_pattern.matches pattern content then
        match Pcre2_.is_match re content with
        | Ok true -> ()
        | Ok false
        | Error _ ->
            Alcotest.failf "case-insensitive byte %d missed byte %d" byte
              haystack_byte
    done
  done

let tests =
  Testo.categorize "pcre2 settings"
    [
      t "match limit ok" test_match_limit_ok;
      t "compile failure" test_compile_failure;
      t "quote" test_quote;
      t "replace" test_replace;
      t "replace function" test_replace_fn;
      t "JIT stack fallback" test_jit_stack_fallback;
      t "shared JIT across domains" test_shared_jit_across_domains;
      t "literal byte search" test_literal_byte_search;
    ]
