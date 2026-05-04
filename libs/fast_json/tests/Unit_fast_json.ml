(*
   Copyright (c) 2026 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Unit tests for [Fast_json].

   Strategy: parse a fixture string with [Fast_json], drop locations via
   [to_yojson], and compare against [Yojson.Safe.from_string] of the same
   fixture. This catches divergence on every shape and escape we care about
   without us having to hand-write expected ASTs.

   Error tests just assert that an obviously-broken input is rejected.
*)

let t = Testo.create

let check_round_trip name input =
  let expected = Yojson.Safe.from_string input in
  match Fast_json.parse_string ~file:(Fpath.v "<test>") input with
  | Error msg -> Alcotest.failf "%s: parse failed: %s" name msg
  | Ok v ->
      let got = Fast_json.to_yojson v in
      if not (Yojson.Safe.equal expected got) then
        Alcotest.failf "%s: round-trip mismatch\n  expected: %s\n  got:      %s"
          name
          (Yojson.Safe.to_string expected)
          (Yojson.Safe.to_string got)

let check_rejects name input =
  match Fast_json.parse_string ~file:(Fpath.v "<test>") input with
  | Ok _ -> Alcotest.failf "%s: expected parse failure on %S" name input
  | Error _ -> ()

(* Each fixture is a JSON string we expect to parse to the same Yojson.Safe.t
   as Yojson itself produces. *)
let round_trip_fixtures =
  [
    (* atoms *)
    ("null", {|null|});
    ("true", {|true|});
    ("false", {|false|});
    ("zero", {|0|});
    ("negative int", {|-42|});
    ("positive int", {|123456789|});
    ("zero float", {|0.0|});
    ("negative float", {|-3.14|});
    ("scientific lower e", {|1e10|});
    ("scientific upper E", {|1E10|});
    ("scientific signed exponent", {|1.5e-3|});
    ("scientific positive exponent", {|2.5E+4|});
    ("empty string", {|""|});
    ("simple string", {|"hello"|});
    (* escapes *)
    ("quote escape", {|"a\"b"|});
    ("backslash escape", {|"a\\b"|});
    ("slash escape", {|"a\/b"|});
    ("backspace escape", {|"a\bb"|});
    ("formfeed escape", {|"a\fb"|});
    ("newline escape", {|"a\nb"|});
    ("carriage return escape", {|"a\rb"|});
    ("tab escape", {|"a\tb"|});
    ("unicode BMP escape", "\"a\\u00e9b\"");
    ("unicode null escape", "\"\\u0000\"");
    ("surrogate pair escape", "\"\\uD83D\\uDE00\"");
    ("raw UTF-8 (é)", {|"café"|});
    ("raw UTF-8 (emoji)", {|"😀"|});
    (* containers *)
    ("empty array", {|[]|});
    ("empty object", {|{}|});
    ("array of atoms", {|[1, 2, 3]|});
    ("mixed array", {|[1, "two", null, true, false, 3.14]|});
    ("simple object", {|{"a": 1}|});
    ("multi-key object", {|{"a": 1, "b": 2, "c": 3}|});
    ("nested arrays", {|[[1, 2], [3, 4], []]|});
    ("nested objects", {|{"a": {"b": {"c": 1}}}|});
    ("array of objects", {|[{"a": 1}, {"b": 2}]|});
    (* whitespace *)
    ("leading whitespace", "   \n\t  null");
    ("trailing whitespace", "null   \n  ");
    ("whitespace inside", {|  {  "a"  :  [  1  ,  2  ]  }  |});
    ("CRLF inside object", "{\r\n  \"a\": 1,\r\n  \"b\": 2\r\n}");
    (* tricky string contents *)
    ("string with embedded JSON-looking content", {|"{\"not\": \"parsed\"}"|});
    ("key with escapes", {|{"a\nb": 1}|});
    ("string with all simple escapes", {|"\"\\\/\b\f\n\r\t"|});
  ]

let test_round_trip () =
  List.iter
    (fun (name, input) -> check_round_trip name input)
    round_trip_fixtures

let test_locations_basic () =
  let input = "{\n  \"key\": 42\n}" in
  match Fast_json.parse_string ~file:(Fpath.v "<test>") input with
  | Error msg -> Alcotest.failf "parse failed: %s" msg
  | Ok (Assoc (lbrace, [ (k, ktok, v) ], _)) ->
      Alcotest.(check string) "key contents" "key" k;
      let lbrace_loc =
        match Tok.loc_of_tok lbrace with
        | Ok l -> l
        | Error e -> Alcotest.failf "no loc for {: %s" e
      in
      Alcotest.(check int) "{ at line 1" 1 lbrace_loc.pos.line;
      Alcotest.(check int) "{ at col 0" 0 lbrace_loc.pos.column;
      let key_loc =
        match Tok.loc_of_tok ktok with
        | Ok l -> l
        | Error e -> Alcotest.failf "no loc for key: %s" e
      in
      Alcotest.(check int) "key at line 2" 2 key_loc.pos.line;
      Alcotest.(check int) "key at col 2" 2 key_loc.pos.column;
      let value_tok =
        match v with
        | Int (_, t) -> t
        | _ -> Alcotest.failf "expected Int value"
      in
      let value_loc =
        match Tok.loc_of_tok value_tok with
        | Ok l -> l
        | Error e -> Alcotest.failf "no loc for value: %s" e
      in
      Alcotest.(check int) "value at line 2" 2 value_loc.pos.line;
      Alcotest.(check int) "value at col 9" 9 value_loc.pos.column
  | Ok _ -> Alcotest.failf "unexpected shape"

let test_to_generic_shape () =
  let input = {|{"k": [1, "x", null]}|} in
  match Fast_json.parse_string ~file:(Fpath.v "<test>") input with
  | Error msg -> Alcotest.failf "parse failed: %s" msg
  | Ok v -> (
      let g = Fast_json.to_generic v in
      match g.e with
      | AST_generic.Container (AST_generic.Dict, (_, fields, _)) -> (
          Alcotest.(check int) "one field" 1 (List.length fields);
          match fields with
          | [
           {
             AST_generic.e =
               AST_generic.Container
                 ( AST_generic.Tuple,
                   ( _,
                     [
                       _key;
                       { e = AST_generic.Container (AST_generic.Array, _); _ };
                     ],
                     _ ) );
             _;
           };
          ] ->
              ()
          | _ -> Alcotest.failf "unexpected field shape")
      | _ -> Alcotest.failf "expected top-level Dict")

(* Object keys that look like metavariables must round-trip through
   [to_generic] as [Id] nodes, not [String] literals. This is required for
   Pro taint rules whose [requires] clauses use metavariable keys, e.g.
   {"$SINK": "__SOURCE__"}. *)
let test_metavariable_keys () =
  let input = {|{"$SINK": "__SOURCE__", "normal": "value"}|} in
  match Fast_json.parse_string ~file:(Fpath.v "<test>") input with
  | Error msg -> Alcotest.failf "parse failed: %s" msg
  | Ok v -> (
      let g = Fast_json.to_generic v in
      match g.e with
      | AST_generic.Container (AST_generic.Dict, (_, fields, _)) -> (
          let key_kinds =
            List.map
              (fun field ->
                match field.AST_generic.e with
                | AST_generic.Container (AST_generic.Tuple, (_, [ key; _ ], _))
                  -> (
                    match key.AST_generic.e with
                    | AST_generic.N (AST_generic.Id ((s, _), _)) -> `Id s
                    | AST_generic.L (AST_generic.String (_, (s, _), _)) ->
                        `String s
                    | _ -> `Other)
                | _ -> `BadShape)
              fields
          in
          match key_kinds with
          | [ `Id "$SINK"; `String "normal" ] -> ()
          | _ ->
              Alcotest.failf
                "expected [Id \"$SINK\"; String \"normal\"], got something else"
          )
      | _ -> Alcotest.failf "expected top-level Dict")

(* Inputs we expect to be rejected. We don't pin the exact error message. *)
let reject_fixtures =
  [
    ("empty input", "");
    ("whitespace only", "   ");
    ("trailing garbage", "null junk");
    ("unterminated string", "\"hello");
    ("unescaped newline in string", "\"a\nb\"");
    ("unterminated array", "[1, 2");
    ("unterminated object", "{\"a\": 1");
    ("trailing comma in array", "[1, 2,]");
    ("trailing comma in object", "{\"a\": 1,}");
    ("missing comma in array", "[1 2]");
    ("missing colon", "{\"a\" 1}");
    ("invalid escape", "\"\\q\"");
    ("truncated unicode escape", "\"\\u00\"");
    ("lone high surrogate", {|"\uD83D"|});
    ("lone low surrogate", {|"\uDE00"|});
    ("bare keyword (typo)", "tru");
    ("comment (not JSON)", "// hi\nnull");
    ("single quoted string", "'hello'");
    ("python-style booleans", "True");
  ]

let test_rejects () =
  List.iter (fun (name, input) -> check_rejects name input) reject_fixtures

(* Sanity: a sequence of carefully-built inputs that aren't JSON should not
   crash with an uncaught exception — they must come back as Error. *)
let test_no_exceptions_on_random_garbage () =
  let inputs = [ "\x00"; "\xff"; "\x01\x02\x03"; "{[}]"; "\\\\\\\\" ] in
  List.iter
    (fun s ->
      match Fast_json.parse_string ~file:(Fpath.v "<test>") s with
      | Ok _
      | Error _ ->
          ())
    inputs

let tests =
  Testo.categorize "Fast_json"
    [
      t "round-trip vs Yojson" test_round_trip;
      t "locations track line/column" test_locations_basic;
      t "to_generic produces expected shape" test_to_generic_shape;
      t "metavariable keys become Id nodes" test_metavariable_keys;
      t "rejects malformed input" test_rejects;
      t "garbage does not raise" test_no_exceptions_on_random_garbage;
    ]
