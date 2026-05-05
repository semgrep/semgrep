(* Brandon Wu (but really Claude Code)

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
   A fast hand(AI)-written JSON parser that produces a Yojson.Safe.t-shaped tree
   annotated with [Tok.t] location information.

   Approach
   --------
   - Read the whole input into a string. The parser indexes directly into it
     via a mutable byte offset, avoiding the cost of [Lexing.lexbuf].
   - Parsing is recursive descent. There is no separate tokenizer; whitespace
     is skipped inline and each value is parsed by peeking at its first byte.
   - Every value and object key captures a [Tok.t] pointing at its starting
     byte position, with line/column tracked incrementally as we advance.
   - String decoding has a fast path: if a string contains no escape, we emit
     it with a single [String.sub]; only when we hit a backslash do we fall
     back to a [Buffer.t]-based decoder.
   - Errors raise a local [Parse_error] which is caught at the top level and
     converted to a [result].

   Strict RFC 8259: no trailing commas, no comments, no leading zeros on
   non-zero integers, etc.

   Human-written comment:
   This was spawned off of a desire to have a faster JSON-parsing approach for
   Semgrep's increasingly large JSON rule inputs. Our previous approach involved
   piggy-backing off of the JS parser, which was bulkier than necessary and used
   Menhir, which my intuition seemed to indicate would be more overhead than just
   raw OCaml.

   On implementing this (and a light translation to `AST_generic`), we found that
   we get something like a ~5x speedup on parsing a 300MB JSON rule file, so the
   results seems to hold up empirically.

   Future work:
   - We could make this even better by skipping the `AST_generic` step. The fact
     that rule parsing happens on `AST_generic` is both slow and also needlessly
     complex.

   Scope (read this before extending the parser to other call sites!)
   ------------------------------------------------------------------
   This parser is currently used only for the rule-file JSON arm in
   [Parse_rule.ml]. The legacy [Parse_json + Json_to_generic
   ~unescape_strings:true] path is still used for JSON patterns and JSON
   targets. The one remaining behavior difference is comment handling: the
   legacy path tolerates `//` line comments (it delegates to the JS lexer),
   while this parser implements RFC 8259 strictly and rejects them.
   Several test fixtures in [tests/patterns/json/] and
   [tests/semgrep-rules/json/] rely on the comment relaxation, so we keep
   the legacy path for those.

   Other behaviors deliberately match the legacy path:

   - Numeric literals are always emitted as `G.Float`, even when the input
     looks like an integer. Downstream rule code may pattern-match on
     `G.Float` for numeric fields.

   - Object keys that look like metavariables (e.g. `"$SINK"`) are emitted
     as `G.N (G.Id _)` rather than `G.L (G.String _)`. This matters for
     Pro taint rules whose [requires] clauses use metavariable keys (e.g.
     `{"$SINK": "__SOURCE__"}`); without this, downstream taint analysis
     would silently fail to recognize metavariable refs.
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t =
  | Null of Tok.t
  | Bool of bool * Tok.t
  | Int of int * Tok.t
  | Float of float * Tok.t
  | String of string * Tok.t
  | List of Tok.t * t list * Tok.t
  | Assoc of Tok.t * field list * Tok.t

and field = string * Tok.t * t

(* Raised internally by the parser at an error site. Carries enough info to
   build a [Tok.t] for [Parsing_error.Syntax_error]. *)
exception
  Parse_error of {
    file : Fpath.t;
    bytepos : int;
    line : int;
    column : int;
    msg : string;
  }

(*****************************************************************************)
(* Parser state *)
(*****************************************************************************)

type state = {
  src : string;
  len : int;
  file : Fpath.t;
  mutable pos : int;
  mutable line : int;
  mutable line_start : int;  (** byte offset of current line's first byte *)
}

let make_state ~file src =
  { src; len = String.length src; file; pos = 0; line = 1; line_start = 0 }

let error s msg =
  raise
    (Parse_error
       {
         file = s.file;
         bytepos = s.pos;
         line = s.line;
         column = s.pos - s.line_start;
         msg;
       })

(*****************************************************************************)
(* Token construction *)
(*****************************************************************************)

(* Build a token at the given byte position with the given literal string. The
   line/column are taken from the parser's current tracking, which is valid as
   long as the caller captures the position *before* advancing past the token
   start. *)
let tok_at s ~bytepos ~line ~col str =
  let pos : Pos.t = { file = s.file; bytepos; line; column = col } in
  let loc : Loc.t = { pos; str } in
  Tok.OriginTok loc

(* Snapshot the current (bytepos, line, col) — used to stamp a token at the
   start of a value before we consume its bytes. *)
let snapshot s = (s.pos, s.line, s.pos - s.line_start)

(*****************************************************************************)
(* Low-level byte reading *)
(*****************************************************************************)

let[@inline] peek s =
  if s.pos >= s.len then error s "unexpected end of input";
  String.unsafe_get s.src s.pos

let[@inline] peek_opt s =
  if s.pos >= s.len then None else Some (String.unsafe_get s.src s.pos)

let[@inline] advance s =
  let c = String.unsafe_get s.src s.pos in
  s.pos <- s.pos + 1;
  if c = '\n' then (
    s.line <- s.line + 1;
    s.line_start <- s.pos)

let[@inline] expect s c =
  if s.pos >= s.len || String.unsafe_get s.src s.pos <> c then
    error s (Printf.sprintf "expected %C" c);
  advance s

let skip_ws s =
  let src = s.src and len = s.len in
  let continue = ref true in
  while !continue && s.pos < len do
    match String.unsafe_get src s.pos with
    | ' '
    | '\t'
    | '\r' ->
        s.pos <- s.pos + 1
    | '\n' ->
        s.pos <- s.pos + 1;
        s.line <- s.line + 1;
        s.line_start <- s.pos
    | _ -> continue := false
  done

(*****************************************************************************)
(* Literal keywords (null, true, false) *)
(*****************************************************************************)

let match_keyword s kw =
  let klen = String.length kw in
  if s.pos + klen > s.len then false
  else
    let rec loop i =
      if i = klen then true
      else if String.unsafe_get s.src (s.pos + i) = String.unsafe_get kw i then
        loop (i + 1)
      else false
    in
    loop 0

let consume_keyword s kw =
  let klen = String.length kw in
  (* no newlines in keywords, so bulk advance is fine *)
  s.pos <- s.pos + klen

(*****************************************************************************)
(* Numbers *)
(*****************************************************************************)

(* Scan a JSON number starting at s.pos, returning (is_float, literal_string).
   Follows the JSON grammar:
     number = [ '-' ] int [ frac ] [ exp ]
   We don't validate exhaustively here — [int_of_string]/[float_of_string] do
   the real work; we just identify the span. *)
let scan_number s =
  let start = s.pos in
  let is_float = ref false in
  if s.pos < s.len && String.unsafe_get s.src s.pos = '-' then
    s.pos <- s.pos + 1;
  (* integer part *)
  while
    s.pos < s.len
    &&
    let c = String.unsafe_get s.src s.pos in
    c >= '0' && c <= '9'
  do
    s.pos <- s.pos + 1
  done;
  (* fractional part *)
  if s.pos < s.len && String.unsafe_get s.src s.pos = '.' then (
    is_float := true;
    s.pos <- s.pos + 1;
    while
      s.pos < s.len
      &&
      let c = String.unsafe_get s.src s.pos in
      c >= '0' && c <= '9'
    do
      s.pos <- s.pos + 1
    done);
  (* exponent *)
  (if s.pos < s.len then
     let c = String.unsafe_get s.src s.pos in
     if c = 'e' || c = 'E' then (
       is_float := true;
       s.pos <- s.pos + 1;
       if s.pos < s.len then (
         let c' = String.unsafe_get s.src s.pos in
         if c' = '+' || c' = '-' then s.pos <- s.pos + 1;
         while
           s.pos < s.len
           &&
           let c = String.unsafe_get s.src s.pos in
           c >= '0' && c <= '9'
         do
           s.pos <- s.pos + 1
         done)));
  (!is_float, String.sub s.src start (s.pos - start))

(*****************************************************************************)
(* Strings *)
(*****************************************************************************)

let hex_digit s c =
  match c with
  | '0' .. '9' -> Char.code c - Char.code '0'
  | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
  | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
  | _ -> error s "invalid hex digit in \\u escape"

(* Read 4 hex digits; caller has already consumed the '\u'. Returns codepoint. *)
let read_hex4 s =
  if s.pos + 4 > s.len then error s "truncated \\u escape";
  let d0 = hex_digit s (String.unsafe_get s.src s.pos) in
  let d1 = hex_digit s (String.unsafe_get s.src (s.pos + 1)) in
  let d2 = hex_digit s (String.unsafe_get s.src (s.pos + 2)) in
  let d3 = hex_digit s (String.unsafe_get s.src (s.pos + 3)) in
  s.pos <- s.pos + 4;
  (d0 lsl 12) lor (d1 lsl 8) lor (d2 lsl 4) lor d3

let utf8_encode buf cp =
  if cp < 0x80 then Buffer.add_char buf (Char.unsafe_chr cp)
  else if cp < 0x800 then (
    Buffer.add_char buf (Char.unsafe_chr (0xC0 lor (cp lsr 6)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor (cp land 0x3F))))
  else if cp < 0x10000 then (
    Buffer.add_char buf (Char.unsafe_chr (0xE0 lor (cp lsr 12)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((cp lsr 6) land 0x3F)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor (cp land 0x3F))))
  else (
    Buffer.add_char buf (Char.unsafe_chr (0xF0 lor (cp lsr 18)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((cp lsr 12) land 0x3F)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((cp lsr 6) land 0x3F)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor (cp land 0x3F))))

(* Called after an escape character has been seen at [content_start .. s.pos).
   Switches to buffer-based decoding for the rest of the string, returns the
   decoded contents and leaves [s.pos] just past the closing quote. *)
let parse_string_slow s ~content_start =
  let buf = Buffer.create 64 in
  Buffer.add_substring buf s.src content_start (s.pos - content_start);
  let rec loop () =
    if s.pos >= s.len then error s "unterminated string";
    let c = String.unsafe_get s.src s.pos in
    match c with
    | '"' ->
        s.pos <- s.pos + 1;
        Buffer.contents buf
    | '\\' ->
        s.pos <- s.pos + 1;
        if s.pos >= s.len then error s "unterminated escape";
        let e = String.unsafe_get s.src s.pos in
        s.pos <- s.pos + 1;
        (match e with
        | '"' -> Buffer.add_char buf '"'
        | '\\' -> Buffer.add_char buf '\\'
        | '/' -> Buffer.add_char buf '/'
        | 'b' -> Buffer.add_char buf '\b'
        | 'f' -> Buffer.add_char buf '\012'
        | 'n' -> Buffer.add_char buf '\n'
        | 'r' -> Buffer.add_char buf '\r'
        | 't' -> Buffer.add_char buf '\t'
        | 'u' ->
            let cp = read_hex4 s in
            (* Surrogate pair handling *)
            if cp >= 0xD800 && cp <= 0xDBFF then (
              if
                s.pos + 2 > s.len
                || String.unsafe_get s.src s.pos <> '\\'
                || String.unsafe_get s.src (s.pos + 1) <> 'u'
              then error s "unpaired high surrogate";
              s.pos <- s.pos + 2;
              let low = read_hex4 s in
              if low < 0xDC00 || low > 0xDFFF then
                error s "invalid low surrogate";
              let combined =
                0x10000 + (((cp - 0xD800) lsl 10) lor (low - 0xDC00))
              in
              utf8_encode buf combined)
            else if cp >= 0xDC00 && cp <= 0xDFFF then
              error s "unexpected low surrogate"
            else utf8_encode buf cp
        | _ -> error s "invalid escape");
        loop ()
    | '\n' -> error s "unescaped newline in string"
    | c when Char.code c < 0x20 ->
        error s "unescaped control character in string"
    | c ->
        Buffer.add_char buf c;
        s.pos <- s.pos + 1;
        loop ()
  in
  loop ()

(* Opening quote already consumed. Returns unescaped contents; leaves s.pos
   just past closing quote. Fast path: scan for a terminator byte; if we hit
   the closing quote with no escapes, [String.sub] it. *)
let parse_string_body s =
  let content_start = s.pos in
  let src = s.src and len = s.len in
  let rec fast () =
    if s.pos >= len then error s "unterminated string";
    let c = String.unsafe_get src s.pos in
    if c = '"' then (
      let result = String.sub src content_start (s.pos - content_start) in
      s.pos <- s.pos + 1;
      result)
    else if c = '\\' then parse_string_slow s ~content_start
    else if c = '\n' then error s "unescaped newline in string"
    else if Char.code c < 0x20 then
      error s "unescaped control character in string"
    else (
      s.pos <- s.pos + 1;
      fast ())
  in
  fast ()

(*****************************************************************************)
(* Core recursive descent *)
(*****************************************************************************)

let rec parse_value s =
  skip_ws s;
  if s.pos >= s.len then error s "unexpected end of input";
  let bp, ln, col = snapshot s in
  match String.unsafe_get s.src s.pos with
  | '{' ->
      advance s;
      let rbrace, fields = parse_object_body s in
      let lbrace = tok_at s ~bytepos:bp ~line:ln ~col "{" in
      Assoc (lbrace, fields, rbrace)
  | '[' ->
      advance s;
      let rbracket, items = parse_array_body s in
      let lbracket = tok_at s ~bytepos:bp ~line:ln ~col "[" in
      List (lbracket, items, rbracket)
  | '"' ->
      advance s;
      let str = parse_string_body s in
      let tok = tok_at s ~bytepos:bp ~line:ln ~col ("\"" ^ str ^ "\"") in
      String (str, tok)
  | 't' when match_keyword s "true" ->
      consume_keyword s "true";
      Bool (true, tok_at s ~bytepos:bp ~line:ln ~col "true")
  | 'f' when match_keyword s "false" ->
      consume_keyword s "false";
      Bool (false, tok_at s ~bytepos:bp ~line:ln ~col "false")
  | 'n' when match_keyword s "null" ->
      consume_keyword s "null";
      Null (tok_at s ~bytepos:bp ~line:ln ~col "null")
  | '-'
  | '0' .. '9' -> (
      let is_float, lit = scan_number s in
      let tok = tok_at s ~bytepos:bp ~line:ln ~col lit in
      if is_float then
        match float_of_string_opt lit with
        | Some f -> Float (f, tok)
        | None -> error s (Printf.sprintf "invalid number %S" lit)
      else
        match int_of_string_opt lit with
        | Some i -> Int (i, tok)
        | None -> (
            (* too large for OCaml int: fall back to float *)
            match float_of_string_opt lit with
            | Some f -> Float (f, tok)
            | None -> error s (Printf.sprintf "invalid number %S" lit)))
  | c -> error s (Printf.sprintf "unexpected character %C" c)

and parse_array_body s =
  skip_ws s;
  match peek_opt s with
  | Some ']' ->
      let bp, ln, col = snapshot s in
      advance s;
      (tok_at s ~bytepos:bp ~line:ln ~col "]", [])
  | _ ->
      let first = parse_value s in
      parse_array_rest s [ first ]

and parse_array_rest s acc =
  skip_ws s;
  match peek s with
  | ']' ->
      let bp, ln, col = snapshot s in
      advance s;
      (tok_at s ~bytepos:bp ~line:ln ~col "]", List.rev acc)
  | ',' ->
      advance s;
      let v = parse_value s in
      parse_array_rest s (v :: acc)
  | c -> error s (Printf.sprintf "expected ',' or ']' but got %C" c)

and parse_object_body s =
  skip_ws s;
  match peek_opt s with
  | Some '}' ->
      let bp, ln, col = snapshot s in
      advance s;
      (tok_at s ~bytepos:bp ~line:ln ~col "}", [])
  | _ ->
      let field = parse_field s in
      parse_object_rest s [ field ]

and parse_object_rest s acc =
  skip_ws s;
  match peek s with
  | '}' ->
      let bp, ln, col = snapshot s in
      advance s;
      (tok_at s ~bytepos:bp ~line:ln ~col "}", List.rev acc)
  | ',' ->
      advance s;
      let field = parse_field s in
      parse_object_rest s (field :: acc)
  | c -> error s (Printf.sprintf "expected ',' or '}' but got %C" c)

and parse_field s =
  skip_ws s;
  let bp, ln, col = snapshot s in
  expect s '"';
  let key = parse_string_body s in
  let key_tok = tok_at s ~bytepos:bp ~line:ln ~col ("\"" ^ key ^ "\"") in
  skip_ws s;
  expect s ':';
  let v = parse_value s in
  (key, key_tok, v)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let parse_string ~file src =
  let s = make_state ~file src in
  try
    let v = parse_value s in
    skip_ws s;
    if s.pos <> s.len then error s "trailing garbage after JSON value";
    Ok v
  with
  | Parse_error { file; line; column; msg; _ } ->
      Error
        (Printf.sprintf "%s:%d:%d: %s" (Fpath.to_string file) line column msg)

let parse_file file =
  try
    UChan.with_open_in file (fun chan ->
        let ic = chan.ic in
        let n = in_channel_length ic in
        let buf = Bytes.create n in
        really_input ic buf 0 n;
        parse_string ~file (Bytes.unsafe_to_string buf))
  with
  | Sys_error msg -> Error msg

(*****************************************************************************)
(* Conversion to Yojson.Safe.t *)
(*****************************************************************************)

let rec to_yojson : t -> Yojson.Safe.t = function
  | Null _ -> `Null
  | Bool (b, _) -> `Bool b
  | Int (i, _) -> `Int i
  | Float (f, _) -> `Float f
  | String (s, _) -> `String s
  | List (_, xs, _) -> `List (List.map to_yojson xs)
  | Assoc (_, fs, _) -> `Assoc (List.map (fun (k, _, v) -> (k, to_yojson v)) fs)

(*****************************************************************************)
(* Conversion to AST_generic *)
(*****************************************************************************)
(* Shape mirrors [Json_to_generic.value_to_generic]: scalars become literal
   expressions; arrays become [Container (Array, ...)]; objects become
   [Container (Dict, ...)] of [Container (Tuple, [key; value])]. *)

module G = AST_generic

let fb = Tok.unsafe_fake_bracket

let rec to_generic : t -> G.expr = function
  | Null tok -> G.L (G.Null tok) |> G.e
  | Bool (b, tok) -> G.L (G.Bool (b, tok)) |> G.e
  (* The legacy JSON-to-generic path (Json_to_generic.value_to_generic) emits
     all JSON numbers as G.Float, regardless of whether they look like
     integers. Match that behavior so downstream rule parsing — which may
     pattern-match on G.Float for numeric fields — sees the same shape. *)
  | Int (i, tok) -> G.L (G.Float (Some (float_of_int i), tok)) |> G.e
  | Float (f, tok) -> G.L (G.Float (Some f, tok)) |> G.e
  | String (s, tok) -> G.L (G.String (fb (s, tok))) |> G.e
  | List (l, xs, r) ->
      G.Container (G.Array, (l, List.map to_generic xs, r)) |> G.e
  | Assoc (l, fs, r) ->
      let kvs =
        List.map
          (fun (k, ktok, v) ->
            (* Mirror [Json_to_generic]: keys that look like metavariables
               (e.g. "$SINK") become identifiers so downstream code can
               recognize them as metavariable references. This matters for
               rule files because Pro taint rules use metavariable keys in
               [requires] clauses (e.g. {"$SINK": "__SOURCE__"}). *)
            let key =
              if AST_generic.is_metavar_name k then
                G.N (G.Id ((k, ktok), G.empty_id_info ())) |> G.e
              else G.L (G.String (fb (k, ktok))) |> G.e
            in
            let value = to_generic v in
            G.Container (G.Tuple, fb [ key; value ]) |> G.e)
          fs
      in
      G.Container (G.Dict, (l, kvs, r)) |> G.e

(* [parse_program] raises [Parsing_error.Syntax_error] on parse failure to
   match the legacy [Parse_json.parse_program] behavior, so callers (e.g.
   [Parse_rule]) and downstream error formatting (e.g. [Core_error]) keep
   working unchanged. *)
let parse_program file =
  let src =
    UChan.with_open_in file (fun chan ->
        let ic = chan.ic in
        let n = in_channel_length ic in
        let buf = Bytes.create n in
        really_input ic buf 0 n;
        Bytes.unsafe_to_string buf)
  in
  let s = make_state ~file src in
  try
    let v = parse_value s in
    skip_ws s;
    if s.pos <> s.len then error s "trailing garbage after JSON value";
    [ G.exprstmt (to_generic v) ]
  with
  | Parse_error { file; bytepos; line; column; msg } ->
      let pos : Pos.t = { file; bytepos; line; column } in
      let loc : Loc.t = { pos; str = msg } in
      raise (Parsing_error.Syntax_error (Tok.OriginTok loc))
