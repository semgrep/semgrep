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
(** Fast JSON parser that produces a Yojson.Safe.t-shaped tree, but with
    [Tok.t] location information on every value and object key.

    Originally written to speed up rule-file parsing in [Parse_rule.ml] (~5x
    over the previous [Parse_json + Json_to_generic] chain).

    {1 Scope and intended use}

    The parser proper is a strict RFC 8259 implementation: it accepts
    every conforming JSON document and rejects extensions like trailing
    commas, single-quoted strings, and [//] comments. The one
    accept/reject difference from the legacy [Parse_json + Json_to_generic
    ~unescape_strings:true] path is:

    - {b Comments are rejected.} The legacy path delegates to the JS lexer,
      which silently accepts [//] line comments. Several test fixtures in
      [tests/patterns/json/] and [tests/semgrep-rules/json/] rely on this
      relaxation. If you parse those files through this module, you will
      get an [Error].

    Note that [to_generic] (the translation from [Fast_json.t] to
    [AST_generic.expr]) is {b not} a generic JSON-to-AST and deliberately
    encodes Semgrep-specific conventions: numeric literals are always
    emitted as [G.Float], and object keys that look like metavariables
    (e.g. ["$SINK"]) are emitted as [G.N (G.Id _)] rather than string
    literals. Both match the legacy path's behavior and are required
    by downstream rule code (the metavariable-key case matters in
    particular for Pro taint rules whose [requires] clauses use
    metavariable keys, e.g. {["{ \"$SINK\": \"__SOURCE__\" }"]}).

    {1 Algorithm}

    See [Fast_json.ml] preamble. *)

type t =
  | Null of Tok.t
  | Bool of bool * Tok.t
  | Int of int * Tok.t
  | Float of float * Tok.t
  | String of string * Tok.t  (** unescaped contents *)
  | List of Tok.t * t list * Tok.t  (** [ ... ] with brackets *)
  | Assoc of Tok.t * field list * Tok.t  (** { ... } with braces *)

and field = string * Tok.t * t
(** Object field: key, key's token, value. *)

val parse_string : file:Fpath.t -> string -> (t, string) result
(** Parse a JSON document from an in-memory string. [file] is used only to
    populate token locations. *)

val parse_file : Fpath.t -> (t, string) result
(** Parse a JSON document from a file. *)

val to_yojson : t -> Yojson.Safe.t
(** Drop location info, producing the equivalent [Yojson.Safe.t]. *)

val to_generic : t -> AST_generic.expr
(** Translate to an [AST_generic] expression, mirroring the shape produced by
    [Json_to_generic.value_to_generic]: objects become [Dict] containers of
    [Tuple]s, arrays become [Array] containers, and scalars become the
    corresponding literal expressions.

    Numeric literals are always emitted as [G.Float] (matching the legacy
    path), regardless of whether they look like integers.

    Object keys that look like metavariables (e.g. ["$X"]) are emitted as
    [G.N (G.Id _)], matching the legacy path. *)

val parse_program : Fpath.t -> AST_generic.program
(** Parse a JSON file directly into an [AST_generic.program] — the typical
    entry point for integrating with Semgrep's parsing pipeline.

    On parse failure, raises [Parsing_error.Syntax_error] with a [Tok.t]
    carrying the file, line, column, and a short reason. This matches the
    legacy [Parse_json.parse_program] behavior so existing callers and
    error-formatting code (e.g. [Core_error]) keep working unchanged.

    See the module preamble for restrictions on what kinds of JSON this is
    safe for. *)
