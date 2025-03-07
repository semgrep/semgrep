(* Yoann Padioleau
 *
 * Copyright (c) 2025 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
module JS = Ast_js

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* AST(s) for JSON.
 *
 * Similar to AST_yaml.ml and AST_toml.ml, it is convenient to have an
 * AST for JSON with location information.
 * There are many JSON parsers for OCaml (see below), but none of them
 * provide finer-grained token location which is useful for error reporting
 * and essential for Semgrep matching.
 *
 * Below we define actually 2 ASTs for JSON:
 *  - an AST that is just an alias for AST_js.expr because we (ab)use the
 *    Javascript parser to actually parse JSON (JSON is a subset of Javascript).
 *    However this AST is more difficult to match over
 *    (AST_js.expr is a complex type with many cases not valid for JSON)
 *  - a precise simple AST with just the relevant construct which is more
 *    convenient for pattern matching.
 *
 * alternatives:
 *  - Yojson, Martin's JSON parser. The AST is defined as:
 *    type yojson =
 *     [ `Null
 *     | `Bool of bool
 *     | `Int of int
 *     | `Float of float
 *     | `String of string
 *     | `Assoc of (string * yojson) list
 *     | `List of yojson list ]
 *  - ezjsonm
 *  - json-wheel Json_type.t, but does not have the location info either
 *
 * related: AST_yaml.ml, AST_xml.ml, AST_toml.ml, AST_sexp.ml
 *)

(*****************************************************************************)
(* Leaf (tokens) *)
(*****************************************************************************)

(* a shortcut to annotate some information with position information *)
type 'a wrap = 'a * Tok.t [@@deriving show]

(* Use for square[], double[[]], curly{}, and strings''"" brackets *)
type 'a bracket = Tok.t * 'a * Tok.t [@@deriving show]

(*****************************************************************************)
(* Precise AST *)
(*****************************************************************************)
type value =
  | Null of Tok.t
  | Bool of bool wrap
  (* JS/JSON do not have an Int case; all numbers are float *)
  | Number of float option wrap
  | String of string wrap
  | Array of value list bracket
  | Object of (string wrap * value) list bracket
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* Alias to the JS AST *)
(*****************************************************************************)

(* Should include only the literals (Bool, Num, String),
 * collections (Obj, Arr), and IdSpecial Null from Ast_js.expr.
 *)
type expr = JS.expr [@@deriving show]
type program = JS.expr [@@deriving show]

type any = E of JS.expr | PartialSingleField of string wrap * Tok.t * JS.expr
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* Conversion function *)
(*****************************************************************************)

let error tok = raise (Parsing_error.Syntax_error tok)

let rec js_expr_to_json_value (e : JS.expr) : value =
  match e with
  | JS.IdSpecial (JS.Null, tok) -> Null tok
  | JS.L (JS.Bool b) -> Bool b
  | JS.L (JS.Num n) -> Number n
  | JS.L (JS.String s) -> String s
  | JS.L (JS.Regexp ((tok, _, _), _)) -> error tok
  | JS.Arr (l, xs, r) -> Array (l, List_.map js_expr_to_json_value xs, r)
  | JS.Obj (l, props, r) ->
      let json_props =
        List_.filter_map
          (fun prop ->
            match prop with
            | JS.Field { fld_name = JS.PN name; fld_body = Some value; _ }
            | JS.FieldColon { fld_name = JS.PN name; fld_body = Some value; _ }
              ->
                Some (name, js_expr_to_json_value value)
            (* In theory we should handle all the cases and raise
             * an error instead
             *)
            | _ -> None)
          props
      in
      Object (l, json_props, r)
  | JS.Id (_, tok) -> error tok
  | JS.IdSpecial (_, tok) -> error tok
  | JS.Assign (_, tok, _) -> error tok
  | JS.Class ({ c_kind = _, tok; _ }, _) -> error tok
  | JS.ObjAccess (_, (_, tok), _) -> error tok
  | JS.ArrAccess (_, (tok, _, _)) -> error tok
  | JS.Fun ({ f_kind = _, tok; _ }, _) -> error tok
  | JS.Apply (_, (tok, _, _)) -> error tok
  | JS.New (tok, _, _) -> error tok
  | JS.Xml xml -> (
      match xml.xml_kind with
      | JS.XmlClassic (tok, _, _, _) -> error tok
      | JS.XmlSingleton (tok, _, _) -> error tok
      | JS.XmlFragment (tok, _) -> error tok)
  | JS.Conditional (_, tok, _, _, _) -> error tok
  | JS.Cast (_, tok, _) -> Null tok
  | JS.TypeAssert (_, tok, _) -> error tok
  | JS.ParenExpr (tok, _, _) -> error tok
  | JS.Ellipsis tok -> error tok
  | JS.DeepEllipsis (tok, _, _) -> error tok
  | JS.ObjAccessEllipsis (_, tok) -> error tok
  | JS.TypedMetavar (_, tok, _) -> error tok
  | JS.ExprTodo ((_, tok), _) -> error tok
