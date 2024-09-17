(* Yoann Padioleau
 *
 * Copyright (C) 2019-2023 Semgrep Inc.
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
open Common
module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Parsing a pattern, using menhir or tree-sitter parsers, or both
 * depending on the language.
 *
 * Like for Parse_target.ml, most of the code is now in Parse_pattern2.ml
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* We used to do this normalization in each
 * Parse_xxx_tree_sitter.parse_pattern or xxx_to_generic.any but it's
 * better to factorize it here.
 *)
let rec normalize_any (lang : Lang.t) (any : G.any) : G.any =
  match any with
  | G.Pr xs -> normalize_any lang (G.Ss xs)
  | G.Ss [ x ] -> normalize_any lang (G.S x)
  | G.S { G.s = G.ExprStmt (e, sc); _ }
    when Tok.is_fake sc || Tok.content_of_tok sc = "" ->
      normalize_any lang (G.E e)
  (* Any name pattern which is a metavariable should be sorted into an
     E pattern, so we can properly match it against E nodes.
  *)
  | G.Name (Id ((s, t), idinfo)) when Mvar.is_metavar_name s ->
      G.E (G.N (Id ((s, t), idinfo)) |> G.e)
  (* TODO: generalizing to other languages generate many regressions *)
  | G.E { e = G.N name; _ } when lang =*= Lang.Rust ->
      normalize_any lang (G.Name name)
  | G.E { e = G.RawExpr x; _ } -> normalize_any lang (G.Raw x)
  | G.E { e = G.StmtExpr s; _ } -> normalize_any lang (G.S s)
  | G.Raw (List [ x ]) -> normalize_any lang (G.Raw x)
  (* TODO: taken from ml_to_generic.ml:
   * | G.E {e = G.StmtExpr s; _} -> G.S s?
   *)
  (* TODO? depending on the shape of Ss xs, we should sometimes return
   * a Flds instead of Ss? For example in terraform,
   * With a = "foo" ... b = "bar", we should return a Flds, but
   * with variable "foo" { } ... variable "bar" { } we should
   * probably return an Ss?
   * Or maybe we should require the user to use curly braces
   * to disambiguate with '{ a = "foo" ... b = "bar" }'?
   * Or maybe we should get rid of F and have field = stmt in AST_generic.
   *)
  | _else_ -> any

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* same "trick" than in Parse_target.ml to generate a smaller JS
 * file for the whole engine *)
let parse_pattern_ref =
  ref (fun _rule_options _lang _str -> failwith "parse_pattern_ref unset")

let parse_pattern ?rule_options lang str =
  let any = !parse_pattern_ref rule_options lang str in
  let any = normalize_any lang any in
  Check_pattern.check lang any |> Result.map (fun () -> any)
[@@profiling]
