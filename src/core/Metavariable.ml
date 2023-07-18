(* Yoann Padioleau
 *
 * Copyright (C) 2019-2023 r2c
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
module H = AST_generic_helpers

(* Provide hash_* for the core ocaml types *)
open Ppx_hash_lib.Std.Hash.Builtin

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* less: could want to remember the position in the pattern of the metavar
 * for error reporting on pattern itself? so use a 'string AST_generic.wrap'?
 *)
type mvar = string [@@deriving show, eq, hash]

(* 'mvalue' below used to be just an alias to AST_generic.any, but it is more
 * precise to have a type just for the metavariable values; we do not
 * need all the AST_generic.any cases (however this forces us to
 * define a few boilerplate functions like mvalue_to_any below).
 *
 * AST_generic.any is already (ab)used for many things: for representing
 * a semgrep pattern, for being able to dump any AST constructs,
 * for poor's man overloading for visiting, mapping, so there's no
 * need to add an extra thing. It would probably be better to also
 * define our own Pattern.t with just the valid cases, but we don't
 * want code in pfff to depend on semgrep/core/Pattern.ml, hence the
 * use of AST_generic.any for patterns.
 *
 * coupling: if you add a constructor here, you probably also want to
 * modify Matching_generic.equal_ast_bound_code!
 *)
type mvalue =
  (* TODO: get rid of Id, N generalize it *)
  | Id of AST_generic.ident * AST_generic.id_info option
  | N of AST_generic.name
  | E of AST_generic.expr
  | S of AST_generic.stmt
  | T of AST_generic.type_
  | P of AST_generic.pattern
  | Raw of AST_generic.raw_tree
  | XmlAt of AST_generic.xml_attribute
  (* Those can be now empty with $...XXX metavariables.
   * coupling: if you add more constructors that allow an empty content,
   * you may need to modify JSON_report.range_of_any to not get
   * some NoTokenLocation exn.
   *)
  | Ss of AST_generic.stmt list
  | Args of AST_generic.argument list
  | Params of AST_generic.parameter list
  | Xmls of AST_generic.xml_body list
  (* Text below is used to match the content of a string or atom, without the
   * enclosing quotes. For a string this can actually be empty. Includes both
   * the original string token with the enclosing quotes (for use in autofix
   * where we want to print the original quotes when possible) and the modified
   * token without the quotes for use in, e.g. metavariable-pattern.
   * TODO? use a separate 'Atom of string wrap' for atoms? This could be useful
   * to allow 'foo :$ATOM ... obj.$ATOM', but not
   * '"$STR" ... obj.$STR'? (even though this could also be useful for PHP
   * where strings are often used to represent entities (e.g., function
   * names).
   *)
  | Text of
      string
      * (* token without enclosing quotes *) AST_generic.tok
      * (* original token *) AST_generic.tok
[@@deriving show, eq]

(* we sometimes need to convert to an any to be able to use
 * Lib_AST.ii_of_any, or Lib_AST.abstract_position_info_any
 *)
(* coupling: this function should be an inverse to the function below! *)
let mvalue_to_any = function
  | E e -> G.E e
  | S s -> G.S s
  (* bugfix: do not return G.I id. We need the id_info because
   * it can be used to check if two metavars are equal and have the same
   * sid (single unique id).
   *)
  | Id (id, Some idinfo) -> G.E (G.N (G.Id (id, idinfo)) |> G.e)
  | Id (id, None) -> G.E (G.N (G.Id (id, G.empty_id_info ())) |> G.e)
  | N x -> G.Name x
  | XmlAt x -> G.XmlAt x
  | Raw x -> G.E (G.RawExpr x |> G.e)
  | Ss x -> G.Ss x
  | Args x -> G.Args x
  | Params x -> G.Params x
  | Xmls x -> G.Xmls x
  | T x -> G.T x
  | P x -> G.P x
  | Text (s, info, _) ->
      G.E (G.L (G.String (Tok.unsafe_fake_bracket (s, info))) |> G.e)

(* coupling: this function should be an inverse to the function above! *)
let mvalue_of_any = function
  | G.E { e = G.N (Id (id, idinfo)); _ } -> Some (Id (id, Some idinfo))
  | E { e = RawExpr x; _ }
  | Raw x ->
      Some (Raw x)
  | E { e = L (String (_, (s, info), _)); _ } ->
      Some (Text (s, info, G.fake ""))
  | E e -> Some (E e)
  | S s -> Some (S s)
  | Name x -> Some (N x)
  | XmlAt x -> Some (XmlAt x)
  | Ss x -> Some (Ss x)
  | Args x -> Some (Args x)
  | Params x -> Some (Params x)
  | Xmls x -> Some (Xmls x)
  | T x -> Some (T x)
  | P x -> Some (P x)
  | At _
  | Fld _
  | Flds _
  | Partial _
  | I _
  | Str _
  | Def _
  | Dir _
  | Pr _
  | Tk _
  | TodoK _
  | Ar _
  | Pa _
  | Tp _
  | Ta _
  | Modn _
  | Ce _
  | Cs _
  | ForOrIfComp _
  | ModDk _
  | En _
  | Dk _
  | Di _
  | Lbli _
  | Anys _ ->
      None

(* This is used for metavariable-pattern: where we need to transform the content
 * of a metavariable into a program so we can use evaluate_formula on it *)
let program_of_mvalue : mvalue -> G.program option =
 fun mval ->
  match mval with
  | E expr -> Some [ G.exprstmt expr ]
  | S stmt -> Some [ stmt ]
  | Id (id, Some idinfo) -> Some [ G.exprstmt (G.N (G.Id (id, idinfo)) |> G.e) ]
  | Id (id, None) ->
      Some [ G.exprstmt (G.N (G.Id (id, G.empty_id_info ())) |> G.e) ]
  | N x -> Some [ G.exprstmt (G.N x |> G.e) ]
  | Raw x -> Some [ G.exprstmt (G.RawExpr x |> G.e) ]
  | Ss stmts -> Some stmts
  | Params _
  | Args _
  | Xmls _
  | T _
  | P _
  | XmlAt _
  | Text _ ->
      logger#debug "program_of_mvalue: not handled '%s'" (show_mvalue mval);
      None

let range_of_mvalue mval =
  let* tok_start, tok_end =
    AST_generic_helpers.range_of_any_opt (mvalue_to_any mval)
  in
  (* We must return both the range *and* the file, due to metavariable-pattern
   * using temporary files. See [Match_rules.satisfies_metavar_pattern_condition]. *)
  Some (tok_start.pos.file, Range.range_of_token_locations tok_start tok_end)

let ii_of_mval x = x |> mvalue_to_any |> AST_generic_helpers.ii_of_any
let str_of_mval x = show_mvalue x

(* note that the mvalue acts as the value of the metavar and also
   as its concrete code "witness". You can get position information from it,
   it is not Parse_info.Ab(stractPos)

   TODO: ensure that ["$A", Foo; "$B", Bar] and ["$B", Bar; "$A", Foo]
   are equivalent for the equal functions.
*)
type bindings = (mvar * mvalue) list (* = Common.assoc *) [@@deriving show, eq]

(* ex: $X, $FAIL, $VAR2, $_
 * Note that some languages such as PHP or Javascript allows '$' in identifier
 * names, so forcing metavariables to have uppercase letters at least allow
 * us to match specifically also identifiers in lower case (e.g., $foo will
 * only match the $foo identifiers in some concrete code; this is not a
 * metavariable).
 * We allow _ as a prefix to disable the unused-metavar check (we use
 * the same convention than OCaml).
 * However this conflicts with PHP superglobals, hence the special
 * cases below in is_metavar_name.
 * coupling: AST_generic.is_metavar_name
 *)
let metavar_regexp_string = "^\\(\\$[A-Z_][A-Z_0-9]*\\)$"

(*
 * Hacks abusing existing constructs to encode extra constructions.
 * One day we will have a pattern_ast.ml that mimics mostly
 * AST.ml and extends it with special sgrep constructs.
 *)
let is_metavar_name s =
  match s with
  (* ugly: we should probably pass the language to is_metavar_name, but
   * that would require to thread it through lots of functions, so for
   * now we have this special case for PHP superglobals.
   * ref: https://www.php.net/manual/en/language.variables.superglobals.php
   *)
  | "$_SERVER"
  | "$_GET"
  | "$_POST"
  | "$_FILES"
  | "$_COOKIE"
  | "$_SESSION"
  | "$_REQUEST"
  | "$_ENV"
    (* todo: there's also "$GLOBALS" but this may interface with existing rules*)
    ->
      false
  | __else__ -> s =~ metavar_regexp_string

(* $...XXX multivariadic metavariables. Note that I initially chose
 * $X... but this leads to parsing conflicts in Javascript.
 *)
let metavar_ellipsis_regexp_string = "^\\(\\$\\.\\.\\.[A-Z_][A-Z_0-9]*\\)$"
let is_metavar_ellipsis s = s =~ metavar_ellipsis_regexp_string

let mvars_of_regexp_string s =
  Regexp_engine.pcre_compile s
  |> Regexp_engine.pcre_regexp |> Pcre.names |> Array.to_list
  |> Common.(map (fun s -> spf "$%s" s))

(* TODO: remove when we kill numeric capture groups *)
let metavar_for_capture_group = "^\\(\\$[0-9]+\\)$"
let is_metavar_for_capture_group s = s =~ metavar_for_capture_group

module Syntactic = struct
  let equal_mvalue = AST_utils.with_syntactic_equal equal_mvalue
  let equal_bindings = AST_utils.with_syntactic_equal equal_bindings
end

module Structural = struct
  let equal_mvalue = AST_utils.with_structural_equal equal_mvalue
  let equal_bindings = AST_utils.with_structural_equal equal_bindings
end
