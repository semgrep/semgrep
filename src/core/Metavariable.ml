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
module H = AST_generic_helpers
module Log = Log_semgrep.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type mvar = Mvar.t [@@deriving show, eq, hash]

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
  (* We keep the `Any` variant here, despite it being a superset of the above
     variants, as a "last resort" so that we can embed any match into an
     mvalue.
     This is primarily useful for the `as:` rule feature, which lets us
     bind arbitrary matches to metavariables.
  *)
  | Any of AST_generic.any
[@@deriving show, eq]

(* we sometimes need to convert to an any to be able to use
 * Lib_AST.ii_of_any, or Lib_AST.abstract_position_info_any
 *)
(* coupling: this function should be an inverse to 'mvalue_of_any' below! *)
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
  | Any any -> any

(* coupling: mvalue_to_any *)
let mvalue_to_expr = function
  | E e -> Some e
  | Id (id, Some idinfo) -> Some (G.N (G.Id (id, idinfo)) |> G.e)
  | Id (id, None) -> Some (G.N (G.Id (id, G.empty_id_info ())) |> G.e)
  | N x -> Some (G.N x |> G.e)
  | Raw x -> Some (G.RawExpr x |> G.e)
  | Text (s, info, _) ->
      Some (G.L (G.String (Tok.unsafe_fake_bracket (s, info))) |> G.e)
  | S _
  | XmlAt _
  | Ss _
  | Args _
  | Params _
  | Xmls _
  | T _
  | P _
  | Any _ ->
      None

(* coupling: this function should be an inverse to the function above! *)
let mvalue_of_any any =
  match any with
  | G.E { e = G.N (Id (id, idinfo)); _ } -> Id (id, Some idinfo)
  | E { e = RawExpr x; _ }
  | Raw x ->
      Raw x
  | E { e = L (String (_, (s, info), _)); _ } -> Text (s, info, G.fake "")
  | E e -> E e
  | S s -> S s
  | Name x -> N x
  | XmlAt x -> XmlAt x
  | Ss x -> Ss x
  | Args x -> Args x
  | Params x -> Params x
  | Xmls x -> Xmls x
  | T x -> T x
  | P x -> P x
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
      Any any

let location_aware_equal_mvalue mval1 mval2 =
  let ranges_equal =
    let any1, any2 = (mvalue_to_any mval1, mvalue_to_any mval2) in
    let range1, range2 =
      ( AST_generic_helpers.range_of_any_opt any1,
        AST_generic_helpers.range_of_any_opt any2 )
    in
    match (range1, range2) with
    | Some (l1, r1), Some (l2, r2) ->
        Tok.equal_location l1 l2 && Tok.equal_location r1 r2
    | _ -> true
  in
  ranges_equal && equal_mvalue mval1 mval2

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
  | Text _
  (* `Any` should not be `Ss` or other variants which could be turned into a program *)
  | Any _ ->
      Log.warn (fun m ->
          m "program_of_mvalue: not handled '%s'" (show_mvalue mval));
      None

let range_of_mvalue (mval : mvalue) : (Fpath.t * Range.t) option =
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
