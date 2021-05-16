(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common
module MV = Metavars_php
module A = Cst_php
module B = Cst_php
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * This module makes it possible to match and transform one PHP AST
 * against another PHP AST providing a kind of patch but at a
 * syntactical level.
 *
 * To understand the logic behind this code it may help to first read
 * this: http://coccinelle.lip6.fr/papers/eurosys08.pdf
 * See also https://github.com/facebook/pfff/wiki/Spatch#wiki-spacing-issues
 *)

(*****************************************************************************)
(* The functor argument *)
(*****************************************************************************)

module XMATCH = struct
  (* ------------------------------------------------------------------------*)
  (* Combinators history *)
  (* ------------------------------------------------------------------------*)
  (*
   * version0:
   *   type ('a, 'b) matcher = 'a -> 'b -> bool
   *
   *   This just lets you know if you matched something.
   *
   * version1:
   *   type ('a, 'b) matcher = 'a -> 'b -> unit -> ('a, 'b) option
   *
   *   The Maybe monad.
   *
   * version2:
   *   type ('a, 'b) matcher = 'a -> 'b -> binding -> binding list
   *
   *   Why not returning a binding option ? because I may need at some
   *   point to return multiple possible bindings for one matching code.
   *   For instance with the pattern do 'f(..., X, ...)', X could be binded
   *   to different parts of the code.
   *   Note that the empty list means a match failure.
   *)

  type tin = MV.metavars_binding

  type 'x tout = ('x * MV.metavars_binding) list

  type ('a, 'b) matcher = 'a -> 'b -> tin -> ('a * 'b) tout

  let (( >>= ) :
        (tin -> ('a * 'b) tout) ->
        ('a * 'b -> tin -> ('c * 'd) tout) ->
        tin ->
        ('c * 'd) tout) =
   fun m1 m2 tin ->
    (* old:
       match m1 tin with
       | None -> None
       | Some (a,b) ->
       m2 (a, b) tin
    *)
    (* let's get a list of possible environment match (could be
     * the empty list when it didn't match, playing the role None
     * had before)
     *)
    let xs = m1 tin in
    (* try m2 on each possible returned bindings *)
    let xxs = xs |> List.map (fun ((a, b), binding) -> m2 (a, b) binding) in
    List.flatten xxs

  let ( >||> ) m1 m2 tin =
    (* CHOICE
          let xs = m1 tin in
          if null xs
          then m2 tin
          else xs
    *)
    (* opti? use set instead of list *)
    m1 tin @ m2 tin

  let return (a, b) tin =
    (* old: Some (a,b) *)
    [ ((a, b), tin) ]

  let fail _tin =
    (* old: None *)
    []

  (* ------------------------------------------------------------------------*)
  (* Environment *)
  (* ------------------------------------------------------------------------*)

  (* pre: both 'a' and 'b' contains only regular PHP code. There is no
   * metavariables in them.
   *)
  let equal_ast_binded_code a b =
    match (a, b) with
    | A.Expr _, A.Expr _
    | A.XhpAttrValue _, A.XhpAttrValue _
    | A.Ident2 _, B.Ident2 _
    | A.Argument _, B.Argument _
    | A.Hint2 _, B.Hint2 _ ->
        (* Note that because we want to retain the position information
         * of the matched code in the environment (e.g. for the -pvar
         * sgrep command line argument), we can not just use the
         * generic '=' OCaml operator as 'a' and 'b' may represent
         * the same code but they will contain leaves in their AST
         * with different position information. So before doing
         * the comparison we just need to remove/abstract-away
         * the line number information in each ASTs.
         *)
        let a = Lib_parsing_php.abstract_position_info_any a in
        let b = Lib_parsing_php.abstract_position_info_any b in
        a =*= b
    | _, _ -> false

  (* This is quite similar to the code in matching_php.ml
   *
   * Note that in spatch we actually first calls match_x_x to get the
   * environment and then we redo another pass by calling transform_x_x.
   * So tin will be already populated with all metavariables so
   * equal_ast_binded_code will be called even when we don't use
   * two times the same metavariable in the pattern.
   *)
  let check_and_add_metavar_binding (mvar, valu) tin =
    match Common2.assoc_opt (mvar : string) tin with
    | Some valu' ->
        (* Should we use php_vs_php itself for comparing the binded code ?
         * Hmmm, we can't because it leads to a circular dependencies.
         * Moreover here we know both valu and valu' are regular PHP code,
         * not PHP patterns, so we can just use the generic '=' of OCaml.
         *)
        if equal_ast_binded_code valu valu' then Some tin else None
    | None ->
        (* first time the metavar is binded. Just add it to the environment *)
        Some (Common2.insert_assoc (mvar, valu) tin)

  let subst_metavars env add =
    let env =
      env |> List.map (fun (mvar, any) -> (mvar, Unparse_php.string_of_any any))
    in
    match add with
    | PI.AddNewlineAndIdent -> PI.AddNewlineAndIdent
    | PI.AddStr s ->
        (* transforming first metavar variable ($X) and then
         * mevar (X)
         *)
        let s =
          s
          |> Common2.global_replace_regexp MV.metavar_variable_regexp_string
               (fun matched ->
                 try List.assoc matched env
                 with Not_found ->
                   failwith
                     (spf "metavariable %s was not found in environment"
                        matched))
        in

        let s =
          s
          |> Common2.global_replace_regexp MV.metavar_regexp_string
               (fun matched ->
                 try List.assoc matched env
                 with Not_found ->
                   failwith
                     (spf "metavariable %s was not found in environment"
                        matched))
        in
        PI.AddStr s

  (* when a transformation contains a '+' part, as in
   * - 2
   * + bar(X)
   *
   * then before applying the transformation we need first to
   * substitute all metavariables by their actual binded value
   * in the environment.
   *)
  let adjust_transfo_with_env env transfo =
    match transfo with
    | PI.NoTransfo | PI.Remove -> transfo
    | PI.AddBefore add -> PI.AddBefore (subst_metavars env add)
    | PI.AddAfter add -> PI.AddAfter (subst_metavars env add)
    | PI.Replace add -> PI.Replace (subst_metavars env add)
    | PI.AddArgsBefore _ -> raise Todo

  (*
   * Sometimes a metavariable like X will match an expression made of
   * multiple tokens  like  '1*2'.
   * This metavariable may have a transformation associated with it,
   * like  '- X',  in which case we want to propagate the removal
   * transformation to all the tokens in the matched expression.
   *
   * In some cases the transformation may also contains a +, as in
   *   - X
   *   + 3
   * in which case we can not just propagate the transformation
   * to all the tokens. Indeed doing so would duplicate the '+ 3'
   * on all the matched tokens. We need instead to distribute
   * the removal transformation and associate the '+' transformation
   * part only to the very last matched token by X (here '2').
   *)

  let distribute_transfo transfo any env =
    let ii = Lib_parsing_php.ii_of_any any in

    match transfo with
    | PI.NoTransfo -> ()
    | PI.Remove -> ii |> List.iter (fun tok -> tok.PI.transfo <- PI.Remove)
    | PI.Replace _add ->
        ii |> List.iter (fun tok -> tok.PI.transfo <- PI.Remove);
        let any_ii = List.hd ii in
        any_ii.PI.transfo <- adjust_transfo_with_env env transfo
    | PI.AddBefore _add -> raise Todo
    | PI.AddAfter _add -> raise Todo
    | PI.AddArgsBefore _ -> raise Todo

  let (envf : (Metavars_php.mvar Cst_php.wrap, Cst_php.any) matcher) =
   fun (mvar, imvar) any tin ->
    match check_and_add_metavar_binding (mvar, any) tin with
    | None -> fail tin
    | Some new_binding ->
        distribute_transfo imvar.PI.transfo any tin;

        return ((mvar, imvar), any) new_binding

  (* ugly: in the case of string metavariables like "A", as in
   *   foo("A")   vs  foo("Ent")
   * we want A to bind to the unquoted string Ent (not "Ent"),
   * but we still want it to apply the possible transfo on "Ent".
   * So we really need to pass two different things, the any
   * we want to add in the environment and the any we want
   * to match against and transform.
   *)
  let (envf2 :
        (Metavars_php.mvar Cst_php.wrap, Cst_php.any * Cst_php.any) matcher) =
   fun (mvar, imvar) (any1, any2) tin ->
    match check_and_add_metavar_binding (mvar, any1) tin with
    | None -> fail tin
    | Some new_binding ->
        distribute_transfo imvar.PI.transfo any2 tin;

        return ((mvar, imvar), (any1, any2)) new_binding

  (* propagate the transformation info *)
  let tokenf a b tin =
    let transfo = a.PI.transfo in
    b.PI.transfo <- adjust_transfo_with_env tin transfo;
    return (a, b) tin
end

(*****************************************************************************)
(* Entry point  *)
(*****************************************************************************)

module MATCH = Php_vs_php.PHP_VS_PHP (XMATCH)

type ('a, 'b) transformer = 'a -> 'b -> Metavars_php.metavars_binding list

let transform_e_e pattern e env = ignore (MATCH.m_expr pattern e env)

let transform_st_st pattern e env = ignore (MATCH.m_stmt pattern e env)

let transform_xhp_xhp pattern e env = ignore (MATCH.m_xhp_html pattern e env)

let transform_hint_hint pattern e env = ignore (MATCH.m_hint_type pattern e env)
