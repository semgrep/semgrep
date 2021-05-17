(* Yoann Padioleau
 *
 * Copyright (C) 2010-2012 Facebook
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
module Ast = Cst_php
module MV = Metavars_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * This module makes it possible to match one PHP AST against another PHP AST
 * providing a kind of grep but at a syntactical level.
 *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, _pr2_once = Common2.mk_pr2_wrappers Flag_matcher_php.verbose

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
   *
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
   * coupling: don't forget to also modify the one in transforming_php.ml
   * todo: factorize code
   *)
  let equal_ast_binded_code a b =
    match (a, b) with
    | Ast.Ident2 _, Ast.Ident2 _
    | Ast.Expr _, Ast.Expr _
    | Ast.XhpAttrValue _, Ast.XhpAttrValue _
    | Ast.Argument _, Ast.Argument _ ->
        (* Note that because we want to retain the position information
         * of the matched code in the environment (e.g. for the -pvar
         * sgrep command line argument), we can not just use the
         * generic '=' OCaml operator as 'a' and 'b' may represent
         * the same code but they will contain leaves in their AST
         * with different position information. So before doing
         * the comparison we just need to remove/abstract-away
         * the line number information in each ASTs.
         *
         * todo: optimize by caching the abstract_lined ?
         *)
        let a = Lib_parsing_php.abstract_position_info_any a in
        let b = Lib_parsing_php.abstract_position_info_any b in
        a =*= b
    | _, _ -> false

  let check_and_add_metavar_binding ((mvar : Metavars_php.mvar), valu) tin =
    match Common2.assoc_opt mvar tin with
    | Some valu' ->
        (* Should we use php_vs_php itself for comparing the binded code ?
         * Hmmm, we can't because it leads to a circular dependencies.
         * Moreover here we know both valu and valu' are regular PHP code,
         * not PHP patterns, so we can just use the generic '=' of OCaml.
         *)
        if equal_ast_binded_code valu valu' then Some tin else None
    | None ->
        (* first time the metavar is binded, just add it to the environment *)
        Some (Common2.insert_assoc (mvar, valu) tin)

  let (envf : (Metavars_php.mvar Cst_php.wrap, Cst_php.any) matcher) =
   fun (mvar, imvar) any tin ->
    match check_and_add_metavar_binding (mvar, any) tin with
    | None ->
        pr2 (spf "envf: fail, %s" mvar);
        fail tin
    | Some new_binding ->
        pr2 (spf "envf: success, %s" mvar);
        return ((mvar, imvar), any) new_binding

  let (envf2 :
        (Metavars_php.mvar Cst_php.wrap, Cst_php.any * Cst_php.any) matcher) =
   fun (mvar, imvar) (any1, any2) tin ->
    match check_and_add_metavar_binding (mvar, any1) tin with
    | None ->
        pr2 (spf "envf2: fail, %s" mvar);
        fail tin
    | Some new_binding ->
        pr2 (spf "envf2: success, %s" mvar);
        return ((mvar, imvar), (any1, any2)) new_binding

  let tokenf a b =
    (* dont care about position, space/indent/comment isomorphism *)
    return (a, b)
end

(*****************************************************************************)
(* Entry point  *)
(*****************************************************************************)

module MATCH = Php_vs_php.PHP_VS_PHP (XMATCH)

type ('a, 'b) matcher = 'a -> 'b -> Metavars_php.metavars_binding list

let empty_environment () = []

let (extract_bindings : 'a XMATCH.tout -> MV.metavars_binding list) =
 fun tout -> tout |> List.map (fun (_term, env) -> env)

(* todo: should maybe have a match_any_any *)
let match_e_e pattern e =
  let env = empty_environment () in
  MATCH.m_expr pattern e env |> extract_bindings

let match_st_st pattern e =
  let env = empty_environment () in
  MATCH.m_stmt pattern e env |> extract_bindings

let match_xhp_xhp pattern e =
  let env = empty_environment () in
  MATCH.m_xhp_html pattern e env |> extract_bindings

let match_hint_hint pattern e =
  let env = empty_environment () in
  MATCH.m_hint_type pattern e env |> extract_bindings
