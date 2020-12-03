(* Yoann Padioleau
 *
 * Copyright (C) 2013 Facebook
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

module MV = Metavars_fuzzy
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * This module makes it possible to match and transform one tree
 * against another tree providing a kind of patch but at a
 * syntactical level.
 *
 * To understand the logic behind this code it may help to first read
 * this: http://coccinelle.lip6.fr/papers/eurosys08.pdf
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

  type tin = MV.fuzzy_binding
  type 'x tout = ('x * MV.fuzzy_binding) list
  type ('a, 'b) matcher = 'a -> 'b  -> tin -> ('a * 'b) tout

  let ((>>=):
         (tin -> ('a * 'b) tout)  ->
       (('a * 'b) -> (tin -> ('c * 'd) tout)) ->
       (tin -> ('c * 'd) tout)) =
    fun m1 m2 ->
    fun tin ->
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
    let xxs = xs |> List.map (fun ((a,b), binding) ->
      m2 (a, b) binding
    ) in
    List.flatten xxs

  let (>||>) m1 m2 = fun tin ->
    (* CHOICE
          let xs = m1 tin in
          if null xs
          then m2 tin
          else xs
    *)
    (* opti? use set instead of list *)
    m1 tin @ m2 tin


  let return (a,b) = fun tin ->
    (* old: Some (a,b) *)
    [(a,b), tin]

  let fail = fun _tin ->
    (* old: None *)
    []

  (* ------------------------------------------------------------------------*)
  (* Environment *)
  (* ------------------------------------------------------------------------*)

  let subst_metavars _env x =
    (* TODO *)
    x

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
    | PI.NoTransfo
    | PI.Remove -> transfo

    | PI.AddBefore add ->
        PI.AddBefore (subst_metavars env add)
    | PI.AddAfter add ->
        PI.AddAfter (subst_metavars env add)
    | PI.Replace add ->
        PI.Replace (subst_metavars env add)
    | PI.AddArgsBefore _ -> raise Todo
  (* propagate the transformation info *)
  let tokenf a b = fun tin ->

    let a1 = Parse_info.str_of_info a in
    let b1 = Parse_info.str_of_info b in
    if a1 =$= b1
    then begin
      let transfo = a.PI.transfo in
      b.PI.transfo <- adjust_transfo_with_env tin transfo;
      return (a, b) tin
    end
    else fail tin

  (* ------------------------------------------------------------------------*)
  (* Environment *)
  (* ------------------------------------------------------------------------*)

  (* pre: both 'a' and 'b' contains only regular PHP code. There is no
   * metavariables in them.
   * coupling: don't forget to also modify the one in matching_fuzzy.ml
   * todo: factorize code
  *)
  let equal_ast_binded_code a b =

    (* Note that because we want to retain the position information
     * of the matched code in the environment (e.g. for the -pvar
     * sgrep command line argument), we can not just use the
     * generic '=' OCaml operator as 'a' and 'b' may represent
     * the same code but they will contain leaves in their AST
     * with different position information. So before doing
     * the comparison we just need to remove/abstract-away
     * the line number information in each ASTs.
     *
     * less: optimize by caching the abstract_lined ?
    *)
    let a = Lib_ast_fuzzy.abstract_position_trees a in
    let b = Lib_ast_fuzzy.abstract_position_trees b in
    a =*= b


  (* This is quite similar to the code in matching_fuzzy.ml
   *
   * Note that in spatch we actually first calls match_x_x to get the
   * environment and then we redo another pass by calling transform_x_x.
   * So tin will be already populated with all metavariables so
   * equal_ast_binded_code will be called even when we don't use
   * two times the same metavariable in the pattern.
  *)
  let check_and_add_metavar_binding((mvar:string), valu) = fun tin ->
    match Common2.assoc_opt mvar tin with
    | Some valu' ->
        (* Should we use fuzzy_vs_fuzzy itself for comparing the binded code ?
         * Hmmm, we can't because it leads to a circular dependencies.
         * Moreover here we know both valu and valu' are regular code,
         * not patterns, so we can just use the generic '=' of OCaml.
        *)
        if equal_ast_binded_code valu valu'
        then Some tin
        else None
    | None ->
        (* first time the metavar is binded, just add it to the environment *)
        Some (Common2.insert_assoc (mvar, valu) tin)




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
    let ii = Lib_ast_fuzzy.toks_of_trees any in

    (match transfo with
     | PI.NoTransfo -> ()
     | PI.Remove ->
         ii |> List.iter (fun tok -> tok.PI.transfo <- PI.Remove)
     | PI.Replace _add ->
         ii |> List.iter (fun tok -> tok.PI.transfo <- PI.Remove);
         (match ii with
          | [ii] -> ii.PI.transfo <- adjust_transfo_with_env env transfo;
          | _ -> failwith "metavar matching multi tokens not supported yet"
         )
     | PI.AddBefore _add -> raise Todo
     | PI.AddAfter _add ->
         (match ii with
          | [ii] -> ii.PI.transfo <- adjust_transfo_with_env env transfo;
          | _ -> failwith "metavar matching multi tokens not supported yet"
         )

     | PI.AddArgsBefore _ -> raise Todo
    )


  let (envf: (Metavars_fuzzy.mvar * Parse_info.t, Ast_fuzzy.trees) matcher) =
    fun (mvar, tok) any  -> fun tin ->
    match check_and_add_metavar_binding (mvar, any) tin with
    | None ->
        fail tin
    | Some new_binding ->
        distribute_transfo tok.PI.transfo any tin;

        return ((mvar, tok), any) new_binding
end

(*****************************************************************************)
(* Entry point  *)
(*****************************************************************************)

module MATCH  = Fuzzy_vs_fuzzy.X_VS_X (XMATCH)

type ('a, 'b) transformer = 'a -> 'b ->
  Metavars_fuzzy.fuzzy_binding list

let transform_trees_trees pattern e   env =
  ignore (MATCH.m_trees pattern e   env)
