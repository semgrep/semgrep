(* Iago Abal
 *
 * Copyright (C) 2024 Semgrep Inc.
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
module Log = Log_tainting.Log
module T = Taint
module Taints = T.Taint_set
open Shape_and_sig.Shape
module Fields = Shape_and_sig.Fields
module Shape = Taint_shape

(* Try to get an idnetifier from a callee/function expression, to be used in
 * a taint trace. *)
let get_ident_of_callee callee =
  match callee with
  | { IL.e = Fetch f; eorig = _ } -> (
      match f with
      (* Case `f()` *)
      | { base = Var { ident; _ }; rev_offset = []; _ }
      (* Case `obj. ... .m()` *)
      | { base = _; rev_offset = { o = Dot { ident; _ }; _ } :: _; _ } ->
          Some ident
      | __else__ -> None)
  | __else__ -> None

(* TODO: Move to 'Taint' module ? *)
let subst_in_precondition ~inst_lval ~inst_ctrl taint =
  let subst taints =
    taints
    |> List.concat_map (fun t ->
           match t.T.orig with
           | Src _ -> [ t ]
           | Var lval -> (
               match inst_lval lval with
               | None -> []
               | Some (var_taints, _var_shape) -> var_taints |> Taints.elements)
           | Shape_var lval -> (
               match inst_lval lval with
               | None -> []
               | Some (_var_taints, var_shape) ->
                   Shape.gather_all_taints_in_shape var_shape |> Taints.elements
               )
           | Control -> inst_ctrl () |> Taints.elements)
  in
  T.map_preconditions subst taint

let instantiate_taint_var ~inst_lval ~inst_ctrl taint =
  match taint.T.orig with
  | Src _ -> None
  | Var lval -> inst_lval lval
  | Shape_var lval ->
      (* This is just a delayed 'gather_all_taints_in_shape'. *)
      let* taints =
        inst_lval lval
        |> Option.map (fun (_taints, shape) ->
               Shape.gather_all_taints_in_shape shape)
      in
      Some (taints, Bot)
  | Control ->
      (* 'Control' is pretty much like a taint variable so we handle all together. *)
      Some (inst_ctrl (), Bot)

(* TODO: Move to 'Taint' module ? *)
let instantiate_taint ~callee ~inst_lval ~inst_ctrl taint =
  let inst_taint_var taint =
    instantiate_taint_var ~inst_lval ~inst_ctrl taint
  in
  match taint.T.orig with
  | Src src -> (
      let taint =
        (* Update taint trace.
         *
         * E.g. the call to 'bar' in:
         *
         *     1 def bar():
         *     2     x = taint
         *     3     return x
         *     4
         *     5 def foo():
         *     6     bar()
         *     7     ...
         *
         * would result in this call trace:
         *
         *     Call('bar' @l.6, ["x" @l.2], "taint" @l.2)
         *)
        match callee with
        | { IL.e = _; eorig = SameAs orig_callee } ->
            let call_trace =
              T.Call (orig_callee, taint.tokens, src.call_trace)
            in
            { T.orig = Src { src with call_trace }; tokens = [] }
        | __else__ -> taint
      in
      match subst_in_precondition ~inst_lval ~inst_ctrl taint with
      | None ->
          (* substitution made preconditon false, so no taint here! *)
          Taints.empty
      | Some taint -> Taints.singleton taint)
  (* Taint variables *)
  | Var _
  | Shape_var _
  | Control -> (
      match inst_taint_var taint with
      | None -> Taints.empty
      | Some (var_taints, _var_shape) ->
          (* Update taint trace.
           *
           * E.g. the call to 'bar' in:
           *
           *     1 def bar(x):
           *     2     y = x
           *     3     return y
           *     4
           *     5 def foo():
           *     6     t = bar(taint)
           *     7     ...
           *
           * would result in this list of tokens (note that is reversed):
           *
           *     ["t" @l.6; "y" @l.2; "x" @l.1; "bar" @l.6]
           *
           * This is a hack we use because taint traces aren't general enough,
           * this should be represented with a call trace.
           *)
          let extra_tokens =
            (match get_ident_of_callee callee with
            | None -> []
            | Some ident -> [ snd ident ])
            @ List.rev taint.tokens
          in
          var_taints
          |> Taints.map (fun taint' ->
                 {
                   taint' with
                   tokens = List.rev_append extra_tokens taint'.tokens;
                 }))

let instantiate_taints ~callee ~inst_lval ~inst_ctrl taints =
  taints |> Taints.elements
  |> List.fold_left
       (fun acc taint ->
         acc
         |> Taints.union (instantiate_taint ~callee ~inst_lval ~inst_ctrl taint))
       Taints.empty

let instantiate_shape ~callee ~inst_lval ~inst_ctrl shape =
  let inst_taints = instantiate_taints ~callee ~inst_lval ~inst_ctrl in
  let rec inst_shape = function
    | Bot -> Bot
    | Obj obj ->
        let obj =
          obj
          |> Fields.filter_map (fun _o cell ->
                 (* This is essentially a recursive call to 'instantiate_shape'!
                  * We rely on 'update_offset_in_cell' to maintain INVARIANT(cell). *)
                 Shape.update_offset_in_cell ~f:inst_xtaint [] cell)
        in
        if Fields.is_empty obj then Bot else Obj obj
    | Arg arg -> (
        match inst_lval (T.lval_of_arg arg) with
        | Some (_taints, shape) -> shape
        | None ->
            Log.warn (fun m ->
                m "Could not instantiate arg shape: %s" (T.show_arg arg));
            Arg arg)
  and inst_xtaint xtaint shape =
    (* This may break INVARIANT(cell) but 'update_offset_in_cell' will restore it. *)
    let xtaint =
      match xtaint with
      | `None
      | `Clean ->
          xtaint
      | `Tainted taints -> `Tainted (inst_taints taints)
    in
    let shape = inst_shape shape in
    (xtaint, shape)
  in
  inst_shape shape
