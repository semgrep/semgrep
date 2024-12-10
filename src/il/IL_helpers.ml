(* Yoann Padioleau
 * Iago Abal
 *
 * Copyright (C) 2019-2022 r2c
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
open IL

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(***********************************************)
(* L-values *)
(***********************************************)

let exp_of_arg arg =
  match arg with
  | Unnamed exp -> exp
  | Named (_, exp) -> exp

let rexps_of_instr x =
  match x.i with
  | Assign (({ base = Var _; rev_offset = _ :: _ } as lval), exp) ->
      [ { e = Fetch { lval with rev_offset = [] }; eorig = NoOrig }; exp ]
  | Assign (_, exp) -> [ exp ]
  | AssignAnon _ -> []
  | Call (_, e1, args) -> e1 :: List_.map exp_of_arg args
  | New (_, _, _, args)
  | CallSpecial (_, _, args) ->
      List_.map exp_of_arg args
  | FixmeInstr _ -> []

(* opti: could use a set *)
let rec lvals_of_exp e =
  match e.e with
  | Fetch lval -> lval :: lvals_in_lval lval
  | Literal _ -> []
  | Cast (_, e) -> lvals_of_exp e
  | Composite (_, (_, xs, _)) -> lvals_of_exps xs
  | Operator (_, xs) -> lvals_of_exps (List_.map exp_of_arg xs)
  | RecordOrDict ys ->
      lvals_of_exps
        (ys
        |> List.concat_map @@ function
           | Field (_, e)
           | Spread e ->
               [ e ]
           | Entry (ke, ve) -> [ ke; ve ])
  | FixmeExp (_, _, Some e) -> lvals_of_exp e
  | FixmeExp (_, _, None) -> []

and lvals_in_lval lval =
  let base_lvals =
    match lval.base with
    | Mem e -> lvals_of_exp e
    | _else_ -> []
  in
  let offset_lvals =
    List.concat_map
      (fun offset ->
        match offset.o with
        | Index e -> lvals_of_exp e
        | Dot _ -> [])
      lval.rev_offset
  in
  base_lvals @ offset_lvals

and lvals_of_exps xs = xs |> List.concat_map lvals_of_exp

(** The lvals in the rvals of the instruction. *)
let rlvals_of_instr x =
  let exps = rexps_of_instr x in
  lvals_of_exps exps

(*****************************************************************************)
(* Public *)
(*****************************************************************************)

let is_pro_resolved_global name =
  match !(name.id_info.id_resolved) with
  | Some (GlobalName _, _sid) -> true
  | Some _
  | None ->
      false

(* HACK: Because we don't have a "Class" type, classes have themselves as types. *)
let is_class_name (name : name) =
  match (!(name.id_info.id_resolved), !(name.id_info.id_type)) with
  | Some resolved1, Some { t = TyN (Id (_, { id_resolved; _ })); _ } -> (
      match !id_resolved with
      | None -> false
      | Some resolved2 ->
          (* If 'name' has type 'name' then we assume it's a class. *)
          AST_generic.equal_resolved_name resolved1 resolved2)
  | _, None
  | _, Some _ ->
      false

(***********************************************)
(* L-values *)
(***********************************************)

let lval_of_var var = { IL.base = Var var; rev_offset = [] }

let is_dots_offset offset =
  offset
  |> List.for_all (fun o ->
         match o.o with
         | Dot _ -> true
         | Index _ -> false)

let lval_of_instr_opt x =
  match x.i with
  | Assign (lval, _)
  | AssignAnon (lval, _)
  | Call (Some lval, _, _)
  | New (lval, _, _, _)
  | CallSpecial (Some lval, _, _) ->
      Some lval
  | Call _
  | CallSpecial _ ->
      None
  | FixmeInstr _ -> None

let lvar_of_instr_opt x =
  match lval_of_instr_opt x with
  | Some { base = Var x; _ } -> Some x
  | Some _
  | None ->
      None

let rlvals_of_node = function
  | Enter
  | Exit
  (* must ignore exp in True and False *)
  | TrueNode _
  | FalseNode _
  | NGoto _
  | Join ->
      []
  | NInstr x -> rlvals_of_instr x
  | NCond (_, e)
  | NReturn (_, e)
  | NThrow (_, e) ->
      lvals_of_exp e
  | NOther _
  | NTodo _ ->
      []

let orig_of_node = function
  | Enter
  | Exit ->
      None
  | TrueNode e
  | FalseNode e
  | NCond (_, e)
  | NReturn (_, e)
  | NThrow (_, e) ->
      Some e.eorig
  | NInstr i -> Some i.iorig
  | NGoto _
  | Join
  | NOther _
  | NTodo _ ->
      None

(***********************************************)
(* CFG *)
(***********************************************)

let rec reachable_nodes fun_cfg =
  let main_nodes = CFG.reachable_nodes fun_cfg.cfg in
  let lambdas_nodes =
    fun_cfg.lambdas |> NameMap.to_seq
    |> Seq.map (fun (_lname, lcfg) -> reachable_nodes lcfg)
  in
  Seq.concat (Seq.cons main_nodes lambdas_nodes)

(***********************************************)
(* Lambdas *)
(***********************************************)

let lval_is_lambda lambdas_cfgs lval =
  match lval with
  | { base = Var name; rev_offset = [] } ->
      let* lambda_cfg = IL.NameMap.find_opt name lambdas_cfgs in
      Some (name, lambda_cfg)
  | { base = Var _ | VarSpecial _ | Mem _; rev_offset = _ } ->
      (* Lambdas are only assigned to plain variables without any offset. *)
      None
