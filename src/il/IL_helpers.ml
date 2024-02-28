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
open IL

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

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
  | Record ys ->
      lvals_of_exps
        (ys
        |> List_.map @@ function
           | Field (_, e)
           | Spread e ->
               e)
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
  | NLambda _
  | NOther _
  | NTodo _ ->
      []

module LvalOrdered = struct
  type t = lval

  let compare lval1 lval2 =
    (* Right now we only care about comparing lvals of the form `x.a_1. ... . a_n,
       so it's OK if `Stdlib.compare` may not be the ideal comparison function for
       the remaining cases. *)
    match (lval1, lval2) with
    | { base = Var x; rev_offset = ro1 }, { base = Var y; rev_offset = ro2 } ->
        let name_cmp = compare_name x y in
        if name_cmp <> 0 then name_cmp
        else
          List.compare
            (fun offset1 offset2 ->
              match (offset1.o, offset2.o) with
              | Dot a, Dot b -> compare_name a b
              | Index { e = Operator ((G.Mult, _), []); _ }, Index _
              | Index _, Index { e = Operator ((G.Mult, _), []); _ } ->
                  (* This stands for an arbitrary index, like '[*]',
                   * see 'Pro_taint_lval_env.lval_index_any' and
                   * 'Pro_taint_lval_lva.normalize_rev_offset'.
                   * Any index will be considered equals to '[*]'. *)
                  0
              | ( Index { e = Literal (Int (Some i1, _)); _ },
                  Index { e = Literal (Int (Some i2, _)); _ } ) ->
                  (* [n] *)
                  Int64.compare i1 i2
              | ( Index { e = Literal (String (_, (s1, _), _)); _ },
                  Index { e = Literal (String (_, (s2, _), _)); _ } ) ->
                  (* ["..."] *)
                  String.compare s1 s2
              | Index _, _
              | _, Index _ ->
                  Stdlib.compare offset1 offset2)
            ro1 ro2
    | _, _ -> Stdlib.compare lval1 lval2
end

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
  | NLambda _
  | NOther _
  | NTodo _ ->
      None