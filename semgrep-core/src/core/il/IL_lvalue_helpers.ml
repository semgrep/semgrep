(* Yoann Padioleau
 * Iago Abal
 *
 * Copyright (C) 2019-2022 r2c
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
open IL

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

let compare_name x y =
  let ident_cmp = String.compare (fst x.ident) (fst y.ident) in
  if ident_cmp <> 0 then ident_cmp else Int.compare x.sid y.sid

let rexps_of_instr x =
  match x.i with
  | Assign (_, exp) -> [ exp ]
  | AssignAnon _ -> []
  | Call (_, e1, args) -> e1 :: args
  | CallSpecial (_, _, args) -> args
  | FixmeInstr _ -> []

(* opti: could use a set *)
let rec lvals_of_exp e =
  match e.e with
  | Fetch lval -> lval :: lvals_in_lval lval
  | Literal _ -> []
  | Cast (_, e) -> lvals_of_exp e
  | Composite (_, (_, xs, _))
  | Operator (_, xs) ->
      lvals_of_exps xs
  | Record ys ->
      lvals_of_exps
        (ys
        |> Common.map @@ function
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
      (function
        | Index e -> lvals_of_exp e
        | _ -> [])
      lval.rev_offset
  in
  base_lvals @ offset_lvals

and lvals_of_exps xs = xs |> Common.map lvals_of_exp |> List.flatten

(** The lvals in the RHS of the instruction. *)
let rlvals_of_instr x =
  let exps = rexps_of_instr x in
  lvals_of_exps exps

(*****************************************************************************)
(* Public *)
(*****************************************************************************)

let lval_is_var_and_dots { base; rev_offset } =
  match base with
  | Var _ ->
      rev_offset
      |> List.for_all (function
           | Dot _ -> true
           | Index _ -> false)
  | VarSpecial _
  | Mem _ ->
      false

let lval_is_dotted_prefix lval1 lval2 =
  let eq_name x y = compare_name x y = 0 in
  let rec offset_prefix os1 os2 =
    match (os1, os2) with
    | [], _ -> true
    | _ :: _, [] -> false
    | Index _ :: _, _
    | _, Index _ :: _ ->
        false
    | Dot a :: os1, Dot b :: os2 -> eq_name a b && offset_prefix os1 os2
  in
  match (lval1, lval2) with
  | { base = Var x; rev_offset = ro1 }, { base = Var y; rev_offset = ro2 } ->
      eq_name x y && offset_prefix (List.rev ro1) (List.rev ro2)
  | _ -> false

let lval_of_instr_opt x =
  match x.i with
  | Assign (lval, _)
  | AssignAnon (lval, _)
  | Call (Some lval, _, _)
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
  | TrueNode
  | FalseNode
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
              match (offset1, offset2) with
              | Dot a, Dot b -> compare_name a b
              | Index _, _
              | _, Index _ ->
                  Stdlib.compare offset1 offset2)
            ro1 ro2
    | _, _ -> Stdlib.compare lval1 lval2
end
