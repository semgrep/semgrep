(* Iago Abal
 *
 * Copyright (C) 2022 r2c
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

module T = Taint
module Taints = T.Taint_set
module LV = IL_lvalue_helpers
module Var_env = Dataflow_var_env
module VarMap = Var_env.VarMap
module LvalMap = Map.Make (LV.LvalOrdered)
module LvalSet = Set.Make (LV.LvalOrdered)

type t = {
  tainted : T.taints LvalMap.t;  (** Lvalues that are tainted. *)
  propagated : T.taints VarMap.t;
      (** Taint that is propagated via taint propagators (internally represented by
    unique propagator variables). *)
  cleaned : LvalSet.t;
      (** Lvalues that are clean, these should be extensions of other lvalues that
      are tainted. *)
}

type env = t

let empty =
  {
    tainted = LvalMap.empty;
    propagated = VarMap.empty;
    cleaned = LvalSet.empty;
  }

let empty_inout = { Dataflow_core.in_env = empty; out_env = empty }

let union le1 le2 =
  let tainted =
    LvalMap.union (fun _ x y -> Some (Taints.union x y)) le1.tainted le2.tainted
  in
  let cleaned1 =
    le1.cleaned |> LvalSet.filter (fun lv -> not @@ LvalMap.mem lv le2.tainted)
  in
  let cleaned2 =
    le2.cleaned |> LvalSet.filter (fun lv -> not @@ LvalMap.mem lv le1.tainted)
  in
  {
    tainted;
    propagated = Var_env.varmap_union Taints.union le1.propagated le2.propagated;
    cleaned = LvalSet.union cleaned1 cleaned2;
  }

let add lval taints ({ tainted; propagated; cleaned } as lval_env) =
  let taints =
    (* If the lvalue is a simple variable, we record it as part of
       the taint trace. *)
    match lval with
    | { IL.base = Var var; rev_offset = [] } ->
        let var_tok = snd var.ident in
        if Parse_info.is_fake var_tok then taints
        else
          taints
          |> Taints.map (fun t -> { t with tokens = var_tok :: t.tokens })
    | _ -> taints
  in
  if Taints.is_empty taints then lval_env
  else
    {
      tainted =
        LvalMap.update lval
          (function
            | None -> Some taints
            (* THINK: couldn't we just replace the existing taints? *)
            | Some taints' -> Some (Taints.union taints taints'))
          tainted;
      propagated;
      cleaned = LvalSet.remove lval cleaned;
    }

(* Add `var -> taints` to `var_env`. *)
let add_var var taints lval_env =
  let aux = { IL.base = Var var; rev_offset = [] } in
  add aux taints lval_env

let propagate_to prop_var taints env =
  if Taints.is_empty taints then env
  else { env with propagated = VarMap.add prop_var taints env.propagated }

let _find_lval { tainted; cleaned; _ } lval =
  if LvalSet.mem lval cleaned then `Clean
  else
    match LvalMap.find_opt lval tainted with
    | None -> `None
    | Some taints -> `Tainted taints

let find_var var lval_env =
  let aux = { IL.base = Var var; rev_offset = [] } in
  match _find_lval lval_env aux with
  | `Clean
  | `None ->
      None
  | `Tainted taints -> Some taints

let rec find lval lval_env =
  match _find_lval lval_env lval with
  | `Clean -> `Clean
  | `Tainted taints -> `Tainted taints
  | `None -> (
      match lval.rev_offset with
      | _ :: rev_offset' -> find { lval with rev_offset = rev_offset' } lval_env
      | [] -> `None)

let propagate_from prop_var env = VarMap.find_opt prop_var env.propagated

let clean lval { tainted; propagated; cleaned } =
  let prefix_is_tainted =
    tainted |> LvalMap.exists (fun lv _ -> LV.lval_is_dotted_prefix lv lval)
  in
  let needs_clean_mark = prefix_is_tainted && lval.rev_offset <> [] in
  (* If [a.b] is clean then [a.b.c] and [a.b.c.d] are too *)
  {
    tainted =
      tainted
      |> LvalMap.filter (fun lv _ -> not (LV.lval_is_dotted_prefix lval lv));
    propagated;
    cleaned =
      (cleaned
      |> LvalSet.filter (fun lv -> not (LV.lval_is_dotted_prefix lval lv))
      |> if needs_clean_mark then LvalSet.add lval else fun x -> x);
  }

let clean_var var { tainted; propagated; cleaned } =
  let aux = { IL.base = Var var; rev_offset = [] } in
  {
    tainted = LvalMap.remove aux tainted;
    propagated;
    (* TODO: Should we remove any var.x ... from cleaned?  *)
    cleaned;
  }

let equal le1 le2 =
  LvalMap.equal Taints.equal le1.tainted le2.tainted
  && VarMap.equal Taints.equal le1.propagated le2.propagated (* ? *)
  && LvalSet.equal le1.cleaned le2.cleaned

let to_string taint_to_str { tainted; propagated; cleaned } =
  (* FIXME: lval_to_str *)
  LvalMap.fold
    (fun dn v s ->
      s ^ Display_IL.string_of_lval dn ^ ":" ^ taint_to_str v ^ " ")
    tainted "[TAINTED]"
  ^ VarMap.fold
      (fun dn v s -> s ^ dn ^ ":" ^ taint_to_str v ^ " ")
      propagated "[PROPAGATED]"
  ^ LvalSet.fold
      (fun dn s -> s ^ Display_IL.string_of_lval dn ^ " ")
      cleaned "[CLEANED]"
