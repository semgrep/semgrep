(* Iago Abal
 *
 * Copyright (C) 2022 r2c
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

(* TODO: This needs some clean up, maybe we shouldn't expect clients of this module
 * to ensure that lvals satisfy IL_helpers.lval_is_var_and_dots, but rather handle
 * that internally. *)

module T = Taint
module Taints = T.Taint_set
module LV = IL_helpers
module Var_env = Dataflow_var_env
module VarMap = Var_env.VarMap
module LvalMap = Map.Make (LV.LvalOrdered)
module LvalSet = Set.Make (LV.LvalOrdered)

let logger = Logging.get_logger [ __MODULE__ ]

type t = {
  tainted : T.taints LvalMap.t;
      (** Lvalues that are tainted, it is only meant to track l-values of the form x.a_1. ... . a_N. *)
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

(* Reduces an l-value into the form x.a_1. ... . a_N, the resulting l-value may
 * not represent the exact same object as the original l-value, but an
 * overapproximation. For example, the normalized l-value of `x[i]` will be `x`,
 * so the taints of any element of an array are tracked via the array itself. *)
let normalize_lval lval =
  let open Common in
  let { IL.base; rev_offset } = lval in
  let* base, rev_offset =
    match base with
    | Var _ -> Some (base, rev_offset)
    | VarSpecial _ -> (
        match List.rev rev_offset with
        (* this.x o_1 ... o_N becomes x o_1 ... o_N *)
        | { o = IL.Dot var; _ } :: offset' -> Some (Var var, List.rev offset')
        (* we do not handle any other case *)
        | []
        | { o = IL.Index _; _ } :: _ ->
            None)
    | Mem _ -> None
  in
  let rev_offset =
    rev_offset
    |> List.filter (fun o ->
           match o.IL.o with
           | IL.Dot _ -> true
           (* TODO: If we check here for constant indexes we could make it index
            * sensitive! But we also need to tweak the look up. Since you may taint
            * `x.a` and then look for `x.a[1]` and it should tell you it's tainted. *)
           | IL.Index _ -> false)
  in
  Some { IL.base; rev_offset }

let lval_is_prefix lval1 lval2 =
  let open IL in
  let eq_name x y = LV.compare_name x y = 0 in
  let rec offset_prefix os1 os2 =
    match (os1, os2) with
    | [], _ -> true
    | _ :: _, []
    | { o = Index _; _ } :: _, { o = Dot _; _ } :: _
    | { o = Dot _; _ } :: _, { o = Index _; _ } :: _ ->
        false
    | { o = Index _; _ } :: os1, { o = Index _; _ } :: os2 ->
        offset_prefix os1 os2
    | { o = Dot a; _ } :: os1, { o = Dot b; _ } :: os2 ->
        eq_name a b && offset_prefix os1 os2
  in
  match (lval1, lval2) with
  | { base = Var x; rev_offset = ro1 }, { base = Var y; rev_offset = ro2 } ->
      eq_name x y && offset_prefix (List.rev ro1) (List.rev ro2)
  | __else__ -> false

(* TODO: This is an experiment, try to raise taint_MAX_TAINTED_LVALS and run
 * some benchmarks, if we can e.g. double the limit without affecting perf then
 * just remove this. We could try something clever based e.g. on live-variable
 * analysis, but there is a high risk that the "solution" may introduce perf
 * problems of its own... *)
let remove_some_lval_from_tainted_set tainted =
  (* Try to make space for a new l-value by removing an auxiliary _tmp one first.
   * By using using `find_first_opt` we try to find the one with the lowest sid,
   * which hopefully isn't needed anymore... (unless it's inside a loop...).
   * This could perhaps (?) break monotonicity and cause divergence of the fixpoint,
   * but the Limits_semgrep.taint_FIXPOINT_TIMEOUT seconds timeout would take care
   * of that. *)
  match
    tainted
    |> LvalMap.find_first_opt (fun lval ->
           match lval.base with
           (* auxiliary _tmp variables get fake tokens *)
           | Var var -> Tok.is_fake (snd var.ident)
           | VarSpecial _
           | Mem _ ->
               false)
  with
  | None -> None
  | Some (lval, _) -> Some (lval, LvalMap.remove lval tainted)

let check_tainted_lvals_limit tainted new_lval =
  if
    (not (LvalMap.mem new_lval tainted))
    && !Flag_semgrep.max_tainted_lvals > 0
    && LvalMap.cardinal tainted > !Flag_semgrep.max_tainted_lvals
  then (
    match remove_some_lval_from_tainted_set tainted with
    | Some (dropped_lval, tainted) ->
        logger#warning
          "Already tracking too many tainted l-values, dropped %s in order to \
           track %s"
          (Display_IL.string_of_lval dropped_lval)
          (Display_IL.string_of_lval new_lval);
        Some tainted
    | None ->
        logger#warning
          "Already tracking too many tainted l-values, will not track %s"
          (Display_IL.string_of_lval new_lval);
        None)
  else Some tainted

let add ({ tainted; propagated; cleaned } as lval_env) lval taints =
  match normalize_lval lval with
  | None ->
      (* Cannot track taint for this l-value; e.g. because the base is not a simple
         variable. We just return the same environment untouched. *)
      lval_env
  | Some lval -> (
      if Taints.is_empty taints then lval_env
      else
        match check_tainted_lvals_limit tainted lval with
        | None -> lval_env
        | Some tainted ->
            let taints =
              (* If the lvalue is a simple variable, we record it as part of
                 the taint trace. *)
              match lval with
              | { IL.base = Var var; rev_offset = [] } ->
                  let var_tok = snd var.ident in
                  if Tok.is_fake var_tok then taints
                  else
                    taints
                    |> Taints.map (fun t ->
                           { t with tokens = var_tok :: t.tokens })
              | __else__ -> taints
            in
            {
              tainted =
                LvalMap.update lval
                  (function
                    | None -> Some taints
                    (* THINK: couldn't we just replace the existing taints? *)
                    | Some taints' ->
                        if
                          !Flag_semgrep.max_taint_set_size = 0
                          || Taints.cardinal taints'
                             < !Flag_semgrep.max_taint_set_size
                        then Some (Taints.union taints taints')
                        else (
                          logger#warning
                            "Already tracking too many taint sources for %s, \
                             will not track more"
                            (Display_IL.string_of_lval lval);
                          Some taints'))
                  tainted;
              propagated;
              cleaned = LvalSet.remove lval cleaned;
            })

let propagate_to prop_var taints env =
  if Taints.is_empty taints then env
  else { env with propagated = VarMap.add prop_var taints env.propagated }

let dumb_find { tainted; cleaned; _ } lval =
  match normalize_lval lval with
  | None -> `None
  | Some lval -> (
      if LvalSet.mem lval cleaned then `Clean
      else
        match LvalMap.find_opt lval tainted with
        | None -> `None
        | Some taints -> `Tainted taints)

let propagate_from prop_var env = VarMap.find_opt prop_var env.propagated

let clean ({ tainted; propagated; cleaned } as lval_env) lval =
  match normalize_lval lval with
  | None ->
      (* Cannot track taint for this l-value; e.g. because the base is not a simple
         variable. We just return the same environment untouched. *)
      lval_env
  | Some lval ->
      let prefix_is_tainted =
        tainted |> LvalMap.exists (fun lv _ -> lval_is_prefix lv lval)
      in
      let needs_clean_mark = prefix_is_tainted && lval.rev_offset <> [] in
      {
        tainted =
          (* If `x.a` is clean then `x.a` and any extension of it (`x.a.b`, `x.a.b.c`,
           * and so on) are clean too, and we remove them all from tainted. *)
          tainted |> LvalMap.filter (fun lv _ -> not (lval_is_prefix lval lv));
        propagated;
        cleaned =
          (* Similarly, if `x.a` will have a "clean" mark, then we can remove any
           * such mark on any extension of `x.a`. It would be redundant to record
           * `x.a.b` as clean when we already have that `x.a` is clean. *)
          (cleaned
          |> LvalSet.filter (fun lv -> not (lval_is_prefix lval lv))
          |> if needs_clean_mark then LvalSet.add lval else fun x -> x);
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

let seq_of_tainted env = LvalMap.to_seq env.tainted
