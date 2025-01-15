(* Iago Abal
 *
 * Copyright (C) 2022-2024 Semgrep Inc.
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

open Common
module Log = Log_tainting.Log
module T = Taint
module Taints = T.Taint_set
module H = IL_helpers
module Var_env = Dataflow_var_env
module VarMap = Var_env.VarMap
module NameMap = IL.NameMap
open Shape_and_sig.Shape
module Shape = Taint_shape

let limits_tags = Logs_.create_tags [ "bad"; "limits" ]

type taints_to_propagate = T.taints VarMap.t
type pending_propagation_dests = IL.lval VarMap.t

type t = {
  tainted : cell NameMap.t;
      (** Lvalues that are tainted, it is only meant to track l-values of the form x.a_1. ... . a_N. *)
  control : T.taints;
      (** Taints propagated via the flow of control (rather than the flow of data). *)
  taints_to_propagate : taints_to_propagate;
      (** Taint that is propagated via taint propagators (internally represented by
    unique propagator variables), this is the taint going into the 'from's.. *)
  pending_propagation_dests : pending_propagation_dests;
      (** By-side-effect propagators waiting for taint to be propagated through
    an l-value. Because arguments are checked left-to-right, we use this trick to
    support right-to-left propagation between arguments, as in `foobar($TO, $FROM)`.
    We record the `$TO` here, and we propagate the taint by side-effect once we
    process the `$FROM`.

    THINK: A more general solution could be to use a "taint variable" as we do for
      the arguments of the function under analysis.
    *)
}

type env = t

type prop_fn =
  taints_to_propagate:taints_to_propagate ->
  pending_propagation_dests:pending_propagation_dests ->
  env

type add_fn = IL.lval -> T.taints -> env -> env

let hook_propagate_to :
    (Var_env.var ->
    T.taints ->
    taints_to_propagate:taints_to_propagate ->
    pending_propagation_dests:pending_propagation_dests ->
    prop:prop_fn ->
    add:add_fn ->
    t)
    option
    ref =
  ref None

let empty =
  {
    tainted = NameMap.empty;
    control = Taints.empty;
    taints_to_propagate = VarMap.empty;
    pending_propagation_dests = VarMap.empty;
  }

let empty_inout = { Dataflow_core.in_env = empty; out_env = empty }

let union le1 le2 =
  let tainted =
    NameMap.union
      (fun _ x y -> Some (Shape.unify_cell x y))
      le1.tainted le2.tainted
  in
  {
    tainted;
    control = Taints.union le1.control le2.control;
    taints_to_propagate =
      Var_env.varmap_union Taints.union le1.taints_to_propagate
        le2.taints_to_propagate;
    pending_propagation_dests =
      (* THINK: Pending propagation is just meant to deal with right-to-left
       * propagation between call arguments, so for now we just kill them all
       * at JOINs. *)
      VarMap.empty;
  }

let union_list ?(default = empty) les = List.fold_left union default les

(* Reduces an l-value into the form x.a_1. ... . a_N, the resulting l-value may
 * not represent the exact same object as the original l-value, but an
 * overapproximation. For example, the normalized l-value of `x[i]` will be `x`,
 * so the taints of any element of an array are tracked via the array itself. *)
let normalize_lval lval =
  let open Common in
  let { IL.base; rev_offset } = lval in
  let* base, rev_offset =
    match base with
    (* explicit dereference of `ptr` e.g. `ptr->x` *)
    | Mem { e = Fetch { base = Var x; rev_offset = [] }; _ } ->
        Some (x, rev_offset)
    | Var name -> (
        match rev_offset with
        (* Static class field, `C.x`, we normalize it to just `x` since `x` is
           a unique global.

           TODO: C.x.y ? *)
        | [ { o = IL.Dot var; _ } ]
          when H.is_class_name name || IdFlags.is_static !(var.id_info.id_flags)
          ->
            Some (var, [])
        | __else__ -> Some (name, rev_offset))
    (* explicit dereference of `this` e.g. `this->x` *)
    | Mem { e = Fetch { base = VarSpecial (This, _); rev_offset = [] }; _ }
    | VarSpecial _ -> (
        match List.rev rev_offset with
        (* this.x o_1 ... o_N becomes x o_1 ... o_N *)
        | { o = IL.Dot var; _ } :: offset' -> Some (var, List.rev offset')
        (* we do not handle any other case *)
        | []
        | { o = IL.Index _; _ } :: _ ->
            None)
    | Mem _ -> None
  in
  let offset = T.offset_of_rev_IL_offset ~rev_offset in
  Some (base, offset)

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
    |> NameMap.find_first_opt (fun var ->
           (* auxiliary _tmp variables get fake tokens *)
           Tok.is_fake (snd var.ident))
  with
  | None -> None
  | Some (var, _) -> Some (var, NameMap.remove var tainted)

let check_tainted_lvals_limit tainted new_var =
  if
    (not (NameMap.mem new_var tainted))
    && !Flag_semgrep.max_tainted_vars > 0
    && NameMap.cardinal tainted > !Flag_semgrep.max_tainted_vars
  then (
    match remove_some_lval_from_tainted_set tainted with
    | Some (dropped_var, tainted) ->
        Log.debug (fun m ->
            m ~tags:limits_tags
              "Already tracking too many tainted l-values, dropped %s in order \
               to track %s"
              (IL.str_of_name dropped_var)
              (IL.str_of_name new_var));
        Some tainted
    | None ->
        Log.debug (fun m ->
            m ~tags:limits_tags
              "Already tracking too many tainted l-values, will not track %s"
              (IL.str_of_name new_var));
        None)
  else Some tainted

let add_shape var offset new_taints new_shape
    ({ tainted; control; taints_to_propagate; pending_propagation_dests } as
     lval_env) =
  match check_tainted_lvals_limit tainted var with
  | None -> lval_env
  | Some tainted ->
      let new_taints =
        let var_tok = snd var.ident in
        if Tok.is_fake var_tok then new_taints
        else
          new_taints
          |> Taints.map (fun t ->
                 { t with rev_tokens = var_tok :: t.rev_tokens })
      in
      {
        tainted =
          NameMap.update var
            (fun opt_var_ref ->
              Shape.update_offset_and_unify new_taints new_shape offset
                opt_var_ref)
            tainted;
        control;
        taints_to_propagate;
        pending_propagation_dests;
      }

let add_lval_shape lval new_taints new_shape lval_env =
  match normalize_lval lval with
  | None ->
      (* Cannot track taint for this l-value; e.g. because the base is not a simple
         variable. We just return the same environment untouched. *)
      lval_env
  | Some (var, offset) -> add_shape var offset new_taints new_shape lval_env

let add var offset new_taints lval_env =
  add_shape var offset new_taints Bot lval_env

let add_lval lval new_taints lval_env =
  add_lval_shape lval new_taints Bot lval_env

let propagate_to prop_var taints env =
  (* THINK: Should we record empty propagations anyways so that we can always
   *   match 'from' and 'to' ? We may be keeping around "pending" propagations
   *   that will never take place. *)
  if Taints.is_empty taints then env
  else
    let env =
      {
        env with
        taints_to_propagate = VarMap.add prop_var taints env.taints_to_propagate;
      }
    in
    match !hook_propagate_to with
    | None -> env
    | Some hook ->
        hook prop_var taints ~taints_to_propagate:env.taints_to_propagate
          ~pending_propagation_dests:env.pending_propagation_dests
          ~prop:(fun ~taints_to_propagate ~pending_propagation_dests ->
            { env with taints_to_propagate; pending_propagation_dests })
          ~add:add_lval

let find_var { tainted; _ } var = NameMap.find_opt var tainted

let find_lval { tainted; _ } lval =
  let* var, offsets = normalize_lval lval in
  let* var_ref = NameMap.find_opt var tainted in
  match Shape.find_in_cell offsets var_ref with
  | `Clean
  | `Not_found _ ->
      None
  | `Found cell -> Some cell

let find_poly { tainted; _ } var offsets =
  let* var_ref = NameMap.find_opt var tainted in
  Shape.find_in_cell_poly offsets var_ref

let find_lval_poly lval_env lval =
  let* var, offsets = normalize_lval lval in
  find_poly lval_env var offsets

let find_lval_xtaint env lval =
  match find_lval env lval with
  | None -> `None
  | Some (Cell (xtaints, _shape)) -> xtaints

let propagate_from prop_var env =
  let opt_taints = VarMap.find_opt prop_var env.taints_to_propagate in
  let env =
    if Option.is_some opt_taints then
      {
        env with
        taints_to_propagate = VarMap.remove prop_var env.taints_to_propagate;
      }
    else env
  in
  (opt_taints, env)

let pending_propagation prop_var lval env =
  {
    env with
    pending_propagation_dests =
      VarMap.add prop_var lval env.pending_propagation_dests;
  }

let clean
    ({ tainted; control; taints_to_propagate; pending_propagation_dests } as
     lval_env) lval =
  match normalize_lval lval with
  | None ->
      (* Cannot track taint for this l-value; e.g. because the base is not a simple
         variable. We just return the same environment untouched. *)
      lval_env
  | Some (var, offsets) ->
      {
        tainted =
          NameMap.update var
            (function
              | None -> None
              | Some var_ref -> Some (Shape.clean_cell offsets var_ref))
            tainted;
        control;
        taints_to_propagate;
        pending_propagation_dests;
        (* THINK: Should we clean propagations before they are executed? *)
      }

let filter_tainted pred ({ tainted; _ } as lval_env) =
  let tainted = tainted |> NameMap.filter (fun var _cell -> pred var) in
  { lval_env with tainted }

let add_control_taints lval_env taints =
  if Taints.is_empty taints then lval_env
  else { lval_env with control = Taints.union taints lval_env.control }

let get_control_taints { control; _ } = control

let equal
    {
      tainted = tainted1;
      control = control1;
      taints_to_propagate = _;
      pending_propagation_dests = _;
    }
    {
      tainted = tainted2;
      control = control2;
      taints_to_propagate = _;
      pending_propagation_dests = _;
    } =
  NameMap.equal equal_cell tainted1 tainted2
  (* NOTE: We ignore 'taints_to_propagate' and 'pending_propagation_dests',
   * we just care how they affect 'tainted'. *)
  && Taints.equal control1 control2

let equal_by_lval { tainted = tainted1; _ } { tainted = tainted2; _ } lval =
  match normalize_lval lval with
  | None ->
      (* Cannot track taint for this l-value; e.g. because the base is not a simple
         variable. We just return the same environment untouched. *)
      false
  | Some (var, _offsets) ->
      let equal_tainted =
        match
          (NameMap.find_opt var tainted1, NameMap.find_opt var tainted2)
        with
        | None, None -> true
        | Some ref1, Some ref2 -> equal_cell ref1 ref2
        | Some _, None
        | None, Some _ ->
            false
      in
      equal_tainted

let to_string
    { tainted; control; taints_to_propagate; pending_propagation_dests } =
  (* FIXME: lval_to_str *)
  (if NameMap.is_empty tainted then ""
   else
     NameMap.fold
       (fun dn v s -> s ^ IL.str_of_name dn ^ ":" ^ show_cell v ^ " ")
       tainted "[TAINTED]")
  ^ (if Taints.is_empty control then ""
     else "[CONTROL] " ^ T.show_taints control)
  ^ (if VarMap.is_empty taints_to_propagate then ""
     else
       VarMap.fold
         (fun dn v s -> s ^ dn ^ "<-" ^ T.show_taints v ^ " ")
         taints_to_propagate "[TAINT TO BE PROPAGATED]")
  ^
  if VarMap.is_empty pending_propagation_dests then ""
  else
    VarMap.fold
      (fun dn v s -> s ^ dn ^ "->" ^ Display_IL.string_of_lval v ^ " ")
      pending_propagation_dests "[PENDING PROPAGATION DESTS]"

let seq_of_tainted env = NameMap.to_seq env.tainted
