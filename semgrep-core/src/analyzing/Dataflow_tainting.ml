(*s: pfff/lang_GENERIC/analyze/Dataflow_tainting.ml *)
(*s: pad/r2c copyright *)
(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 r2c
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
(*e: pad/r2c copyright *)
open Common
open IL
module G = AST_generic
module F = IL
module D = Dataflow
module VarMap = Dataflow.VarMap

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Tainting dataflow analysis.
 *
 * This is a very rudimentary tainting analysis. Just intraprocedural,
 * very coarse grained (taint whole array/object).
 * This is step1 for taint tracking support in semgrep.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*s: type [[Dataflow_tainting.mapping]] *)
type mapping = unit Dataflow.mapping
(** Map for each node/var whether a variable is "tainted" *)

(*e: type [[Dataflow_tainting.mapping]] *)

(* Tracks tainted functions. *)
type fun_env = (Dataflow.var, unit) Hashtbl.t

(*s: type [[Dataflow_tainting.config]] *)
type config = {
  is_source : G.any -> bool;
  is_sink : G.any -> bool;
  is_sanitizer : G.any -> bool;
  found_tainted_sink : G.any -> unit Dataflow.env -> unit;
}
(** This can use semgrep patterns under the hood. Note that a source can be an
  * instruction but also an expression. *)

(*e: type [[Dataflow_tainting.config]] *)

(*s: module [[Dataflow.Make(Il)]] *)
module DataflowX = Dataflow.Make (struct
  type node = F.node

  type edge = F.edge

  type flow = (node, edge) Ograph_extended.ograph_mutable

  let short_string_of_node n = Display_IL.short_string_of_node_kind n.F.n
end)

(*e: module [[Dataflow.Make(Il)]] *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: function [[Dataflow_tainting.str_of_name]] *)
let str_of_name ((s, _tok), sid) = spf "%s:%d" s sid

(*e: function [[Dataflow_tainting.str_of_name]] *)

(*s: function [[Dataflow_tainting.option_to_varmap]] *)
let option_to_varmap = function
  | None -> VarMap.empty
  | Some lvar -> VarMap.singleton (str_of_name lvar) ()

(*e: function [[Dataflow_tainting.option_to_varmap]] *)

(*****************************************************************************)
(* Tainted *)
(*****************************************************************************)

let sanitized_instr config instr =
  match instr.i with
  | Call (_, { e = Fetch { base = Var (("sanitize", _), _); _ }; _ }, []) ->
      true
  | ___else___ -> config.is_sanitizer (G.E instr.iorig)

(* Test whether an expression is tainted, and if it is also a sink,
 * report the finding too (by side effect).
 *
 * When [in_a_sink] we do not report findings but wait for the finding
 * to be reported by the caller. E.g. if the pattern-sink is `sink(...)`
 * then `E` in `sink(E)` will also satisfy [config.is_sink], but we
 * want to report the finding on `sink(E)` rather than on `E`. *)
let rec check_tainted_expr ~in_a_sink config fun_env env exp =
  let is_sink = config.is_sink (G.E exp.eorig) in
  let check =
    check_tainted_expr ~in_a_sink:(in_a_sink || is_sink) config fun_env env
  in
  let check_base = function
    | Var var ->
        VarMap.mem (str_of_name var) env
        || Hashtbl.mem fun_env (str_of_name var)
    | VarSpecial _ -> false
    | Mem e -> check e
  in
  let check_offset = function
    | NoOffset | Dot _ -> false
    | Index e -> check e
  in
  let check_subexpr = function
    | Fetch { base = VarSpecial (This, _); offset = Dot fld; _ } ->
        Hashtbl.mem fun_env (str_of_name fld)
    | Fetch { base; offset; _ } -> check_base base || check_offset offset
    | Literal _ | FixmeExp _ -> false
    | Composite (_, (_, es, _)) | Operator (_, es) -> List.exists check es
    | Record fields -> List.exists (fun (_, e) -> check e) fields
    | Cast (_, e) -> check e
  in
  let is_sanitized = config.is_sanitizer (G.E exp.eorig) in
  (not is_sanitized)
  &&
  let is_tainted = config.is_source (G.E exp.eorig) || check_subexpr exp.e in
  if is_tainted && is_sink && not in_a_sink then
    config.found_tainted_sink (G.E exp.eorig) env;
  is_tainted

(* Test whether an instruction is tainted, and if it is also a sink,
 * report the finding too (by side effect). *)
let check_tainted_instr config fun_env env instr =
  let is_sink = config.is_sink (G.E instr.iorig) in
  let check_expr = check_tainted_expr ~in_a_sink:is_sink config fun_env env in
  let tainted_args = function
    | Assign (_, e) -> check_expr e
    | AssignAnon _ -> false (* TODO *)
    | Call (_, { e = Fetch { base = Var (("source", _), _); _ }; _ }, []) ->
        true
    | Call (_, e, args) -> check_expr e || List.exists check_expr args
    | CallSpecial (_, _, args) -> List.exists check_expr args
    | FixmeInstr _ -> false
  in
  let is_sanitized = sanitized_instr config instr in
  (not is_sanitized)
  &&
  let is_tainted = config.is_source (G.E instr.iorig) || tainted_args instr.i in
  if is_tainted && is_sink then config.found_tainted_sink (G.E instr.iorig) env;
  is_tainted

(* Test whether a `return' is tainted, and if it is also a sink,
 * report the finding too (by side effect). *)
let check_tainted_return config fun_env env tok e =
  let orig_return = G.s (G.Return (tok, Some e.eorig, tok)) in
  let is_sink = config.is_sink (G.S orig_return) in
  let check_expr = check_tainted_expr ~in_a_sink:is_sink config fun_env env e in
  if check_expr && is_sink then config.found_tainted_sink (G.S orig_return) env;
  check_expr

(*****************************************************************************)
(* Transfer *)
(*****************************************************************************)
(* Not sure we can use the Gen/Kill framework here.
*)

(*s: constant [[Dataflow_tainting.union]] *)
let union = Dataflow.varmap_union (fun () () -> ())

(*e: constant [[Dataflow_tainting.union]] *)
(*s: constant [[Dataflow_tainting.diff]] *)
let diff = Dataflow.varmap_diff (fun () () -> ()) (fun () -> true)

(*e: constant [[Dataflow_tainting.diff]] *)

(*s: function [[Dataflow_tainting.transfer]] *)
let (transfer :
      config -> fun_env -> IL.name option -> flow:F.cfg -> unit Dataflow.transfn)
    =
 fun config fun_env opt_name ~flow
     (* the transfer function to update the mapping at node index ni *)
       mapping ni ->
  let in' =
    (flow#predecessors ni)#fold
      (fun acc (ni_pred, _) -> union acc mapping.(ni_pred).D.out_env)
      VarMap.empty
  in
  let node = flow#nodes#assoc ni in

  let gen_ni_opt =
    match node.F.n with
    | NInstr x ->
        if check_tainted_instr config fun_env in' x then IL.lvar_of_instr_opt x
        else None
    (* if just a single return is tainted then the function is tainted *)
    | NReturn (tok, e) when check_tainted_return config fun_env in' tok e ->
        (match opt_name with
        | Some var -> Hashtbl.add fun_env (str_of_name var) ()
        | None -> ());
        None
    | Enter | Exit | TrueNode | FalseNode | Join | NCond _ | NGoto _ | NReturn _
    | NThrow _ | NOther _ | NTodo _ ->
        None
  in
  let kill_ni_opt =
    (* old:
     *  if gen_ni_opt <> None
     *  then None
     * but now gen_ni <> None does not necessarily mean we had a source().
     * It can also be one tainted rvars which propagate to the lvar
     *)
    match node.F.n with
    | NInstr x ->
        if check_tainted_instr config fun_env in' x then None
        else
          (* all clean arguments should reset the taint *)
          IL.lvar_of_instr_opt x
    | Enter | Exit | TrueNode | FalseNode | Join | NCond _ | NGoto _ | NReturn _
    | NThrow _ | NOther _ | NTodo _ ->
        None
  in
  let gen_ni = option_to_varmap gen_ni_opt in
  let kill_ni = option_to_varmap kill_ni_opt in

  let out' = diff (union in' gen_ni) kill_ni in
  { D.in_env = in'; out_env = out' }

(*e: function [[Dataflow_tainting.transfer]] *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(*s: function [[Dataflow_tainting.fixpoint]] *)
let (fixpoint : config -> fun_env -> IL.name option -> F.cfg -> mapping) =
 fun config fun_env opt_name flow ->
  DataflowX.fixpoint
    ~eq:(fun () () -> true)
    ~init:(DataflowX.new_node_array flow (Dataflow.empty_inout ()))
    ~trans:
      (transfer config fun_env opt_name ~flow)
      (* tainting is a forward analysis! *)
    ~forward:true ~flow

(*e: function [[Dataflow_tainting.fixpoint]] *)

(*e: pfff/lang_GENERIC/analyze/Dataflow_tainting.ml *)
