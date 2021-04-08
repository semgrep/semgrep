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
(* map for each node/var whether a variable is "tainted" *)
type mapping = unit Dataflow.mapping
(*e: type [[Dataflow_tainting.mapping]] *)

(*s: type [[Dataflow_tainting.config]] *)
(* this can use semgrep patterns under the hood *)
type config = {
  is_source: IL.instr -> bool;
  is_source_exp: IL.exp -> bool;
  is_sink: IL.instr -> bool;
  is_sanitizer: IL.instr -> bool;

  found_tainted_sink: IL.instr -> unit Dataflow.env -> unit;
}
(*e: type [[Dataflow_tainting.config]] *)

(*s: module [[Dataflow.Make(Il)]] *)
module DataflowX = Dataflow.Make (struct
    type node = F.node
    type edge = F.edge
    type flow = (node, edge) Ograph_extended.ograph_mutable
    let short_string_of_node n =
      Display_IL.short_string_of_node_kind n.F.n
  end)
(*e: module [[Dataflow.Make(Il)]] *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: function [[Dataflow_tainting.str_of_name]] *)
let str_of_name ((s, _tok), sid) =
  spf "%s:%d" s sid
(*e: function [[Dataflow_tainting.str_of_name]] *)

(*s: function [[Dataflow_tainting.option_to_varmap]] *)
let option_to_varmap = function
  | None -> VarMap.empty
  | Some lvar -> VarMap.singleton (str_of_name lvar) ()
(*e: function [[Dataflow_tainting.option_to_varmap]] *)

(*****************************************************************************)
(* Tainted *)
(*****************************************************************************)

let sanitized config instr =
  match instr.i with
  | Call (_, {e=Lvalue({base=Var(("sanitize",_),_);_}); _}, [])
    -> true
  | ___else___
    -> config.is_sanitizer instr

let rec tainted env config exp =
  (* We call `tainted` recursively on each subexpression, so each subexpression
   * is checked against `pattern-sources`. For example, if `location.href` were
   * a source, this would infer that `"aa" + location.href + "bb"` is tainted.
   * Also note that any arbitrary expression can be source! *)
  let go_into = function
    | Lvalue {base=Var var;_}
      -> VarMap.mem (str_of_name var) env
    | Lvalue _
    | Literal _
    | FixmeExp _
      -> false
    | Composite (_, (_, es, _))
    | Operator (_, es)
      -> List.exists (tainted env config) es
    | Record fields
      -> List.exists (fun (_, e) -> tainted env config e) fields
    | Cast (_, e)
      -> tainted env config e
  in
  config.is_source_exp exp || go_into exp.e

let tainted_instr env config instr =
  let tainted_args = function
    | Assign (_, e) -> tainted env config e
    | AssignAnon _ -> false (* TODO *)
    | Call (_, {e=Lvalue({base=Var(("source",_),_);_}); _}, []) -> true
    | Call (_, e, args) ->
        tainted env config e || List.exists (tainted env config) args
    | CallSpecial (_, _, args) -> List.exists (tainted env config) args
    | FixmeInstr _ -> false
  in
  not (sanitized config instr)
  && (config.is_source instr || tainted_args instr.i)

(*****************************************************************************)
(* Transfer *)
(*****************************************************************************)
(* Not sure we can use the Gen/Kill framework here.
*)

(*s: constant [[Dataflow_tainting.union]] *)
let union =
  Dataflow.varmap_union (fun () () -> ())
(*e: constant [[Dataflow_tainting.union]] *)
(*s: constant [[Dataflow_tainting.diff]] *)
let diff =
  Dataflow.varmap_diff (fun () () -> ()) (fun () -> true)
(*e: constant [[Dataflow_tainting.diff]] *)

(*s: function [[Dataflow_tainting.transfer]] *)
let (transfer: config -> flow:F.cfg -> unit Dataflow.transfn) =
  fun config ~flow ->
  (* the transfer function to update the mapping at node index ni *)
  fun mapping ni ->

  let in' =
    (flow#predecessors ni)#fold (fun acc (ni_pred, _) ->
      union acc mapping.(ni_pred).D.out_env
    ) VarMap.empty
  in
  let node = flow#nodes#assoc ni in

  (* TODO: do that later? once everything if finished? *)
  (match node.F.n with
   | NInstr x ->
       (* TODO: use metavar in sink to know which argument we should check
        * for taint? *)
       if config.is_sink x && tainted_instr in' config x
       then config.found_tainted_sink x in'
   | Enter | Exit | TrueNode | FalseNode | Join
   | NCond _ | NGoto _ | NReturn _ | NThrow _ | NOther _
   | NTodo _ -> ()
  );


  let gen_ni_opt =
    match node.F.n with
    | NInstr x ->
        if tainted_instr in' config x then
          IL.lvar_of_instr_opt x
        else
          None

    | Enter | Exit | TrueNode | FalseNode | Join
    | NCond _ | NGoto _ | NReturn _ | NThrow _ | NOther _
    | NTodo _ -> None
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
        if tainted_instr in' config x then
          None
        else
          (* all clean arguments should reset the taint *)
          IL.lvar_of_instr_opt x

    | Enter | Exit | TrueNode | FalseNode | Join
    | NCond _ | NGoto _ | NReturn _ | NThrow _ | NOther _
    | NTodo _ -> None
  in
  let gen_ni = option_to_varmap gen_ni_opt in
  let kill_ni = option_to_varmap kill_ni_opt in

  let out' = diff (union in' gen_ni) kill_ni in
  {D. in_env = in'; out_env = out'}
(*e: function [[Dataflow_tainting.transfer]] *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(*s: function [[Dataflow_tainting.fixpoint]] *)
let (fixpoint: config -> F.cfg -> mapping) = fun config flow ->
  DataflowX.fixpoint
    ~eq:(fun () () -> true)
    ~init:(DataflowX.new_node_array flow (Dataflow.empty_inout ()))
    ~trans:(transfer config ~flow)
    (* tainting is a forward analysis! *)
    ~forward:true
    ~flow
(*e: function [[Dataflow_tainting.fixpoint]] *)

(*e: pfff/lang_GENERIC/analyze/Dataflow_tainting.ml *)
