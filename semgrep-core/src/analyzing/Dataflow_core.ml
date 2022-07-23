(* Iain Proctor, Yoann Padioleau, Jiao Li
 *
 * Copyright (C) 2009-2010 Facebook
 * Copyright (C) 2019 r2c
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Dataflow analysis "framework".
 *
 * The goal of a dataflow analysis is to store information about each
 * variable at each program point, that is each node in a CFG
 * (e.g. whether a variable is "live" at a program point).
 * As you may want different kinds of information, the types below
 * are polymorphic. But each take as a key a variable name.
 *
 * This file used to be called Dataflow.ml, but this conflicts with
 * OCaml compiler-libs/dataflow.ml which is exposed when compiling
 * with 4.12.0+domains (a.k.a. OCaml multicore preview).
 *
 * todo:
 *  - could use a functor, so would not have all those 'a?
 *  - do we need other kind of information than variable environment?
 *    Dataflow analysis talks only about variables? for the belief analysis
 *    we actually want expressions instead.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* I was using directly Controlflow.xxx before, but now that we have both
 * Controlflow.flow and Il.cfg, we need to functorize things.
 *)

module type Flow = sig
  type node
  type edge
  type flow = (node, edge) CFG.t

  val short_string_of_node : node -> string
end

type nodei = int

(* The comparison function uses only the name of a variable (a string), so
 * two variables at different positions in the code will be agglomerated
 * correctly in the Set or Map.
 *)
type var = string

(* convenient aliases *)
module VarMap = Map.Make (String)
module VarSet = Set.Make (String)
module NodeiSet = Set.Make (Int)

(* The final dataflow result; a map from each program point to a map containing
 * information for each variables.
 *
 * opti: this used to be a 'NodeiMap.t' instead of an 'array' but 'nodei'
 * are always int and array gives a 6x speedup according to Iain
 * so let's use array.
 *)
type 'a mapping = 'a inout array

(* the In and Out sets, as in Appel Modern Compiler in ML book *)
and 'a inout = { in_env : 'a env; out_env : 'a env }
and 'a env = 'a VarMap.t

let empty_env () = VarMap.empty
let empty_inout () = { in_env = empty_env (); out_env = empty_env () }

(*****************************************************************************)
(* Equality *)
(*****************************************************************************)

(* the environment is polymorphic, so we require to pass an eq for 'a *)
let eq_env eq env1 env2 = VarMap.equal eq env1 env2

let eq_inout eq io1 io2 =
  let eqe = eq_env eq in
  eqe io1.in_env io2.in_env && eqe io1.out_env io2.out_env

(*****************************************************************************)
(* Env manipulation *)
(*****************************************************************************)

let (varmap_union : ('a -> 'a -> 'a) -> 'a env -> 'a env -> 'a env) =
 fun union_op env1 env2 ->
  let union _ x y = Some (union_op x y) in
  VarMap.union union env1 env2

let (varmap_diff :
      ('a -> 'a -> 'a) -> ('a -> bool) -> 'a env -> 'a env -> 'a env) =
 fun diff_op is_empty env1 env2 ->
  let merge _ opt_x opt_y =
    match (opt_x, opt_y) with
    | None, _ -> None
    | Some x, None -> Some x
    | Some x, Some y ->
        let diff = diff_op x y in
        if is_empty diff then None else Some diff
  in
  VarMap.merge merge env1 env2

(* useful helpers when the environment maps to a set of Nodes, e.g.,
 * for reaching definitions.
 *)
let (union_env : NodeiSet.t env -> NodeiSet.t env -> NodeiSet.t env) =
 fun env1 env2 ->
  let union _ x y = Some (NodeiSet.union x y) in
  VarMap.union union env1 env2

let (diff_env : NodeiSet.t env -> NodeiSet.t env -> NodeiSet.t env) =
 fun env1 env2 ->
  let merge _ opt_x opt_y =
    match (opt_x, opt_y) with
    | None, _ -> None
    | Some x, None -> Some x
    | Some x, Some y ->
        let diff = NodeiSet.diff x y in
        if NodeiSet.is_empty diff then None else Some diff
  in
  VarMap.merge merge env1 env2

let (add_var_and_nodei_to_env :
      var -> nodei -> NodeiSet.t env -> NodeiSet.t env) =
 fun var ni env ->
  let set =
    try NodeiSet.add ni (VarMap.find var env) with
    | Not_found -> NodeiSet.singleton ni
  in
  VarMap.add var set env

let (add_vars_and_nodei_to_env :
      VarSet.t -> nodei -> NodeiSet.t env -> NodeiSet.t env) =
 fun varset ni env ->
  let acc = env in
  VarSet.fold (fun var acc -> add_var_and_nodei_to_env var ni acc) varset acc

(*****************************************************************************)
(* Debugging support *)
(*****************************************************************************)

let csv_append s v = if String.length s = 0 then v else s ^ "," ^ v

let array_fold_left_idx f =
  let idx = ref 0 in
  Array.fold_left (fun v e ->
      let r = f v !idx e in
      incr idx;
      r)

let ns_to_str ns =
  "{" ^ NodeiSet.fold (fun n s -> csv_append s (string_of_int n)) ns "" ^ "}"

let (env_to_str : ('a -> string) -> 'a env -> string) =
 fun val2str env ->
  VarMap.fold (fun dn v s -> s ^ dn ^ ":" ^ val2str v ^ " ") env ""

let (inout_to_str : ('a -> string) -> 'a inout -> string) =
 fun val2str inout ->
  spf "IN= %15s  OUT = %15s"
    (env_to_str val2str inout.in_env)
    (env_to_str val2str inout.out_env)

(*****************************************************************************)
(* Main generic entry point *)
(*****************************************************************************)

(* The transition/transfer function. It is usually made from the
 * gens and kills.
 *
 * todo? having only a transfer function is enough ? do we need to pass
 * extra information to it ? maybe only the mapping is not enough. For
 * instance if in the code there is $x = &$g, a reference, then
 * we may want later to have access to this information. Maybe we
 * should pass an extra env argument ? Or maybe can encode this
 * sharing of reference in the 'a, so that when one update the
 * value associated to a var, its reference variable get also
 * the update.
 *)
type 'a transfn = 'a mapping -> nodei -> 'a inout

module Make (F : Flow) = struct
  let mapping_to_str (f : F.flow) val2str mapping =
    array_fold_left_idx
      (fun s ni v ->
        s
        ^ spf "%2d <- %7s: %15s %s\n" ni
            ((f.graph#predecessors ni)#fold
               (fun s (ni, _) -> csv_append s (string_of_int ni))
               "")
            (F.short_string_of_node (f.graph#nodes#find ni))
            (inout_to_str val2str v))
      "" mapping

  let (display_mapping : F.flow -> 'a mapping -> ('a -> string) -> unit) =
   fun flow mapping string_of_val ->
    pr (* nosemgrep: no-print-in-semgrep *)
      (mapping_to_str flow string_of_val mapping)

  let rec fixpoint_worker eq mapping trans flow succs workset =
    if NodeiSet.is_empty workset then mapping
    else
      let ni = NodeiSet.choose workset in
      let work' = NodeiSet.remove ni workset in
      (*pr2 (Printf.sprintf "accessing node %d" ni);
        display_mapping flow mapping (fun _ -> "<hidden>");
      *)
      let old = mapping.(ni) in
      let new_ = trans mapping ni in
      let work'' =
        if eq_inout eq old new_ then work'
        else (
          mapping.(ni) <- new_;
          NodeiSet.union work' (succs flow ni))
      in
      fixpoint_worker eq mapping trans flow succs work''

  let forward_succs (f : F.flow) n =
    (f.graph#successors n)#fold
      (fun s (ni, _) -> NodeiSet.add ni s)
      NodeiSet.empty

  let backward_succs (f : F.flow) n =
    (f.graph#predecessors n)#fold
      (fun s (ni, _) -> NodeiSet.add ni s)
      NodeiSet.empty

  let (fixpoint :
        eq:('a -> 'a -> bool) ->
        init:'a mapping ->
        trans:'a transfn ->
        flow:F.flow ->
        forward:bool ->
        'a mapping) =
   fun ~eq ~init ~trans ~flow ~forward ->
    let succs = if forward then forward_succs else backward_succs in
    let work =
      (* This prevents dead code from getting analyzed. *)
      flow.reachable
    in
    fixpoint_worker eq init trans flow succs work

  (*****************************************************************************)
  (* Helpers *)
  (*****************************************************************************)

  let new_node_array (f : F.flow) v =
    let nb_nodes = f.graph#nb_nodes in
    let max_nodei = ref (-1) in

    f.graph#nodes#tolist
    |> List.iter (fun (ni, _nod) ->
           (* actually there are some del_node done in cfg_build, for
            * switch, so sometimes ni is >= len
            *
            * old:
            * if ni >= nb_nodes
            * then pr2 "the CFG nodei is bigger than the number of nodes"
            *)
           if ni > !max_nodei then max_nodei := ni);
    assert (!max_nodei + 1 >= nb_nodes);
    Array.make (!max_nodei + 1) v
end

(*
module F = Controlflow
module X1 = Make (struct
  type node = F.node
  type edge = F.edge
  type flow = (node, edge) Ograph_extended.ograph_mutable
  let short_string_of_node = F.short_string_of_node
end
)
*)
