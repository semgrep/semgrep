(* Iain Proctor, Yoann Padioleau, Jiao Li
 *
 * Copyright (C) 2009-2010 Facebook
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
module Log = Log_analyzing.Log

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

module NodeiSet = Set.Make (Int)

(* The final dataflow result; a map from each program point to a map containing
 * information for each variables.
 *
 * opti: this used to be a 'NodeiMap.t' instead of an 'array' but 'nodei'
 * are always int and array gives a 6x speedup according to Iain
 * so let's use array.
 *)
type 'env mapping = 'env inout array

(* the In and Out sets, as in Appel Modern Compiler in ML book *)
and 'env inout = { in_env : 'env; out_env : 'env }

(*****************************************************************************)
(* Equality *)
(*****************************************************************************)

let eq_inout eq_env io1 io2 =
  let eqe = eq_env in
  eqe io1.in_env io2.in_env && eqe io1.out_env io2.out_env

(*****************************************************************************)
(* Debugging support *)
(*****************************************************************************)

let csv_append s v = if String.length s =|= 0 then v else s ^ "," ^ v

let array_fold_left_idx f =
  let idx = ref 0 in
  Array.fold_left (fun v e ->
      let r = f v !idx e in
      incr idx;
      r)

let ns_to_str ns =
  "{" ^ NodeiSet.fold (fun n s -> csv_append s (string_of_int n)) ns "" ^ "}"

let (inout_to_str : ('env -> string) -> 'env inout -> string) =
 fun env_to_str inout ->
  spf "IN= %15s  OUT = %15s" (env_to_str inout.in_env)
    (env_to_str inout.out_env)

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
type 'env transfn = 'env mapping -> nodei -> 'env inout

module Make (F : Flow) = struct
  let mapping_to_str (f : F.flow) env_to_str mapping =
    array_fold_left_idx
      (fun s ni v ->
        s
        ^ spf "%2d <- %7s: %15s %s\n" ni
            ((f.graph#predecessors ni)#fold
               (fun s (ni, _) -> csv_append s (string_of_int ni))
               "")
            (F.short_string_of_node (f.graph#nodes#find ni))
            (inout_to_str env_to_str v))
      "" mapping

  let (display_mapping : F.flow -> 'env mapping -> ('env -> string) -> unit) =
   fun flow mapping env_to_str ->
    (* nosemgrep: no-print-in-semgrep *)
    UCommon.pr (mapping_to_str flow env_to_str mapping)

  let fixpoint_worker ~timeout eq_env mapping trans flow succs workset =
    let t0 = Sys.time () in
    let rec loop i work =
      if NodeiSet.is_empty work then (mapping, `Ok)
      else
        (* 'Time_limit.set_timeout' cannot be nested and we want to make sure that
         * fixpoint computations run for a limited amount of time. *)
        let t1 = Sys.time () in
        if i > Limits_semgrep.dataflow_FIXPOINT_MIN_ITERS && t1 -. t0 >= timeout
        then (mapping, `Timeout)
        else
          let ni = NodeiSet.choose work in
          let work' = NodeiSet.remove ni work in
          let old = mapping.(ni) in
          let new_ = trans mapping ni in
          let work'' =
            if eq_inout eq_env old new_ then work'
            else (
              mapping.(ni) <- new_;
              NodeiSet.union work' (succs flow ni))
          in
          loop (i + 1) work''
    in
    loop 0 workset

  let forward_succs (f : F.flow) n =
    (f.graph#successors n)#fold
      (fun s (ni, _) -> NodeiSet.add ni s)
      NodeiSet.empty

  let backward_succs (f : F.flow) n =
    (f.graph#predecessors n)#fold
      (fun s (ni, _) -> NodeiSet.add ni s)
      NodeiSet.empty

  let (fixpoint :
        timeout:float ->
        eq_env:('env -> 'env -> bool) ->
        init:'env mapping ->
        trans:'env transfn ->
        flow:F.flow ->
        forward:bool ->
        'env mapping * [ `Ok | `Timeout ]) =
   fun ~timeout ~eq_env ~init ~trans ~flow ~forward ->
    let succs = if forward then forward_succs else backward_succs in
    let work =
      (* This prevents dead code from getting analyzed. *)
      flow.reachable
    in
    fixpoint_worker ~timeout eq_env init trans flow succs work

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
