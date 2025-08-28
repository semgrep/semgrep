(*
   Copyright (c) 2021-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
type nodei = int

(* A set of nodes (via their indices),
 * used for example in the reaching analysis.
 *)
module NodeiSet : Set.S with type elt = Int.t

(* Return value of a dataflow analysis.
 * The array is indexed by nodei.
 *)
type 'env mapping = 'env inout array
and 'env inout = { in_env : 'env; out_env : 'env }

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

(* helpers *)
val ns_to_str : NodeiSet.t -> string

(* we use now a functor so we can reuse the same code for dataflow on
 * the IL (IL.cfg) or generic AST (Controlflow.flow)
 *)
module type Flow = sig
  type node
  type edge
  type flow = (node, edge) CFG.t

  val short_string_of_node : node -> string
end

module Make (F : Flow) : sig
  (* main entry point *)
  val fixpoint :
    timeout:float
      (** We set a "soft" timeout to guard against insanely complex/large functions,
      and potential bugs. The timeout condition is checked only after each call to
      the 'trans' function, so there is no strict time guarantee for completion. *) ->
    eq_env:('env -> 'env -> bool) ->
    init:'env mapping ->
    trans:'env transfn ->
    flow:F.flow ->
    forward:bool ->
    'env mapping * [ `Ok | `Timeout ]

  val new_node_array : F.flow -> 'a -> 'a array

  (* debugging output *)
  val display_mapping : F.flow -> 'env mapping -> ('env -> string) -> unit
end
