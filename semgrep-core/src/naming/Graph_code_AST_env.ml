(* Yoann Padioleau
 *
 * Copyright (C) 2012 Facebook
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

module E = Entity_code
module G = Graph_code
module AST = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The environment used in Graph_code_AST.ml
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type qualifier = AST.dotted_ident

type env = {
  (* this is modified by side effects *)
  g : Graph_code.t;
  phase : phase;
  hooks : hooks;
  readable : Common.filename;
  lang : Lang.t;
  (* for lookup_local_file_opt *)
  file_qualifier : qualifier;
  (* the parent to connect to when creating new nodes *)
  current_parent : Graph_code.node;
  (* the current "scope", everthing that is enclosing the current code.
   * less: no support for functors or complex modules *)
  current_qualifier : qualifier;
  (* for resolving self.foo() *)
  class_qualifier : qualifier option;
}

(* We need 2 phases:
 * - one to get all the definitions
 * - one to get all the Uses.
 *
 * - still? one to get the inheritance information,
 * The inheritance is a kind of use, but certain uses like using
 * a field needs the full inheritance tree to already be computed
 * as we may need to lookup entities up in the parents.
 *)
and phase =
  | Defs
  (* still? | Inheritance *)
  | Uses

and hooks = {
  on_def_node : Graph_code.node -> AST_generic.definition -> unit;
  on_extend_edge :
    Graph_code.node ->
    Graph_code.node ->
    AST_generic.entity * AST_generic.class_definition ->
    unit;
  (* TODO: fill with something useful *)
  on_misc : unit -> unit;
}
