(* Amarin Phaosawasdi
 *
 * Copyright (C) 2023 Semgrep, Inc.
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
open AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Implicit return support.
 *
 * For languages that support implicit return, this analysis marks expressions
 * that are executed before exiting the function as returning nodes.
 *
 * These nodes will be later used to allow matching to match
 *   return e
 * with
 *   e
 * when e is a returning node.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let rec mark_first_instr_ancestor (cfg : IL.cfg) i =
  let node = cfg.graph#nodes#find i in
  match node.n with
  | Exit
  | NOther (Noop _)
  | NGoto _
  | Join ->
      CFG.predecessors cfg i
      |> List.iter (fun (pred_i, _) -> mark_first_instr_ancestor cfg pred_i)
  | NInstr instr -> (
      match instr with
      | { i = Assign (_, { eorig = SameAs e; _ }); _ } ->
          e.is_implicit_return <- true
      | _else_ -> ())
  | _else_ -> ()

let mark_implicit_return_nodes_one_function (cfg : IL.cfg) =
  (* Traverse backward from exit and mark the expression in the
   * first instruction node along each path.
   *)
  mark_first_instr_ancestor cfg cfg.exit

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let lang_supports_implicit_return (lang : Lang.t) =
  match lang with
  | Ruby
  | Rust
  | Julia ->
      true
  | _else_ -> false

let mark_implicit_return_nodes (fun_cfgs : CFG_build.fun_cfg list) =
  fun_cfgs
  |> List.iter (fun fun_cfg ->
         match (fun_cfg : CFG_build.fun_cfg) with
         | { fparams = _; fcfg } -> mark_implicit_return_nodes_one_function fcfg)
