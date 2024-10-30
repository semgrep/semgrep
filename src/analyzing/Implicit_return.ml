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
  (* If control reaches a catch node, it's an exception. It can't be a return,
   * so stop visiting here.
   *)
  | NOther (Noop "catch") -> ()
  (* Visit ancestor for exit, noop, goto, and join nodes. *)
  | Exit
  | NOther (Noop _)
  | NGoto _
  | Join ->
      CFG.predecessors cfg i
      |> List.iter (fun (pred_i, _) -> mark_first_instr_ancestor cfg pred_i)
  (* Certain instruction nodes may be implicitly returned. *)
  | NInstr instr -> (
      match instr with
      | { i = Assign (_, { eorig = SameAs e; _ }); _ }
      | { i = Call _; iorig = SameAs e } ->
          e.is_implicit_return <- true
      | _else_ -> ())
  | _else_ -> ()

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let lang_supports_implicit_return (lang : Lang.t) =
  match lang with
  | Elixir
  | Ruby
  | Rust
  | Scala
  | Julia ->
      true
  | _else_ -> false

let mark_implicit_return_nodes (cfg : IL.cfg) =
  (* Traverse backward from exit and mark the expression in the
   * first instruction node along each path.
   *)
  mark_first_instr_ancestor cfg cfg.exit

let mark_implicit_return_fdef lang ~tok fdef =
  let fdef_il = AST_to_IL.function_definition lang fdef in
  let fcfg, _flambdas = CFG_build.cfg_of_stmts ~tok fdef_il.fbody in
  (* Lambdas are separately visited by 'mark_implicit_return',
   * see 'LambdaKind' case. *)
  mark_implicit_return_nodes fcfg

let mark_implicit_return lang ast =
  ast
  |> Visit_function_defs.visit (fun _ent fdef ->
         let fkind, tok = fdef.fkind in
         match fkind with
         | __any__ when lang_supports_implicit_return lang ->
             mark_implicit_return_fdef lang ~tok fdef
         | LambdaKind ->
             (* Lambdas expressions tend to always support implicit returns,
              * even in languages that require explicit returns like Java. *)
             mark_implicit_return_fdef lang ~tok fdef
         | __else__ -> ())
