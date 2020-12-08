(*s: semgrep/tainting/Tainting_generic.ml *)
(*s: pad/r2c copyright *)
(* Yoann Padioleau
 *
 * Copyright (C) 2019-2020 r2c
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
module AST = AST_generic
module V = Visitor_AST
module R = Tainting_rule
module Flag = Flag_semgrep

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Simple wrapper around the tainting dataflow-based analysis in pfff.
 *
 * Here we pass matcher functions that uses semgrep patterns to
 * describe the source/sink/sanitizers.
*)
let _logger = Logging.get_logger [__MODULE__]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

module F2 = IL
module DataflowY = Dataflow.Make (struct
    type node = F2.node
    type edge = F2.edge
    type flow = (node, edge) Ograph_extended.ograph_mutable
    let short_string_of_node n = Display_IL.short_string_of_node_kind n.F2.n
  end)

(*s: function [[Tainting_generic.match_pat_instr]] *)
let match_pat_instr pat =
  match pat with
  | [] -> (fun _ -> false)
  | xs ->
      let xs = xs |> List.map (function
        | AST.E e -> e
        | _ -> failwith "Only Expr patterns are supported in tainting rules"
      )
      in
      let pat = Common2.foldl1 (fun x acc -> AST.DisjExpr (x, acc)) xs in
      (fun instr ->
         let eorig = instr.IL.iorig in
         (* the rule is just used by match_e_e for profiling stats *)
         let rule = { Rule.id = "<tainting>";
                      pattern = AST.E pat; pattern_string = "<tainting> pat";
                      message = ""; severity = Rule.Error;
                      languages = []; } in

         let matches_with_env = Semgrep_generic.match_e_e rule pat eorig in
         matches_with_env <> []
      )
(*e: function [[Tainting_generic.match_pat_instr]] *)


(*s: function [[Tainting_generic.config_of_rule]] *)
let config_of_rule found_tainted_sink rule =
  { Dataflow_tainting.
    is_source = match_pat_instr rule.R.source;
    is_sanitizer = match_pat_instr rule.R.sanitizer;
    is_sink = match_pat_instr rule.R.sink;

    found_tainted_sink;
  }
(*e: function [[Tainting_generic.config_of_rule]] *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(*s: function [[Tainting_generic.check2]] *)
let check rules file ast =
  let matches = ref [] in

  let v = V.mk_visitor { V.default_visitor with
                         V.kfunction_definition = (fun (_k, _) def ->
                           let xs = AST_to_IL.stmt def.AST.fbody in
                           let flow = CFG_build.cfg_of_stmts xs in

                           rules |> List.iter (fun rule ->
                             let found_tainted_sink = (fun instr _env ->
                               Common.push { Match_result.
                                             rule = Tainting_rule.rule_of_tainting_rule rule;
                                             file;
                                             code = AST.E (instr.IL.iorig);
                                             (* todo: use env from sink matching func?  *)
                                             env = [];
                                           } matches;
                             ) in
                             let config = config_of_rule found_tainted_sink rule in
                             let mapping = Dataflow_tainting.fixpoint config flow in
                             ignore (mapping);
                             (* TODO
                                         logger#sdebug (DataflowY.mapping_to_str flow
                                                          (fun () -> "()") mapping);
                             *)
                           )
                         );
                       } in
  v (AST.Pr ast);

  !matches
[@@profiling]
(*e: function [[Tainting_generic.check2]] *)

(*e: semgrep/tainting/Tainting_generic.ml *)
