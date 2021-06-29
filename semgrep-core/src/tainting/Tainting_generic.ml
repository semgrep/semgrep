(*s: semgrep/tainting/Tainting_generic.ml *)
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
module AST = AST_generic
module V = Visitor_AST
module R = Tainting_rule
module R2 = Mini_rule
module Flag = Flag_semgrep
module PM = Pattern_match

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Simple wrapper around the tainting dataflow-based analysis in pfff.
 *
 * Here we pass matcher functions that uses semgrep patterns to
 * describe the source/sink/sanitizers.
 *)
let _logger = Logging.get_logger [ __MODULE__ ]

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

let match_pat_eorig pat =
  match pat with
  | [] -> fun _ -> false
  | xs ->
      let xs =
        xs
        |> List.map (function
             | AST.E e -> e
             | AST.S { AST.s = AST.ExprStmt (e, _); _ } ->
                 (* Some statements in the input language are translated into
                  * expressions in the Generic AST. This is e.g. the case of
                  * `echo' in PHP. This small hack allows us to annotate those
                  * statements as souces/sanitizers/sinks. *)
                 e
             | _ ->
                 failwith "Only Expr patterns are supported in tainting rules")
      in
      let pat = Common2.foldl1 (fun x acc -> AST.DisjExpr (x, acc)) xs in
      fun eorig ->
        (* the rule is just used by match_e_e for profiling stats *)
        let rule =
          {
            R2.id = "<tainting>";
            pattern = AST.E pat;
            pattern_string = "<tainting> pat";
            message = "";
            severity = R2.Error;
            languages = [];
          }
        in

        let env =
          Matching_generic.empty_environment None Config_semgrep.default_config
        in
        let matches_with_env = Match_patterns.match_e_e rule pat eorig env in
        matches_with_env <> []

let match_pat_exp pat exp =
  let eorig = exp.IL.eorig in
  match_pat_eorig pat eorig

(*s: function [[Tainting_generic.match_pat_instr]] *)
let match_pat_instr pat instr =
  let eorig = instr.IL.iorig in
  match_pat_eorig pat eorig

(*e: function [[Tainting_generic.match_pat_instr]] *)

(*s: function [[Tainting_generic.config_of_rule]] *)
let config_of_rule found_tainted_sink rule =
  {
    Dataflow_tainting.is_source = match_pat_instr rule.R.source;
    is_source_exp = match_pat_exp rule.R.source;
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

  let fun_env = Hashtbl.create 8 in

  let check_stmt opt_name def_body =
    let xs = AST_to_IL.stmt def_body in
    let flow = CFG_build.cfg_of_stmts xs in

    rules
    |> List.iter (fun rule ->
           let found_tainted_sink instr _env =
             let code = AST.E instr.IL.iorig in
             let range_loc = V.range_of_any code in
             let tokens = lazy (V.ii_of_any code) in
             let rule_id = Tainting_rule.rule_id_of_tainting_rule rule in
             (* todo: use env from sink matching func?  *)
             Common.push
               { PM.rule_id; file; range_loc; tokens; env = [] }
               matches
           in
           let config = config_of_rule found_tainted_sink rule in
           let mapping =
             Dataflow_tainting.fixpoint config fun_env opt_name flow
           in
           ignore mapping
           (* TODO
              logger#sdebug (DataflowY.mapping_to_str flow
               (fun () -> "()") mapping);
           *))
  in

  let v =
    V.mk_visitor
      {
        V.default_visitor with
        V.kdef =
          (fun (k, _) ((ent, def_kind) as def) ->
            match def_kind with
            | AST.FuncDef fdef ->
                let opt_name = AST_to_IL.name_of_entity ent in
                check_stmt opt_name fdef.AST.fbody
            | __else__ -> k def);
        V.kfunction_definition =
          (fun (_k, _) def -> check_stmt None def.AST.fbody);
      }
  in
  (* Check each function definition. *)
  v (AST.Pr ast);
  (* Check the top-level statements.
   * In scripting languages it is not unusual to write code outside
   * function declarations and we want to check this too. We simply
   * treat the program itself as an anonymous function. *)
  check_stmt None (AST.stmt1 ast);

  !matches
  [@@profiling]

(*e: function [[Tainting_generic.check2]] *)

(*e: semgrep/tainting/Tainting_generic.ml *)
