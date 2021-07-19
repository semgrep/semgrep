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
module R = Rule
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

let any_in_ranges any ranges =
  (* This is potentially slow. We may need to store range position in
   * the AST at some point. *)
  let tok1, tok2 = Visitor_AST.range_of_any any in
  let r = { Range.start = tok1.charpos; end_ = tok2.charpos } in
  List.exists (Range.( $<=$ ) r) ranges

let ranges_of_pformula config equivs file_and_more rule_id pformula =
  let file, _, _ = file_and_more in
  let lazy_content = lazy (Common.read_file file) in
  let formula = Rule.formula_of_pformula pformula in
  Match_rules.matches_of_formula config equivs rule_id file_and_more
    lazy_content formula None
  |> snd
  |> List.map (fun rwm -> rwm.Range_with_metavars.r)

(*s: function [[Tainting_generic.config_of_rule]] *)

let taint_config_of_rule default_config equivs file ast_and_errors
    (rule : R.rule) (spec : R.taint_spec) found_tainted_sink =
  let config = Common.( ||| ) rule.options default_config in
  let lazy_ast_and_errors = lazy ast_and_errors in
  let file_and_more = (file, rule.languages, lazy_ast_and_errors) in
  let find_ranges pfs =
    (* if perf is a problem, we could build an interval set here *)
    pfs
    |> List.map (ranges_of_pformula config equivs file_and_more (fst rule.id))
    |> List.concat
  in
  let sources_ranges = find_ranges spec.sources
  and sanitizers_ranges = find_ranges spec.sanitizers
  and sinks_ranges = find_ranges spec.sinks in
  {
    Dataflow_tainting.is_source = (fun x -> any_in_ranges x sources_ranges);
    is_sanitizer = (fun x -> any_in_ranges x sanitizers_ranges);
    is_sink = (fun x -> any_in_ranges x sinks_ranges);
    found_tainted_sink;
  }

(*e: function [[Tainting_generic.config_of_rule]] *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(*s: function [[Tainting_generic.check2]] *)
let check hook default_config (taint_rules : (Rule.rule * Rule.taint_spec) list)
    equivs file ast =
  let matches = ref [] in

  let taint_configs =
    taint_rules
    |> List.map (fun (rule, taint_spec) ->
           let rule_id =
             {
               Pattern_match.id = fst rule.Rule.id;
               message = rule.Rule.message;
               pattern_string = "TODO: no pattern_string";
             }
           in
           let found_tainted_sink code _env =
             let range_loc = V.range_of_any code in
             let tokens = lazy (V.ii_of_any code) in
             (* todo: use env from sink matching func?  *)
             Common.push
               { PM.rule_id; file; range_loc; tokens; env = [] }
               matches
           in
           taint_config_of_rule default_config equivs file (ast, []) rule
             taint_spec found_tainted_sink)
  in

  let fun_env = Hashtbl.create 8 in

  let check_stmt opt_name def_body =
    let xs = AST_to_IL.stmt def_body in
    let flow = CFG_build.cfg_of_stmts xs in

    taint_configs
    |> List.iter (fun taint_config ->
           let mapping =
             Dataflow_tainting.fixpoint taint_config fun_env opt_name flow
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
  (* same post-processing as for search-mode in Match_rules.ml *)
  |> Common.uniq_by (AST_utils.with_structural_equal PM.equal)
  |> Common.before_return (fun v ->
         v
         |> List.iter (fun (m : Pattern_match.t) ->
                let str = Common.spf "with rule %s" m.rule_id.id in
                hook str m.env m.tokens))
  [@@profiling]

(*e: function [[Tainting_generic.check2]] *)

(*e: semgrep/tainting/Tainting_generic.ml *)
