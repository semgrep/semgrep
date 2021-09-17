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
module G = AST_generic
module V = Visitor_AST
module F = File_and_more
module R = Rule
module PM = Pattern_match

(* Taint-tracking via ranges
 * -------------------------
 *
 * First we run a bunch of search queries to obtain the ranges of sources,
 * sanitizers, and sinks. The actual analysis happens in Dataflow_tainting
 * using the IL representation. There we check whether the eorig's and iorig's
 * are within those ranges to decide whether an expression or instruction is a
 * source of taint, a sanitizer, or a sink. Finally we collect the results
 * produced by the dataflow analysis and filter out duplicates.
 *
 * We could think of using an equality test to compare ranges, e.g., an
 * expression would be a source of taint only if its range is exactly one of
 * the ranges matched by `pattern-sources`. In practice, this does not work
 * because `pattern-sources` etc can match anything, and the IL eorig's and
 * iorig's are only expressions. For example, `pattern-sources` can match
 * `foo(x);` but the eorig in the IL will be `foo(x)` whose range does not
 * include the ending `;`.
 *
 * So, we use sub-range checks. And this actually provides some extra power,
 * as it allows us to mark anything as a source/sanitizer/sink. For example,
 * we could use a pattern like `if (E) { ... }` to specify that anything
 * inside such an `if` statement should be considered sanitized. We are not
 * limited to expressions or single statements.
 *
 * However, using sub-range checks leads to duplicates. For example, the PHP
 * expression `sink("$source" . 'here')` will be transalted to IL as two
 * instructions `tmp = "$source" . 'here'` and `sink(tmp)`. If `sink(...)`
 * is a `pattern-sinks`, then both instructions' ranges are inside
 * the `pattrn-sinks` ranges. If `$source` is a `pattern-sources`, then both
 * instructions are also tainted, and Dataflow_tainting will report two matches.
 *
 * So, we need to remove duplicate subamtches at the end.
 * TODO: We could perhaps do this in a cleaner way by having an intermediate
 * step where we annotate the Generic AST, marking which statements and
 * expressions are sources, sanitizers, or sinks. If e.g. an expression is a
 * sink, we take care not to mark as sinks any of its subexpressions, in order
 * to prevent duplicates.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Simple wrapper around the tainting dataflow-based analysis in pfff.
 *
 * Here we pass matcher functions that uses semgrep patterns to
 * describe the source/sink/sanitizers.
 *)
let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

module F2 = IL

module Y = struct
  type node = F2.node

  type edge = F2.edge

  type flow = {
    graph : (node, edge) Ograph_extended.ograph_mutable;
    entry : int;
  }

  let short_string_of_node n = Display_IL.short_string_of_node_kind n.F2.n
end

module DataflowY = Dataflow.Make (Y)

let any_in_ranges any ranges =
  (* This is potentially slow. We may need to store range position in
   * the AST at some point. *)
  match Visitor_AST.range_of_any_opt any with
  | None ->
      logger#debug
        "Cannot compute range, there are no real tokens in this AST: %s"
        (G.show_any any);
      false
  | Some (tok1, tok2) ->
      let r = Range.range_of_token_locations tok1 tok2 in
      List.exists (Range.( $<=$ ) r) ranges

let ranges_of_pformula config equivs file_and_more rule_id pformula =
  let formula = Rule.formula_of_pformula pformula in
  Match_rules.matches_of_formula config equivs rule_id file_and_more formula
    None
  |> snd
  |> List.map (fun rwm -> rwm.Range_with_metavars.r)

(*s: function [[Tainting_generic.config_of_rule]] *)

let taint_config_of_rule default_config equivs file ast_and_errors
    (rule : R.rule) (spec : R.taint_spec) found_tainted_sink =
  let config = Common.( ||| ) rule.options default_config in
  let lazy_ast_and_errors = lazy ast_and_errors in
  let file_and_more =
    {
      F.file;
      xlang = rule.languages;
      lazy_content = lazy (Common.read_file file);
      lazy_ast_and_errors;
    }
  in
  let find_ranges pfs =
    (* TODO: Make an Or formula and run a single query. *)
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
             match V.range_of_any_opt code with
             | None ->
                 (* TODO: Report a warning to the user? *)
                 logger#error
                   "Cannot report taint-mode match because we lack range info: \
                    %s"
                   (G.show_any code);
                 ()
             | Some range_loc ->
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
          (fun (k, _v) ((ent, def_kind) as def) ->
            match def_kind with
            | G.FuncDef fdef ->
                let opt_name = AST_to_IL.name_of_entity ent in
                check_stmt opt_name fdef.G.fbody;
                (* go into nested functions *)
                k def
            | __else__ -> k def);
        V.kfunction_definition =
          (fun (k, _v) def ->
            check_stmt None def.G.fbody;
            (* go into nested functions *)
            k def);
      }
  in
  (* Check each function definition. *)
  v (G.Pr ast);
  (* Check the top-level statements.
   * In scripting languages it is not unusual to write code outside
   * function declarations and we want to check this too. We simply
   * treat the program itself as an anonymous function. *)
  check_stmt None (G.stmt1 ast);

  !matches
  (* same post-processing as for search-mode in Match_rules.ml *)
  |> PM.uniq
  |> PM.no_submatches (* see "Taint-tracking via ranges" *)
  |> Common.before_return (fun v ->
         v
         |> List.iter (fun (m : Pattern_match.t) ->
                let str = Common.spf "with rule %s" m.rule_id.id in
                hook str m.env m.tokens))
  [@@profiling]

(*e: function [[Tainting_generic.check2]] *)

(*e: semgrep/tainting/Tainting_generic.ml *)
