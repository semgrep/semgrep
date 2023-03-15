(*
 * The author disclaims copyright to this source code.  In place of
 * a legal notice, here is a blessing:
 *
 *    May you do good and not evil.
 *    May you find forgiveness for yourself and forgive others.
 *    May you share freely, never taking more than you give.
 *)
open Common
open Runner_config
module Flag = Flag_semgrep
module E = Semgrep_error_code
module J = JSON

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This module contains the main command line parsing logic.
 *
 * It is packaged as a library so it can be used both for the stand-alone
 * semgrep-core binary as well as the semgrep_bridge.so shared library.
 * The code here used to be in Main.ml.
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* debugging/profiling/logging flags *)
(* ------------------------------------------------------------------------- *)

(* You can set those environment variables to enable debugging/profiling
 * instead of using -debug or -profile. This is useful when you don't call
 * directly semgrep-core but instead use the semgrep Python wrapper.
 *)
let env_debug = "SEMGREP_CORE_DEBUG"
let env_profile = "SEMGREP_CORE_PROFILE"
let env_extra = "SEMGREP_CORE_EXTRA"
let log_config_file = ref Runner_config.default.log_config_file
let log_to_file = ref None

(* see also verbose/... flags in Flag_semgrep.ml *)
(* to test things *)
let test = ref Runner_config.default.test
let debug = ref Runner_config.default.debug

(* related:
 * - Flag_semgrep.debug_matching
 * - Flag_semgrep.fail_fast
 * - Trace_matching.on
 *)

(* try to continue processing files, even if one has a parse error with -e/f *)
let error_recovery = ref Runner_config.default.error_recovery
let profile = ref Runner_config.default.profile

(* report matching times per file *)
let report_time = ref Runner_config.default.report_time

(* used for -json -profile *)
let profile_start = ref Runner_config.default.profile_start

(* step-by-step matching debugger *)
let matching_explanations = ref Runner_config.default.matching_explanations

(* ------------------------------------------------------------------------- *)
(* main flags *)
(* ------------------------------------------------------------------------- *)

(* -e *)
let pattern_string = ref ""

(* -f *)
let pattern_file = ref ""

(* -rules *)
let rule_source = ref None
let equivalences_file = ref ""

(* TODO: infer from basename argv(0) ? *)
let lang = ref None
let output_format = ref Runner_config.default.output_format
let match_format = ref Runner_config.default.match_format
let mvars = ref ([] : Metavariable.mvar list)
let lsp = ref Runner_config.default.lsp

(* ------------------------------------------------------------------------- *)
(* limits *)
(* ------------------------------------------------------------------------- *)

(* timeout in seconds; 0 or less means no timeout *)
let timeout = ref Runner_config.default.timeout
let timeout_threshold = ref Runner_config.default.timeout_threshold
let max_memory_mb = ref Runner_config.default.max_memory_mb (* in MiB *)

(* arbitrary limit *)
let max_match_per_file = ref Runner_config.default.max_match_per_file

(* -j *)
let ncores = ref Runner_config.default.ncores

(* ------------------------------------------------------------------------- *)
(* optional optimizations *)
(* ------------------------------------------------------------------------- *)
(* see Flag_semgrep.ml *)
let use_parsing_cache = ref Runner_config.default.parsing_cache_dir

(* similar to filter_irrelevant_patterns, but use the whole rule to extract
 * the regexp *)
let filter_irrelevant_rules = ref Runner_config.default.filter_irrelevant_rules

(* ------------------------------------------------------------------------- *)
(* flags used by the semgrep-python wrapper *)
(* ------------------------------------------------------------------------- *)

(* take the list of files in a file (given by semgrep-python) *)
let target_source = ref None

(* ------------------------------------------------------------------------- *)
(* pad's action flag *)
(* ------------------------------------------------------------------------- *)

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let version = spf "semgrep-core version: %s" Version.version

(* Note that set_gc() may not interact well with Memory_limit and its use of
 * Gc.alarm. Indeed, the Gc.alarm triggers only at major cycle
 * and the tuning below raise significantly the major cycle trigger.
 * This is why we call set_gc() only when max_memory_mb is unset.
 *)
let set_gc () =
  logger#info "Gc tuning";
  (*
  if !Flag.debug_gc
  then Gc.set { (Gc.get()) with Gc.verbose = 0x01F };
*)
  (* only relevant in bytecode, in native the stacklimit is the os stacklimit,
   * which usually requires a ulimit -s 40000
   *)
  Gc.set { (Gc.get ()) with Gc.stack_limit = 1000 * 1024 * 1024 };

  (* see www.elehack.net/michael/blog/2010/06/ocaml-memory-tuning *)
  Gc.set { (Gc.get ()) with Gc.minor_heap_size = 4_000_000 };
  Gc.set { (Gc.get ()) with Gc.major_heap_increment = 8_000_000 };
  Gc.set { (Gc.get ()) with Gc.space_overhead = 300 };
  ()

(*****************************************************************************)
(* Dumpers *)
(*****************************************************************************)

(* used for the Dump AST in semgrep.live *)
let json_of_v (v : OCaml.v) =
  let rec aux v =
    match v with
    | OCaml.VUnit -> J.String "()"
    | OCaml.VBool v1 -> if v1 then J.String "true" else J.String "false"
    | OCaml.VFloat v1 -> J.Float v1 (* ppf "%f" v1 *)
    | OCaml.VChar v1 -> J.String (spf "'%c'" v1)
    | OCaml.VString v1 -> J.String v1
    | OCaml.VInt i -> J.Int i
    | OCaml.VTuple xs -> J.Array (Common.map aux xs)
    | OCaml.VDict xs -> J.Object (Common.map (fun (k, v) -> (k, aux v)) xs)
    | OCaml.VSum (s, xs) -> (
        match xs with
        | [] -> J.String (spf "%s" s)
        | [ one_element ] -> J.Object [ (s, aux one_element) ]
        | _ :: _ :: _ -> J.Object [ (s, J.Array (Common.map aux xs)) ])
    | OCaml.VVar (s, i64) -> J.String (spf "%s_%d" s (Int64.to_int i64))
    | OCaml.VArrow _ -> failwith "Arrow TODO"
    | OCaml.VNone -> J.Null
    | OCaml.VSome v -> J.Object [ ("some", aux v) ]
    | OCaml.VRef v -> J.Object [ ("ref@", aux v) ]
    | OCaml.VList xs -> J.Array (Common.map aux xs)
    | OCaml.VTODO _ -> J.String "VTODO"
  in
  aux v

let dump_v_to_format (v : OCaml.v) =
  match !output_format with
  | Text -> OCaml.string_of_v v
  | Json _ -> J.string_of_json (json_of_v v)

(* works with -lang *)
let dump_pattern (file : Common.filename) =
  let file = Run_semgrep.replace_named_pipe_by_regular_file file in
  let s = Common.read_file file in
  (* mostly copy-paste of parse_pattern in runner, but with better error report *)
  let lang = Xlang.lang_of_opt_xlang_exn !lang in
  E.try_with_print_exn_and_reraise file (fun () ->
      let any = Parse_pattern.parse_pattern lang ~print_errors:true s in
      let v = Meta_AST.vof_any any in
      let s = dump_v_to_format v in
      pr s)

let dump_ast ?(naming = false) lang file =
  let file = Run_semgrep.replace_named_pipe_by_regular_file file in
  E.try_with_print_exn_and_reraise file (fun () ->
      let { Parse_target.ast; skipped_tokens; _ } =
        if naming then Parse_target.parse_and_resolve_name lang file
        else Parse_target.just_parse_with_lang lang file
      in
      let v = Meta_AST.vof_any (AST_generic.Pr ast) in
      (* 80 columns is too little *)
      Format.set_margin 120;
      let s = dump_v_to_format v in
      pr s;
      if skipped_tokens <> [] then (
        pr2 (spf "WARNING: fail to fully parse %s" file);
        pr2
          (Common.map (fun e -> "  " ^ Dumper.dump e) skipped_tokens
          |> String.concat "\n");
        Runner_exit.(exit_semgrep False)))

(* mostly a copy paste of Test_analyze_generic.ml *)
let dump_il_all file =
  let lang = List.hd (Lang.langs_of_filename file) in
  let ast = Parse_target.parse_program file in
  Naming_AST.resolve lang ast;
  let xs = AST_to_IL.stmt lang (AST_generic.stmt1 ast) in
  List.iter (fun stmt -> pr2 (IL.show_stmt stmt)) xs
  [@@action]

let dump_il file =
  let module G = AST_generic in
  let module V = Visitor_AST in
  let lang = List.hd (Lang.langs_of_filename file) in
  let ast = Parse_target.parse_program file in
  Naming_AST.resolve lang ast;
  let report_func_def_with_name ent_opt fdef =
    let name =
      match ent_opt with
      | None -> "<lambda>"
      | Some { G.name = EN n; _ } -> G.show_name n
      | Some _ -> "<entity>"
    in
    pr2 (spf "Function name: %s" name);
    let s =
      AST_generic.show_any
        (G.S (AST_generic_helpers.funcbody_to_stmt fdef.G.fbody))
    in
    pr2 s;
    pr2 "==>";

    let _, xs = AST_to_IL.function_definition lang fdef in
    let s = IL.show_any (IL.Ss xs) in
    pr2 s
  in
  Visit_function_defs.visit report_func_def_with_name ast

let dump_v1_json file =
  let file = Run_semgrep.replace_named_pipe_by_regular_file file in
  match Lang.langs_of_filename file with
  | lang :: _ ->
      E.try_with_print_exn_and_reraise file (fun () ->
          let { Parse_target.ast; skipped_tokens; _ } =
            Parse_target.parse_and_resolve_name lang file
          in
          let v1 = AST_generic_to_v1.program ast in
          let s = Ast_generic_v1_j.string_of_program v1 in
          pr s;
          if skipped_tokens <> [] then
            pr2 (spf "WARNING: fail to fully parse %s" file))
  | [] -> failwith (spf "unsupported language for %s" file)

let generate_ast_json file =
  match Lang.langs_of_filename file with
  | lang :: _ ->
      let ast = Parse_target.parse_and_resolve_name_warn_if_partial lang file in
      let v1 = AST_generic_to_v1.program ast in
      let s = Ast_generic_v1_j.string_of_program v1 in
      let file = file ^ ".ast.json" in
      Common.write_file ~file s;
      pr2 (spf "saved JSON output in %s" file)
  | [] -> failwith (spf "unsupported language for %s" file)

let generate_ast_binary lang file =
  let final =
    Parse_with_caching.versioned_parse_result_of_file Version.version lang file
  in
  let file = file ^ Parse_with_caching.binary_suffix in
  assert (Parse_with_caching.is_binary_ast_filename file);
  Common2.write_value final file;
  pr2 (spf "saved marshalled generic AST in %s" file)

let dump_ext_of_lang () =
  let lang_to_exts =
    Lang.keys
    |> Common.map (fun lang_str ->
           match Lang.of_string_opt lang_str with
           | Some lang ->
               lang_str ^ "->" ^ String.concat ", " (Lang.ext_of_lang lang)
           | None -> "")
  in
  pr2
    (spf "Language to supported file extension mappings:\n %s"
       (String.concat "\n" lang_to_exts))

let dump_equivalences file =
  let file = Run_semgrep.replace_named_pipe_by_regular_file file in
  let xs = Parse_equivalences.parse file in
  pr2_gen xs

let dump_rule file =
  let file = Run_semgrep.replace_named_pipe_by_regular_file file in
  let rules = Parse_rule.parse file in
  rules |> List.iter (fun r -> pr (Rule.show r))

let prefilter_of_rules file =
  let rules = Parse_rule.parse file in
  let xs =
    rules
    |> Common.map (fun r ->
           let pre_opt = Analyze_rule.regexp_prefilter_of_rule r in
           let pre_atd_opt =
             Option.map Analyze_rule.prefilter_formula_of_prefilter pre_opt
           in
           let id = r.Rule.id |> fst in
           { Semgrep_prefilter_t.rule_id = id; filter = pre_atd_opt })
  in
  let s = Semgrep_prefilter_j.string_of_prefilters xs in
  pr s

(*****************************************************************************)
(* Config *)
(*****************************************************************************)

let mk_config () =
  {
    log_config_file = !log_config_file;
    log_to_file = !log_to_file;
    test = !test;
    debug = !debug;
    profile = !profile;
    report_time = !report_time;
    error_recovery = !error_recovery;
    profile_start = !profile_start;
    matching_explanations = !matching_explanations;
    pattern_string = !pattern_string;
    pattern_file = !pattern_file;
    rule_source = !rule_source;
    lang_job = None;
    filter_irrelevant_rules = !filter_irrelevant_rules;
    (* not part of CLI *)
    equivalences_file = !equivalences_file;
    lang = !lang;
    output_format = !output_format;
    match_format = !match_format;
    mvars = !mvars;
    lsp = !lsp;
    timeout = !timeout;
    timeout_threshold = !timeout_threshold;
    max_memory_mb = !max_memory_mb;
    max_match_per_file = !max_match_per_file;
    ncores = !ncores;
    parsing_cache_dir = !use_parsing_cache;
    target_source = !target_source;
    action = !action;
    version = Version.version;
    roots = [] (* This will be set later in main () *);
  }

(*****************************************************************************)
(* Experiments *)
(*****************************************************************************)
(* See Experiments.ml now *)

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () =
  [
    (* possibly useful to the user *)
    ( "-show_ast_json",
      " <file> dump on stdout the generic AST of file in JSON",
      Arg_helpers.mk_action_1_arg dump_v1_json );
    ( "-generate_ast_json",
      " <file> save in file.ast.json the generic AST of file in JSON",
      Arg_helpers.mk_action_1_arg generate_ast_json );
    ( "-generate_ast_binary",
      " <file> save in file.ast.binary the marshalled generic AST of file",
      Arg_helpers.mk_action_1_arg (fun file ->
          generate_ast_binary (Xlang.lang_of_opt_xlang_exn !lang) file) );
    ( "-prefilter_of_rules",
      " <file> dump the prefilter regexps of rules in JSON ",
      Arg_helpers.mk_action_1_arg prefilter_of_rules );
    ( "-parsing_stats",
      " <files or dirs> generate parsing statistics (use -json for JSON output)",
      Arg_helpers.mk_action_n_arg (fun xs ->
          Test_parsing.parsing_stats
            (Xlang.lang_of_opt_xlang_exn !lang)
            ~json:(!output_format <> Text) ~verbose:true xs) );
    (* the dumpers *)
    ( "-dump_extensions",
      " print file extension to language mapping",
      Arg_helpers.mk_action_0_arg dump_ext_of_lang );
    ("-dump_pattern", " <file>", Arg_helpers.mk_action_1_arg dump_pattern);
    ( "-dump_ast",
      " <file>",
      fun file ->
        Arg_helpers.mk_action_1_arg
          (dump_ast ~naming:false (Xlang.lang_of_opt_xlang_exn !lang))
          file );
    ( "-dump_named_ast",
      " <file>",
      fun file ->
        Arg_helpers.mk_action_1_arg
          (dump_ast ~naming:true (Xlang.lang_of_opt_xlang_exn !lang))
          file );
    ("-dump_il_all", " <file>", Arg_helpers.mk_action_1_arg dump_il_all);
    ("-dump_il", " <file>", Arg_helpers.mk_action_1_arg dump_il);
    ("-dump_rule", " <file>", Arg_helpers.mk_action_1_arg dump_rule);
    ( "-dump_equivalences",
      " <file> (deprecated)",
      Arg_helpers.mk_action_1_arg dump_equivalences );
    ( "-dump_jsonnet_ast",
      " <file>",
      Arg_helpers.mk_action_1_arg Test_ojsonnet.dump_jsonnet_ast );
    ( "-dump_jsonnet_core",
      " <file>",
      Arg_helpers.mk_action_1_arg Test_ojsonnet.dump_jsonnet_core );
    ( "-dump_jsonnet_value",
      " <file>",
      Arg_helpers.mk_action_1_arg Test_ojsonnet.dump_jsonnet_value );
    ( "-dump_jsonnet_json",
      " <file>",
      Arg_helpers.mk_action_1_arg Test_ojsonnet.dump_jsonnet_json );
    ( "-dump_tree_sitter_cst",
      " <file> dump the CST obtained from a tree-sitter parser",
      Arg_helpers.mk_action_1_arg (fun file ->
          let file = Run_semgrep.replace_named_pipe_by_regular_file file in
          Test_parsing.dump_tree_sitter_cst
            (Xlang.lang_of_opt_xlang_exn !lang)
            file) );
    ( "-dump_tree_sitter_pattern_cst",
      " <file>",
      Arg_helpers.mk_action_1_arg (fun file ->
          let file = Run_semgrep.replace_named_pipe_by_regular_file file in
          Parse_pattern.dump_tree_sitter_pattern_cst
            (Xlang.lang_of_opt_xlang_exn !lang)
            file) );
    ( "-dump_pfff_ast",
      " <file> dump the generic AST obtained from a pfff parser",
      Arg_helpers.mk_action_1_arg (fun file ->
          let file = Run_semgrep.replace_named_pipe_by_regular_file file in
          Test_parsing.dump_pfff_ast (Xlang.lang_of_opt_xlang_exn !lang) file)
    );
    ( "-diff_pfff_tree_sitter",
      " <file>",
      Arg_helpers.mk_action_n_arg Test_parsing.diff_pfff_tree_sitter );
    (* Misc stuff *)
    ( "-expr_at_range",
      " <l:c-l:c> <file>",
      Arg_helpers.mk_action_2_arg Test_synthesizing.expr_at_range );
    ( "-synthesize_patterns",
      " <l:c-l:c> <file>",
      Arg_helpers.mk_action_2_arg Test_synthesizing.synthesize_patterns );
    ( "-generate_patterns",
      " <l:c-l:c>+ <file>",
      Arg_helpers.mk_action_n_arg Test_synthesizing.generate_pattern_choices );
    ( "-locate_patched_functions",
      " <file>",
      Arg_helpers.mk_action_1_arg Test_synthesizing.locate_patched_functions );
    ( "-stat_matches",
      " <marshalled file>",
      Arg_helpers.mk_action_1_arg Experiments.stat_matches );
    ( "-ebnf_to_menhir",
      " <ebnf file>",
      Arg_helpers.mk_action_1_arg Experiments.ebnf_to_menhir );
    ( "-parsing_regressions",
      " <files or dirs> look for parsing regressions",
      Arg_helpers.mk_action_n_arg (fun xs ->
          Test_parsing.parsing_regressions
            (Xlang.lang_of_opt_xlang_exn !lang)
            xs) );
    ( "-test_parse_tree_sitter",
      " <files or dirs> test tree-sitter parser on target files",
      Arg_helpers.mk_action_n_arg (fun xs ->
          Test_parsing.test_parse_tree_sitter
            (Xlang.lang_of_opt_xlang_exn !lang)
            xs) );
    ( "-check_rules",
      " <metachecks file> <files or dirs>",
      Arg_helpers.mk_action_n_arg
        (Check_rule.check_files mk_config Parse_rule.parse) );
    ( "-translate_rules",
      " <files or dirs>",
      Arg_helpers.mk_action_n_arg
        (Translate_rule.translate_files Parse_rule.parse) );
    ( "-stat_rules",
      " <files or dirs>",
      Arg_helpers.mk_action_n_arg (Check_rule.stat_files Parse_rule.parse) );
    ( "-test_rules",
      " <files or dirs>",
      Arg_helpers.mk_action_n_arg Test_engine.test_rules );
    ( "-parse_rules",
      " <files or dirs>",
      Arg_helpers.mk_action_n_arg Test_parsing.test_parse_rules );
    ( "-datalog_experiment",
      " <file> <dir>",
      Arg_helpers.mk_action_2_arg Datalog_experiment.gen_facts );
    ( "-postmortem",
      " <log file",
      Arg_helpers.mk_action_1_arg Statistics_report.stat );
    ( "-test_comby",
      " <pattern> <file>",
      Arg_helpers.mk_action_2_arg Test_comby.test_comby );
    ( "-test_eval",
      " <JSON file>",
      Arg_helpers.mk_action_1_arg Eval_generic.test_eval );
  ]
  @ Test_analyze_generic.actions ~parse_program:Parse_target.parse_program
  @ Test_dataflow_tainting.actions ()
  @ Test_naming_generic.actions ~parse_program:Parse_target.parse_program

let options actions =
  [
    ("-e", Arg.Set_string pattern_string, " <str> use the string as the pattern");
    ( "-f",
      Arg.Set_string pattern_file,
      " <file> use the file content as the pattern" );
    ( "-rules",
      Arg.String (fun s -> rule_source := Some (Rule_file s)),
      " <file> obtain formula of patterns from YAML/JSON/Jsonnet file" );
    ( "-lang",
      Arg.String (fun s -> lang := Some (Xlang.of_string s)),
      spf " <str> choose language (valid choices:\n     %s)"
        Xlang.supported_xlangs );
    ( "-l",
      Arg.String (fun s -> lang := Some (Xlang.of_string s)),
      spf " <str> shortcut for -lang" );
    ( "-targets",
      Arg.String (fun s -> target_source := Some (Target_file s)),
      " <file> obtain list of targets to run patterns on" );
    ( "-equivalences",
      Arg.Set_string equivalences_file,
      " <file> obtain list of code equivalences from YAML file" );
    ("-j", Arg.Set_int ncores, " <int> number of cores to use (default = 1)");
    ( "-use_parsing_cache",
      Arg.Set_string use_parsing_cache,
      " <dir> store and use the parsed generic ASTs in dir" );
    ( "-opt_cache",
      Arg.Set Flag.with_opt_cache,
      " enable caching optimization during matching" );
    ( "-no_opt_cache",
      Arg.Clear Flag.with_opt_cache,
      " disable caching optimization during matching" );
    ( "-opt_max_cache",
      Arg.Unit
        (fun () ->
          Flag.with_opt_cache := true;
          Flag.max_cache := true),
      " cache matches more aggressively; implies -opt_cache (experimental)" );
    ( "-max_target_bytes",
      Arg.Set_int Flag.max_target_bytes,
      " maximum size of a single target file, in bytes. This applies to \
       regular target filtering and might be overridden in some contexts. \
       Specify '0' to disable this filtering. Default: 5 MB" );
    ( "-no_gc_tuning",
      Arg.Clear Flag.gc_tuning,
      " use OCaml's default garbage collector settings" );
    ( "-emacs",
      Arg.Unit (fun () -> match_format := Matching_report.Emacs),
      " print matches on the same line than the match position" );
    ( "-oneline",
      Arg.Unit (fun () -> match_format := Matching_report.OneLine),
      " print matches on one line, in normalized form" );
    ( "-json",
      Arg.Unit (fun () -> output_format := Json true),
      " output JSON format" );
    ( "-json_nodots",
      Arg.Unit (fun () -> output_format := Json false),
      " output JSON format but without intermediate dots" );
    ( "-json_time",
      Arg.Unit
        (fun () ->
          output_format := Json true;
          report_time := true),
      " report detailed matching times as part of the JSON response. Implies \
       '-json'." );
    ( "-pvar",
      Arg.String (fun s -> mvars := Common.split "," s),
      " <metavars> print the metavariables, not the matched code" );
    ( "-error_recovery",
      Arg.Unit
        (fun () ->
          error_recovery := true;
          Flag_parsing.error_recovery := true),
      " do not stop at first parsing error with -e/-f" );
    ( "-fail_fast",
      Arg.Set Flag.fail_fast,
      " stop at first exception (and get a backtrace)" );
    ( "-filter_irrelevant_patterns",
      Arg.Set Flag.filter_irrelevant_patterns,
      " filter patterns not containing any strings in target file" );
    ( "-no_filter_irrelevant_patterns",
      Arg.Clear Flag.filter_irrelevant_patterns,
      " do not filter patterns" );
    ( "-filter_irrelevant_rules",
      Arg.Set filter_irrelevant_rules,
      " filter rules not containing any strings in target file" );
    ( "-no_filter_irrelevant_rules",
      Arg.Clear filter_irrelevant_rules,
      " do not filter rules" );
    ( "-fast",
      Arg.Set filter_irrelevant_rules,
      " filter rules not containing any strings in target file" );
    ( "-bloom_filter",
      Arg.Set Flag.use_bloom_filter,
      " use a bloom filter to only attempt matches when strings in the pattern \
       are in the target" );
    ( "-no_bloom_filter",
      Arg.Clear Flag.use_bloom_filter,
      " do not use bloom filter" );
    ( "-tree_sitter_only",
      Arg.Set Flag.tree_sitter_only,
      " only use tree-sitter-based parsers" );
    ( "-timeout",
      Arg.Set_float timeout,
      " <float> maxinum time to spend running a rule on a single file (in \
       seconds); 0 disables timeouts (default is 0)" );
    ( "-timeout_threshold",
      Arg.Set_int timeout_threshold,
      " <int> maximum number of rules that can timeout on a file before the \
       file is skipped; 0 disables it (default is 0)" );
    ( "-max_memory",
      Arg.Set_int max_memory_mb,
      "<int>  maximum memory available (in MiB); allows for clean termination \
       when running out of memory. This value should be less than the actual \
       memory available because the limit will be exceeded before it gets \
       detected. Try 5% less or 15000 if you have 16 GB." );
    ( "-max_match_per_file",
      Arg.Set_int max_match_per_file,
      " <int> maximum numbers of match per file" );
    ("-debug", Arg.Set debug, " output debugging information");
    ("--debug", Arg.Set debug, " output debugging information");
    ( "-debug_matching",
      Arg.Set Flag.debug_matching,
      " raise an exception at the first match failure" );
    ( "-matching_explanations",
      Arg.Set matching_explanations,
      " output intermediate matching explanations" );
    ( "-log_config_file",
      Arg.Set_string log_config_file,
      " <file> logging configuration file" );
    ( "-log_to_file",
      Arg.String (fun file -> log_to_file := Some file),
      " <file> log debugging info to file" );
    ("-test", Arg.Set test, " (internal) set test context");
    ("-lsp", Arg.Set lsp, " connect to LSP lang server to get type information");
    ("-raja", Arg.Set Flag_semgrep.raja, " undocumented");
  ]
  @ Flag_parsing_cpp.cmdline_flags_macrofile ()
  (* inlining of: Common2.cmdline_flags_devel () @ *)
  @ [
      ( "-debugger",
        Arg.Set Common.debugger,
        " option to set if launched inside ocamldebug" );
      ( "-profile",
        Arg.Unit
          (fun () ->
            Profiling.profile := Profiling.ProfAll;
            profile := true),
        " output profiling information" );
      ( "-keep_tmp_files",
        Arg.Set Common.save_tmp_files,
        " keep temporary generated files" );
    ]
  @ Meta_parse_info.cmdline_flags_precision ()
  @ Arg_helpers.options_of_actions action (actions ())
  @ [
      ( "-version",
        Arg.Unit
          (fun () ->
            pr2 version;
            Runner_exit.(exit_semgrep Success)),
        "  guess what" );
    ]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* 'sys_argv' is to be interpreted like 'Sys.argv' ordinarily would.
 * When semgrep is a stand-alone program, they are equal, but when it is
 * a shared library, 'Sys.argv' is empty. *)
let main (sys_argv : string array) : unit =
  profile_start := Unix.gettimeofday ();

  (* SIGXFSZ (file size limit exceeded)
   * ----------------------------------
   * By default this signal will kill the process, which is not good. If we
   * would raise an exception from within the handler, the exception could
   * appear anywhere, which is not good either if you want to recover from it
   * gracefully. So, we ignore it, and that causes the syscalls to fail and
   * we get a `Sys_error` or some other exception. Apparently this is standard
   * behavior under both Linux and MacOS:
   *
   * > The SIGXFSZ signal is sent to the process. If the process is holding or
   * > ignoring SIGXFSZ, continued attempts to increase the size of a file
   * > beyond the limit will fail with errno set to EFBIG.
   *)
  Sys.set_signal Sys.sigxfsz Sys.Signal_ignore;

  let usage_msg =
    spf
      "Usage: %s [options] -lang <str> [-e|-f|-rules] <pattern> \
       (<files_or_dirs> | -targets <file>) \n\
       Options:"
      (Filename.basename sys_argv.(0))
  in

  (* --------------------------------------------------------- *)
  (* Setting up debugging/profiling *)
  (* --------------------------------------------------------- *)
  let argv =
    Array.to_list sys_argv
    @ (if Sys.getenv_opt env_debug <> None then [ "-debug" ] else [])
    @ (if Sys.getenv_opt env_profile <> None then [ "-profile" ] else [])
    @
    match Sys.getenv_opt env_extra with
    | Some s -> Common.split "[ \t]+" s
    | None -> []
  in

  (* does side effect on many global flags *)
  let args =
    Arg_helpers.parse_options (options all_actions) usage_msg
      (Array.of_list argv)
  in

  let config = mk_config () in

  if config.debug then Report.mode := MDebug
  else if config.report_time then Report.mode := MTime
  else Report.mode := MNo_info;

  Logging_helpers.setup ~debug:config.debug
    ~log_config_file:config.log_config_file ~log_to_file:config.log_to_file;

  logger#info "Executed as: %s" (argv |> String.concat " ");
  logger#info "Version: %s" version;
  let config =
    if config.profile then (
      logger#info "Profile mode On";
      logger#info "disabling -j when in profiling mode";
      { config with ncores = 1 })
    else config
  in

  if config.lsp then LSP_client.init ();

  (* must be done after Arg.parse, because Common.profile is set by it *)
  Profiling.profile_code "Main total" (fun () ->
      match args with
      (* --------------------------------------------------------- *)
      (* actions, useful to debug subpart *)
      (* --------------------------------------------------------- *)
      | xs
        when List.mem config.action (Arg_helpers.action_list (all_actions ()))
        ->
          Arg_helpers.do_action config.action xs (all_actions ())
      | _ when not (Common.null_string config.action) ->
          failwith ("unrecognized action or wrong params: " ^ !action)
      (* --------------------------------------------------------- *)
      (* main entry *)
      (* --------------------------------------------------------- *)
      | roots ->
          (* TODO: We used to tune the garbage collector but from profiling
             we found that the effect was small. Meanwhile, the memory
             consumption causes some machines to freeze. We may want to
             tune these parameters in the future/do more testing, but
             for now just turn it off *)
          (* if !Flag.gc_tuning && config.max_memory_mb = 0 then set_gc (); *)
          let config = { config with roots } in
          Run_semgrep.semgrep_dispatch config)

(*****************************************************************************)

(*
   Register global exception printers defined by the various libraries
   and modules.

   The main advantage of doing this here is the ability to override
   undesirable printers defined by some libraries. The order of registration
   is the order in which modules are initialized, which isn't something
   that in general we know or want to rely on.
   For example, JaneStreet Core prints (or used to print) some stdlib
   exceptions as S-expressions without giving us a choice. Overriding those
   can be tricky.
*)
let register_exception_printers () =
  Parse_info.register_exception_printer ();
  SPcre.register_exception_printer ();
  Rule.register_exception_printer ()

(* Entry point from either the executable or the shared library. *)
let main (argv : string array) : unit =
  Common.main_boilerplate (fun () ->
      register_exception_printers ();
      Common.finalize
        (fun () -> main argv)
        (fun () -> !Hooks.exit |> List.iter (fun f -> f ())))
