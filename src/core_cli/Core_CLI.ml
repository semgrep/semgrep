(*
 * The author disclaims copyright to this source code.  In place of
 * a legal notice, here is a blessing:
 *
 *    May you do good and not evil.
 *    May you find forgiveness for yourself and forgive others.
 *    May you share freely, never taking more than you give.
 *)
open Common
open Fpath_.Operators
module Flag = Flag_semgrep
module E = Core_error
module J = JSON
module Out = Semgrep_output_v1_j

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This module contains the main command-line parsing logic of semgrep-core.
 *
 * It is packaged as a library so it can be used both for the stand-alone
 * semgrep-core binary as well as the semgrep-core-proprietary one.
 * history: the code here used to be in Main.ml.
 *
 * DEPRECATED: semgrep-core used to recognize lots of options (e.g., -e/-f) and
 * is still used extensively by PA for many things. It was doing its own file
 * targeting, its own text output, but all of this should be gradually removed.
 * Ideally semgrep-core should support just the options that are required
 * by pysemgrep in core_runner.py and nothing else. You should use
 * osemgrep if you want some of the old benefits of semgrep-core (e.g., -e).
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* debugging/profiling/logging flags *)
(* ------------------------------------------------------------------------- *)

(* This is useful when you don't call directly semgrep-core but instead use
 * pysemgrep but still want to tweak the call to semgrep-core.
 *)
let env_extra = "SEMGREP_CORE_EXTRA"
let log_to_file = ref None

(* related:
 * - Flag_semgrep.debug_matching
 * - Flag_semgrep.fail_fast
 * - Trace_matching.on
 *
 * see also verbose/... flags in Flag_semgrep.ml
 *)
let debug = ref false
let trace = ref false
let trace_endpoint = ref None

(* ------------------------------------------------------------------------- *)
(* main flags *)
(* ------------------------------------------------------------------------- *)

(* -rules *)
let rule_source : Core_scan_config.rule_source option ref = ref None

let add_rule_file file =
  match !rule_source with
  | None -> rule_source := Some (Core_scan_config.Rule_files [ file ])
  | Some (Core_scan_config.Rule_files files) ->
      rule_source := Some (Core_scan_config.Rule_files (files @ [ file ]))
  | Some (Core_scan_config.Rules _) ->
      failwith "internal error: cannot combine preparsed rules with rule files"

(* -targets (takes the list of files in a file given by pysemgrep) *)
let target_file : Fpath.t option ref = ref None

(* This used to be used for `semgrep-core -l <lang> -e <pattern> <single file>`
 * instead of `semgrep-core -targets`. But we now use osemgrep directly for
 * such use cases. Now, this is mostly useful for semgrep-core "actions" as
 * in `semgrep-core -l <lang> -dump_ast <file` (even if in this case
 * you should use 'osemgrep show dump-ast file' where we even infer the language
 * from the filename)
 * old: this used to be an Analyzer.t option, but "regex" and "generic" are
 * not useful for the new uses of -lang, and in any case if it was we should
 * rename the flag -analyzer instead of -lang.
 *)
let lang : Lang.t option ref = ref None

(* this is used not only by pysemgrep but also by a few actions *)
let output_format = ref Core_scan_config.default.output_format
let strict = ref Core_scan_config.default.strict
let respect_rule_paths = ref Core_scan_config.default.respect_rule_paths

(* step-by-step matching debugger *)
let matching_explanations = ref Core_scan_config.default.matching_explanations

(* report matching times per file *)
let report_time = ref Core_scan_config.default.report_time

(* ------------------------------------------------------------------------- *)
(* limits *)
(* ------------------------------------------------------------------------- *)

(* timeout in seconds; 0 or less means no timeout *)
let timeout = ref Core_scan_config.default.timeout
let timeout_threshold = ref Core_scan_config.default.timeout_threshold
let max_memory_mb = ref Core_scan_config.default.max_memory_mb (* in MiB *)

(* arbitrary limit *)
let max_match_per_file = ref Core_scan_config.default.max_match_per_file

(* -j *)
let num_jobs = ref Core_scan_config.default.num_jobs
let use_parmap = ref false

(* ------------------------------------------------------------------------- *)
(* optional optimizations *)
(* ------------------------------------------------------------------------- *)
(* similar to filter_irrelevant_patterns, but use the whole rule to extract
 * the regexp *)
let filter_irrelevant_rules =
  ref Core_scan_config.default.filter_irrelevant_rules

(* ------------------------------------------------------------------------- *)
(* scan-adjacent information *)
(* ------------------------------------------------------------------------- *)

let symbol_analysis = ref Core_scan_config.default.symbol_analysis

(* ------------------------------------------------------------------------- *)
(* pad's action flag *)
(* ------------------------------------------------------------------------- *)

(* action mode *)
let action = ref ""
let is_rpc_call () = !action = "-rpc"
let is_action () = !action <> ""

(*****************************************************************************)
(* Dumpers (see also Core_actions.ml) *)
(*****************************************************************************)

let dump_v_to_format (v : OCaml.v) =
  match !output_format with
  | NoOutput -> "<NoOutput>"
  | Text -> OCaml.string_of_v v
  | Json _ -> J.string_of_json (Core_actions.json_of_v v)

let log_parsing_errors file (res : Parsing_result2.t) =
  Logs.warn (fun m -> m "fail to fully parse %s" !!file);
  Logs.debug (fun m ->
      m "errs = %s" (Parsing_result2.format_errors ~style:Auto res.errors));
  Logs.debug (fun m ->
      m "skipped = %s"
        (List.map (fun e -> "  " ^ Dumper.dump e) res.skipped_tokens
        |> String.concat "\n"))

(* works with -lang *)
let dump_pattern (file : Fpath.t) =
  let s = UFile.read_file file in
  (* mostly copy-paste of parse_pattern in runner, but with better error report *)
  let lang = Lang.of_opt_exn !lang in
  Core_actions.try_with_log_exn_and_reraise file (fun () ->
      (* TODO? enable "semgrep.parsing" log level *)
      match Parse_pattern.parse_pattern lang s with
      | Ok any ->
          let v = Meta_AST.vof_any any in
          let s = dump_v_to_format v in
          UCommon.pr s
      | Error e -> Logs.app (fun m -> m "Parse error: %s" e))
[@@action]

(* TODO: remove, deprecated by osemgrep show dump-rule *)
let dump_patterns_of_rule (file : Fpath.t) =
  match Parse_rule.parse file with
  | Ok rules ->
      let xpats = List.concat_map Visit_rule.xpatterns_of_rule rules in
      List.iter
        (fun { Xpattern.pat; _ } ->
          match pat with
          | Sem (pat, _) ->
              let v = Meta_AST.vof_any pat in
              let s = dump_v_to_format v in
              UCommon.pr s
          | _ -> UCommon.pr (Xpattern.show_xpattern_kind pat))
        xpats
      (* TODO: handle better *)
  | Error e -> failwith (Rule_error.string_of_error e)
[@@action]

(* TODO: remove, deprecated by osemgrep show dump-ast *)
let dump_ast ?(naming = false) (lang : Lang.t) (file : Fpath.t) =
  Core_actions.try_with_log_exn_and_reraise file (fun () ->
      let res =
        if naming then Parse_target.parse_and_resolve_name lang file
        else Parse_target.just_parse_with_lang lang file
      in
      let v = Meta_AST.vof_any (AST_generic.Pr res.ast) in
      (* 80 columns is too little *)
      Format.set_margin 120;
      let s = dump_v_to_format v in
      UConsole.print s;
      if Parsing_result2.has_error res then (
        log_parsing_errors file res;
        Core_exit_code.(exit_semgrep False)))
[@@action]

(*****************************************************************************)
(* Output *)
(*****************************************************************************)

(* also used in semgrep-pro *)
let output_core_results (result_or_exn : Core_result.result_or_exn)
    (config : Core_scan_config.t) : unit =
  match config.output_format with
  (* note that the dots have been displayed before in Core_scan.scan ()
   * for pysemgrep. Here we print the matches (and errors).
   *)
  | Json _ -> (
      let res =
        match result_or_exn with
        | Ok r -> r
        | Error exn ->
            let err = E.exn_to_error exn in
            Core_result.mk_result_with_just_errors [ err ]
      in
      let res =
        Logs_.with_debug_trace ~__FUNCTION__ (fun () ->
            Core_json_output.core_output_of_matches_and_errors res)
      in
      (*
        Not pretty-printing the json output (Yojson.Safe.prettify)
        because it kills performance, adding an extra 50% time on our
        old calculate_ci_perf.py benchmark.
        User should use an external tool like jq or ydump (latter comes with
        yojson) for pretty-printing json.
      *)
      let s = Out.string_of_core_output res in
      Logs.debug (fun m ->
          m "size of returned JSON string: %d" (String.length s));
      UConsole.print s;
      match result_or_exn with
      | Error exn -> Core_exit_code.exit_semgrep (Unknown_exception exn)
      | Ok _ -> ())
  (* The matches have already been printed before in Core_scan.scan(). We just
   * print the errors here (and matching explanations).
   * LATER: you should now use osemgrep for this
   *)
  | Text -> (
      match result_or_exn with
      | Ok res ->
          let matches =
            res.processed_matches
            |> List.filter_map (fun processed_match ->
                match Core_json_output.match_to_match processed_match with
                | Error (e : Core_error.t) ->
                    UConsole.eprint (Core_error.string_of_error e);
                    None
                | Ok (match_ : Out.core_match) -> Some match_)
          in
          let matches = Core_json_output.dedup_and_sort matches in
          matches |> List.iter Core_text_output.print_match;
          if config.matching_explanations then
            res.explanations
            |> Option.iter (List.iter Matching_explanation.print);
          if not (List_.null res.errors) then (
            Logs.warn (fun m ->
                m "some files were skipped or only partially analyzed");
            res.errors
            |> List.iter (fun err ->
                Logs.warn (fun m -> m "%s" (E.string_of_error err))))
      | Error exn -> Exception.reraise exn)
  | NoOutput -> ()

(*****************************************************************************)
(* Config *)
(*****************************************************************************)

(* Coupling: these need to be kept in sync with tracing.py *)
let default_trace_endpoint = Uri.of_string "https://telemetry.semgrep.dev"
let default_dev_endpoint = Uri.of_string "https://telemetry.dev2.semgrep.dev"
let default_local_endpoint = Uri.of_string "http://localhost:4318"

let mk_config ?rules () : Core_scan_config.t =
  {
    rule_source =
      (match !rule_source with
      | None -> (
          match rules with
          | None ->
              (* Actions don't need rules *)
              if is_action () then Rules [] else failwith "missing -rules"
          | Some rules -> Rules rules)
      | Some x -> x);
    target_source =
      (match !target_file with
      | None -> Targets [] (* will be adjusted later in main_exn() *)
      | Some file -> Target_file file);
    output_format = !output_format;
    strict = !strict;
    report_time = !report_time;
    matching_explanations = !matching_explanations;
    respect_rule_paths = !respect_rule_paths;
    file_match_hook = None;
    (* limits and perf *)
    timeout = !timeout;
    timeout_threshold = !timeout_threshold;
    max_memory_mb = !max_memory_mb;
    max_match_per_file = !max_match_per_file;
    num_jobs = !num_jobs;
    filter_irrelevant_rules = !filter_irrelevant_rules;
    (* open telemetry *)
    telemetry =
      (let env = Sys.getenv_opt "SEMGREP_DEPLOYMENT_ENV" in
       match (!trace, !trace_endpoint) with
       | true, Some url ->
           let endpoint =
             match url with
             (* coupling: cli/src/semgrep/telemetry.py _OTEL_ENDPOINT_ALIASES *)
             | "semgrep-prod" -> default_trace_endpoint
             | "semgrep-dev" -> default_dev_endpoint
             | "semgrep-local" -> default_local_endpoint
             | _ -> Uri.of_string url
           in
           Some { endpoint; top_level_scope = None; env }
       | true, None ->
           Some
             { endpoint = default_trace_endpoint; top_level_scope = None; env }
       | false, Some _ ->
           Logs.warn (fun m ->
               m
                 "Tracing is disabled because -trace_endpoint is specified \
                  without -trace.");
           None
       | false, None -> None);
    (* Only settable through a response from the App *)
    fips_mode = false;
    (* only settable via the Pro binary *)
    symbol_analysis = !symbol_analysis;
    use_parmap = !use_parmap;
    par_conf = Parallelism_config.default;
  }

(*****************************************************************************)
(* The actions *)
(*****************************************************************************)

let all_actions ?(par_conf = Parallelism_config.default) () =
  let num_jobs = Core_scan_config.finalize_num_jobs !num_jobs in
  [
    (* this is run by pysemgrep --validate *)
    ( "-check_rules",
      " <metachecks file> <files or dirs>",
      Arg_.mk_action_n_conv Fpath.v (Check_rule.check_files !output_format) );
    (* this is run by scripts (stats/.../run-lang) used by some of our workflows
     * (e.g., cron-parsing-stats.jsonnet)
     *)
    ( "-parsing_stats",
      " <files or dirs> generate parsing statistics (use -json for JSON output)",
      Arg_.mk_action_n_arg (fun xs ->
          Test_parsing.parsing_stats (Lang.of_opt_exn !lang)
            ~json:
              (match !output_format with
              | Json _ -> true
              | Text
              | NoOutput ->
                  false)
            ~verbose:true xs) );
    (* The rest should be used just interactively by PA developers *)
    ( "-prefilter_of_rules",
      " <file> dump the prefilter regexps of rules in JSON ",
      Arg_.mk_action_1_conv Fpath.v
        (Core_actions.prefilter_of_rules ~interfile:false) );
    ( "-prefilter_of_rules_interfile",
      " <file> dump the prefilter regexps of rules in JSON ",
      Arg_.mk_action_1_conv Fpath.v
        (Core_actions.prefilter_of_rules ~interfile:true) );
    (* the dumpers *)
    ( "-dump_extensions",
      " print file extension to language mapping",
      Arg_.mk_action_0_arg Core_actions.dump_exts_of_lang );
    ("-dump_pattern", " <file>", Arg_.mk_action_1_conv Fpath.v dump_pattern);
    ( "-dump_patterns_of_rule",
      " <file>",
      Arg_.mk_action_1_conv Fpath.v dump_patterns_of_rule );
    ( "-dump_ast",
      " <file>",
      fun file ->
        Arg_.mk_action_1_conv Fpath.v
          (dump_ast ~naming:false (Lang.of_opt_exn !lang))
          file );
    ( "-dump_lang_ast",
      " <file>",
      fun file ->
        Arg_.mk_action_1_conv Fpath.v
          (Test_parsing.dump_lang_ast (Lang.of_opt_exn !lang))
          file );
    ( "-dump_named_ast",
      " <file>",
      fun file ->
        Arg_.mk_action_1_conv Fpath.v
          (dump_ast ~naming:true (Lang.of_opt_exn !lang))
          file );
    ("-dump_il", " <file>", Arg_.mk_action_1_conv Fpath.v Core_actions.dump_il);
    ( "-pp_il",
      " <file> pretty-print IL in familiar-ish syntax",
      Arg_.mk_action_1_conv Fpath.v Core_actions.pp_il );
    ( "-dump_rule",
      " <file>",
      Arg_.mk_action_1_conv Fpath.v Core_actions.dump_rule );
    ( "-dump_tree_sitter_cst",
      " <file> dump the CST obtained from a tree-sitter parser",
      Arg_.mk_action_1_conv Fpath.v (fun file ->
          Test_parsing.dump_tree_sitter_cst (Lang.of_opt_exn !lang) file) );
    ( "-dump_tree_sitter_pattern_cst",
      " <file>",
      Arg_.mk_action_1_conv Fpath.v (fun file ->
          Parse_pattern.dump_tree_sitter_pattern_cst (Lang.of_opt_exn !lang)
            file) );
    ( "-dump_pfff_ast",
      " <file> dump the generic AST obtained from a pfff parser",
      Arg_.mk_action_1_conv Fpath.v (fun file ->
          Test_parsing.dump_pfff_ast (Lang.of_opt_exn !lang) file) );
    ( "-diff_pfff_tree_sitter",
      " <file>",
      Arg_.mk_action_n_arg (fun xs ->
          Test_parsing.diff_pfff_tree_sitter (Fpath_.of_strings xs)) );
    (* Misc stuff *)
    ( "-test_parse_tree_sitter",
      " <dir> test tree-sitter parser on target files",
      Arg_.mk_action_1_arg (fun root ->
          Test_parsing.test_parse_tree_sitter (Lang.of_opt_exn !lang)
            (Fpath.v root)) );
    ( "-translate_rules",
      " <files or dirs>",
      Arg_.mk_action_n_conv Fpath.v
        (Translate_rule.translate_files Parse_rule.parse) );
    ( "-stat_rules",
      " <files or dirs>",
      Arg_.mk_action_n_conv Fpath.v Check_rule.stat_files );
    ( "-parse_rules",
      " <dir>",
      Arg_.mk_action_1_conv Fpath.v Test_parsing.test_parse_rules );
    ("-test_eval", " <JSON file>", Arg_.mk_action_1_arg Eval_generic.test_eval);
    ( "-sarif_sort",
      " <JSON file>",
      Arg_.mk_action_1_conv Fpath.v Core_actions.sarif_sort );
    ( "-rpc",
      " don't use this unless you already know",
      Arg_.mk_action_0_arg (fun () -> RPC.(main { par_conf; num_jobs })) );
  ]
  @ Test_analyze_generic.actions ~parse_program:Parse_target.parse_program
  @ Test_dataflow_tainting.actions ()
  @ Test_naming_generic.actions ~parse_program:Parse_target.parse_program

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

(* Rolls back the mutable state associated with core CLI parsing to their
 * default values.  This is only needed in a testing context, where we find
 * ourselves re-entering [main_exn] in a single process and want to ensure
 * unset arguments fall back to their original values.  Currently, the test
 * suite still passes without this, but depending on what changes take place
 * in the future this might not always be the case, so hence being proactive
 * about this right now. *)
let reset_options () =
  rule_source := None;
  target_file := None;
  lang := None;
  num_jobs := Core_scan_config.default.num_jobs;
  output_format := Core_scan_config.default.output_format;
  strict := Core_scan_config.default.strict;
  report_time := Core_scan_config.default.report_time;
  filter_irrelevant_rules := Core_scan_config.default.filter_irrelevant_rules;
  respect_rule_paths := Core_scan_config.default.respect_rule_paths;
  timeout := Core_scan_config.default.timeout;
  timeout_threshold := Core_scan_config.default.timeout_threshold;
  max_memory_mb := Core_scan_config.default.max_memory_mb;
  max_match_per_file := Core_scan_config.default.max_match_per_file;
  use_parmap := false;
  filter_irrelevant_rules := Core_scan_config.default.filter_irrelevant_rules;
  symbol_analysis := Core_scan_config.default.symbol_analysis;
  action := ""

let options (actions : unit -> Arg_.cmdline_actions) =
  [
    ( "-rules",
      Arg.String (fun s -> add_rule_file (Fpath.v s)),
      " <file> obtain formula of patterns from YAML/JSON/Jsonnet file" );
    ( "-targets",
      Arg.String (fun s -> target_file := Some (Fpath.v s)),
      " <file> obtain list of targets to run patterns on" );
    ( "-lang",
      Arg.String (fun s -> lang := Some (Lang.of_string s)),
      spf " <str> choose language (valid choices:\n     %s)"
        Analyzer.supported_analyzers );
    ( "-l",
      Arg.String (fun s -> lang := Some (Lang.of_string s)),
      spf " <str> shortcut for -lang" );
    ( "-j",
      Arg.Int (fun n -> num_jobs := Core_scan_config.Force n),
      " <int> number of cores to use (default: automatic)" );
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
    ( "-fail_fast",
      Hook.Arg.set Flag.fail_fast,
      " stop at first exception (and get a backtrace)" );
    ( "-filter_irrelevant_patterns",
      Hook.Arg.set Flag.filter_irrelevant_patterns,
      " filter patterns not containing any strings in target file" );
    ( "-no_filter_irrelevant_patterns",
      Hook.Arg.clear Flag.filter_irrelevant_patterns,
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
    ( "-disable_rule_paths",
      Arg.Clear respect_rule_paths,
      " do not honor the paths: directive of the rule" );
    ( "-tree_sitter_only",
      Hook.Arg.set Flag.tree_sitter_only,
      " only use tree-sitter-based parsers" );
    ("-pfff_only", Hook.Arg.set Flag.pfff_only, " only use pfff-based parsers");
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
    ( "-max_tainted_vars",
      Arg.Set_int Flag_semgrep.max_tainted_vars,
      "<int> maximum number of vars to store. This is mostly for internal use \
       to make performance testing easier" );
    ( "-max_taint_set_size",
      Arg.Set_int Flag_semgrep.max_taint_set_size,
      "<int> maximum size of a taint set. This is mostly for internal use to \
       make performance testing easier" );
    ( "-max_match_per_file",
      Arg.Set_int max_match_per_file,
      " <int> maximum numbers of match per file" );
    ("-debug", Arg.Set debug, " output debugging information");
    ("-strict", Arg.Set strict, " fail on warnings");
    ( "-debug_matching",
      Hook.Arg.set Flag.debug_matching,
      " raise an exception at the first match failure" );
    ( "-matching_explanations",
      Arg.Set matching_explanations,
      " output intermediate matching explanations" );
    ( "-log_to_file",
      Arg.String (fun file -> log_to_file := Some (Fpath.v file)),
      " <file> log debugging info to file" );
    ("-trace", Arg.Set trace, " output tracing information");
    ( "-trace_endpoint",
      Arg.String (fun url -> trace_endpoint := Some url),
      " url endpoint for collecting tracing information" );
  ]
  @ Flag_parsing_cpp.cmdline_flags_macrofile ()
  @ Profiling.flags ()
  (* inlining of: Common2.cmdline_flags_devel () @ *)
  @ [
      ( "-debugger",
        Arg.Set Common.debugger,
        " option to set if launched inside ocamldebug" );
      ( "-keep_tmp_files",
        (* nosemgrep: forbid-tmp *)
        Arg.Set UTmp.save_temp_files,
        " keep temporary generated files" );
    ]
  @ Meta_AST.cmdline_flags_precision () (* -full_token_info *)
  @ Arg_.options_of_actions action (actions ())
  @ [
      ( "-version",
        Arg.Unit
          (fun () ->
            let version = spf "semgrep-core version: %s" Version.version in
            UConsole.print version;
            Core_exit_code.(exit_semgrep Success)),
        "  guess what" );
      ( "-ocaml_version",
        Arg.Unit
          (fun () ->
            let version = spf "OCaml version: %s" Sys.ocaml_version in
            UConsole.print version;
            Core_exit_code.(exit_semgrep Success)),
        "  The version of OCaml that was used to build this binary" );
    ]
  @ [
      ( "-use_parmap",
        Arg.Set use_parmap,
        "  Rely on a legacy Parmap implementation of `-j`" );
    ]

(*****************************************************************************)
(* Exception printers *)
(*****************************************************************************)

(*
   Slightly nicer exception printers than the default.
*)
let register_stdlib_exn_printers () =
  Printexc.register_printer (function
    | Failure msg ->
        (* Avoid unnecessary quoting of the error message *)
        Some ("Failure: " ^ msg)
    | Invalid_argument msg -> Some ("Invalid_argument: " ^ msg)
    | _ -> None)

let register_unix_exn_printers () =
  Printexc.register_printer (function
    | Unix.Unix_error (e, fm, argm) ->
        Some (spf "Unix_error: %s %s %s" (Unix.error_message e) fm argm)
    | _ -> None)

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
  register_stdlib_exn_printers ();
  register_unix_exn_printers ();
  Pcre2_.register_exception_printer ();
  Pcre_.register_exception_printer ()
[@@alert "-deprecated"]

(*****************************************************************************)
(* Run a scan *)
(*****************************************************************************)

let run (config : Core_scan_config.t) : unit =
  let res = Core_scan.scan config in
  output_core_results res config

(* We want to only run the Parmap runtime iff --x-parmap is set.
 * coupling: Pro_CLI.ml
 *)
let maybe_with_eio ?rules (f : Core_scan_config.t -> 'a) : 'a =
  let config = mk_config ?rules () in
  Core_profiling.profiling := config.report_time;
  let num_jobs : Core_scan_config.num_jobs =
    if !Profiling.profile =*= Profiling.ProfAll then (
      Logs.info (fun m -> m "Profile mode On");
      Logs.info (fun m -> m "disabling -j when in profiling mode");
      Default 1)
    else config.num_jobs
  in
  let config = { config with num_jobs } in
  if !use_parmap then f config
  else
    Eio_main.run (fun env ->
        Logs_threaded.enable ();
        f { config with par_conf = Parallelism_config.create env })

let maybe_with_tracing function_name engine analysis_flags
    (config : Core_scan_config.t) (f : Core_scan_config.t -> 'a) : 'a =
  match config.telemetry with
  | None -> f config
  | Some tracing ->
      let eio = not !use_parmap in
      let resource_attrs =
        (* Let's make sure all traces/logs/metrics etc. are tagged as
                   coming from the pro invocation *)
        Trace_data.get_resource_attrs ?env:tracing.env ~engine ~analysis_flags
          ~jobs:(Core_scan_config.finalize_num_jobs config.num_jobs)
          ~eio ()
      in
      let configure_otel ?eio_sw_base () =
        Telemetry.configure_otel ?eio_sw_base ~attrs:resource_attrs
          "semgrep-core" tracing.endpoint
      in
      let run () =
        if is_rpc_call () then
          (* RPC mode: configure OTel but don't create a session-level span.
             Per-request spans are created by handle_request. *)
          Common.protect ~finally:Telemetry.stop_otel (fun () -> f config)
        else
          Tracing.with_tracing function_name [] (fun scope ->
              let telemetry = { tracing with top_level_scope = Some scope } in
              f { config with telemetry = Some telemetry })
      in
      if not eio then (
        configure_otel ();
        run ())
      else
        Eio.Switch.run (fun sw ->
            let env =
              match Parallelism_config.unsafe_get_base config.par_conf with
              | Error (NotInEio _) ->
                  (* Since we are running within an Eio loop, this code path can never be
                 hit; we would have previously already errored out by now. *)
                  raise Impossible
              | Ok env -> env
            in
            configure_otel ~eio_sw_base:(sw, env) ();
            run ())
(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main_exn (argv : string array) : unit =
  Logs_.with_basic_setup ~level:None @@ fun () ->
  reset_options ();
  (* coupling: lots of similarities with what we do in CLI.main *)
  register_exception_printers ();

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
  if Sys.unix then Sys.set_signal Sys.sigxfsz Sys.Signal_ignore;

  let usage_msg =
    spf "Usage: %s [options] -rules <file> -targets <file>\nOptions:"
      (Filename.basename argv.(0))
  in

  (* --------------------------------------------------------- *)
  (* Setting up debugging/profiling *)
  (* --------------------------------------------------------- *)
  let argv =
    Array.to_list argv
    @
    match Sys.getenv_opt env_extra with
    | Some s -> String_.split ~sep:"[ \t]+" s
    | None -> []
  in

  (* does side effect on many global flags *)
  let args =
    Arg_.parse_options (options all_actions) usage_msg (Array.of_list argv)
  in
  (* Duplicated in Pro_core_CLI.ml *)
  let level : Logs.level option = if !debug then Some Debug else Some Warning in

  (* coupling: lots of similarities with what we do in Scan_subcommand.ml *)
  Log_semgrep.with_setup ~log_to_otel:!trace ?log_to_file:!log_to_file
    ?require_one_of_these_tags:None ~quiet_log_setup:(is_rpc_call ()) ~color:On
    ~level
  @@ fun () ->
  Logs.info (fun m -> m "Executed as: %s" (argv |> String.concat " "));
  Logs.info (fun m -> m "Version: %s" Version.version);

  (* coupling: if you add an init() call here, you probably need to modify
     also tests/Test.ml, CLI.ml, and Pro_core_CLI.ml
  *)
  Proxy.configure_proxy (Proxy.settings_from_env ());
  Http_helpers.set_client_ref (module Cohttp_lwt_unix.Client);

  (* must be done after Arg.parse, because Common.profile is set by it *)
  Profiling.measure "Main total" (fun () ->
      maybe_with_eio (fun config ->
          let analysis_flags = Trace_data.no_analysis_features () in
          (* Enclose action execution to enable tracing for actions. *)
          maybe_with_tracing "Core_CLI.main_exn" "oss" analysis_flags config
            (fun config ->
              match args with
              (* --------------------------------------------------------- *)
              (* actions, useful to debug subpart *)
              (* --------------------------------------------------------- *)
              | xs
                when List.mem !action
                       (Arg_.action_list
                          (all_actions ~par_conf:config.par_conf ())) ->
                  Arg_.do_action !action xs
                    (all_actions ~par_conf:config.par_conf ())
              | _ when not (String_.empty !action) ->
                  failwith ("unrecognized action or wrong params: " ^ !action)
              (* --------------------------------------------------------- *)
              (* main entry *)
              (* --------------------------------------------------------- *)
              | roots ->
                  let roots = Fpath_.of_strings roots in
                  let target_source : Core_scan_config.target_source =
                    match (!target_file, !lang, roots) with
                    | Some file, None, [] -> Target_file file
                    | None, Some lang, [ file ]
                      when UFile.is_reg ~follow_symlinks:true file ->
                        Targets [ Target.mk_unfilterable_lang_target lang file ]
                    | _ ->
                        (* alt: use the file targeting in Find_targets_lang but better
                 to "dumb-down" semgrep-core to its minimum.
              *)
                        failwith
                          "this combination of targets and flags is not \
                           supported; semgrep-core supports either the use of \
                           -targets, or -lang and a single target file; if you \
                           need more complex file targeting use semgrep"
                  in
                  let config = { config with target_source } in
                  run config)))

let main (argv : string array) : unit =
  UCommon.main_boilerplate (fun () -> main_exn argv)
