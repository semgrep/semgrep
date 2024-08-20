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
open Core_scan_config
module Flag = Flag_semgrep
module E = Core_error
module J = JSON

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This module contains the main command-line parsing logic of semgrep-core.
 *
 * It is packaged as a library so it can be used both for the stand-alone
 * semgrep-core binary as well as the semgrep-core-proprietary one.
 * history: the code here used to be in Main.ml.
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
let log_to_file = ref None
let strict = ref Core_scan_config.default.strict

(* see also verbose/... flags in Flag_semgrep.ml *)
(* to test things *)
let debug = ref Core_scan_config.default.debug

(* related:
 * - Flag_semgrep.debug_matching
 * - Flag_semgrep.fail_fast
 * - Trace_matching.on
 *)

(* try to continue processing files, even if one has a parse error with -e/f *)
let error_recovery = ref Core_scan_config.default.error_recovery
let profile = ref Core_scan_config.default.profile
let trace = ref Core_scan_config.default.trace
let trace_endpoint = ref Core_scan_config.default.trace_endpoint

(* report matching times per file *)
let report_time = ref Core_scan_config.default.report_time

(* step-by-step matching debugger *)
let matching_explanations = ref Core_scan_config.default.matching_explanations

(* ------------------------------------------------------------------------- *)
(* main flags *)
(* ------------------------------------------------------------------------- *)

(* -rules *)
let rule_source = ref None
let equivalences_file = ref None

(* TODO: infer from basename argv(0) ? *)
let lang = ref None
let output_format = ref Core_scan_config.default.output_format
let match_format = ref Core_scan_config.default.match_format
let mvars = ref ([] : Metavariable.mvar list)
let respect_rule_paths = ref Core_scan_config.default.respect_rule_paths

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
let ncores = ref Core_scan_config.default.ncores

(* ------------------------------------------------------------------------- *)
(* optional optimizations *)
(* ------------------------------------------------------------------------- *)
(* similar to filter_irrelevant_patterns, but use the whole rule to extract
 * the regexp *)
let filter_irrelevant_rules =
  ref Core_scan_config.default.filter_irrelevant_rules

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
let _set_gc_TODO () =
  Logs.debug (fun m -> m "Gc tuning");
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
(* Dumpers (see also Core_actions.ml) *)
(*****************************************************************************)

let dump_v_to_format (v : OCaml.v) =
  match !output_format with
  | Text -> OCaml.string_of_v v
  | Json _ -> J.string_of_json (Core_actions.json_of_v v)

let log_parsing_errors file (res : Parsing_result2.t) =
  Logs.warn (fun m -> m "fail to fully parse %s" !!file);
  Logs.debug (fun m ->
      m "errs = %s" (Parsing_result2.format_errors ~style:Auto res.errors));
  Logs.debug (fun m ->
      m "skipped = %s"
        (List_.map (fun e -> "  " ^ Dumper.dump e) res.skipped_tokens
        |> String.concat "\n"))

(* works with -lang *)
let dump_pattern (file : Fpath.t) =
  let s = UFile.read_file file in
  (* mostly copy-paste of parse_pattern in runner, but with better error report *)
  let lang = Xlang.lang_of_opt_xlang_exn !lang in
  E.try_with_log_exn_and_reraise file (fun () ->
      (* TODO? enable "semgrep.parsing" log level *)
      match Parse_pattern.parse_pattern lang s with
      | Ok any ->
          let v = Meta_AST.vof_any any in
          let s = dump_v_to_format v in
          UCommon.pr s
      | Error e -> Logs.app (fun m -> m "Parse error: %s" e))
[@@action]

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

let dump_ast ?(naming = false) (caps : < Cap.stdout ; Cap.exit >)
    (lang : Language.t) (file : Fpath.t) =
  E.try_with_log_exn_and_reraise file (fun () ->
      let res =
        if naming then Parse_target.parse_and_resolve_name lang file
        else Parse_target.just_parse_with_lang lang file
      in
      let v = Meta_AST.vof_any (AST_generic.Pr res.ast) in
      (* 80 columns is too little *)
      Format.set_margin 120;
      let s = dump_v_to_format v in
      CapConsole.print caps#stdout s;
      if Parsing_result2.has_error res then (
        log_parsing_errors file res;
        Core_exit_code.(exit_semgrep caps#exit False)))
[@@action]

(*****************************************************************************)
(* Experiments *)
(*****************************************************************************)
(* See Experiments.ml now *)

(*****************************************************************************)
(* Config *)
(*****************************************************************************)

let mk_config () =
  {
    strict = !strict;
    debug = !debug;
    trace = !trace;
    trace_endpoint = !trace_endpoint;
    profile = !profile;
    report_time = !report_time;
    error_recovery = !error_recovery;
    matching_explanations = !matching_explanations;
    rule_source = !rule_source;
    filter_irrelevant_rules = !filter_irrelevant_rules;
    respect_rule_paths = !respect_rule_paths;
    (* not part of CLI *)
    equivalences_file = !equivalences_file;
    lang = !lang;
    output_format = !output_format;
    match_format = !match_format;
    mvars = !mvars;
    timeout = !timeout;
    timeout_threshold = !timeout_threshold;
    max_memory_mb = !max_memory_mb;
    max_match_per_file = !max_match_per_file;
    ncores = !ncores;
    target_source = !target_source;
    file_match_hook = None;
    action = !action;
    roots = [] (* This will be set later in main () *);
    top_level_span = None;
  }

(*****************************************************************************)
(* The actions *)
(*****************************************************************************)

(* Obtain the language set with -lang if it provides a generic AST
   TODO: don't rely on a ref being initialized to do this.
*)
let get_lang () =
  match !lang with
  | None -> None
  | Some x -> (
      match x with
      | L (lang, _other_langs) -> Some lang
      | LRegex
      | LSpacegrep
      | LAliengrep ->
          None)

let all_actions (caps : Cap.all_caps) () =
  [
    (* possibly useful to the user *)
    ( "-show_ast_json",
      " <file> dump on stdout the generic AST of file in JSON",
      Arg_.mk_action_1_conv Fpath.v (Core_actions.dump_v1_json ~get_lang) );
    ( "-generate_ast_json",
      " <file> save in file.ast.json the generic AST of file in JSON",
      Arg_.mk_action_1_conv Fpath.v Core_actions.generate_ast_json );
    ( "-prefilter_of_rules",
      " <file> dump the prefilter regexps of rules in JSON ",
      Arg_.mk_action_1_conv Fpath.v Core_actions.prefilter_of_rules );
    ( "-parsing_stats",
      " <files or dirs> generate parsing statistics (use -json for JSON output)",
      Arg_.mk_action_n_arg (fun xs ->
          Test_parsing.parsing_stats
            (Xlang.lang_of_opt_xlang_exn !lang)
            ~json:(!output_format <> Text) ~verbose:true xs) );
    (* the dumpers *)
    ( "-dump_extensions",
      " print file extension to language mapping",
      Arg_.mk_action_0_arg
        (Core_actions.dump_ext_of_lang (caps :> < Cap.stdout >)) );
    ("-dump_pattern", " <file>", Arg_.mk_action_1_conv Fpath.v dump_pattern);
    ( "-dump_patterns_of_rule",
      " <file>",
      Arg_.mk_action_1_conv Fpath.v dump_patterns_of_rule );
    ( "-dump_ast",
      " <file>",
      fun file ->
        Arg_.mk_action_1_conv Fpath.v
          (dump_ast ~naming:false
             (caps :> < Cap.stdout ; Cap.exit >)
             (Xlang.lang_of_opt_xlang_exn !lang))
          file );
    ( "-dump_lang_ast",
      " <file>",
      fun file ->
        Arg_.mk_action_1_conv Fpath.v
          (Test_parsing.dump_lang_ast (Xlang.lang_of_opt_xlang_exn !lang))
          file );
    ( "-dump_named_ast",
      " <file>",
      fun file ->
        Arg_.mk_action_1_conv Fpath.v
          (dump_ast ~naming:true
             (caps :> < Cap.stdout ; Cap.exit >)
             (Xlang.lang_of_opt_xlang_exn !lang))
          file );
    ( "-dump_il_all",
      " <file>",
      Arg_.mk_action_1_conv Fpath.v
        (Core_actions.dump_il_all (caps :> < Cap.stdout >)) );
    ( "-dump_il",
      " <file>",
      Arg_.mk_action_1_conv Fpath.v
        (Core_actions.dump_il (caps :> < Cap.stdout >)) );
    ( "-dump_rule",
      " <file>",
      Arg_.mk_action_1_conv Fpath.v Core_actions.dump_rule );
    ( "-dump_equivalences",
      " <file> (deprecated)",
      Arg_.mk_action_1_conv Fpath.v
        (Core_actions.dump_equivalences (caps :> < Cap.stdout >)) );
    ( "-dump_tree_sitter_cst",
      " <file> dump the CST obtained from a tree-sitter parser",
      Arg_.mk_action_1_conv Fpath.v (fun file ->
          Test_parsing.dump_tree_sitter_cst
            (Xlang.lang_of_opt_xlang_exn !lang)
            !!file) );
    ( "-dump_tree_sitter_pattern_cst",
      " <file>",
      Arg_.mk_action_1_conv Fpath.v (fun file ->
          Parse_pattern2.dump_tree_sitter_pattern_cst
            (Xlang.lang_of_opt_xlang_exn !lang)
            file) );
    ( "-dump_pfff_ast",
      " <file> dump the generic AST obtained from a pfff parser",
      Arg_.mk_action_1_conv Fpath.v (fun file ->
          Test_parsing.dump_pfff_ast (Xlang.lang_of_opt_xlang_exn !lang) file)
    );
    ( "-diff_pfff_tree_sitter",
      " <file>",
      Arg_.mk_action_n_arg (fun xs ->
          Test_parsing.diff_pfff_tree_sitter (Fpath_.of_strings xs)) );
    (* Misc stuff *)
    ( "-expr_at_range",
      " <l:c-l:c> <file>",
      Arg_.mk_action_2_arg (fun range file ->
          Test_synthesizing.expr_at_range range (Fpath.v file)) );
    ( "-synthesize_patterns",
      " <l:c-l:c> <file>",
      Arg_.mk_action_2_arg (fun range file ->
          Test_synthesizing.synthesize_patterns range (Fpath.v file)) );
    ( "-generate_patterns",
      " <l:c-l:c>+ <file>",
      Arg_.mk_action_n_arg Test_synthesizing.generate_pattern_choices );
    ( "-locate_patched_functions",
      " <file>",
      Arg_.mk_action_1_conv Fpath.v Test_synthesizing.locate_patched_functions
    );
    ( "-stat_matches",
      " <marshalled file>",
      Arg_.mk_action_1_conv Fpath.v
        (Experiments.stat_matches (caps :> < Cap.stdout >)) );
    ( "-ebnf_to_menhir",
      " <ebnf file>",
      Arg_.mk_action_1_conv Fpath.v
        (Experiments.ebnf_to_menhir (caps :> < Cap.stdout >)) );
    ( "-parsing_regressions",
      " <files or dirs> look for parsing regressions",
      Arg_.mk_action_n_arg (fun xs ->
          Test_parsing.parsing_regressions
            (Xlang.lang_of_opt_xlang_exn !lang)
            (Fpath_.of_strings xs)) );
    ( "-test_parse_tree_sitter",
      " <files or dirs> test tree-sitter parser on target files",
      Arg_.mk_action_n_arg (fun xs ->
          Test_parsing.test_parse_tree_sitter
            (Xlang.lang_of_opt_xlang_exn !lang)
            (Fpath_.of_strings xs)) );
    ( "-check_rules",
      " <metachecks file> <files or dirs>",
      Arg_.mk_action_n_conv Fpath.v
        (Check_rule.check_files
           (caps :> < Cap.stdout ; Cap.tmp >)
           mk_config Parse_rule.parse) );
    ( "-translate_rules",
      " <files or dirs>",
      Arg_.mk_action_n_conv Fpath.v
        (Translate_rule.translate_files Parse_rule.parse) );
    ( "-stat_rules",
      " <files or dirs>",
      Arg_.mk_action_n_conv Fpath.v
        (Check_rule.stat_files (caps :> < Cap.stdout >) Parse_rule.parse) );
    ( "-test_rules",
      " <files or dirs>",
      Arg_.mk_action_n_conv Fpath.v
        (Core_actions.test_rules (caps :> < Cap.stdout ; Cap.exit >)) );
    ( "-parse_rules",
      " <files or dirs>",
      Arg_.mk_action_n_conv Fpath.v Test_parsing.test_parse_rules );
    ( "-datalog_experiment",
      " <file> <dir>",
      Arg_.mk_action_2_arg (fun a b ->
          Datalog_experiment.gen_facts (Fpath.v a) (Fpath.v b)) );
    ("-test_eval", " <JSON file>", Arg_.mk_action_1_arg Eval_generic.test_eval);
  ]
  @ Test_analyze_generic.actions ~parse_program:Parse_target.parse_program
  @ Test_dataflow_tainting.actions ()
  @ Test_naming_generic.actions ~parse_program:Parse_target.parse_program

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let options caps (actions : unit -> Arg_.cmdline_actions) =
  [
    ( "-rules",
      Arg.String (fun s -> rule_source := Some (Rule_file (Fpath.v s))),
      " <file> obtain formula of patterns from YAML/JSON/Jsonnet file" );
    ( "-targets",
      Arg.String (fun s -> target_source := Some (Target_file (Fpath.v s))),
      " <file> obtain list of targets to run patterns on" );
    ( "-lang",
      Arg.String (fun s -> lang := Some (Xlang.of_string s)),
      spf " <str> choose language (valid choices:\n     %s)"
        Xlang.supported_xlangs );
    ( "-l",
      Arg.String (fun s -> lang := Some (Xlang.of_string s)),
      spf " <str> shortcut for -lang" );
    ( "-equivalences",
      Arg.String (fun s -> equivalences_file := Some (Fpath.v s)),
      " <file> obtain list of code equivalences from YAML file" );
    ("-j", Arg.Set_int ncores, " <int> number of cores to use (default = 1)");
    ( "-max_target_bytes",
      Arg.Set_int Flag.max_target_bytes,
      " maximum size of a single target file, in bytes. This applies to \
       regular target filtering and might be overridden in some contexts. \
       Specify '0' to disable this filtering. Default: 5 MB" );
    ( "-no_gc_tuning",
      Arg.Clear Flag.gc_tuning,
      " use OCaml's default garbage collector settings" );
    ( "-emacs",
      Arg.Unit (fun () -> match_format := Core_text_output.Emacs),
      " print matches on the same line than the match position" );
    ( "-oneline",
      Arg.Unit (fun () -> match_format := Core_text_output.OneLine),
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
      Arg.String (fun s -> mvars := String_.split ~sep:"," s),
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
    ( "-disable_rule_paths",
      Arg.Clear respect_rule_paths,
      " do not honor the paths: directive of the rule" );
    ( "-tree_sitter_only",
      Arg.Set Flag.tree_sitter_only,
      " only use tree-sitter-based parsers" );
    ("-pfff_only", Arg.Set Flag.pfff_only, " only use pfff-based parsers");
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
    ("--debug", Arg.Set debug, " output debugging information");
    ( "-debug_matching",
      Arg.Set Flag.debug_matching,
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
            CapConsole.print caps#stdout version;
            Core_exit_code.(exit_semgrep caps#exit Success)),
        "  guess what" );
      ( "-rpc",
        Arg.Unit
          (fun () ->
            RPC.main (caps :> < Cap.exec ; Cap.tmp >);
            Core_exit_code.(exit_semgrep caps#exit Success)),
        " don't use this unless you already know" );
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
(* Main entry point *)
(*****************************************************************************)

let main_exn (caps : Cap.all_caps) (sys_argv : string array) : unit =
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
    | Some s -> String_.split ~sep:"[ \t]+" s
    | None -> []
  in

  (* does side effect on many global flags *)
  let args =
    Arg_.parse_options
      (options caps (all_actions caps))
      usage_msg (Array.of_list argv)
  in

  let config = mk_config () in

  Core_profiling.profiling := config.report_time;

  Log_semgrep.setup ?log_to_file:!log_to_file ?require_one_of_these_tags:None
    ~force_color:true
    ~level:
      (* TODO: command-line option or env variable to choose the log level *)
      (if config.debug then Some Debug else Some Info)
    ();

  Logs.info (fun m -> m "Executed as: %s" (argv |> String.concat " "));
  Logs.info (fun m -> m "Version: %s" version);
  let config =
    if config.profile then (
      Logs.info (fun m -> m "Profile mode On");
      Logs.info (fun m -> m "disabling -j when in profiling mode");
      { config with ncores = 1 })
    else config
  in

  (* hacks to reduce the size of engine.js
   * coupling: if you add an init() call here, you probably need to modify
   * also tests/Test.ml and osemgrep/cli/CLI.ml
   *)
  Parsing_init.init ();
  Data_init.init ();

  (* must be done after Arg.parse, because Common.profile is set by it *)
  Profiling.profile_code "Main total" (fun () ->
      match args with
      (* --------------------------------------------------------- *)
      (* actions, useful to debug subpart *)
      (* --------------------------------------------------------- *)
      | xs when List.mem config.action (Arg_.action_list (all_actions caps ()))
        ->
          Arg_.do_action config.action xs (all_actions caps ())
      | _ when not (String_.empty config.action) ->
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
          let config =
            { config with roots = List_.map Scanning_root.of_string roots }
          in

          (* Set up tracing and run it for the duration of scanning. Note that this will
             only trace `semgrep_core_dispatch` and the functions it calls.
           * TODO when osemgrep is the default entry point, we will also be able to
             instrument the pre- and post-scan code in the same way. *)
          if config.trace then (
            let trace_data =
              Trace_data.get_top_level_data config.ncores Version.version
                (Trace_data.no_analysis_features ())
            in
            Tracing.configure_tracing "semgrep-oss";
            Tracing.with_tracing "Core_command.semgrep_core_dispatch"
              config.trace_endpoint trace_data (fun sp ->
                Core_command.run_conf
                  (caps :> < Cap.stdout ; Cap.tmp ; Cap.exit >)
                  { config with top_level_span = Some sp }))
          else
            Core_command.run_conf
              (caps :> < Cap.stdout ; Cap.tmp ; Cap.exit >)
              config)

let with_exception_trace f =
  Printexc.record_backtrace true;
  try f () with
  | exn ->
      let e = Exception.catch exn in
      Printf.eprintf "Exception: %s\n%!" (Exception.to_string e);
      raise (UnixExit 1)

let main (caps : Cap.all_caps) (argv : string array) : unit =
  UCommon.main_boilerplate (fun () ->
      Common.finalize
        (fun () -> with_exception_trace (fun () -> main_exn caps argv))
        (fun () -> !Hooks.exit |> List.iter (fun f -> f ())))
