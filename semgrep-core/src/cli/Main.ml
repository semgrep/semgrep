(*
 * The author disclaims copyright to this source code.  In place of
 * a legal notice, here is a blessing:
 *
 *    May you do good and not evil.
 *    May you find forgiveness for yourself and forgive others.
 *    May you share freely, never taking more than you give.
 *)
open Common
module Flag = Flag_semgrep
module PI = Parse_info
module E = Semgrep_error_code
module MR = Mini_rule
module R = Rule
module J = JSON
module RP = Report
module S = Run_semgrep
module P = Parse_with_caching

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* A semantic grep.
 * See https://semgrep.dev/ for more information.
 *
 * Right now there is:
 *  - good support for: Python, Java, Go, Ruby,
 *    Javascript (and JSX), Typescript (and TSX), JSON
 *  - partial support for: C, C#, PHP, OCaml, Scala, Rust, Lua,
 *    YAML, HTML, Vue
 *  - almost support for: R, Kotlin, Bash, Docker, C++
 *
 * opti: git grep foo | xargs semgrep -e 'foo(...)'
 *
 * related:
 *  - Structural Search and Replace (SSR) in Jetbrains IDE
 *    http://www.jetbrains.com/idea/documentation/ssr.html
 *    http://tv.jetbrains.net/videocontent/intellij-idea-static-analysis-custom-rules-with-structural-search-replace
 *  - gogrep: https://github.com/mvdan/gogrep/
 *  - ruleguard: https://github.com/quasilyte/go-ruleguard
 *    (use gogrep internally)
 *  - phpgrep: https://github.com/quasilyte/phpgrep
 *    https://github.com/VKCOM/noverify/blob/master/docs/dynamic-rules.md
 *    https://speakerdeck.com/quasilyte/phpgrep-syntax-aware-code-search
 *  - rubocop pattern
 *    https://github.com/marcandre/rubocop/blob/master/manual/node_pattern.md
 *  - astpath, using XPATH on ASTs https://github.com/hchasestevens/astpath
 *  - ack http://beyondgrep.com/
 *  - cgrep http://awgn.github.io/cgrep/
 *  - hound https://codeascraft.com/2015/01/27/announcing-hound-a-lightning-fast-code-search-tool/
 *  - many grep-based linters (in Zulip, autodesk, bento, etc.)
 *
 * See also codequery for more structural queries.
 * See also old information at https://github.com/facebook/pfff/wiki/Sgrep.
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

let logger = Logging.get_logger [ __MODULE__ ]

let log_config_file = ref "log_config.json"

(* see also verbose/... flags in Flag_semgrep.ml *)
(* to test things *)
let test = ref false

let debug = ref false

let profile = ref false

(* report matching times per file *)
let report_time = ref false

(* try to continue processing files, even if one has a parse error with -e/f.
 * note that -rules_file does its own error recovery.
 *)
let error_recovery = ref false

(* related: Flag_semgrep.debug_matching *)
let fail_fast = ref false

(* used for -json -profile *)
let profile_start = ref 0.

(* there are a few other debugging flags in Flag_semgrep.ml
 * (e.g., debug_matching)
 *)
(* ------------------------------------------------------------------------- *)
(* main flags *)
(* ------------------------------------------------------------------------- *)

(* -e *)
let pattern_string = ref ""

(* -f *)
let pattern_file = ref ""

(* -rules_file (mini rules) *)
let rules_file = ref ""

(* -config *)
let config_file = ref ""

let equivalences_file = ref ""

(* todo: infer from basename argv(0) ? *)
let lang = ref "unset"

type output_format = Text | Json

let output_format = ref Text

let match_format = ref Matching_report.Normal

let mvars = ref ([] : Metavariable.mvar list)

let lsp = ref false

(* ------------------------------------------------------------------------- *)
(* limits *)
(* ------------------------------------------------------------------------- *)

let timeout = ref 0. (* in seconds; 0 or less means no timeout *)

let max_memory_mb = ref 0 (* in MiB *)

(* arbitrary limit *)
let max_match_per_file = ref 10_000

(* -j *)
let ncores = ref 1

(* ------------------------------------------------------------------------- *)
(* optional optimizations *)
(* ------------------------------------------------------------------------- *)
(* see Flag_semgrep.ml *)

(* ------------------------------------------------------------------------- *)
(* flags used by the semgrep-python wrapper *)
(* ------------------------------------------------------------------------- *)

(* path to cache (given by semgrep-python) *)
let use_parsing_cache = ref ""

(* take the list of files in a file (given by semgrep-python) *)
let target_file = ref ""

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let logger = Logging.get_logger [ __MODULE__ ]

let version =
  spf "semgrep-core version: %s, pfff: %s" Version.version Config_pfff.version

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

(*
   If the target is a named pipe, copy it into a regular file and return
   that. This allows multiple reads on the file.

   This is intended to support one or a small number of targets created
   manually on the command line with e.g. <(echo 'eval(x)') which the
   shell replaces by a named pipe like '/dev/fd/63'.
*)
let replace_named_pipe_by_regular_file path =
  match (Unix.stat path).st_kind with
  | Unix.S_FIFO ->
      let data = Common.read_file path in
      let prefix = spf "semgrep-core-" in
      let suffix = spf "-%s" (Filename.basename path) in
      let tmp_path, oc =
        Filename.open_temp_file
          ~mode:[ Open_creat; Open_excl; Open_wronly; Open_binary ]
          prefix suffix
      in
      let remove () = if Sys.file_exists tmp_path then Sys.remove tmp_path in
      (* Try to remove temporary file when program exits. *)
      at_exit remove;
      Fun.protect
        ~finally:(fun () -> close_out_noerr oc)
        (fun () -> output_string oc data);
      tmp_path
  | _ -> path

(* for -gen_layer, see Experiments.ml *)
let _matching_tokens = ref []

let print_match ?str mvars mvar_binding ii_of_any tokens_matched_code =
  (* there are a few fake tokens in the generic ASTs now (e.g.,
   * for DotAccess generated outside the grammar) *)
  let toks = tokens_matched_code |> List.filter PI.is_origintok in
  (if mvars = [] then
   Matching_report.print_match ?str ~format:!match_format toks
  else
    (* similar to the code of Lib_matcher.print_match, maybe could
     * factorize code a bit.
     *)
    let mini, _maxi = PI.min_max_ii_by_pos toks in
    let file, line = (PI.file_of_info mini, PI.line_of_info mini) in

    let strings_metavars =
      mvars
      |> List.map (fun x ->
             match Common2.assoc_opt x mvar_binding with
             | Some any ->
                 any |> ii_of_any
                 |> List.filter PI.is_origintok
                 |> List.map PI.str_of_info
                 |> Matching_report.join_with_space_if_needed
             | None -> failwith (spf "the metavariable '%s' was not binded" x))
    in
    pr (spf "%s:%d: %s" file line (Common.join ":" strings_metavars));
    ());
  toks |> List.iter (fun x -> Common.push x _matching_tokens)

let lang_of_string s =
  match Lang.lang_of_string_opt s with
  | Some x -> x
  | None -> failwith (Lang.unsupported_language_message s)

(* when called from semgrep-python, error messages in semgrep-core or
 * certain profiling statistics may refer to rule id that are generated
 * by semgrep-python, making it hard to know what the problem is.
 * At least we can save this generated rule file to help debugging.
 *)
let save_rules_file_in_tmp () =
  let tmp = Filename.temp_file "semgrep_core_rule-" ".yaml" in
  pr2 (spf "saving rules file for debugging in: %s" tmp);
  Common.write_file ~file:tmp (Common.read_file !rules_file)

(*****************************************************************************)
(* Checker *)
(*****************************************************************************)
(* We do not use the easier Stdlib.input_line here because this function
 * does remove newlines (and may do other clever things), but
 * newlines have a special meaning in some languages
 * (e.g., Python), so we use the lower-level Stdlib.input instead.
 *)
let rec read_all chan =
  let buf = Bytes.create 4096 in
  let len = input chan buf 0 4096 in
  if len = 0 then ""
  else
    let rest = read_all chan in
    Bytes.sub_string buf 0 len ^ rest

(* works with -lang *)
let validate_pattern () =
  let chan = stdin in
  let s = read_all chan in
  try
    let lang = lang_of_string !lang in
    let _ = P.parse_pattern lang s in
    exit 0
  with _exn -> exit 1

(* See also Check_rule.check_files *)

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
    | OCaml.VTuple xs -> J.Array (List.map aux xs)
    | OCaml.VDict xs -> J.Object (List.map (fun (k, v) -> (k, aux v)) xs)
    | OCaml.VSum (s, xs) -> (
        match xs with
        | [] -> J.String (spf "%s" s)
        | [ one_element ] -> J.Object [ (s, aux one_element) ]
        | _ -> J.Object [ (s, J.Array (List.map aux xs)) ])
    | OCaml.VVar (s, i64) -> J.String (spf "%s_%d" s (Int64.to_int i64))
    | OCaml.VArrow _ -> failwith "Arrow TODO"
    | OCaml.VNone -> J.Null
    | OCaml.VSome v -> J.Object [ ("some", aux v) ]
    | OCaml.VRef v -> J.Object [ ("ref@", aux v) ]
    | OCaml.VList xs -> J.Array (List.map aux xs)
    | OCaml.VTODO _ -> J.String "VTODO"
  in
  aux v

let dump_v_to_format (v : OCaml.v) =
  match !output_format with
  | Text -> OCaml.string_of_v v
  | Json -> J.string_of_json (json_of_v v)

(* works with -lang *)
let dump_pattern (file : Common.filename) =
  let s = Common.read_file file in
  (* mostly copy-paste of parse_pattern above, but with better error report *)
  let lang = lang_of_string !lang in
  E.try_with_print_exn_and_reraise file (fun () ->
      let any = Parse_pattern.parse_pattern lang ~print_errors:true s in
      let v = Meta_AST.vof_any any in
      let s = dump_v_to_format v in
      pr s)

let dump_ast ?(naming = false) lang file =
  E.try_with_print_exn_and_exit_fast file (fun () ->
      let { Parse_target.ast; errors; _ } =
        if naming then
          Parse_target.parse_and_resolve_name_use_pfff_or_treesitter lang file
        else Parse_target.just_parse_with_lang lang file
      in
      let v = Meta_AST.vof_any (AST_generic.Pr ast) in
      let s = dump_v_to_format v in
      pr s;
      if errors <> [] then (
        pr2 (spf "WARNING: fail to fully parse %s" file);
        exit 1))

let dump_v1_json file =
  match Lang.langs_of_filename file with
  | lang :: _ ->
      E.try_with_print_exn_and_reraise file (fun () ->
          let { Parse_target.ast; errors; _ } =
            Parse_target.parse_and_resolve_name_use_pfff_or_treesitter lang file
          in
          let v1 = AST_generic_to_v1.program ast in
          let s = AST_generic_v1_j.string_of_program v1 in
          pr s;
          if errors <> [] then pr2 (spf "WARNING: fail to fully parse %s" file))
  | [] -> failwith (spf "unsupported language for %s" file)

let dump_ext_of_lang () =
  let lang_to_exts =
    Lang.keys
    |> List.map (fun lang_str ->
           match Lang.lang_of_string_opt lang_str with
           | Some lang ->
               lang_str ^ "->" ^ String.concat ", " (Lang.ext_of_lang lang)
           | None -> "")
  in
  pr2
    (spf "Language to supported file extension mappings:\n %s"
       (String.concat "\n" lang_to_exts))

let dump_equivalences file =
  let xs = Parse_equivalences.parse file in
  pr2_gen xs

let dump_rule file =
  let rules = Parse_rule.parse file in
  rules |> List.iter (fun r -> pr (Rule.show r))

(*****************************************************************************)
(* Config *)
(*****************************************************************************)

let mk_config () =
  {
    S.log_config_file = !log_config_file;
    test = !test;
    debug = !debug;
    profile = !profile;
    report_time = !report_time;
    error_recovery = !error_recovery;
    fail_fast = !fail_fast;
    profile_start = !profile_start;
    pattern_string = !pattern_string;
    (* -e *)
    pattern_file = !pattern_file;
    (* -f *)
    rules_file = !rules_file;
    (* -rules_file *)
    config_file = !config_file;
    (* -config *)
    equivalences_file = !equivalences_file;
    lang = !lang;
    output_format =
      (match !output_format with
      | Text -> S.Text
      | Json -> S.Json);
    match_format = !match_format;
    mvars = !mvars;
    lsp = !lsp;
    timeout = !timeout;
    max_memory_mb = !max_memory_mb;
    max_match_per_file = !max_match_per_file;
    ncores = !ncores;
    use_parsing_cache = !use_parsing_cache;
    target_file = !target_file;
    action = !action;
    version = Version.version;
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
    ( "-dump_extensions",
      " print file extension to language mapping",
      Common.mk_action_0_arg dump_ext_of_lang );
    ("-dump_pattern", " <file>", Common.mk_action_1_arg dump_pattern);
    ( "-dump_ast",
      " <file>",
      fun file ->
        Common.mk_action_1_arg
          (dump_ast ~naming:false (lang_of_string !lang))
          file );
    ( "-dump_named_ast",
      " <file>",
      fun file ->
        Common.mk_action_1_arg
          (dump_ast ~naming:true (lang_of_string !lang))
          file );
    ("-dump_v1_json", " <file>", Common.mk_action_1_arg dump_v1_json);
    ("-dump_equivalences", " <file>", Common.mk_action_1_arg dump_equivalences);
    ("-dump_rule", " <file>", Common.mk_action_1_arg dump_rule);
    ( "-dump_tree_sitter_cst",
      " <file> dump the CST obtained from a tree-sitter parser",
      Common.mk_action_1_arg (fun file ->
          Test_parsing.dump_tree_sitter_cst (lang_of_string !lang) file) );
    ( "-dump_tree_sitter_pattern_cst",
      " <file>",
      Common.mk_action_1_arg (fun file ->
          Parse_pattern.dump_tree_sitter_pattern_cst (lang_of_string !lang) file)
    );
    ( "-dump_pfff_ast",
      " <file> dump the generic AST obtained from a pfff parser",
      Common.mk_action_1_arg (fun file ->
          Test_parsing.dump_pfff_ast (lang_of_string !lang) file) );
    ("-dump_il", " <file>", Common.mk_action_1_arg Datalog_experiment.dump_il);
    ( "-diff_pfff_tree_sitter",
      " <file>",
      Common.mk_action_n_arg Test_parsing.diff_pfff_tree_sitter );
    ( "--validate-pattern-stdin",
      " check the syntax of a pattern ",
      Common.mk_action_0_arg validate_pattern );
    ( "-expr_at_range",
      " <l:c-l:c> <file>",
      Common.mk_action_2_arg Test_synthesizing.expr_at_range );
    ( "-synthesize_patterns",
      " <l:c-l:c> <file>",
      Common.mk_action_2_arg Test_synthesizing.synthesize_patterns );
    ( "-generate_patterns",
      " <l:c-l:c>+ <file>",
      Common.mk_action_n_arg Test_synthesizing.generate_pattern_choices );
    ( "-stat_matches",
      " <marshalled file>",
      Common.mk_action_1_arg Experiments.stat_matches );
    ( "-ebnf_to_menhir",
      " <ebnf file>",
      Common.mk_action_1_arg Experiments.ebnf_to_menhir );
    ( "-parsing_stats",
      " <files or dirs> generate parsing statistics (use -json for JSON output)",
      Common.mk_action_n_arg (fun xs ->
          Test_parsing.parsing_stats (lang_of_string !lang)
            (!output_format = Json) xs) );
    ( "-parsing_regressions",
      " <files or dirs> look for parsing regressions",
      Common.mk_action_n_arg (fun xs ->
          Test_parsing.parsing_regressions (lang_of_string !lang) xs) );
    ( "-test_parse_tree_sitter",
      " <files or dirs> test tree-sitter parser on target files",
      Common.mk_action_n_arg (fun xs ->
          Test_parsing.test_parse_tree_sitter (lang_of_string !lang) xs) );
    ( "-check_rules",
      " <metachecks file> <files or dirs>",
      Common.mk_action_n_arg (Check_rule.check_files Parse_rule.parse) );
    ( "-stat_rules",
      " <files or dirs>",
      Common.mk_action_n_arg (Check_rule.stat_files Parse_rule.parse) );
    ( "-test_rules",
      " <files or dirs>",
      Common.mk_action_n_arg Test_engine.test_rules );
    ( "-parse_rules",
      " <files or dirs>",
      Common.mk_action_n_arg Test_parsing.test_parse_rules );
    ( "-datalog_experiment",
      " <file> <dir>",
      Common.mk_action_2_arg Datalog_experiment.gen_facts );
    ("-postmortem", " <log file", Common.mk_action_1_arg Statistics_report.stat);
    ( "-test_comby",
      " <pattern> <file>",
      Common.mk_action_2_arg Test_comby.test_comby );
    ("-eval", " <JSON file>", Common.mk_action_1_arg Eval_generic.eval_json_file);
    ("-test_eval", " <JSON file>", Common.mk_action_1_arg Eval_generic.test_eval);
  ]
  @ Test_analyze_generic.actions ()

let options () =
  [
    ("-e", Arg.Set_string pattern_string, " <str> use the string as the pattern");
    ( "-f",
      Arg.Set_string pattern_file,
      " <file> use the file content as the pattern" );
    ( "-rules_file",
      Arg.Set_string rules_file,
      " <file> obtain a list of patterns from YAML file (implies -json)" );
    ( "-config",
      Arg.Set_string config_file,
      " <file> obtain formula of patterns from YAML/JSON/Jsonnet file" );
    ( "-lang",
      Arg.Set_string lang,
      spf " <str> choose language (valid choices:\n     %s)"
        Lang.supported_langs );
    ( "-target_file",
      Arg.Set_string target_file,
      " <file> obtain list of targets to run patterns on" );
    ( "-equivalences",
      Arg.Set_string equivalences_file,
      " <file> obtain list of code equivalences from YAML file" );
    ("-j", Arg.Set_int ncores, " <int> number of cores to use (default = 1)");
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
    ("-json", Arg.Unit (fun () -> output_format := Json), " output JSON format");
    ( "-json_time",
      Arg.Unit
        (fun () ->
          output_format := Json;
          report_time := true),
      " report detailed matching times as part of the JSON response. Implies \
       '-json'." );
    ( "-pvar",
      Arg.String (fun s -> mvars := Common.split "," s),
      " <metavars> print the metavariables, not the matched code" );
    ( "-gen_layer",
      Arg.String (fun s -> Experiments.layer_file := Some s),
      " <file> save result in a codemap layer file" );
    ( "-error_recovery",
      Arg.Unit
        (fun () ->
          error_recovery := true;
          Flag_parsing.error_recovery := true),
      " do not stop at first parsing error with -e/-f" );
    ( "-fail_fast",
      Arg.Set fail_fast,
      " stop at first exception (and get a backtrace)" );
    ( "-use_parsing_cache",
      Arg.Set_string use_parsing_cache,
      " <dir> save and use parsed ASTs in a cache at given directory.\n\
      \    It is the caller's responsiblity to clear the cache" );
    ( "-filter_irrelevant_patterns",
      Arg.Set Flag.filter_irrelevant_patterns,
      " filter patterns not containing any strings in target file" );
    ( "-no_filter_irrelevant_patterns",
      Arg.Clear Flag.filter_irrelevant_patterns,
      " do not filter patterns" );
    ( "-filter_irrelevant_rules",
      Arg.Set Flag.filter_irrelevant_rules,
      " filter rules not containing any strings in target file" );
    ( "-no_filter_irrelevant_rules",
      Arg.Clear Flag.filter_irrelevant_rules,
      " do not filter rules" );
    ( "-fast",
      Arg.Set Flag.filter_irrelevant_rules,
      " filter rules not containing any strings in target file" );
    ( "-bloom_filter",
      Arg.Set Flag.use_bloom_filter,
      " use a bloom filter to only attempt matches when strings in the pattern \
       are in the target" );
    ( "-no_bloom_filter",
      Arg.Clear Flag.use_bloom_filter,
      " do not use bloom filter" );
    ( "-set_filter",
      Arg.Set Flag.set_instead_of_bloom_filter,
      "use a set instead of bloom filters" );
    ( "-tree_sitter_only",
      Arg.Set Flag.tree_sitter_only,
      " only use tree-sitter-based parsers" );
    ( "-timeout",
      Arg.Set_float timeout,
      " <float> time limit to process one input program (in seconds); 0 \
       disables timeouts (default is 0)" );
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
    ( "-debug_matching",
      Arg.Set Flag.debug_matching,
      " raise an exception at the first match failure" );
    ( "-log_config_file",
      Arg.Set_string log_config_file,
      " <file> logging configuration file" );
    ( "-log_to_file",
      Arg.String
        (fun file ->
          let open Easy_logging in
          let h = Handlers.make (File (file, Debug)) in
          logger#add_handler h;
          logger#set_level Debug),
      " <file> log debugging info to file" );
    ("-test", Arg.Set test, " (internal) set test context");
    ("-lsp", Arg.Set lsp, " connect to LSP lang server to get type information");
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
            Common.profile := Common.ProfAll;
            profile := true),
        " output profiling information" );
      ( "-keep_tmp_files",
        Arg.Set Common.save_tmp_files,
        " keep temporary generated files" );
    ]
  @ Meta_parse_info.cmdline_flags_precision ()
  @ Semgrep_error_code.options ()
  @ Common.options_of_actions action (all_actions ())
  @ [
      ( "-version",
        Arg.Unit
          (fun () ->
            pr2 version;
            exit 0),
        "  guess what" );
    ]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () =
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

  profile_start := Unix.gettimeofday ();

  let usage_msg =
    spf
      "Usage: %s [options] -lang <str> [-e|-f|-rules_file|-config] <pattern> \
       <files_or_dirs> \n\
       Options:"
      (Filename.basename Sys.argv.(0))
  in

  (* --------------------------------------------------------- *)
  (* Setting up debugging/profiling *)
  (* --------------------------------------------------------- *)
  let argv =
    Array.to_list Sys.argv
    @ (if Sys.getenv_opt env_debug <> None then [ "-debug" ] else [])
    @ (if Sys.getenv_opt env_profile <> None then [ "-profile" ] else [])
    @
    match Sys.getenv_opt env_extra with
    | Some s -> Common.split "[ \t]+" s
    | None -> []
  in

  (* does side effect on many global flags *)
  let args = Common.parse_options (options ()) usage_msg (Array.of_list argv) in
  let args = if !target_file = "" then args else Common.cat !target_file in

  if Sys.file_exists !log_config_file then (
    Logging.load_config_file !log_config_file;
    logger#info "loaded %s" !log_config_file);
  if !debug then (
    let open Easy_logging in
    let h = Handlers.make (CliErr Debug) in
    logger#add_handler h;
    logger#set_level Debug;
    ());

  logger#info "Executed as: %s" (Sys.argv |> Array.to_list |> String.concat " ");
  logger#info "Version: %s" version;
  if !profile then (
    logger#info "Profile mode On";
    logger#info "disabling -j when in profiling mode";
    ncores := 1);

  if !lsp then LSP_client.init ();

  (* must be done after Arg.parse, because Common.profile is set by it *)
  Common.profile_code "Main total" (fun () ->
      match args with
      (* --------------------------------------------------------- *)
      (* actions, useful to debug subpart *)
      (* --------------------------------------------------------- *)
      | xs when List.mem !action (Common.action_list (all_actions ())) ->
          Common.do_action !action xs (all_actions ())
      | _ when not (Common.null_string !action) ->
          failwith ("unrecognized action or wrong params: " ^ !action)
      (* --------------------------------------------------------- *)
      (* main entry *)
      (* --------------------------------------------------------- *)
      | _ :: _ as roots -> (
          if !Flag.gc_tuning && !max_memory_mb = 0 then set_gc ();

          match () with
          | _ when !config_file <> "" ->
              S.semgrep_with_rules_file (mk_config ()) roots
          | _ when !rules_file <> "" ->
              S.semgrep_with_patterns_file (mk_config ()) roots
          | _ -> S.semgrep_with_one_pattern (mk_config ()) roots)
      (* --------------------------------------------------------- *)
      (* empty entry *)
      (* --------------------------------------------------------- *)
      (* TODO: should not need that, semgrep should not call us when there
       * are no files to process. *)
      | [] when !target_file <> "" && !config_file <> "" ->
          S.semgrep_with_rules_file (mk_config ()) []
      | [] -> Common.usage usage_msg (options ()))

(*****************************************************************************)
let () =
  Common.main_boilerplate (fun () ->
      (* semgrep-specific initializations. Move to a dedicated module? *)
      Pcre_settings.register_exception_printer ();
      Common.finalize
        (fun () -> main ())
        (fun () -> !Hooks.exit |> List.iter (fun f -> f ())))
