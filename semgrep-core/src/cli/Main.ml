(*s: semgrep/CLI/Main.ml *)
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
module S = Scope_code
module E = Error_code
module MR = Mini_rule
module R = Rule
module J = JSON
module FT = File_type
module RP = Report
module SJ = Spacegrep.Semgrep_j

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* A semantic grep.
 * See https://semgrep.dev/ for more information.
 *
 * Right now there is:
 *  - good support for: Python, Java, Go, Ruby,
 *    Javascript (and JSX), Typescript (and TSX), JSON
 *  - partial support for: PHP, C, OCaml, Lua, C#, YAML
 *  - almost support for: Rust, R, Kotlin.
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

(*s: constant [[Main_semgrep_core.verbose]] *)
(*e: constant [[Main_semgrep_core.verbose]] *)
(*s: constant [[Main_semgrep_core.debug]] *)
let debug = ref false

(*e: constant [[Main_semgrep_core.debug]] *)
let profile = ref false

(* report matching times per file *)
let report_time = ref false

(*s: constant [[Main_semgrep_core.error_recovery]] *)
(* try to continue processing files, even if one has a parse error with -e/f.
 * note that -rules_file does its own error recovery.
 *)
let error_recovery = ref false

(*e: constant [[Main_semgrep_core.error_recovery]] *)
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

(*s: constant [[Main_semgrep_core.pattern_string]] *)
(* -e *)
let pattern_string = ref ""

(*e: constant [[Main_semgrep_core.pattern_string]] *)
(*s: constant [[Main_semgrep_core.pattern_file]] *)
(* -f *)
let pattern_file = ref ""

(*e: constant [[Main_semgrep_core.pattern_file]] *)
(*s: constant [[Main_semgrep_core.rules_file]] *)
(* -rules_file (mini rules) *)
let rules_file = ref ""

(*e: constant [[Main_semgrep_core.rules_file]] *)
(*s: constant [[Main_semgrep_core.tainting_rules_file]] *)
(* -tainting_rules_file *)
let tainting_rules_file = ref ""

(*e: constant [[Main_semgrep_core.tainting_rules_file]] *)

(* -config *)
let config_file = ref ""

(*s: constant [[Main_semgrep_core.equivalences_file]] *)
let equivalences_file = ref ""

(*e: constant [[Main_semgrep_core.equivalences_file]] *)

(* todo: infer from basename argv(0) ? *)
(*s: constant [[Main_semgrep_core.lang]] *)
let lang = ref "unset"

(*e: constant [[Main_semgrep_core.lang]] *)

(*s: constant [[Main_semgrep_core.excludes]] *)
(*e: constant [[Main_semgrep_core.excludes]] *)
(*s: constant [[Main_semgrep_core.includes]] *)
(*e: constant [[Main_semgrep_core.includes]] *)
(*s: constant [[Main_semgrep_core.exclude_dirs]] *)
(*e: constant [[Main_semgrep_core.exclude_dirs]] *)
(*s: constant [[Main_semgrep_core.include_dirs]] *)
(*e: constant [[Main_semgrep_core.include_dirs]] *)

type output_format = Text | Json

(*s: constant [[Main_semgrep_core.output_format_json]] *)
let output_format = ref Text

(*e: constant [[Main_semgrep_core.output_format_json]] *)

(*s: constant [[Main_semgrep_core.match_format]] *)
let match_format = ref Matching_report.Normal

(*e: constant [[Main_semgrep_core.match_format]] *)

(*s: constant [[Main_semgrep_core.mvars]] *)
let mvars = ref ([] : Metavariable.mvar list)

(*e: constant [[Main_semgrep_core.mvars]] *)

(*s: constant [[Main_semgrep_core.layer_file]] *)
(*e: constant [[Main_semgrep_core.layer_file]] *)

(*s: constant [[Main_semgrep_core.keys]] *)
let keys = Common2.hkeys Lang.lang_of_string_map

(*e: constant [[Main_semgrep_core.keys]] *)
(*s: constant [[Main_semgrep_core.supported_langs]] *)
let supported_langs : string = String.concat ", " keys

(*e: constant [[Main_semgrep_core.supported_langs]] *)

(* ------------------------------------------------------------------------- *)
(* limits *)
(* ------------------------------------------------------------------------- *)

let timeout = ref 0. (* in seconds; 0 or less means no timeout *)

let max_memory = ref 0 (* in MB *)

(* arbitrary limit *)
let max_match_per_file = ref 10_000

(*s: constant [[Main_semgrep_core.ncores]] *)
(* -j *)
let ncores = ref 1

(*e: constant [[Main_semgrep_core.ncores]] *)

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

(*s: constant [[Main_semgrep_core.action]] *)
(* action mode *)
let action = ref ""

(*e: constant [[Main_semgrep_core.action]] *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let version =
  spf "semgrep-core version: %s, pfff: %s" Version.version Config_pfff.version

(*s: function [[Main_semgrep_core.set_gc]] *)
let set_gc () =
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

(*e: function [[Main_semgrep_core.set_gc]] *)

(*s: function [[Main_semgrep_core.map]] *)
let map f xs =
  if !ncores <= 1 then List.map f xs
  else
    let n = List.length xs in
    (* Heuristic. Note that if you don't set a chunksize, Parmap
     * will evenly split the list xs, which does not provide any load
     * balancing.
     *)
    let chunksize =
      match n with
      | _ when n > 1000 -> 10
      | _ when n > 100 -> 5
      | _ when n = 0 -> 1
      | _ when n <= !ncores -> 1
      | _ -> n / !ncores
    in
    assert (!ncores > 0 && chunksize > 0);
    Parmap.parmap ~ncores:!ncores ~chunksize f (Parmap.L xs)

(*e: function [[Main_semgrep_core.map]] *)

(*s: constant [[Main_semgrep_core._matching_tokens]] *)
(* for -gen_layer, see Experiments.ml *)
let _matching_tokens = ref []

(*e: constant [[Main_semgrep_core._matching_tokens]] *)

(*s: function [[Main_semgrep_core.print_match]] *)
let print_match ?str mvars mvar_binding ii_of_any tokens_matched_code =
  (* there are a few fake tokens in the generic ASTs now (e.g.,
   * for DotAccess generated outside the grammar) *)
  let toks = tokens_matched_code |> List.filter PI.is_origintok in
  ( if mvars = [] then
    Matching_report.print_match ?str ~format:!match_format toks
  else
    (*s: [[Main_semgrep_core.print_match()]] when non empty [[mvars]] *)
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
    (*e: [[Main_semgrep_core.print_match()]] when non empty [[mvars]] *)
    () );
  (*s: [[Main_semgrep_core.print_match()]] hook *)
  toks |> List.iter (fun x -> Common.push x _matching_tokens)

(*e: [[Main_semgrep_core.print_match()]] hook *)
(*e: function [[Main_semgrep_core.print_match]] *)
(*s: function [[Main_semgrep_core.gen_layer]] *)
(*e: function [[Main_semgrep_core.gen_layer]] *)

(*s: function [[Main_semgrep_core.unsupported_language_message]] *)
let unsupported_language_message lang =
  if lang = "unset" then "no language specified; use -lang"
  else
    spf "unsupported language: %s; supported language tags are: %s" lang
      supported_langs

(*e: function [[Main_semgrep_core.unsupported_language_message]] *)

let lang_of_string s =
  match Lang.lang_of_string_opt s with
  | Some x -> x
  | None -> failwith (unsupported_language_message s)

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
(* Caching *)
(*****************************************************************************)

let filemtime file = (Unix.stat file).Unix.st_mtime

(* The function below is mostly a copy-paste of Common.cache_computation.
 * This function is slightly more flexible because we can put the cache file
 * anywhere thanks to the argument 'cache_file_of_file'.
 * We also try to be a bit more type-safe by using the version tag above.
 * TODO: merge in pfff/commons/Common.ml at some point
 *)
let cache_computation file cache_file_of_file f =
  if !use_parsing_cache = "" then f ()
  else if not (Sys.file_exists file) then (
    pr2 ("WARNING: cache_computation: can't find file " ^ file);
    pr2 "defaulting to calling the function";
    f () )
  else
    Common.profile_code "Main.cache_computation" (fun () ->
        let file_cache = cache_file_of_file file in
        if Sys.file_exists file_cache && filemtime file_cache >= filemtime file
        then (
          logger#info "using cache: %s" file_cache;
          let version, file2, res = Common2.get_value file_cache in
          if version <> Version.version then
            failwith
              (spf "Version mismatch! Clean the cache file %s" file_cache);
          if file <> file2 then
            failwith
              (spf
                 "Not the same file! Md5sum collision! Clean the cache file %s"
                 file_cache);

          res )
        else
          let res = f () in
          Common2.write_value (Version.version, file, res) file_cache;
          res)

let cache_file_of_file filename =
  let dir = !use_parsing_cache in
  if not (Sys.file_exists dir) then Unix.mkdir dir 0o700;
  (* hopefully there will be no collision *)
  let md5 = Digest.string filename in
  Filename.concat dir (spf "%s.ast_cache" (Digest.to_hex md5))

(*****************************************************************************)
(* Timeout *)
(*****************************************************************************)

(* subtle: You have to make sure that Timeout is not intercepted, so
 * avoid exn handler such as try (...) with _ -> otherwise Timeout will
 * not bubble up enough. In such case, add a case before such as
 * with Timeout -> raise Timeout | _ -> ...
 *)
let timeout_function file f =
  let timeout = !timeout in
  if timeout <= 0. then f ()
  else
    Common.timeout_function_float ~verbose:false timeout (fun () ->
        try f ()
        with Timeout ->
          logger#info "raised Timeout in timeout_function for %s" file;
          raise Timeout)

(*
   Fail gracefully if memory becomes insufficient.

   It raises Out_of_memory if we're over the memory limit at the end of a
   major GC cycle.

   See https://discuss.ocaml.org/t/todays-trick-memory-limits-with-gc-alarms/4431
   for detailed explanations.
*)
let run_with_memory_limit limit_mb f =
  if limit_mb = 0 then f ()
  else if limit_mb < 0 then
    invalid_arg (spf "run_with_memory_limit: negative argument %i" limit_mb)
  else
    let limit = limit_mb * 1024 * 1024 in
    let limit_memory () =
      let mem = (Gc.quick_stat ()).Gc.heap_words in
      if mem > limit / (Sys.word_size / 8) then (
        logger#info "maxout allocated memory: %d" (mem * (Sys.word_size / 8));
        raise Out_of_memory )
    in
    let alarm = Gc.create_alarm limit_memory in
    Fun.protect f ~finally:(fun () ->
        Gc.delete_alarm alarm;
        Gc.compact ())

(* Certain patterns may be too general and match too many times on big files.
 * This does not cause a Timeout during parsing or matching, but returning
 * a huge number of matches can stress print_matches_and_errors_json
 * and anyway is probably a sign that the pattern should be rewritten.
 * This puts also lots of stress on the semgrep Python wrapper which has
 * to do lots of range intersections with all those matches.
 *)
let filter_files_with_too_many_matches_and_transform_as_timeout matches =
  let per_files =
    matches
    |> List.map (fun m -> (m.Pattern_match.file, m))
    |> Common.group_assoc_bykey_eff
  in
  let offending_files =
    per_files
    |> List.filter_map (fun (file, xs) ->
           if List.length xs > !max_match_per_file then Some file else None)
    |> Common.hashset_of_list
  in
  let new_matches =
    matches
    |> Common.exclude (fun m ->
           Hashtbl.mem offending_files m.Pattern_match.file)
  in
  let new_errors =
    offending_files |> Common.hashset_to_list
    |> List.map (fun file ->
           (* logging useful info for rule writers *)
           logger#info "too many matches on %s, generating exn for it" file;
           let biggest_offending_rule =
             let matches = List.assoc file per_files in
             matches
             |> List.map (fun m ->
                    let rule_id = m.Pattern_match.rule_id in
                    ( ( rule_id.Pattern_match.id,
                        rule_id.Pattern_match.pattern_string ),
                      m ))
             |> Common.group_assoc_bykey_eff
             |> List.map (fun (k, xs) -> (k, List.length xs))
             |> Common.sort_by_val_highfirst |> List.hd
             (* nosemgrep *)
           in
           let (id, pat), cnt = biggest_offending_rule in
           logger#info
             "most offending rule: id = %s, matches = %d, pattern = %s" id cnt
             pat;

           (* todo: we should maybe use a new error: TooManyMatches of int * string*)
           let loc = Parse_info.first_loc_of_file file in
           Error_code.mk_error_loc loc (Error_code.TooManyMatches pat))
  in
  (new_matches, new_errors)
  [@@profiling "Main.filter_too_many_matches"]

(*****************************************************************************)
(* Parsing *)
(*****************************************************************************)

(*s: function [[Main_semgrep_core.parse_generic]] *)
(* It should really be just a call to Parse_target.parse_and_resolve...
 * but the semgrep python wrapper calls semgrep-core separately for each
 * rule, so we need to cache parsed AST to avoid extra work.
 *)
let parse_generic lang file =
  (*s: [[Main_semgrep_core.parse_generic()]] use standard macros if parsing C *)
  if lang = Lang.C && Sys.file_exists !Flag_parsing_cpp.macros_h then
    Parse_cpp.init_defs !Flag_parsing_cpp.macros_h;

  (*e: [[Main_semgrep_core.parse_generic()]] use standard macros if parsing C *)
  let v =
    cache_computation file
      (fun file ->
        (* we may use different parsers for the same file (e.g., in Python3 or
         * Python2 mode), so put the lang as part of the cache "dependency".
         * We also add ast_version here so bumping the version will not
         * try to use the old cache file (which should generate an exception).
         *)
        let full_filename =
          spf "%s__%s__%s" file (Lang.string_of_lang lang) Version.version
        in
        cache_file_of_file full_filename)
      (fun () ->
        try
          logger#info "parsing %s" file;
          (* finally calling the actual function *)
          let { Parse_target.ast; errors; _ } =
            Parse_target.parse_and_resolve_name_use_pfff_or_treesitter lang file
          in
          (*s: [[Main_semgrep_core.parse_generic()]] resolve names in the AST *)
          (*e: [[Main_semgrep_core.parse_generic()]] resolve names in the AST *)
          Left (ast, errors)
          (* This is a bit subtle, but we now store in the cache whether we had
           * an exception on this file, especially Timeout. Indeed, semgrep now calls
           * semgrep-core per rule, and if one file timeout during parsing, it would
           * timeout for each rule, but we don't want to wait each time 5sec for each
           * rule. So here we store the exn in the cache, and below we reraise it
           * after we got it back from the cache.
           *
           * TODO: right now we just capture Timeout, but we should capture any exn.
           *  However this introduces some weird regressions in CI so we focus on
           *  just Timeout for now.
           *)
        with Timeout -> Right Timeout)
  in
  match v with Left x -> x | Right exn -> raise exn
  [@@profiling]

(*e: function [[Main_semgrep_core.parse_generic]] *)

(*s: function [[Main_semgrep_core.parse_equivalences]] *)
let parse_equivalences () =
  match !equivalences_file with
  | "" -> []
  | file -> Parse_equivalences.parse file
  [@@profiling]

(*e: function [[Main_semgrep_core.parse_equivalences]] *)

(*s: type [[Main_semgrep_core.ast]] *)
(*s: [[Main_semgrep_core.ast]] other cases *)
(*e: [[Main_semgrep_core.ast]] other cases *)
(*e: type [[Main_semgrep_core.ast]] *)

(*s: function [[Main_semgrep_core.create_ast]] *)
(*s: [[Main_semgrep_core.create_ast()]] when not a supported language *)
(*e: [[Main_semgrep_core.create_ast()]] when not a supported language *)
(*e: function [[Main_semgrep_core.create_ast]] *)

(*s: type [[Main_semgrep_core.pattern]] *)
(*s: [[Main_semgrep_core.pattern]] other cases *)
(*e: [[Main_semgrep_core.pattern]] other cases *)
(*e: type [[Main_semgrep_core.pattern]] *)

(*s: function [[Main_semgrep_core.parse_pattern]] *)
let parse_pattern lang_pattern str =
  try
    Common.save_excursion Flag_parsing.sgrep_mode true (fun () ->
        let res =
          Parse_pattern.parse_pattern lang_pattern ~print_errors:false str
        in
        (*s: [[Main_semgrep_core.parse_pattern()]] when not a supported language *)
        (*e: [[Main_semgrep_core.parse_pattern()]] when not a supported language *)
        res)
  with exn ->
    raise
      (Parse_mini_rule.InvalidPatternException
         ("no-id", str, !lang, Common.exn_to_s exn))
  [@@profiling]

(*e: function [[Main_semgrep_core.parse_pattern]] *)

(*****************************************************************************)
(* Iteration helpers *)
(*****************************************************************************)
(*s: function [[Main_semgrep_core.filter_files]] *)
(*e: function [[Main_semgrep_core.filter_files]] *)

(*s: function [[Main_semgrep_core.get_final_files]] *)
(* Small wrapper around Lang.files_of_dirs_or_files to also accept
 * an explicit list of files that may not be recognized as part
 * of the language (e.g., files without an extension but that we still
 * want to process). This is especially useful when called from the
 * Python wrapper which gives us an explicit list of files to process.
 *)
let files_of_dirs_with_lang_and_explicit_files lang xs =
  let files = Lang.files_of_dirs_or_files lang xs in
  let explicit_files =
    xs
    |> List.filter (fun file ->
           Sys.file_exists file && not (Sys.is_directory file))
  in
  Common2.uniq_eff (files @ explicit_files)
  [@@profiling]

(*e: function [[Main_semgrep_core.get_final_files]] *)

(*s: function [[Main_semgrep_core.iter_generic_ast_of_files_and_get_matches_and_exn_to_errors]] *)
let iter_files_and_get_matches_and_exn_to_errors f files =
  files
  |> map (fun file ->
         logger#info "Analyzing %s" file;
         let res, run_time =
           Common.with_time (fun () ->
               try
                 run_with_memory_limit !max_memory (fun () ->
                     timeout_function file (fun () ->
                         f file |> fun v ->
                         (* This is just to test -max_memory, to give a chance
                          * to Gc.create_alarm to run even if the program does
                          * not even need to run the Gc. However, this has a slow
                          * perf penality on small programs, which is why it's
                          * better to keep guarded when you're
                          * not testing -max_memory.
                          *)
                         if !test then Gc.full_major ();
                         logger#info "done with %s" file;
                         v))
               with
               (* note that Error_code.exn_to_error already handles Timeout
                * and would generate a TimeoutError code for it, but we intercept
                * Timeout here to give a better diagnostic.
                *)
               | (Timeout | Out_of_memory) as exn ->
                   let str_opt =
                     match !Match_patterns.last_matched_rule with
                     | None -> None
                     | Some rule ->
                         logger#info "critical exn while matching ruleid %s"
                           rule.MR.id;
                         logger#info "full pattern is: %s"
                           rule.MR.pattern_string;
                         Some (spf " with ruleid %s" rule.MR.id)
                   in
                   let loc = Parse_info.first_loc_of_file file in
                   {
                     RP.matches = [];
                     errors =
                       [
                         Error_code.mk_error_loc loc
                           ( match exn with
                           | Timeout ->
                               logger#info "Timeout on %s" file;
                               Error_code.Timeout str_opt
                           | Out_of_memory ->
                               logger#info "OutOfMemory on %s" file;
                               Error_code.OutOfMemory str_opt
                           | _ -> raise Impossible );
                       ];
                     profiling = RP.empty_partial_profiling file;
                   }
               | exn when not !fail_fast ->
                   {
                     RP.matches = [];
                     errors = [ Error_code.exn_to_error file exn ];
                     profiling = RP.empty_partial_profiling file;
                   })
         in
         RP.add_run_time run_time res)

(*e: function [[Main_semgrep_core.iter_generic_ast_of_files_and_get_matches_and_exn_to_errors]] *)

(*s: function [[Main_semgrep_core.print_matches_and_errors]] *)
(*e: function [[Main_semgrep_core.print_matches_and_errors]] *)
(*s: function [[Main_semgrep_core.format_output_exception]] *)
(*e: function [[Main_semgrep_core.format_output_exception]] *)

(*****************************************************************************)
(* xLang *)
(*****************************************************************************)

(* coupling: Parse_mini_rule.parse_languages *)
let xlang_of_string s =
  match s with
  | "none" | "regex" -> R.LNone
  | "generic" -> R.LGeneric
  | _ ->
      let lang = lang_of_string s in
      R.L (lang, [])

let xlang_files_of_dirs_or_files xlang files_or_dirs =
  match xlang with
  | R.LNone | R.LGeneric ->
      (* TODO: assert is_file ? spacegrep filter files?
       * Anyway right now the Semgrep python wrapper is
       * calling -config with an explicit list of files.
       *)
      files_or_dirs
  | R.L (lang, _) ->
      files_of_dirs_with_lang_and_explicit_files lang files_or_dirs

(*****************************************************************************)
(* Semgrep -rules_file *)
(*****************************************************************************)
(* This is the main function used by the semgrep python wrapper right now.
 * It takes a language, a set of mini rules (rules with a single pattern,
 * no formula) and a set of files or dirs and recursively process those
 * files or dirs.
 *)
(*s: function [[Main_semgrep_core.semgrep_with_rules]] *)
let semgrep_with_patterns lang (rules, rule_parse_time) files_or_dirs =
  let files = files_of_dirs_with_lang_and_explicit_files lang files_or_dirs in
  logger#info "processing %d files" (List.length files);
  let file_results =
    files
    |> iter_files_and_get_matches_and_exn_to_errors (fun file ->
           let (ast, errors), parse_time =
             Common.with_time (fun () -> parse_generic lang file)
           in
           let (matches, errors), match_time =
             Common.with_time (fun () ->
                 let rules =
                   rules |> List.filter (fun r -> List.mem lang r.MR.languages)
                 in
                 ( Match_patterns.check
                     ~hook:(fun _ _ -> ())
                     Config_semgrep.default_config rules (parse_equivalences ())
                     (file, lang, ast),
                   errors ))
           in
           { RP.matches; errors; profiling = { file; parse_time; match_time } })
  in
  let res = RP.make_rule_result file_results !report_time rule_parse_time in
  logger#info "found %d matches and %d errors"
    (List.length res.RP.matches)
    (List.length res.RP.errors);
  let matches, new_errors =
    filter_files_with_too_many_matches_and_transform_as_timeout res.RP.matches
  in
  let errors = new_errors @ res.RP.errors in
  let res = { RP.matches; errors; rule_profiling = res.RP.rule_profiling } in
  (* note: uncomment the following and use semgrep-core -stat_matches
   * to debug too-many-matches issues.
   * Common2.write_value matches "/tmp/debug_matches";
   *)
  let res = JSON_report.match_results_of_matches_and_errors files res in
  (* TODO need change type match_results.time to a choice
     let res =
       if !profile then (
         let json = JSON_report.json_of_profile_info !profile_start in
         (* so we don't get also the profile output of Common.main_boilerplate*)
         Common.profile := Common.ProfNone;
         flds @ [ ("profiling", json) ] )
       else flds
     in
  *)
  (*
     Not pretty-printing the json output (Yojson.Safe.prettify)
     because it kills performance, adding an extra 50% time on our
     calculate_ci_perf.py benchmarks.
     User should use an external tool like jq or ydump (latter comes with
     yojson) for pretty-printing json.
  *)
  let s = SJ.string_of_match_results res in
  logger#info "size of returned JSON string: %d" (String.length s);
  pr s

(*e: function [[Main_semgrep_core.semgrep_with_rules]] *)

let semgrep_with_patterns_file lang rules_file files_or_dirs =
  try
    (*s: [[Main_semgrep_core.semgrep_with_rules()]] if [[verbose]] *)
    logger#info "Parsing %s" rules_file;
    (*e: [[Main_semgrep_core.semgrep_with_rules()]] if [[verbose]] *)
    let timed_rules =
      Common.with_time (fun () -> Parse_mini_rule.parse rules_file)
    in
    semgrep_with_patterns lang timed_rules files_or_dirs;
    if !profile then save_rules_file_in_tmp ()
  with exn ->
    logger#debug "exn before exit %s" (Common.exn_to_s exn);
    (* if !Flag.debug then save_rules_file_in_tmp (); *)
    let json = JSON_report.json_of_exn exn in
    let s = J.string_of_json json in
    pr s;
    exit 2

(*****************************************************************************)
(* Semgrep -config *)
(*****************************************************************************)

let semgrep_with_rules (rules, rule_parse_time) files_or_dirs =
  (* todo: at some point we should infer the lang from the rules and
   * apply different rules with different languages and different files
   * automatically, like the semgrep python wrapper.
   *
   * For now python wrapper passes down all files that should be scanned
   *)
  let xlang = xlang_of_string !lang in
  let files = xlang_files_of_dirs_or_files xlang files_or_dirs in
  logger#info "processing %d files" (List.length files);

  let file_results =
    files
    |> iter_files_and_get_matches_and_exn_to_errors (fun file ->
           let rules =
             rules
             |> List.filter (fun r ->
                    match (r.R.languages, xlang) with
                    | R.L (x, xs), R.L (lang, _) -> List.mem lang (x :: xs)
                    | R.LNone, R.LNone | R.LGeneric, R.LGeneric -> true
                    | _ -> false)
           in
           let hook str env matched_tokens =
             if !output_format = Text then
               let xs = Lazy.force matched_tokens in
               print_match ~str !mvars env Metavariable.ii_of_mval xs
           in
           let lazy_ast_and_errors =
             lazy
               ( match xlang with
               | R.L (lang, _) -> parse_generic lang file
               | R.LNone | R.LGeneric ->
                   failwith "requesting generic AST for LNone|LGeneric" )
           in
           let res =
             Match_rules.check hook Config_semgrep.default_config rules
               (parse_equivalences ())
               (file, xlang, lazy_ast_and_errors)
           in
           RP.add_file file res)
  in
  let res = RP.make_rule_result file_results !report_time rule_parse_time in
  logger#info "found %d matches and %d errors" (List.length res.matches)
    (List.length res.errors);
  let matches, new_errors =
    filter_files_with_too_many_matches_and_transform_as_timeout res.matches
  in
  let errors = new_errors @ res.errors in
  let res = { RP.matches; errors; rule_profiling = res.RP.rule_profiling } in
  (* note: uncomment the following and use semgrep-core -stat_matches
   * to debug too-many-matches issues.
   * Common2.write_value matches "/tmp/debug_matches";
   *)
  match !output_format with
  | Json ->
      let res = JSON_report.match_results_of_matches_and_errors files res in
      (* TODO
         let flds =
           if !profile then (
             let json = JSON_report.json_of_profile_info !profile_start in
             (* so we don't get also the profile output of Common.main_boilerplate*)
             Common.profile := Common.ProfNone;
             flds @ [ ("profiling", json) ] )
           else flds
         in
      *)
      let s = SJ.string_of_match_results res in
      logger#info "size of returned JSON string: %d" (String.length s);
      pr s
  | Text ->
      (* the match has already been printed above. We just print errors here *)
      (* pr (spf "number of errors: %d" (List.length errs)); *)
      errors |> List.iter (fun err -> pr (E.string_of_error err))

let semgrep_with_rules_file rules_file files_or_dirs =
  try
    logger#info "Parsing %s" rules_file;
    let timed_rules =
      Common.with_time (fun () -> Parse_rule.parse rules_file)
    in
    semgrep_with_rules timed_rules files_or_dirs
  with exn when !output_format = Json ->
    logger#debug "exn before exit %s" (Common.exn_to_s exn);
    let json = JSON_report.json_of_exn exn in
    let s = J.string_of_json json in
    pr s;
    exit 2

(*****************************************************************************)
(* Semgrep -e/-f *)
(*****************************************************************************)

let rule_of_pattern lang pattern_string pattern =
  {
    MR.id = "-e/-f";
    pattern_string;
    pattern;
    message = "";
    severity = MR.Error;
    languages = [ lang ];
  }

(*s: function [[Main_semgrep_core.sgrep_ast]] *)
(*s: [[Main_semgrep_core.sgrep_ast()]] [[hook]] argument to [[check]] *)
(*e: [[Main_semgrep_core.sgrep_ast()]] [[hook]] argument to [[check]] *)
(*s: [[Main_semgrep_core.sgrep_ast()]] match [[pattern]] and [[any_ast]] other cases *)
(*e: [[Main_semgrep_core.sgrep_ast()]] match [[pattern]] and [[any_ast]] other cases *)

(*e: function [[Main_semgrep_core.sgrep_ast]] *)

(*s: function [[Main_semgrep_core.semgrep_with_one_pattern]] *)
(* simpler code path compared to semgrep_with_rules *)
let semgrep_with_one_pattern lang xs =
  (* old: let xs = List.map Common.fullpath xs in
   * better no fullpath here, not our responsability.
   *)
  let pattern, pattern_string =
    match (!pattern_file, !pattern_string) with
    (*s: [[Main_semgrep_core.semgrep_with_one_pattern()]] sanity check cases *)
    | "", "" -> failwith "I need a pattern; use -f or -e"
    | s1, s2 when s1 <> "" && s2 <> "" ->
        failwith "I need just one pattern; use -f OR -e (not both)"
    (*e: [[Main_semgrep_core.semgrep_with_one_pattern()]] sanity check cases *)
    (*s: [[Main_semgrep_core.semgrep_with_one_pattern()]] pattern file case *)
    | file, _ when file <> "" ->
        let s = Common.read_file file in
        (parse_pattern lang s, s)
    (*e: [[Main_semgrep_core.semgrep_with_one_pattern()]] pattern file case *)
    (* this is for Emma, who often confuses -e with -f :) *)
    | _, s when s =~ ".*\\.sgrep$" ->
        failwith "you probably want -f with a .sgrep file, not -e"
    | _, s when s <> "" -> (parse_pattern lang s, s)
    | _ -> raise Impossible
  in
  let rule, rule_parse_time =
    Common.with_time (fun () -> [ rule_of_pattern lang pattern_string pattern ])
  in

  match !output_format with
  | Json ->
      (* closer to -rules_file, but no incremental match output *)
      semgrep_with_patterns lang (rule, rule_parse_time) xs
  | Text ->
      (* simpler code path than in semgrep_with_rules *)
      let files = Lang.files_of_dirs_or_files lang xs in
      (*s: [[Main_semgrep_core.semgrep_with_one_pattern()]] no [[lang]] specified *)
      (*e: [[Main_semgrep_core.semgrep_with_one_pattern()]] no [[lang]] specified *)
      (*s: [[Main_semgrep_core.semgrep_with_one_pattern()]] filter [[files]] *)
      (*e: [[Main_semgrep_core.semgrep_with_one_pattern()]] filter [[files]] *)
      files
      |> List.iter (fun file ->
             (*s: [[Main_semgrep_core.semgrep_with_one_pattern()]] if [[verbose]] *)
             logger#info "processing: %s" file;
             (*e: [[Main_semgrep_core.semgrep_with_one_pattern()]] if [[verbose]] *)
             let process file =
               timeout_function file (fun () ->
                   let ast, errors = parse_generic lang file in
                   if errors <> [] then
                     pr2 (spf "WARNING: fail to fully parse %s" file);
                   Match_patterns.check
                     ~hook:(fun env matched_tokens ->
                       let xs = Lazy.force matched_tokens in
                       print_match !mvars env Metavariable.ii_of_mval xs)
                     Config_semgrep.default_config rule (parse_equivalences ())
                     (file, lang, ast)
                   |> ignore)
             in

             if not !error_recovery then
               E.try_with_print_exn_and_reraise file (fun () -> process file)
             else E.try_with_exn_to_error file (fun () -> process file));

      (*s: [[Main_semgrep_core.semgrep_with_one_pattern()]] display error count *)
      let n = List.length !E.g_errors in
      if n > 0 then pr2 (spf "error count: %d" n);
      (*e: [[Main_semgrep_core.semgrep_with_one_pattern()]] display error count *)
      (*s: [[Main_semgrep_core.semgrep_with_one_pattern()]] optional layer generation *)
      Experiments.gen_layer_maybe _matching_tokens pattern_string xs

(*e: [[Main_semgrep_core.semgrep_with_one_pattern()]] optional layer generation *)

(*e: function [[Main_semgrep_core.semgrep_with_one_pattern]] *)

(*****************************************************************************)
(* Semgrep -tainting_rules_file *)
(*****************************************************************************)

module TR = Tainting_rule

(*s: function [[Main_semgrep_core.tainting_with_rules]] *)
let tainting_with_rules lang rules_file files_or_dirs =
  try
    logger#info "Parsing %s" rules_file;
    let rules = Parse_tainting_rules.parse rules_file in

    let files = files_of_dirs_with_lang_and_explicit_files lang files_or_dirs in
    let file_results =
      files
      |> iter_files_and_get_matches_and_exn_to_errors (fun file ->
             let ast, errors = parse_generic lang file in
             let rules =
               rules |> List.filter (fun r -> List.mem lang r.TR.languages)
             in
             {
               matches = Tainting_generic.check rules file ast;
               errors;
               profiling = RP.empty_partial_profiling file (* TODO? *);
             })
    in
    let res =
      RP.make_rule_result file_results ~report_time:false ~rule_parse_time:0.0
    in
    let res = JSON_report.match_results_of_matches_and_errors files res in
    let s = SJ.string_of_match_results res in
    pr s
  with exn ->
    let json = JSON_report.json_of_exn exn in
    let s = J.string_of_json json in
    pr s;
    exit 2

(*e: function [[Main_semgrep_core.tainting_with_rules]] *)

(*****************************************************************************)
(* Checker *)
(*****************************************************************************)
(*s: function [[Main_semgrep_core.read_all]] *)
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

(*e: function [[Main_semgrep_core.read_all]] *)

(*s: function [[Main_semgrep_core.validate_pattern]] *)
(* works with -lang *)
let validate_pattern () =
  let chan = stdin in
  let s = read_all chan in
  try
    let lang = lang_of_string !lang in
    let _ = parse_pattern lang s in
    exit 0
  with _exn -> exit 1

(*e: function [[Main_semgrep_core.validate_pattern]] *)

(* See also Check_rule.check_files *)

(*****************************************************************************)
(* Dumpers *)
(*****************************************************************************)

(* used for the Dump AST in semgrep.live *)
(*s: function [[Main_semgrep_core.json_of_v]] *)
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
        | _ -> J.Object [ (s, J.Array (List.map aux xs)) ] )
    | OCaml.VVar (s, i64) -> J.String (spf "%s_%d" s (Int64.to_int i64))
    | OCaml.VArrow _ -> failwith "Arrow TODO"
    | OCaml.VNone -> J.Null
    | OCaml.VSome v -> J.Object [ ("some", aux v) ]
    | OCaml.VRef v -> J.Object [ ("ref@", aux v) ]
    | OCaml.VList xs -> J.Array (List.map aux xs)
    | OCaml.VTODO _ -> J.String "VTODO"
  in
  aux v

(*e: function [[Main_semgrep_core.json_of_v]] *)

(*s: function [[Main_semgrep_core.dump_v_to_format]] *)
let dump_v_to_format (v : OCaml.v) =
  match !output_format with
  | Text -> OCaml.string_of_v v
  | Json -> J.string_of_json (json_of_v v)

(*e: function [[Main_semgrep_core.dump_v_to_format]] *)

(*s: function [[Main_semgrep_core.dump_pattern]] *)
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

(*e: function [[Main_semgrep_core.dump_pattern]] *)

(*s: function [[Main_semgrep_core.dump_ast]] *)
let dump_ast ?(naming = false) file =
  match Lang.langs_of_filename file with
  | lang :: _ ->
      E.try_with_print_exn_and_reraise file (fun () ->
          let { Parse_target.ast; errors; _ } =
            if naming then
              Parse_target.parse_and_resolve_name_use_pfff_or_treesitter lang
                file
            else Parse_target.just_parse_with_lang lang file
          in
          let v = Meta_AST.vof_any (AST_generic.Pr ast) in
          let s = dump_v_to_format v in
          pr s;
          if errors <> [] then pr2 (spf "WARNING: fail to fully parse %s" file))
  | [] -> failwith (spf "unsupported language for %s" file)

(*e: function [[Main_semgrep_core.dump_ast]] *)
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

(*s: function [[Main_semgrep_core.dump_ext_of_lang]] *)
let dump_ext_of_lang () =
  let lang_to_exts =
    keys
    |> List.map (fun lang_str ->
           match Lang.lang_of_string_opt lang_str with
           | Some lang ->
               lang_str ^ "->" ^ String.concat ", " (Lang.ext_of_lang lang)
           | None -> "")
  in
  pr2
    (spf "Language to supported file extension mappings:\n %s"
       (String.concat "\n" lang_to_exts))

(*e: function [[Main_semgrep_core.dump_ext_of_lang]] *)

(*s: function [[Main_semgrep_core.dump_equivalences]] *)
let dump_equivalences file =
  let xs = Parse_equivalences.parse file in
  pr2_gen xs

(*e: function [[Main_semgrep_core.dump_equivalences]] *)

(*s: function [[Main_semgrep_core.dump_tainting_rules]] *)
let dump_tainting_rules file =
  let xs = Parse_tainting_rules.parse file in
  pr2_gen xs

(*e: function [[Main_semgrep_core.dump_tainting_rules]] *)

let dump_rule file =
  let rules = Parse_rule.parse file in
  rules |> List.iter (fun r -> pr (Rule.show r))

(*****************************************************************************)
(* Experiments *)
(*****************************************************************************)
(* See Experiments.ml now *)

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

(*s: function [[Main_semgrep_core.all_actions]] *)
let all_actions () =
  [
    (*s: [[Main_semgrep_core.all_actions]] dumper cases *)
    ( "-dump_extensions",
      " print file extension to language mapping",
      Common.mk_action_0_arg dump_ext_of_lang );
    (*x: [[Main_semgrep_core.all_actions]] dumper cases *)
    ("-dump_pattern", " <file>", Common.mk_action_1_arg dump_pattern);
    (*x: [[Main_semgrep_core.all_actions]] dumper cases *)
    ("-dump_ast", " <file>", Common.mk_action_1_arg (dump_ast ~naming:false));
    ( "-dump_named_ast",
      " <file>",
      Common.mk_action_1_arg (dump_ast ~naming:true) );
    ("-dump_v1_json", " <file>", Common.mk_action_1_arg dump_v1_json);
    (*x: [[Main_semgrep_core.all_actions]] dumper cases *)
    ("-dump_equivalences", " <file>", Common.mk_action_1_arg dump_equivalences);
    (*x: [[Main_semgrep_core.all_actions]] dumper cases *)
    ( "-dump_tainting_rules",
      " <file>",
      Common.mk_action_1_arg dump_tainting_rules );
    (*e: [[Main_semgrep_core.all_actions]] dumper cases *)
    ("-dump_rule", " <file>", Common.mk_action_1_arg dump_rule);
    ( "-dump_tree_sitter_cst",
      " <file>",
      Common.mk_action_1_arg Test_parsing.dump_tree_sitter_cst );
    ( "-dump_tree_sitter_pattern_cst",
      " <file>",
      Common.mk_action_1_arg (fun file ->
          Parse_pattern.dump_tree_sitter_pattern_cst (lang_of_string !lang) file)
    );
    ( "-dump_ast_pfff",
      " <file>",
      Common.mk_action_1_arg Test_parsing.dump_ast_pfff );
    ("-dump_il", " <file>", Common.mk_action_1_arg Datalog_experiment.dump_il);
    ( "-diff_pfff_tree_sitter",
      " <file>",
      Common.mk_action_n_arg Test_parsing.diff_pfff_tree_sitter );
    (*s: [[Main_semgrep_core.all_actions]] other cases *)
    ( "--validate-pattern-stdin",
      " check the syntax of a pattern ",
      Common.mk_action_0_arg validate_pattern );
    (*e: [[Main_semgrep_core.all_actions]] other cases *)
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
      " <files or dirs>",
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

(*e: function [[Main_semgrep_core.all_actions]] *)

(*s: function [[Main_semgrep_core.options]] *)
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
      spf " <str> choose language (valid choices:\n     %s)" supported_langs );
    ( "-target_file",
      Arg.Set_string target_file,
      " <file> obtain list of targets to run patterns on" );
    (*s: [[Main_semgrep_core.options]] user-defined equivalences case *)
    ( "-equivalences",
      Arg.Set_string equivalences_file,
      " <file> obtain list of code equivalences from YAML file" );
    (*e: [[Main_semgrep_core.options]] user-defined equivalences case *)
    (*s: [[Main_semgrep_core.options]] file filters cases *)
    (*e: [[Main_semgrep_core.options]] file filters cases *)
    (*s: [[Main_semgrep_core.options]] [[-j]] case *)
    ("-j", Arg.Set_int ncores, " <int> number of cores to use (default = 1)");
    (*e: [[Main_semgrep_core.options]] [[-j]] case *)
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
    ( "-no_gc_tuning",
      Arg.Clear Flag.gc_tuning,
      " use OCaml's default garbage collector settings" );
    (*s: [[Main_semgrep_core.options]] report match mode cases *)
    ( "-emacs",
      Arg.Unit (fun () -> match_format := Matching_report.Emacs),
      " print matches on the same line than the match position" );
    ( "-oneline",
      Arg.Unit (fun () -> match_format := Matching_report.OneLine),
      " print matches on one line, in normalized form" );
    (*x: [[Main_semgrep_core.options]] report match mode cases *)
    ("-json", Arg.Unit (fun () -> output_format := Json), " output JSON format");
    ( "-json_time",
      Arg.Unit
        (fun () ->
          output_format := Json;
          report_time := true),
      " report detailed matching times as part of the JSON response. Implies \
       '-json'." );
    (*e: [[Main_semgrep_core.options]] report match mode cases *)
    (*s: [[Main_semgrep_core.options]] other cases *)
    ( "-pvar",
      Arg.String (fun s -> mvars := Common.split "," s),
      " <metavars> print the metavariables, not the matched code" );
    (*x: [[Main_semgrep_core.options]] other cases *)
    ( "-gen_layer",
      Arg.String (fun s -> Experiments.layer_file := Some s),
      " <file> save result in a codemap layer file" );
    (*x: [[Main_semgrep_core.options]] other cases *)
    ( "-tainting_rules_file",
      Arg.Set_string tainting_rules_file,
      " <file> obtain source/sink/sanitizer patterns from YAML file" );
    (*x: [[Main_semgrep_core.options]] other cases *)
    ( "-error_recovery",
      Arg.Unit
        (fun () ->
          error_recovery := true;
          Flag_parsing.error_recovery := true),
      " do not stop at first parsing error with -e/-f" );
    ( "-fail_fast",
      Arg.Set fail_fast,
      " stop at first exception (and get a backtrace)" );
    (*e: [[Main_semgrep_core.options]] other cases *)
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
      Arg.Set_int max_memory,
      " <int> maximum memory available (in MB); allows for clean termination\n\
      \     when running out of memory. This value should be less than the \
       actual\n\
      \     memory available because the limit will be exceeded before it gets\n\
      \     detected. Try 5% less or 15000 if you have 16 GB." );
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
    ( "-lsp",
      Arg.Unit (fun () -> LSP_client.init ()),
      " connect to LSP lang server to get type information" );
  ]
  (*s: [[Main_semgrep_core.options]] concatenated flags *)
  @ Flag_parsing_cpp.cmdline_flags_macrofile ()
  (*x: [[Main_semgrep_core.options]] concatenated flags *)
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
    ]
  (*x: [[Main_semgrep_core.options]] concatenated flags *)
  @ Meta_parse_info.cmdline_flags_precision ()
  (*x: [[Main_semgrep_core.options]] concatenated flags *)
  @ Error_code.options ()
  (*e: [[Main_semgrep_core.options]] concatenated flags *)
  (*s: [[Main_semgrep_core.options]] concatenated actions *)
  @ Common.options_of_actions action (all_actions ())
  (*e: [[Main_semgrep_core.options]] concatenated actions *)
  @ [
      ( "-version",
        Arg.Unit
          (fun () ->
            pr2 version;
            exit 0),
        "  guess what" );
    ]

(*e: function [[Main_semgrep_core.options]] *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(*s: function [[Main_semgrep_core.main]] *)
let main () =
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
  if !Flag.gc_tuning then set_gc ();
  let args = if !target_file = "" then args else Common.cat !target_file in

  if Sys.file_exists !log_config_file then (
    Logging.load_config_file !log_config_file;
    logger#info "loaded %s" !log_config_file );
  if !debug then (
    let open Easy_logging in
    let h = Handlers.make (CliErr Debug) in
    logger#add_handler h;
    logger#set_level Debug;
    () );

  logger#info "Executed as: %s" (Sys.argv |> Array.to_list |> String.concat " ");
  logger#info "Version: %s" version;
  if !profile then (
    logger#info "Profile mode On";
    logger#info "disabling -j when in profiling mode";
    ncores := 1 );

  (* must be done after Arg.parse, because Common.profile is set by it *)
  Common.profile_code "Main total" (fun () ->
      match args with
      (*s: [[Main_semgrep_core.main()]] match [[args]] actions *)
      (* --------------------------------------------------------- *)
      (* actions, useful to debug subpart *)
      (* --------------------------------------------------------- *)
      | xs when List.mem !action (Common.action_list (all_actions ())) ->
          Common.do_action !action xs (all_actions ())
      | _ when not (Common.null_string !action) ->
          failwith ("unrecognized action or wrong params: " ^ !action)
      (*e: [[Main_semgrep_core.main()]] match [[args]] actions *)
      (* --------------------------------------------------------- *)
      (* main entry *)
      (* --------------------------------------------------------- *)
      | x :: xs -> (
          match () with
          | _ when !config_file <> "" ->
              semgrep_with_rules_file !config_file (x :: xs)
          (*s: [[Main_semgrep_core.main()]] main entry match cases *)
          | _ when !rules_file <> "" ->
              let lang = lang_of_string !lang in
              semgrep_with_patterns_file lang !rules_file (x :: xs)
          (*x: [[Main_semgrep_core.main()]] main entry match cases *)
          | _ when !tainting_rules_file <> "" ->
              let lang = lang_of_string !lang in
              tainting_with_rules lang !tainting_rules_file (x :: xs)
          (*e: [[Main_semgrep_core.main()]] main entry match cases *)
          (*s: [[Main_semgrep_core.main()]] main entry match cases default case *)
          | _ ->
              let lang = lang_of_string !lang in
              semgrep_with_one_pattern lang (x :: xs)
              (*e: [[Main_semgrep_core.main()]] main entry match cases default case *)
          )
      (* --------------------------------------------------------- *)
      (* empty entry *)
      (* --------------------------------------------------------- *)
      (* TODO: should not need that, semgrep should not call us when there
       * are no files to process. *)
      | [] when !target_file <> "" && !config_file <> "" ->
          semgrep_with_rules_file !config_file []
      | [] -> Common.usage usage_msg (options ()))

(*e: function [[Main_semgrep_core.main]] *)

(*****************************************************************************)
(*s: toplevel [[Main_semgrep_core._1]] *)
let _ =
  Common.main_boilerplate (fun () ->
      Common.finalize
        (fun () -> main ())
        (fun () -> !Hooks.exit |> List.iter (fun f -> f ())))

(*e: toplevel [[Main_semgrep_core._1]] *)
(*e: semgrep/CLI/Main.ml *)
