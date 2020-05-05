(*s: semgrep/bin/main_semgrep_core.ml *)
(*
 * The author disclaims copyright to this source code.  In place of
 * a legal notice, here is a blessing:
 *
 *    May you do good and not evil.
 *    May you find forgiveness for yourself and forgive others.
 *    May you share freely, never taking more than you give.
 *)
open Common

module Flag = Flag_parsing
module PI = Parse_info
module S = Scope_code
module E = Error_code
module R = Rule
module J = Json_type

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* A syntactical grep. https://sgrep.dev/
 * Right now there is good support for Python, Javascript, Java, Go, and C
 * and partial support for PHP, C++, and OCaml.
 * 
 * opti: git grep foo | xargs sgrep -e 'foo(...)'
 * 
 * related: 
 *  - Structural Search and Replace (SSR) in Jetbrains IDE
 *    http://www.jetbrains.com/idea/documentation/ssr.html
 *    http://tv.jetbrains.net/videocontent/intellij-idea-static-analysis-custom-rules-with-structural-search-replace
 *  - gogrep: https://github.com/mvdan/gogrep/
 *  - phpgrep: https://github.com/quasilyte/phpgrep
 *    https://github.com/VKCOM/noverify/blob/master/docs/dynamic-rules.md
 *    https://speakerdeck.com/quasilyte/phpgrep-syntax-aware-code-search
 *  - rubocop pattern
 *    https://github.com/marcandre/rubocop/blob/master/manual/node_pattern.md
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

(*s: constant [[Main_semgrep_core.verbose]] *)
let verbose = ref false
(*e: constant [[Main_semgrep_core.verbose]] *)
(*s: constant [[Main_semgrep_core.debug]] *)
let debug = ref false
(*e: constant [[Main_semgrep_core.debug]] *)
(*s: constant [[Main_semgrep_core.error_recovery]] *)
(* try to continue processing files, even if one has a parse error with -e/f.
 * note that -rules_file does its own error recovery.
 *)
let error_recovery = ref false
(*e: constant [[Main_semgrep_core.error_recovery]] *)

(*s: constant [[Main_semgrep_core.pattern_string]] *)
(* -e *)
let pattern_string = ref ""
(*e: constant [[Main_semgrep_core.pattern_string]] *)
(*s: constant [[Main_semgrep_core.pattern_file]] *)
(* -f *)
let pattern_file = ref ""
(*e: constant [[Main_semgrep_core.pattern_file]] *)
(*s: constant [[Main_semgrep_core.rules_file]] *)
(* -rules_file *)
let rules_file = ref ""
(*e: constant [[Main_semgrep_core.rules_file]] *)
(*s: constant [[Main_semgrep_core.tainting_rules_file]] *)
(* -tainting_rules_file *)
let tainting_rules_file = ref ""
(*e: constant [[Main_semgrep_core.tainting_rules_file]] *)

(*s: constant [[Main_semgrep_core.equivalences_file]] *)
let equivalences_file = ref ""
(*e: constant [[Main_semgrep_core.equivalences_file]] *)

(*s: constant [[Main_semgrep_core.lang]] *)
(* todo: infer from basename argv(0) ? *)
let lang = ref "unset"
(*e: constant [[Main_semgrep_core.lang]] *)

(*s: constant [[Main_semgrep_core.excludes]] *)
(* similar to grep options (see man grep) *)
let excludes = ref []
(*e: constant [[Main_semgrep_core.excludes]] *)
(*s: constant [[Main_semgrep_core.includes]] *)
let includes = ref []
(*e: constant [[Main_semgrep_core.includes]] *)
(*s: constant [[Main_semgrep_core.exclude_dirs]] *)
let exclude_dirs = ref []
(*e: constant [[Main_semgrep_core.exclude_dirs]] *)

(*s: constant [[Main_semgrep_core.output_format_json]] *)
let output_format_json = ref false
(*e: constant [[Main_semgrep_core.output_format_json]] *)
(*s: constant [[Main_semgrep_core.match_format]] *)
let match_format = ref Matching_report.Normal
(*e: constant [[Main_semgrep_core.match_format]] *)

(*s: constant [[Main_semgrep_core.mvars]] *)
let mvars = ref ([]: Metavars_fuzzy.mvar list)
(*e: constant [[Main_semgrep_core.mvars]] *)

(*s: constant [[Main_semgrep_core.layer_file]] *)
let layer_file = ref (None: filename option)
(*e: constant [[Main_semgrep_core.layer_file]] *)

(*s: constant [[Main_semgrep_core.keys]] *)
let keys = Common2.hkeys Lang.lang_of_string_map
(*e: constant [[Main_semgrep_core.keys]] *)
(*s: constant [[Main_semgrep_core.supported_langs]] *)
let supported_langs: string = String.concat ", " keys
(*e: constant [[Main_semgrep_core.supported_langs]] *)

(*s: constant [[Main_semgrep_core.ncores]] *)
(* -j *)
let ncores = ref 1
(*e: constant [[Main_semgrep_core.ncores]] *)

(*s: constant [[Main_semgrep_core.action]] *)
(* action mode *)
let action = ref ""
(*e: constant [[Main_semgrep_core.action]] *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: function [[Main_semgrep_core.set_gc]] *)
let set_gc () =
(*
  if !Flag.debug_gc
  then Gc.set { (Gc.get()) with Gc.verbose = 0x01F };
*)
  (* only relevant in bytecode, in native the stacklimit is the os stacklimit,
   * which usually requires a ulimit -s 40000
  *)
  Gc.set {(Gc.get ()) with Gc.stack_limit = 1000 * 1024 * 1024};
  (* see www.elehack.net/michael/blog/2010/06/ocaml-memory-tuning *)
  Gc.set { (Gc.get()) with Gc.minor_heap_size = 4_000_000 };
  Gc.set { (Gc.get()) with Gc.major_heap_increment = 8_000_000 };
  Gc.set { (Gc.get()) with Gc.space_overhead = 300 };
  ()
(*e: function [[Main_semgrep_core.set_gc]] *)

(*s: function [[Main_semgrep_core.map]] *)
let map f xs =
  if !ncores <= 1
  then List.map f xs
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

(*s: function [[Main_semgrep_core.mk_one_info_from_multiple_infos]] *)
(* TODO? could do slicing of function relative to the pattern, so 
 * would see where the parameters come from :)
 *)

let mk_one_info_from_multiple_infos xs =
  List.hd xs
(*e: function [[Main_semgrep_core.mk_one_info_from_multiple_infos]] *)

(*s: constant [[Main_semgrep_core._matching_tokens]] *)
(* for -gen_layer *)
let _matching_tokens = ref []
(*e: constant [[Main_semgrep_core._matching_tokens]] *)

(*s: function [[Main_semgrep_core.print_match]] *)
let print_match mvars mvar_binding ii_of_any tokens_matched_code = 
  (* there are a few fake tokens in the generic ASTs now (e.g., 
   * for DotAccess generated outside the grammar) *)
  let toks = tokens_matched_code |> List.filter PI.is_origintok in
  (match mvars with
  | [] ->
      Matching_report.print_match ~format:!match_format toks
  | xs ->
      (* similar to the code of Lib_matcher.print_match, maybe could
       * factorize code a bit.
       *)
      let (mini, _maxi) = 
        PI.min_max_ii_by_pos toks in
      let (file, line) = 
        PI.file_of_info mini, PI.line_of_info mini in

      let strings_metavars =
        xs |> List.map (fun x ->
          match Common2.assoc_opt x mvar_binding with
          | Some any ->
              ii_of_any any
              |> List.map PI.str_of_info 
              |> Matching_report.join_with_space_if_needed
          | None ->
              failwith (spf "the metavariable '%s' was not binded" x)
          )
      in
      pr (spf "%s:%d: %s" file line (Common.join ":" strings_metavars));
  );
  toks |> List.iter (fun x -> Common.push x _matching_tokens)
(*e: function [[Main_semgrep_core.print_match]] *)

(*s: function [[Main_semgrep_core.print_simple_match]] *)
let print_simple_match tokens_matched_code =
  print_match [] [] tokens_matched_code
(*e: function [[Main_semgrep_core.print_simple_match]] *)


(*s: function [[Main_semgrep_core.gen_layer]] *)
(* a layer need readable path, hence the ~root argument *)
let gen_layer ~root ~query file =
  ignore(query);
  pr2 ("generating layer in " ^ file);

  let root = Common2.relative_to_absolute root in

  let toks = !_matching_tokens in
  let kinds = ["m" (* match *), "red"] in
  
  (* todo: could now use Layer_code.simple_layer_of_parse_infos *)
  let files_and_lines = toks |> List.map (fun tok ->
    let file = PI.file_of_info tok in
    let line = PI.line_of_info tok in
    let file = Common2.relative_to_absolute file in 
    Common.readable root file, line
  )
  in
  let group = Common.group_assoc_bykey_eff files_and_lines in
  let layer = { Layer_code.
    title = "Sgrep";
    description = "output of sgrep";
    kinds = kinds;
    files = group |> List.map (fun (file, lines) ->
      let lines = Common2.uniq lines in
      (file, { Layer_code.
               micro_level = (lines |> List.map (fun l -> l, "m"));
               macro_level =  if null lines then [] else ["m", 1.];
      })
    );
  }
  in
  Layer_code.save_layer layer file;
  ()
(*e: function [[Main_semgrep_core.gen_layer]] *)

(*s: function [[Main_semgrep_core.parse_generic]] *)
(* coupling: you need also to modify tests/test.ml *)
let parse_generic lang file = 
  if lang = Lang.C && Sys.file_exists !Flag_parsing_cpp.macros_h
  then Parse_cpp.init_defs !Flag_parsing_cpp.macros_h;

  let ast = Parse_generic.parse_with_lang lang file in
  (* to be deterministic, reset the gensym; anyway right now sgrep is
   * used only for local per-file analysis, so no need to have a unique ID
   * among a set of files in a project like codegraph.
   *)
  Ast_generic.gensym_counter := 0;
  Naming_ast.resolve lang ast;
  ast
(*e: function [[Main_semgrep_core.parse_generic]] *)

(*s: function [[Main_semgrep_core.parse_equivalences]] *)
let parse_equivalences () =
  match !equivalences_file with
  | "" -> []
  | file -> Parse_equivalences.parse file
(*e: function [[Main_semgrep_core.parse_equivalences]] *)

(*s: function [[Main_semgrep_core.unsupported_language_message]] *)
let unsupported_language_message some_lang =
  spf "unsupported language: %s; supported language tags are: %s" 
      some_lang supported_langs
(*e: function [[Main_semgrep_core.unsupported_language_message]] *)
  
(*****************************************************************************)
(* Language specific *)
(*****************************************************************************)

(*s: type [[Main_semgrep_core.ast]] *)
type ast =
  | Gen of Ast_generic.program * Lang.t
  | Fuzzy of Ast_fuzzy.trees
  | NoAST
(*e: type [[Main_semgrep_core.ast]] *)

(*s: function [[Main_semgrep_core.create_ast]] *)
let create_ast file =
  match Lang.lang_of_string_opt !lang with
  | Some lang -> Gen (parse_generic lang file, lang)
  | None ->
    (match Lang_fuzzy.lang_of_string_opt !lang with
    | Some _ -> Fuzzy (Parse_fuzzy.parse file)
    | None -> failwith (unsupported_language_message !lang)
    )
(*e: function [[Main_semgrep_core.create_ast]] *)
  

(*s: type [[Main_semgrep_core.pattern]] *)
type pattern =
  | PatFuzzy of Ast_fuzzy.tree list
  | PatGen of Rule.pattern
(*e: type [[Main_semgrep_core.pattern]] *)

(*s: function [[Main_semgrep_core.parse_pattern]] *)
let parse_pattern str =
 try (
  Common.save_excursion Flag_parsing.sgrep_mode true (fun () ->
   match Lang.lang_of_string_opt !lang with
   | Some lang -> PatGen (Check_semgrep.parse_check_pattern lang str)
   | None ->
     (match Lang_fuzzy.lang_of_string_opt !lang with
     | Some lang -> PatFuzzy (Parse_fuzzy.parse_pattern lang str)
     | None -> failwith (unsupported_language_message !lang)
     )
  ))
  with exn ->
      raise (Parse_rules.InvalidPatternException ("no-id", str, !lang, (Common.exn_to_s exn)))
(*e: function [[Main_semgrep_core.parse_pattern]] *)
 

(*s: function [[Main_semgrep_core.sgrep_ast]] *)
let sgrep_ast pattern file any_ast =
  match pattern, any_ast with
  |  _, NoAST -> () (* skipping *)
  | PatGen pattern, Gen (ast, lang) ->
    let rule = { R.
      id = "-e/-f"; pattern; message = ""; severity = R.Error; 
      languages = [lang] 
    } in
    Semgrep_generic.check
      ~hook:(fun env matched_tokens ->
        let xs = Lazy.force matched_tokens in
        print_match !mvars env Lib_ast.ii_of_any xs
      )
      [rule] (parse_equivalences ())
      file ast |> ignore;

  | PatFuzzy pattern, Fuzzy ast ->
    Semgrep_fuzzy.sgrep
      ~hook:(fun env matched_tokens ->
        print_match !mvars env Lib_ast_fuzzy.toks_of_trees matched_tokens
      )
      pattern ast

  | _ ->
    failwith ("unsupported  combination or " ^ (unsupported_language_message !lang))
(*e: function [[Main_semgrep_core.sgrep_ast]] *)

(*****************************************************************************)
(* Iteration helpers *)
(*****************************************************************************)

(*s: function [[Main_semgrep_core.filter_files]] *)
let filter_files files =
  files |> Files_filter.filter (Files_filter.mk_filters
      ~excludes:!excludes 
      ~exclude_dirs:!exclude_dirs 
      ~includes:!includes
  )
(*e: function [[Main_semgrep_core.filter_files]] *)

(*s: function [[Main_semgrep_core.get_final_files]] *)
let get_final_files xs =
  let files =
    match Lang.lang_of_string_opt !lang with
    | None -> Files_finder.files_of_dirs_or_files xs
    | Some lang -> Lang.files_of_dirs_or_files lang xs 
  in
  let files = filter_files files in
  files
(*e: function [[Main_semgrep_core.get_final_files]] *)
  
(*s: function [[Main_semgrep_core.iter_generic_ast_of_files_and_get_matches_and_exn_to_errors]] *)
let iter_generic_ast_of_files_and_get_matches_and_exn_to_errors f files =
  let matches_and_errors = 
    files |> map (fun file ->
       if !verbose then pr2 (spf "Analyzing %s" file);
       try 
         let lang = 
           match Lang.langs_of_filename file with
           | [lang] -> lang
           | x::_xs ->
               (match Lang.lang_of_string_opt !lang with
               | Some lang -> lang
               | None -> 
                 pr2 (spf "no language specified, defaulting to %s for %s"
                      (Lang.string_of_lang x) file);
                 x
               )
           | [] -> 
              failwith (spf "can not extract generic AST from %s" file)
         in
         let ast = parse_generic lang file in
         (* calling the hook *)
         f file lang ast, []
       with exn -> 
         [], [Error_code.exn_to_error file exn]
    )
  in
  let matches = matches_and_errors |> List.map fst |> List.flatten in
  let errors = matches_and_errors |> List.map snd |> List.flatten in  
  matches, errors
(*e: function [[Main_semgrep_core.iter_generic_ast_of_files_and_get_matches_and_exn_to_errors]] *)

(*s: function [[Main_semgrep_core.print_matches_and_errors]] *)
let print_matches_and_errors files matches errs =
  let count_errors = (List.length errs) in
  let count_ok = (List.length files) - count_errors in
  let stats = J.Object [ "okfiles", J.Int count_ok; "errorfiles", J.Int count_errors; ] in
  let json = J.Object [
     "matches", J.Array (matches |> List.map Json_report.match_to_json);
     "errors", J.Array (errs |> List.map R2c.error_to_json);        
     "stats", stats
  ] in
  let s = Json_io.string_of_json json in
  pr s
(*e: function [[Main_semgrep_core.print_matches_and_errors]] *)

(*****************************************************************************)
(* Semgrep -e/-f *)
(*****************************************************************************)
(*s: function [[Main_semgrep_core.sgrep_with_one_pattern]] *)
(* simpler code path compared to sgrep_with_rules *)
let sgrep_with_one_pattern xs =
  (* old: let xs = List.map Common.fullpath xs in
   * better no fullpath here, not our responsability.
   *)

  let pattern, query_string =
    match !pattern_file, !pattern_string with
    | "", "" ->
        failwith "I need a pattern; use -f or -e"
    | s1, s2 when s1 <> "" && s2 <> "" ->
        failwith "I need just one pattern; use -f OR -e (not both)"
    | file, _ when file <> "" ->
        let s = Common.read_file file in
        parse_pattern s, s
    | _, s when s <> ""->
        parse_pattern s, s
    | _ -> raise Impossible
  in
  let files = 
    match Lang.lang_of_string_opt !lang with
    | Some lang -> Lang.files_of_dirs_or_files lang xs
    (* should remove at some point *)
    | None -> Find_source.files_of_dir_or_files ~lang:!lang xs
  in
  let files = filter_files files in
  
  files |> List.iter (fun file ->
    if !verbose 
    then pr2 (spf "processing: %s" file);
    let process file = sgrep_ast pattern file (create_ast file) in
    if not !error_recovery
    then E.try_with_print_exn_and_reraise file (fun () -> process file)
    else E.try_with_exn_to_error file (fun () -> process file)
  );

  let n = List.length !E.g_errors in
  if n > 0 then pr2 (spf "error count: %d" n);

  !layer_file |> Common.do_option (fun file ->
    let root = Common2.common_prefix_of_files_or_dirs xs in
    gen_layer ~root ~query:query_string  file
  );
  ()
(*e: function [[Main_semgrep_core.sgrep_with_one_pattern]] *)

(*****************************************************************************)
(* Semgrep -rules_file *)
(*****************************************************************************)

(*s: function [[Main_semgrep_core.sgrep_with_rules]] *)
(* less: could factorize even more and merge sgrep_with_rules and
 * sgrep_with_one_pattern now.
 *)
let sgrep_with_rules rules_file xs =

  if !verbose then pr2 (spf "Parsing %s" rules_file);
  let rules = Parse_rules.parse rules_file in

  let files = get_final_files xs in
  let matches, errs = 
     files |> iter_generic_ast_of_files_and_get_matches_and_exn_to_errors 
       (fun file lang ast ->
         let rules = 
            rules |> List.filter (fun r -> List.mem lang r.R.languages) in
         Semgrep_generic.check ~hook:(fun _ _ -> ()) 
            rules (parse_equivalences ())
            file ast
       )
  in
  print_matches_and_errors files matches errs
(*e: function [[Main_semgrep_core.sgrep_with_rules]] *)

(*****************************************************************************)
(* Semgrep -tainting_rules_file *)
(*****************************************************************************)

module TR = Tainting_rule

(*s: function [[Main_semgrep_core.tainting_with_rules]] *)
let tainting_with_rules rules_file xs =
  if !verbose then pr2 (spf "Parsing %s" rules_file);
  let rules = Parse_tainting_rules.parse rules_file in

  let files = get_final_files xs in
  let matches, errs = 
     files |> iter_generic_ast_of_files_and_get_matches_and_exn_to_errors 
       (fun file lang ast ->
         let rules = 
            rules |> List.filter (fun r -> List.mem lang r.TR.languages) in
         Tainting_generic.check rules file ast
       )
  in
  print_matches_and_errors files matches errs
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
  if len = 0
  then ""
  else 
    let rest = read_all chan in
    Bytes.sub_string buf 0 len ^ rest
(*e: function [[Main_semgrep_core.read_all]] *)

(*s: function [[Main_semgrep_core.validate_pattern]] *)
(* works with -lang *)
let validate_pattern () =
  let chan = stdin in
  let s = read_all chan in
  try (
  match parse_pattern s with
  | PatGen _ -> exit 0
  | _ -> exit 1
  ) with _exn -> exit 1
(*e: function [[Main_semgrep_core.validate_pattern]] *)

(*s: function [[Main_semgrep_core.json_of_v]] *)
let json_of_v (v: Ocaml.v) = 
  let rec aux v = 
    match v with
    | Ocaml.VUnit -> J.String "()"
    | Ocaml.VBool v1 ->
        if v1
        then J.String "true"
        else J.String "false"
    | Ocaml.VFloat v1 -> J.Float v1 (* ppf "%f" v1 *)
    | Ocaml.VChar v1 -> J.String (spf "'%c'" v1)
    | Ocaml.VString v1 -> J.String v1
    | Ocaml.VInt i -> J.Int i
    | Ocaml.VTuple xs -> J.Array (List.map aux xs)
    | Ocaml.VDict xs ->
        J.Object (List.map (fun (k, v) -> (k, (aux v))) xs)
    | Ocaml.VSum ((s, xs)) ->
        (match xs with
        | [] -> J.String (spf "%s" s)
        | [one_element] -> J.Object [s, (aux one_element)]
        | _ -> J.Object [s, J.Array (List.map aux xs)]
        )          
    | Ocaml.VVar (s, i64) -> J.String (spf "%s_%d" s (Int64.to_int i64))
    | Ocaml.VArrow _ -> failwith "Arrow TODO"
    | Ocaml.VNone -> J.Null
    | Ocaml.VSome v -> J.Object [ "some", aux v ]
    | Ocaml.VRef v -> J.Object [ "ref@", aux v ];
    | Ocaml.VList xs -> J.Array (List.map aux xs)
    | Ocaml.VTODO _ -> J.String "VTODO"
  in
  aux v
(*e: function [[Main_semgrep_core.json_of_v]] *)


(*****************************************************************************)
(* Dumpers *)
(*****************************************************************************)
(*s: function [[Main_semgrep_core.dump_v_to_format]] *)
let dump_v_to_format (v: Ocaml.v) = 
  if (not !output_format_json)
    then (Ocaml.string_of_v v)
    else (Json_io.string_of_json (json_of_v v))
(*e: function [[Main_semgrep_core.dump_v_to_format]] *)

(*s: function [[Main_semgrep_core.dump_pattern]] *)
(* works with -lang *)
let dump_pattern (file: Common.filename) =
  let s = Common.read_file file in
  (* mostly copy-paste of parse_pattern above, but with better error report *)
  match Lang.lang_of_string_opt !lang with
  | Some lang ->
    E.try_with_print_exn_and_reraise file (fun () ->
      let any = Parse_generic.parse_pattern lang s in
      let v = Meta_ast.vof_any any in
      let s = dump_v_to_format v in
      pr s
    )
  | None ->
     failwith (unsupported_language_message !lang)
(*e: function [[Main_semgrep_core.dump_pattern]] *)

(*s: function [[Main_semgrep_core.dump_ast]] *)
let dump_ast file =
  match Lang.langs_of_filename file with
  | lang::_ -> 
    let x = parse_generic lang file in
    let v = Meta_ast.vof_any (Ast_generic.Pr x) in
    let s = dump_v_to_format v in
    pr s
  | [] -> failwith (spf "unsupported language for %s" file)
(*e: function [[Main_semgrep_core.dump_ast]] *)

(*s: function [[Main_semgrep_core.dump_ext_of_lang]] *)
let dump_ext_of_lang () =
  let lang_to_exts = keys |> List.map (
    fun lang_str -> 
      match Lang.lang_of_string_opt lang_str with
      | Some lang -> lang_str ^ "->" ^ String.concat ", " (Lang.ext_of_lang lang)
      | None -> ""
    ) in
  pr2 (spf "Language to supported file extension mappings:\n %s" (String.concat "\n" lang_to_exts))
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

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

(*s: function [[Main_semgrep_core.all_actions]] *)
let all_actions () = [
  "--validate-pattern-stdin", " you also need to pass -lang",
  Common.mk_action_0_arg validate_pattern;
  "-dump_pattern", " <file>",
  Common.mk_action_1_arg dump_pattern;
  "-dump_ast", " <file>",
  Common.mk_action_1_arg dump_ast;
  "-dump_equivalences", " <file>",
  Common.mk_action_1_arg dump_equivalences;
  "-dump_tainting_rules", " <file>",
  Common.mk_action_1_arg dump_tainting_rules;
  "-dump_extensions", " print file extension to language mapping",
  Common.mk_action_0_arg dump_ext_of_lang;
 ]
(*e: function [[Main_semgrep_core.all_actions]] *)

(*s: function [[Main_semgrep_core.options]] *)
let options () = 
  [
    "-e", Arg.Set_string pattern_string, 
    " <pattern> expression pattern (need -lang)";
    "-f", Arg.Set_string pattern_file, 
    " <file> obtain pattern from file (need -lang)";
    "-rules_file", Arg.Set_string rules_file,
    " <file> obtain list of patterns from YAML file";
    "-tainting_rules_file", Arg.Set_string tainting_rules_file,
    " <file> obtain source/sink/sanitizer patterns from YAML file";

    "-lang", Arg.Set_string lang, 
    (spf " <str> choose language (valid choices: %s)" supported_langs);

    "-equivalences", Arg.Set_string equivalences_file,
    " <file> obtain list of code equivalences from YAML file";

    "-exclude", Arg.String (fun s -> Common.push s excludes),
    " <GLOB> skip files whose basename matches GLOB";
    "-include", Arg.String (fun s -> Common.push s includes),
    " <GLOB> search only files whose basename matches GLOB";
    "-exclude-dir", Arg.String (fun s -> Common.push s exclude_dirs),
    " <DIR> exclude directories matching the pattern DIR";

    "-j", Arg.Set_int ncores, 
    " <int> number of cores to use (default = 1)";

    "-json", Arg.Set output_format_json, 
    " output JSON format";
    "-emacs", Arg.Unit (fun () -> match_format := Matching_report.Emacs ),
    " print matches on the same line than the match position";
    "-oneline", Arg.Unit (fun () -> match_format := Matching_report.OneLine),
    " print matches on one line, in normalized form";

    "-pvar", Arg.String (fun s -> mvars := Common.split "," s),
    " <metavars> print the metavariables, not the matched code";

    "-gen_layer", Arg.String (fun s -> layer_file := Some s),
    " <file> save result in a codemap layer file";

    "-error_recovery", Arg.Unit (fun () ->
        error_recovery := true;
        Flag_parsing.error_recovery := true;
    ),  
    " do not stop at first parsing error with -e/-f";
    "-verbose", Arg.Unit (fun () -> 
      verbose := true;
      Flag_semgrep.verbose := true;
    ),
    " ";
    "-debug", Arg.Set debug,
    " add debugging information in the output (e.g., tracing)";
  ] @
  Error_code.options () @
  Common.options_of_actions action (all_actions()) @
  Flag_parsing_cpp.cmdline_flags_macrofile () @
  Meta_parse_info.cmdline_flags_precision () @
  Common2.cmdline_flags_devel () @
  [ "-version",   Arg.Unit (fun () -> 
    pr2 (spf "sgrep version: %s" Config_pfff.version);
    exit 0;
    ), "  guess what"; 
  ]
(*e: function [[Main_semgrep_core.options]] *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(*s: function [[Main_semgrep_core.format_output_exception]] *)
let format_output_exception e : string = 
  (* if (ouptut_as_json) then *)
  let msg = match e with 
    | Parse_rules.InvalidRuleException (pattern_id, msg)     ->
      J.Object [ "pattern_id", J.String pattern_id;
       "error", J.String "invalid rule";
       "message", J.String msg; ]
    | Parse_rules.InvalidLanguageException (pattern_id, language) -> 
      J.Object [ "pattern_id", J.String pattern_id; 
      "error", J.String "invalid language"; 
      "language", J.String language; ]
    | Parse_rules.InvalidPatternException (pattern_id, pattern, lang, message) ->
      J.Object [ "pattern_id", J.String pattern_id;
       "error", J.String "invalid pattern";
       "pattern", J.String pattern; 
       "language", J.String lang;
       "message", J.String message; ]
    | Parse_rules.UnparsableYamlException msg ->
      J.Object [  "error", J.String "unparsable yaml"; "message", J.String msg; ]
    | Parse_rules.InvalidYamlException msg ->
      J.Object [  "error", J.String "invalid yaml"; "message", J.String msg; ]
    | exn -> 
      J.Object [  "error", J.String "unknown exception"; "message", J.String (Common.exn_to_s exn); ]
  in 
    Json_io.string_of_json msg
(*e: function [[Main_semgrep_core.format_output_exception]] *)


(*s: function [[Main_semgrep_core.main]] *)
let main () = 
  set_gc ();
  let usage_msg = 
    spf "Usage: %s [options] <pattern> <files_or_dirs> \nOptions:"
      (Filename.basename Sys.argv.(0))
  in
  (* does side effect on many global flags *)
  let args = Common.parse_options (options()) usage_msg Sys.argv in

  (* must be done after Arg.parse, because Common.profile is set by it *)
  Common.profile_code "Main total" (fun () -> 

    (match args with
   
    (* --------------------------------------------------------- *)
    (* actions, useful to debug subpart *)
    (* --------------------------------------------------------- *)
    | xs when List.mem !action (Common.action_list (all_actions())) -> 
        Common.do_action !action xs (all_actions())

    | _ when not (Common.null_string !action) -> 
        failwith ("unrecognized action or wrong params: " ^ !action)

    (* --------------------------------------------------------- *)
    (* main entry *)
    (* --------------------------------------------------------- *)
    | x::xs -> 
        (match () with
        | _ when !tainting_rules_file <> "" ->
           (try  tainting_with_rules !tainting_rules_file (x::xs)
            with exn -> begin
             pr (format_output_exception exn);
             exit 2
             end
            )
            
        | _ when !rules_file <> "" ->
           (try  sgrep_with_rules !rules_file (x::xs)
            with exn -> begin
             pr (format_output_exception exn);
             exit 2
             end
            )
        | _ -> sgrep_with_one_pattern (x::xs)
        )
    (* --------------------------------------------------------- *)
    (* empty entry *)
    (* --------------------------------------------------------- *)
    | [] -> 
        Common.usage usage_msg (options())
    )
  )
(*e: function [[Main_semgrep_core.main]] *)

(*****************************************************************************)
(*s: toplevel [[Main_semgrep_core._1]] *)
let _ =
  Common.main_boilerplate (fun () -> 
      main ();
  )
(*e: toplevel [[Main_semgrep_core._1]] *)
(*e: semgrep/bin/main_semgrep_core.ml *)
