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
 * 
 * See also codequery for more structural queries.
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

let verbose = ref false
let debug = ref false

let pattern_string = ref ""
let pattern_file = ref ""
let rules_file = ref ""

(* todo: infer from basename argv(0) ? *)
let lang = ref "unset"

let output_format_json = ref false
let case_sensitive = ref false
let match_format = ref Matching_report.Normal

let mvars = ref ([]: Metavars_fuzzy.mvar list)

let layer_file = ref (None: filename option)

let keys = Common2.hkeys Lang.lang_of_string_map
let supported_langs: string = String.concat ", " keys

let unsupported_language_message = fun some_lang: string -> 
  (spf "unsupported language: %s; supported langauge tags are: %s" some_lang supported_langs)

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

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


(* for -gen_layer *)
let _matching_tokens = ref []

(* TODO? could do slicing of function relative to the pattern, so 
 * would see where the parameters come from :)
 *)

let mk_one_info_from_multiple_infos xs =
  List.hd xs

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

let print_simple_match tokens_matched_code =
  print_match [] [] tokens_matched_code


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
  
(*****************************************************************************)
(* Language specific *)
(*****************************************************************************)

type ast =
  | Gen of Ast_generic.program
  | Fuzzy of Ast_fuzzy.trees

  | Php of Cst_php.program

  | NoAST

(* coupling: you need also to modify tests/test.ml *)
let parse_generic file = 
  let ast = Parse_generic.parse_program file in
  let lang = Common2.some (Lang.lang_of_filename_opt file) in
  Naming_ast.resolve lang ast;
  ast

let create_ast file =
  match !lang with
  | s when Lang.lang_of_string_opt s <> None ->
    Gen (parse_generic file)
  | s when Lang_fuzzy.lang_of_string_opt s <> None ->
    Fuzzy (Parse_fuzzy.parse file)
  | "php" ->
    Php (Parse_php.parse_program file)
  | _ -> failwith (unsupported_language_message !lang)
  

type pattern =
  | PatFuzzy of Ast_fuzzy.tree list
  | PatGen of Sgrep_generic.pattern
(*  | PatPhp of Sgrep_php.pattern *)


let parse_pattern str =
 try (
  Common.save_excursion Flag_parsing.sgrep_mode true (fun () ->
   match Lang.lang_of_string_opt !lang with
   | Some lang ->
       PatGen (Parse_generic.parse_pattern lang str)
   | None ->
     (match Lang_fuzzy.lang_of_string_opt !lang with
     | Some lang -> 
       PatFuzzy (Parse_fuzzy.parse_pattern lang str)
     | None ->
       (match !lang with
       | "php" -> (* PatPhp (Sgrep_php.parse str) *) raise Todo
       | _ -> failwith (unsupported_language_message !lang)
       )
     )
  ))
  with exn ->
      failwith (spf "fail to parse pattern: '%s' in lang %s (exn = %s)" 
          str !lang (Common.exn_to_s exn))
 

let sgrep_ast pattern any_ast =
  match pattern, any_ast with
  |  _, NoAST -> () (* skipping *)
  | PatGen pattern, Gen ast ->
    Sgrep_generic.sgrep_ast
      ~hook:(fun env matched_tokens ->
        print_match !mvars env Lib_ast.ii_of_any matched_tokens
      )
      pattern ast

  | PatFuzzy pattern, Fuzzy ast ->
    Sgrep_fuzzy.sgrep
      ~hook:(fun env matched_tokens ->
        print_match !mvars env Lib_ast_fuzzy.toks_of_trees matched_tokens
      )
      pattern ast

(*
  | PatPhp pattern, Php ast ->
    Sgrep_php.sgrep_ast
      ~case_sensitive:!case_sensitive
      ~hook:(fun env matched_tokens ->
        print_match !mvars env Lib_parsing_php.ii_of_any matched_tokens
      )
      pattern ast
*)
  | _ ->
    failwith ("unsupported  combination or " ^ (unsupported_language_message !lang))

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)
let sgrep_with_one_pattern xs =
  let xs = List.map Common.fullpath xs in

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

  files |> List.iter (fun file ->
    if !verbose 
    then pr2 (spf "processing: %s" file);
    let process file = sgrep_ast pattern (create_ast file) in
      E.try_with_print_exn_and_reraise file (fun () ->
            process file
         )
  );

  !layer_file |> Common.do_option (fun file ->
    let root = Common2.common_prefix_of_files_or_dirs xs in
    gen_layer ~root ~query:query_string  file
  );
  ()

(*****************************************************************************)
(* Sgrep lint *)
(*****************************************************************************)

let sgrep_with_rules rules_file xs =

  if !verbose then pr2 (spf "Parsing %s" rules_file);
  (* todo: call Normalize_ast.normalize here after or in parse()? *)
  let rules = Parse_rules.parse rules_file in

  match Lang.lang_of_string_opt !lang with
  | None -> 
        failwith (unsupported_language_message !lang)
  | Some lang ->
    let files = Lang.files_of_dirs_or_files lang xs in
    let rules = rules |> List.filter (fun r -> List.mem lang r.R.languages) in

    let errs = ref [] in
    let matches = 
      files |> List.map (fun file ->
         if !verbose then pr2 (spf "Analyzing %s" file);
         try 
           let ast = Parse_generic.parse_with_lang lang file in
           Sgrep_lint_generic.check rules file ast
         with exn -> 
            Common.push (Error_code.exn_to_error file exn) errs;
            []
      ) |> List.flatten
    in
    let errs = E.filter_maybe_parse_and_fatal_errors !errs in
    let json = J.Object [
       "matches", J.Array (matches |> List.map Match_result.match_to_json);
       "errors", J.Array (errs |> List.map R2c.error_to_json)
    ] in
    let s = Json_io.string_of_json json in
    pr s

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
  if len = 0
  then ""
  else 
    let rest = read_all chan in
    Bytes.sub_string buf 0 len ^ rest

(* works with -lang *)
let validate_pattern () =
  let chan = stdin in
  let s = read_all chan in
  try (
  match parse_pattern s with
  | PatGen _ -> exit 0
  | _ -> exit 1
  ) with _exn -> exit 1

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


(*****************************************************************************)
(* Dumpers *)
(*****************************************************************************)
let dump_v_to_format (v: Ocaml.v) = 
  if (not !output_format_json)
    then (Ocaml.string_of_v v)
    else (Json_io.string_of_json (json_of_v v))

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
      pr2 s
    )
  | None ->
     failwith (unsupported_language_message !lang)

let dump_ast file =
  let x = parse_generic file in
  let v = Meta_ast.vof_any (Ast_generic.Pr x) in
  let s = dump_v_to_format v in
  pr2 s

let dump_ext_of_lang () =
  let lang_to_exts = keys |> List.map (
    fun lang_str -> 
      match Lang.lang_of_string_opt lang_str with
      | Some lang -> lang_str ^ "->" ^ String.concat ", " (Lang.ext_of_lang lang)
      | None -> ""
    ) in
  pr2 (spf "Language to supported file extension mappings:\n %s" (String.concat "\n" lang_to_exts))

  (*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = [
  "--validate-pattern-stdin", " you also need to pass -lang",
  Common.mk_action_0_arg validate_pattern;
  "-dump_pattern", " <file>",
  Common.mk_action_1_arg dump_pattern;
  "-dump_ast", " <file>",
  Common.mk_action_1_arg dump_ast;
  "-dump_extensions", "print file extension to language mapping",
  Common.mk_action_0_arg dump_ext_of_lang;
 ]

let options () = 
  [
    "-lang", Arg.Set_string lang, 
    (spf " <str> choose language (valid choices: %s)" supported_langs);

    "-e", Arg.Set_string pattern_string, 
    " <pattern> expression pattern";
    "-f", Arg.Set_string pattern_file, 
    " <file> obtain pattern from file";
    "-rules_file", Arg.Set_string rules_file,
    " <file> obtain list of patterns from YAML file";

    "-json", Arg.Set output_format_json, "output JSON format";

    "-case_sensitive", Arg.Set case_sensitive, 
    " match code in a case sensitive manner";

    "-emacs", Arg.Unit (fun () -> match_format := Matching_report.Emacs ),
    " print matches on the same line than the match position";
    "-oneline", Arg.Unit (fun () -> match_format := Matching_report.OneLine),
    " print matches on one line, in normalized form";

    "-pvar", Arg.String (fun s -> mvars := Common.split "," s),
    " <metavars> print the metavariables, not the matched code";

    "-gen_layer", Arg.String (fun s -> layer_file := Some s),
    " <file> save result in a pfff layer file\n";

    "-verbose", Arg.Unit (fun () -> 
      verbose := true;
      Flag_matcher.verbose := true;
      Generic_vs_generic.verbose := true;
      (* Flag_matcher_php.verbose := true; *)
    ),
    " ";
    "-debug", Arg.Set debug,
    " add debugging information in the output (e.g., tracing)";
  ] @
  Error_code.options () @
  Common.options_of_actions action (all_actions()) @
  Common2.cmdline_flags_devel () @
  [ "-version",   Arg.Unit (fun () -> 
    pr2 (spf "sgrep version: %s" Config_pfff.version);
    exit 0;
    ), "  guess what"; 
  ]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

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
        if !rules_file <> ""
        then sgrep_with_rules !rules_file (x::xs)
        else sgrep_with_one_pattern (x::xs)

    (* --------------------------------------------------------- *)
    (* empty entry *)
    (* --------------------------------------------------------- *)
    | [] -> 
        Common.usage usage_msg (options())
    )
  )

(*****************************************************************************)
let _ =
  Common.main_boilerplate (fun () -> 
      main ();
  )
