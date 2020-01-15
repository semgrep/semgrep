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

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* A syntactical grep. https://github.com/facebook/pfff/wiki/Sgrep
 * Right now there is good support for Python, Javascript, Java, and C
 * and partial support (fuzzy matcher) for C++, OCaml.
 * 
 * opti: git grep foo | xargs sgrep -e 'foo(...)'
 * 
 * related: 
 *  - SSR http://www.jetbrains.com/idea/documentation/ssr.html
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

let pattern_file = ref ""
let pattern_string = ref ""

(* todo: could get rid of sgrep_lint if implied by using -rule_file *)
let sgrep_lint = ref false
let rule_file = ref "data/basic.yml"
let project_rule_file = ".bento-sgrep.yml"

let use_multiple_patterns = ref false

(* todo: infer from basename argv(0) ? *)
let lang = ref "python"

let case_sensitive = ref false
let match_format = ref Matching_report.Normal
let r2c = ref false

let mvars = ref ([]: Metavars_fuzzy.mvar list)

let layer_file = ref (None: filename option)

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
  (match mvars with
  | [] ->
      if !r2c
      then 
         let info = mk_one_info_from_multiple_infos tokens_matched_code in
         (* todo? use the -e pattern for the check_id? *)
         E.error info (E.SgrepLint ("sgrep", "found a match"))
      else    
        Matching_report.print_match ~format:!match_format tokens_matched_code
  | xs ->
      (* similar to the code of Lib_matcher.print_match, maybe could
       * factorize code a bit.
       * This assumes there is no FakeTok in tokens_matched_code.
       * Currently the only fake tokens generated in parser_php.mly are
       * for abstract methods and sgrep/spatch do not have metavariables
       * to match such construct so we should be safe.
       *)
      let (mini, _maxi) = 
        PI.min_max_ii_by_pos tokens_matched_code in
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
  tokens_matched_code |> List.iter (fun x -> Common.push x _matching_tokens)

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

let create_ast file =
  match !lang with
  | s when Lang.lang_of_string_opt s <> None ->
    Gen (Parse_generic.parse_program file)
  | s when Lang_fuzzy.lang_of_string_opt s <> None ->
    Fuzzy (Parse_fuzzy.parse file)
  | "php" ->
    Php (Parse_php.parse_program file)
  | _ -> failwith ("unsupported language: " ^ !lang)
  

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
       | _ -> failwith ("unsupported language for the pattern: " ^ !lang)
       )
     )
  ))
  with 
  | Parsing.Parse_error -> 
      failwith (spf "fail to parse pattern: '%s' in lang %s" str !lang)
 

let read_patterns name =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop ((parse_pattern s) :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

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
    failwith ("unsupported language or combination: " ^ !lang)

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)
let main_action xs =
  set_gc ();
  let xs = List.map Common.fullpath xs in

  let patterns, query_string =
    match !pattern_file, !pattern_string, !use_multiple_patterns with
    | "", "", _ ->
        failwith "I need a pattern; use -f or -e"
    | file, _, true when file <> "" ->
        read_patterns file, "multi"
    | file, _, _ when file <> "" ->
        let s = Common.read_file file in
        [parse_pattern s], s
    | _, s, true when s <> ""->
        failwith "cannot combine -multi with -e"
    | _, s, _ when s <> ""->
        [parse_pattern s], s
    | _ -> raise Impossible
  in

  let root, files = 
    match xs with
    | [x] when Sys.is_directory x ->
       x, Find_source.files_of_root ~lang:!lang x
    | _ -> 
       "/", Find_source.files_of_dir_or_files ~lang:!lang xs
  in

  files |> List.iter (fun file ->
    if !verbose 
    then pr2 (spf "processing: %s" file);
    let process file =
      patterns |> List.iter (fun pattern -> 
        sgrep_ast pattern (create_ast file)
      )
    in
    if !r2c
    then E.try_with_exn_to_error file (fun () ->
          Common.save_excursion Flag.error_recovery false (fun () ->
          Common.save_excursion Flag.exn_when_lexical_error true (fun () ->
          Common.save_excursion Flag.show_parsing_error false (fun () ->
            process file
         ))))
    else E.try_with_print_exn_and_reraise file (fun () ->
            process file
         )
  );
  if !r2c then begin
   let errs = !E.g_errors 
          |> E.filter_maybe_parse_and_fatal_errors
          |> E.adjust_paths_relative_to_root root
   in
   pr (R2c.string_of_errors errs)
  end;

  !layer_file |> Common.do_option (fun file ->
    let root = Common2.common_prefix_of_files_or_dirs xs in
    gen_layer ~root ~query:query_string  file
  );
  ()

(*****************************************************************************)
(* Sgrep lint *)
(*****************************************************************************)

let lint (xs: Common.path list) : unit =

  (* to adjust paths in errors *)
  let root =
    match xs with
    | [x] when Common2.is_directory x -> Common.fullpath x
    | _ -> "/"
  in
  let project_file = Filename.concat root project_rule_file in
  let file = 
    if Sys.file_exists project_file
    then project_file
    else !rule_file
  in
  if !verbose then pr2 (spf "Parsing %s" file);
  let rules = Parse_rules.parse file in

  match Lang.lang_of_string_opt !lang with
  | Some lang ->
    let files = Lang.files_of_dirs_or_files lang xs in

    let rules = rules |> List.filter (fun r -> List.mem lang r.R.languages) in

    files |> List.iter (fun file ->
      E.try_with_exn_to_error file (fun () ->
        let ast = Parse_generic.parse_with_lang lang file in
        Sgrep_lint_generic.check rules ast
      )
    );
    let errs = !(Error_code.g_errors) 
       |> E.filter_maybe_parse_and_fatal_errors
    in

    let errs = E.adjust_paths_relative_to_root root errs in
    pr (R2c.string_of_errors errs)

  | None -> failwith (spf "unsupported language: %s" !lang)


(*****************************************************************************)
(* Extra actions *)
(*****************************************************************************)

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
 []

let options () = 
  [
    "-lang", Arg.Set_string lang, 
    (spf " <str> choose language (default = %s)" !lang);

    "-e", Arg.Set_string pattern_string, 
    " <pattern> expression pattern";
    "-f", Arg.Set_string pattern_file, 
    " <file> obtain pattern from file";
    "-multi", Arg.Set use_multiple_patterns,
    " combine with -f <file> to obtain multiple patterns from file, one per line";

    "-sgrep_lint", Arg.Set sgrep_lint,
    " run sgrep in lint mode using a rule file";
    "-rule_file", Arg.Set_string rule_file,
    " which rules to use";

    "-case_sensitive", Arg.Set case_sensitive, 
    " match code in a case sensitive manner";

    "-emacs", Arg.Unit (fun () -> match_format := Matching_report.Emacs ),
    " print matches on the same line than the match position";
    "-oneline", Arg.Unit (fun () -> match_format := Matching_report.OneLine),
    " print matches on one line, in normalized form";
    "-r2c", Arg.Unit (fun () -> r2c := true;),
    " use r2c platform error format for output";

    "-pvar", Arg.String (fun s -> mvars := Common.split "," s),
    " <metavars> print the metavariables, not the matched code";

    "-gen_layer", Arg.String (fun s -> layer_file := Some s),
    " <file> save result in a pfff layer file\n";

    "-verbose", Arg.Unit (fun () -> 
      verbose := true;
      Flag_matcher.verbose := true;
      (* Flag_matcher_php.verbose := true; *)
    ),
    " ";
    "-debug", Arg.Set debug,
    " add debugging information in the output (e.g., tracing)";
  ] @
  Error_code.options () @
  Common2.cmdline_flags_devel () @
  (* old: Flag_parsing_php.cmdline_flags_pp () ++ *)
  Common.options_of_actions action (all_actions()) @
  [ "-version",   Arg.Unit (fun () -> 
    pr2 (spf "sgrep version: %s" Config_pfff.version);
    exit 0;
    ), "  guess what"; 
  ]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () = 
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
        if !sgrep_lint 
        then lint (x::xs)
        else main_action (x::xs)

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
