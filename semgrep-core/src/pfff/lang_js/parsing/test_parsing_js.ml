open Common

module Flag = Flag_parsing

module J = JSON
module PI = Parse_info

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_js file =
  if not (file =~ ".*\\.js")
  then pr2 "warning: seems not a .js file";

  Flag.verbose_lexing := true;
  Flag.verbose_parsing := true;

  let toks = Parse_js.tokens file
  (* |> Parsing_hacks_js.fix_tokens  *)
  in
  toks |> List.iter (fun x -> pr2_gen x);
  ()

let test_parse_common xs fullxs ext  =
  let fullxs, _skipped_paths =
    match xs with
    | [x] when Common2.is_directory x ->
        let skip_list =
          if Sys.file_exists (x ^ "/skip_list.txt")
          then Skip_code.load (x ^ "/skip_list.txt")
          else []
        in
        Skip_code.filter_files skip_list x fullxs
    | _ -> fullxs, []
  in

  let stat_list = ref [] in
  let newscore  = Common2.empty_score () in

  Common2.check_stack_nbfiles (List.length fullxs);

  fullxs |> Console.progress (fun k -> List.iter (fun file ->
    k();

    let {Parse_info. stat; _} =
      try (
        Common.save_excursion Flag.error_recovery true (fun () ->
          Common.save_excursion Flag.exn_when_lexical_error false (fun () ->
            Parse_js.parse ~timeout:5.0 file

           (* to have a better comparison with semgrep -lang js -test_parse_lang: *)
           (* let _gen = Js_to_generic.program ast in *)
           (* note that there's still Naming_AST.resolve and
            * Constant_propafation.propagate done with -test_parse_lang.
            * You may also want to remove the timeout and save_excursion above.
           *)

          ))
      ) with Stack_overflow as exn ->
        pr2 (spf "PB on %s, exn = %s" file (Common.exn_to_s exn));
        { Parse_info. ast = []; tokens = []; stat = PI.bad_stat file }
    in
    Common.push stat stat_list;
    let s = spf "bad = %d" stat.Parse_info.error_line_count in
    if stat.Parse_info.error_line_count = 0
    then Hashtbl.add newscore file (Common2.Ok)
    else Hashtbl.add newscore file (Common2.Pb s)
  ));
  Parse_info.print_parsing_stat_list !stat_list;
  Parse_info.print_regression_information ~ext xs newscore;
  ()

let test_parse_js xs =
  let fullxs =
    Lib_parsing_js.find_source_files_of_dir_or_files ~include_scripts:false xs
  in
  test_parse_common xs fullxs "js"

module FT = File_type
let test_parse_ts xs =
  let fullxs =
    Common.files_of_dir_or_files_no_vcs_nofilter xs
    |> List.filter (fun filename ->
      match FT.file_type_of_file filename with
      | FT.PL (FT.Web FT.TypeScript) -> true
      | _ -> false
    ) |> Common.sort
  in
  (* typescript and JSX have lexing conflicts *)
  Common.save_excursion Flag_parsing_js.jsx false (fun () ->
    test_parse_common xs fullxs "ts"
  )

let test_dump_js file =
  let ast = Parse_js.parse_program file in
  let s = Ast_js.show_a_program ast in
  pr s

let test_dump_ts file =
  (* typescript and JSX have lexing conflicts *)
  Common.save_excursion Flag_parsing_js.jsx false (fun () ->
    let ast = Parse_js.parse_program file in
    let s = Ast_js.show_a_program ast in
    pr s
  )

(*****************************************************************************)
(* JSON output *)
(*****************************************************************************)

let info_to_json_range info =
  let loc = PI.unsafe_token_location_of_info info in
  J.Object [
    "line", J.Int loc.PI.line;
    "col", J.Int loc.PI.column;
  ],
  J.Object [
    "line", J.Int loc.PI.line;
    "col", J.Int (loc.PI.column + String.length loc.PI.str);
  ]

let parse_js_r2c xs =
  let fullxs =
    Lib_parsing_js.find_source_files_of_dir_or_files ~include_scripts:false xs
  in
  let json = J.Array (fullxs |> Common.map_filter (fun file ->
    let nblines = Common.cat file |> List.length in
    try
      let _res =
        Common.save_excursion Flag.error_recovery false (fun () ->
          Common.save_excursion Flag.exn_when_lexical_error true (fun () ->
            Common.save_excursion Flag.show_parsing_error true (fun () ->
              Parse_js.parse file
            )))
      in
      (* only return a finding if there was a parse error so we can
       * sort by the number of parse errors in the triage tool
      *)
      None
    with (Parse_info.Parsing_error info | Parse_info.Lexical_error (_,info))
      as exn ->
        let (startp, endp) = info_to_json_range info in
        let message =
          match exn with
          | Parse_info.Parsing_error _ -> "parse error"
          | Parse_info.Lexical_error (s, _) -> "lexical error: " ^ s
          | _ -> raise Impossible
        in
        Some (J.Object [
          "check_id", J.String "pfff-parse_js_r2c";
          "path", J.String file;
          "start", startp;
          "end", endp;
          "extra", J.Object [
            "size", J.Int nblines;
            "message", J.String message;
(*
         "correct", J.Int stat.PI.correct;
         "bad", J.Int stat.PI.bad;
         "timeout", J.Bool stat.PI.have_timeout;
*)
          ]
        ])
  ))
  in
  let json = J.Object ["results", json] in
  let s = J.string_of_json json in
  pr s

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-tokens_js", "   <file>",
  Common.mk_action_1_arg test_tokens_js;

  "-parse_js", "   <file or dir>",
  Common.mk_action_n_arg test_parse_js;
  "-parse_ts", "   <file or dir>",
  Common.mk_action_n_arg test_parse_ts;

  "-dump_js", "   <file>",
  Common.mk_action_1_arg test_dump_js;
  "-dump_ts", "   <file>",
  Common.mk_action_1_arg test_dump_ts;

  "-parse_js_r2c", "   <file or dir>",
  Common.mk_action_n_arg parse_js_r2c;

  (* old:
     "-json_js", "   <file> export the AST of file into JSON",
     Common.mk_action_1_arg test_json_js;
     "-parse_esprima_json", " <file> ",
     Common.mk_action_1_arg test_esprima;
  *)
]
