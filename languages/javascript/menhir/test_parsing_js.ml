open Common
open File.Operators
module Flag = Flag_parsing
module J = JSON
module PS = Parsing_stat
module FT = File_type

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_js file =
  if not (file =~ ".*\\.js") then pr2 "warning: seems not a .js file";

  Flag.verbose_lexing := true;
  Flag.verbose_parsing := true;

  let toks =
    Parse_js.tokens (Parsing_helpers.file file)
    (* |> Parsing_hacks_js.fix_tokens  *)
  in
  toks |> List.iter (fun x -> pr2_gen x);
  ()

let test_parse_common xs fullxs ext =
  let fullxs, _skipped_paths =
    match xs with
    | [ x ] when Common2.is_directory !!x ->
        let skip_list =
          if Sys.file_exists (!!x ^ "/skip_list.txt") then
            Skip_code.load (Fpath.v (!!x ^ "/skip_list.txt"))
          else []
        in
        Skip_code.filter_files skip_list x fullxs
    | _ -> (fullxs, [])
  in

  let stat_list = ref [] in
  let newscore = Common2.empty_score () in

  Common2.check_stack_nbfiles (List.length fullxs);

  fullxs
  |> Console.progress (fun k ->
         List.iter (fun file ->
             k ();

             let { Parsing_result.stat; _ } =
               try
                 Common.save_excursion Flag.error_recovery true (fun () ->
                     Common.save_excursion Flag.exn_when_lexical_error false
                       (fun () ->
                         Parse_js.parse ~timeout:5.0 !!file
                         (* to have a better comparison with semgrep -lang js -test_parse_lang: *)
                         (* let _gen = Js_to_generic.program ast in *)
                         (* note that there's still Naming_AST.resolve and
                          * Constant_propafation.propagate done with -test_parse_lang.
                          * You may also want to remove the timeout and save_excursion above.
                          *)))
               with
               | Stack_overflow as exn ->
                   pr2 (spf "PB on %s, exn = %s" !!file (Common.exn_to_s exn));
                   {
                     Parsing_result.ast = [];
                     tokens = [];
                     stat = Parsing_stat.bad_stat !!file;
                   }
             in
             Common.push stat stat_list;
             let s = spf "bad = %d" stat.PS.error_line_count in
             if stat.PS.error_line_count =|= 0 then
               Hashtbl.add newscore !!file Common2.Ok
             else Hashtbl.add newscore !!file (Common2.Pb s)));
  Parsing_stat.print_parsing_stat_list !stat_list;
  Parsing_stat.print_regression_information ~ext xs newscore;
  ()

let test_parse_js xs =
  let xs = File.Path.of_strings xs in
  let fullxs =
    File.files_of_dirs_or_files_no_vcs_nofilter xs
    |> List.filter (fun filename ->
           match FT.file_type_of_file filename with
           | FT.PL (FT.Web FT.Js) -> true
           | _else_ -> false)
    |> Common.sort
  in
  test_parse_common xs fullxs "js"

let test_parse_ts xs =
  let xs = File.Path.of_strings xs in
  let fullxs =
    File.files_of_dirs_or_files_no_vcs_nofilter xs
    |> List.filter (fun filename ->
           match FT.file_type_of_file filename with
           | FT.PL (FT.Web FT.TypeScript) -> true
           | _ -> false)
    |> Common.sort
  in
  (* typescript and JSX have lexing conflicts *)
  Common.save_excursion Flag_parsing_js.jsx false (fun () ->
      test_parse_common xs fullxs "ts")

let test_dump_js file =
  let ast = Parse_js.parse_program file in
  let s = Ast_js.show_a_program ast in
  pr s

let test_dump_ts file =
  (* typescript and JSX have lexing conflicts *)
  Common.save_excursion Flag_parsing_js.jsx false (fun () ->
      let ast = Parse_js.parse_program file in
      let s = Ast_js.show_a_program ast in
      pr s)

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () =
  [
    ("-tokens_js", "   <file>", Arg_helpers.mk_action_1_arg test_tokens_js);
    ("-parse_js", "   <file or dir>", Arg_helpers.mk_action_n_arg test_parse_js);
    ("-parse_ts", "   <file or dir>", Arg_helpers.mk_action_n_arg test_parse_ts);
    ("-dump_js", "   <file>", Arg_helpers.mk_action_1_arg test_dump_js);
    ("-dump_ts", "   <file>", Arg_helpers.mk_action_1_arg test_dump_ts);
  ]
