open Common

module Flag = Flag_parsing

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_python file =
  if not (file =~ ".*\\.py")
  then pr2 "warning: seems not a python file";

  Flag.verbose_lexing := true;
  Flag.verbose_parsing := true;
  Flag.exn_when_lexical_error := true;
  let parsing_mode = Parse_python.Python in

  let toks = Parse_python.tokens parsing_mode file
             |> Parsing_hacks_python.fix_tokens in
  toks |> List.iter (fun x -> pr2_gen x);
  ()

let test_parse_python_common parsing_mode xs =
  let xs = List.map Common.fullpath xs in

  let fullxs, _skipped_paths =
    Lib_parsing_python.find_source_files_of_dir_or_files xs
    |> Skip_code.filter_files_if_skip_list ~root:xs
  in

  let stat_list = ref [] in
  let newscore  = Common2.empty_score () in
  let ext = "python" in

  fullxs |> Console.progress (fun k -> List.iter (fun file ->
    k();

    let { Parse_info. stat; _ } =
      Common.save_excursion Flag.error_recovery true (fun () ->
        Common.save_excursion Flag.exn_when_lexical_error false (fun () ->
          Parse_python.parse ~parsing_mode file
        )) in
    Common.push stat stat_list;
    let s = spf "bad = %d" stat.Parse_info.error_line_count in
    if stat.Parse_info.error_line_count = 0
    then Hashtbl.add newscore file (Common2.Ok)
    else Hashtbl.add newscore file (Common2.Pb s)
  ));
  Parse_info.print_parsing_stat_list !stat_list;
  Parse_info.print_regression_information ~ext xs newscore;
  ()

let test_dump_python file =
  Common.save_excursion Flag.error_recovery true (fun () ->
    Common.save_excursion Flag.exn_when_lexical_error false (fun () ->
      let ast = Parse_python.parse_program file in
      let s = AST_python.show_program ast in
      pr s
    ))

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-tokens_python", "   <file>",
  Common.mk_action_1_arg test_tokens_python;
  "-parse_python", "   <files or dirs>",
  Common.mk_action_n_arg (test_parse_python_common Parse_python.Python);
  "-parse_python2", "   <files or dirs>",
  Common.mk_action_n_arg (test_parse_python_common Parse_python.Python2);
  "-parse_python3", "   <files or dirs>",
  Common.mk_action_n_arg (test_parse_python_common Parse_python.Python3);
  "-dump_python", "   <file>",
  Common.mk_action_1_arg test_dump_python;
]
