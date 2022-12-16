open Common

module PI = Parse_info
module Flag = Flag_parsing

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_go file =
  if not (file =~ ".*\\.go")
  then pr2 "warning: seems not a Go file";

  Flag.verbose_lexing := true;
  Flag.verbose_parsing := true;
  Flag.exn_when_lexical_error := true;

  let toks = Parse_go.tokens file in
  let toks = Parsing_hacks_go.fix_tokens toks in
  toks |> List.iter (fun x -> pr2_gen x);
  ()

let test_parse_go xs =
  let xs = List.map Common.fullpath xs in

  let fullxs, _skipped_paths =
    Lib_parsing_go.find_source_files_of_dir_or_files xs
    |> Skip_code.filter_files_if_skip_list ~root:xs
  in

  let stat_list = ref [] in
  let newscore  = Common2.empty_score () in
  let ext = "go" in

  fullxs |> Console.progress (fun k -> List.iter (fun file ->
    k();
    Error_code.try_with_print_exn_and_reraise file(fun () ->
      let {Parse_info. stat; _} =
        Common.save_excursion Flag.error_recovery true (fun () ->
          Common.save_excursion Flag.exn_when_lexical_error false (fun () ->
            Parse_go.parse file
          )) in
      Common.push stat stat_list;
      let s = spf "bad = %d" stat.PI.error_line_count in
      if stat.PI.error_line_count = 0
      then Hashtbl.add newscore file (Common2.Ok)
      else Hashtbl.add newscore file (Common2.Pb s)
    )
  ));

  flush stdout; flush stderr;
  Parse_info.print_parsing_stat_list !stat_list;
  Parse_info.print_regression_information ~ext xs newscore;
  ()


let test_dump_go file =
  let ast = Parse_go.parse_program file in
  let s = Ast_go.show_program ast in
  pr s

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-tokens_go", "   <file>",
  Common.mk_action_1_arg test_tokens_go;
  "-parse_go", "   <files or dirs>",
  Common.mk_action_n_arg test_parse_go;
  "-dump_go", "   <file>",
  Common.mk_action_1_arg test_dump_go;
]
