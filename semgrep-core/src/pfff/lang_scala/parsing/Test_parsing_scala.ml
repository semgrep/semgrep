open Common

module Flag = Flag_parsing

let logger = Logging.get_logger [__MODULE__]

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens file =
  if not (file =~ ".*\\.scala")
  then pr2 "warning: seems not a Scala file";

  Flag.verbose_lexing := true;
  Flag.verbose_parsing := true;
  Flag.exn_when_lexical_error := true;

  let toks = Parse_scala.tokens file in
  toks |> List.iter (fun x -> pr2_gen x);
  ()

let test_parse xs =
  let xs = List.map Common.fullpath xs in

  let fullxs, _skipped_paths =
    Parse_scala.find_source_files_of_dir_or_files xs
    |> Skip_code.filter_files_if_skip_list ~root:xs
  in
  let stat_list = ref [] in

  fullxs |> Console.progress (fun k -> List.iter (fun file ->
    logger#info "processing %s" file;
    k();

    let { Parse_info.stat; _ } =
      Common.save_excursion Flag.error_recovery true (fun () ->
        Parse_scala.parse file
      )
    in
    Common.push stat stat_list;
  ));
  Parse_info.print_parsing_stat_list !stat_list;
  ()

let test_dump file =
  let ast = Parse_scala.parse_program file in
  (* alt: pr (AST_scala.show_program ast) *)
  let formatter = Format.std_formatter in
  Format.pp_set_margin formatter 120;
  AST_scala.pp_program formatter ast;
  ()

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-tokens_scala", "   <file>",
  Common.mk_action_1_arg test_tokens;
  "-parse_scala", "   <files or dirs>",
  Common.mk_action_n_arg test_parse;
  "-dump_scala", "   <file>",
  Common.mk_action_1_arg test_dump;
]
