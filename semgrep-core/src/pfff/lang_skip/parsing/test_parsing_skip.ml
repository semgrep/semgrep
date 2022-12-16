open Common
open Ast_skip

module Flag = Flag_parsing

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens file =
  if not (file =~ ".*\\.sk")
  then pr2 "warning: seems not a Skip file";

  Flag.verbose_lexing := true;
  Flag.verbose_parsing := true;
  Flag.exn_when_lexical_error := true;

  let toks = Parse_skip.tokens file in
  toks |> List.iter (fun x -> pr2_gen x);
  ()

let test_parse xs =
  let xs = List.map Common.fullpath xs in

  let fullxs, _skipped_paths =
    Lib_parsing_skip.find_source_files_of_dir_or_files xs
    |> Skip_code.filter_files_if_skip_list ~root:xs
  in
  let stat_list = ref [] in

  fullxs |> Console.progress (fun k -> List.iter (fun file ->
    k();

    let (_xs, stat) =
      Common.save_excursion Flag.error_recovery true (fun () ->
        Parse_skip.parse file
      )
    in
    Common.push stat stat_list;
  ));
  Parse_info.print_parsing_stat_list !stat_list;
  ()

let test_dump file =
  let ast = Parse_skip.parse_program file in
  let v = Meta_ast_skip.vof_any (Program ast) in
  let s = OCaml.string_of_v v in
  pr s


(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-tokens_sk", "   <file>",
  Common.mk_action_1_arg test_tokens;
  "-parse_sk", "   <files or dirs>",
  Common.mk_action_n_arg test_parse;
  "-dump_sk", "   <file>",
  Common.mk_action_1_arg test_dump;
]
