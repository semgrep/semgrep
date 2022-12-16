open Common

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_parse_ast_ml xs =
  let xs = List.map Common.fullpath xs in
  let fullxs, _skipped_paths =
    Lib_parsing_ml.find_source_files_of_dir_or_files xs
    |> Skip_code.filter_files_if_skip_list ~root:xs
  in
  fullxs |> Console.progress (fun k -> List.iter (fun file ->
    k();
    try
      Common.save_excursion Flag_parsing.error_recovery true (fun () ->
        Common.save_excursion Flag_parsing.show_parsing_error false (fun () ->
          Common.save_excursion Flag_parsing.exn_when_lexical_error false (fun ()->
            let { Parse_info. ast = _ast; _ }  = Parse_ml.parse file in
            (*             let _gen = Ml_to_generic.program ast in *)
            ()
          )))
    with exn -> Exception.catch_and_reraise exn
  ))

let test_dump_ml file =
  let ast = Parse_ml.parse_program file in
  let s = Ast_ml.show_program ast in
  pr2 s

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-parse_ast_ml", "   <files or dirs>",
  Common.mk_action_n_arg test_parse_ast_ml;
  "-dump_ast_ml", "   <file>",
  Common.mk_action_1_arg test_dump_ml;
]
