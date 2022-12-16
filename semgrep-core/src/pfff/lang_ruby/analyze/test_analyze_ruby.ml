open Common

module Flag = Flag_parsing
module PI = Parse_info

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_parse xs =
  let xs = List.map Common.fullpath xs in

  let fullxs, _skipped_paths =
    Lib_parsing_ruby.find_source_files_of_dir_or_files xs
    |> Skip_code.filter_files_if_skip_list ~root:xs
  in

  let stat_list = ref [] in
  let newscore  = Common2.empty_score () in
  let ext = "rb" in

  fullxs |> Console.progress (fun k -> List.iter (fun file ->
    k();
    let stat = Parse_info.default_stat file in
    let () =
      Common.save_excursion Flag.error_recovery true (fun () ->
        Common.save_excursion Flag.exn_when_lexical_error false (fun () ->
          try
            let ast =  Parse_ruby.parse_program file in
            let _cfg = Il_ruby_build.refactor_ast ast in
            ()
          with exn ->
            pr2 (Common.exn_to_s exn);
            stat.PI.error_line_count <- stat.PI.total_line_count;
        )) in
    Common.push stat stat_list;
    let s = spf "bad = %d" stat.PI.error_line_count in
    if stat.PI.error_line_count = 0
    then Hashtbl.add newscore file (Common2.Ok)
    else Hashtbl.add newscore file (Common2.Pb s)
  ));
  flush stdout; flush stderr;

  Parse_info.print_parsing_stat_list !stat_list;
  Parse_info.print_regression_information ~ext xs newscore;
  ()


let test_dump file =
  let ast = Parse_ruby.parse_program file in
  let cfg = Il_ruby_build.refactor_ast ast in
  let s = Il_ruby.show_stmt cfg in
  pr s

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-parse_il_ruby", "   <files or dirs>",
  Common.mk_action_n_arg test_parse;
  "-dump_il_ruby", "   <file>",
  Common.mk_action_1_arg test_dump;
]
