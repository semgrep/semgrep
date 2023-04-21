open Common
open File.Operators
module PS = Parsing_stat
module Flag = Flag_parsing

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_go file =
  if not (file =~ ".*\\.go") then pr2 "warning: seems not a Go file";

  Flag.verbose_lexing := true;
  Flag.verbose_parsing := true;
  Flag.exn_when_lexical_error := true;

  let toks = Parse_go.tokens (Parsing_helpers.file file) in
  let toks = Parsing_hacks_go.fix_tokens toks in
  toks |> List.iter (fun x -> pr2_gen x);
  ()

(* TODO: was Error_code.try_with_print_exn_and_reraise *)
let try_with_print_exn_and_reraise _a b = b ()

let test_parse_go xs =
  let xs = xs |> File.Path.of_strings |> List.map File.fullpath in

  let fullxs, _skipped_paths =
    Lib_parsing_go.find_source_files_of_dir_or_files xs
    |> Skip_code.filter_files_if_skip_list ~root:xs
  in

  let stat_list = ref [] in
  let newscore = Common2.empty_score () in
  let ext = "go" in

  fullxs
  |> Console.progress (fun k ->
         List.iter (fun file ->
             k ();
             try_with_print_exn_and_reraise file (fun () ->
                 let { Parsing_result.stat; _ } =
                   Common.save_excursion Flag.error_recovery true (fun () ->
                       Common.save_excursion Flag.exn_when_lexical_error false
                         (fun () -> Parse_go.parse !!file))
                 in
                 Common.push stat stat_list;
                 let s = spf "bad = %d" stat.PS.error_line_count in
                 if stat.PS.error_line_count =|= 0 then
                   Hashtbl.add newscore !!file Common2.Ok
                 else Hashtbl.add newscore !!file (Common2.Pb s))));

  flush stdout;
  flush stderr;
  Parsing_stat.print_parsing_stat_list !stat_list;
  Parsing_stat.print_regression_information ~ext xs newscore;
  ()

let test_dump_go file =
  let ast = Parse_go.parse_program file in
  let s = Ast_go.show_program ast in
  pr s

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () =
  [
    ("-tokens_go", "   <file>", Arg_helpers.mk_action_1_arg test_tokens_go);
    ( "-parse_go",
      "   <files or dirs>",
      Arg_helpers.mk_action_n_arg test_parse_go );
    ("-dump_go", "   <file>", Arg_helpers.mk_action_1_arg test_dump_go);
  ]
