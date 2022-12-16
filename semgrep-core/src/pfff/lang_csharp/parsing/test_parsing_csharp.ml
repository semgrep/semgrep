open Common

module Flag = Flag_parsing

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_csharp file =
  if not (file =~ ".*\\.py")
  then pr2 "warning: seems not a python file";

  Flag.verbose_lexing := true;
  Flag.verbose_parsing := true;

  let toks = Parse_csharp.tokens file in
  toks |> List.iter (fun x -> pr2_gen x);
  ()

let test_parse_csharp xs =

  let fullxs = Lib_parsing_csharp.find_csharp_files_of_dir_or_files xs in
  let stat_list = ref [] in

  fullxs |> List.iter (fun file ->
    pr2 ("PARSING: " ^ file);

    let (_xs, stat) = Parse_csharp.parse file in
    Common.push stat stat_list;
  );
  Parse_info.print_parsing_stat_list !stat_list;
  ()

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-tokens_csharp", "   <file>",
  Common.mk_action_1_arg test_tokens_csharp;
  "-parse_csharp", "   <files or dirs>",
  Common.mk_action_n_arg test_parse_csharp;
]
