open Common
open File.Operators
module Flag = Flag_parsing

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_lisp file =
  let file = Fpath.v file in
  (match File_type.file_type_of_file file with
  | File_type.PL (File_type.Lisp _) -> ()
  | _ -> pr2 "warning: seems not a lisp file");

  Flag.verbose_lexing := true;
  Flag.verbose_parsing := true;

  let toks = Parse_lisp.tokens (Parsing_helpers.File file) in
  toks |> List.iter (fun x -> pr2_gen x);
  ()

let test_parse_lisp xs =
  let xs = File.Path.of_strings xs in
  let fullxs = Lib_parsing_lisp.find_source_files_of_dir_or_files xs in
  let stat_list = ref [] in

  fullxs
  |> List.iter (fun file ->
         pr2 ("PARSING: " ^ !!file);

         let _xs, stat = Parse_lisp.parse !!file in
         Common.push stat stat_list);
  Parsing_stat.print_parsing_stat_list !stat_list;
  ()

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () =
  [
    ("-tokens_lisp", "   <file>", Arg_helpers.mk_action_1_arg test_tokens_lisp);
    ( "-parse_lisp",
      "   <files or dirs>",
      Arg_helpers.mk_action_n_arg test_parse_lisp );
  ]
