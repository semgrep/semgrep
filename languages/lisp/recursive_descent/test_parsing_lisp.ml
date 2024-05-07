open Fpath_.Operators
module Flag = Flag_parsing

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_lisp (file : Fpath.t) =
  (match File_type.file_type_of_file file with
  | File_type.PL (File_type.Lisp _) -> ()
  | _ -> UCommon.pr2 "warning: seems not a lisp file");

  Flag.verbose_lexing := true;
  Flag.verbose_parsing := true;

  let toks = Parse_lisp.tokens (Parsing_helpers.File file) in
  toks |> List.iter (fun x -> UCommon.pr2_gen x);
  ()

let test_parse_lisp xs =
  let fullxs = Lib_parsing_lisp.find_source_files_of_dir_or_files xs in
  let stat_list = ref [] in

  fullxs
  |> List.iter (fun file ->
         UCommon.pr2 ("PARSING: " ^ !!file);

         let _xs, stat = Parse_lisp.parse file in
         Stack_.push stat stat_list);
  UCommon.pr2 (Parsing_stat.string_of_stats !stat_list);
  ()

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () =
  [
    ("-tokens_lisp", "   <file>", Arg_.mk_action_1_conv Fpath.v test_tokens_lisp);
    ( "-parse_lisp",
      "   <files or dirs>",
      Arg_.mk_action_n_conv Fpath.v test_parse_lisp );
  ]
