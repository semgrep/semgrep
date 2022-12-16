open Common

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_css file =
  if not (file =~ ".*\\.css")
  then pr2 "warning: seems not a css file";
(*
  Flag.verbose_lexing := true;
  Flag.verbose_parsing := true;
*)
  let toks = Parse_css.tokens file in
  toks |> List.iter (fun x -> pr2_gen x);
  ()

let test_parse_css xs =

  let fullxs = Lib_parsing_css.find_css_files_of_dir_or_files xs in
  fullxs |> List.iter (fun file ->
    pr2 ("PARSING: " ^ file);
    Common.save_excursion Flag_parsing.error_recovery true (fun () ->
      let (ast, _toks) = Parse_css.parse file in
      pr2_gen ast;
    )
  );
  ()

let test_dump_css file =
  let (ast, _toks) = Parse_css.parse file in
  let s = Export_ast_css.ml_pattern_string_of_stylesheet ast in
  pr s


(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-tokens_css", "   <file>",
  Common.mk_action_1_arg test_tokens_css;
  "-parse_css", "   <files or dirs>",
  Common.mk_action_n_arg test_parse_css;
  "-dump_css", "   <file>",
  Common.mk_action_1_arg test_dump_css;
]
