open Common

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_html file =
  if not (file =~ ".*\\.html")
  then pr2 "warning: seems not a html file";
(*
  Flag.verbose_lexing := true;
  Flag.verbose_parsing := true;
*)
  let (_ast, toks) = Parse_html.parse file in
  toks |> List.iter (fun x -> pr2_gen x);
  ()


let test_parse_html xs =

  let fullxs = Lib_parsing_html.find_html_files_of_dir_or_files xs in

  fullxs |> List.iter (fun file ->
    pr2 ("PARSING: " ^ file);

    (* old:
     *  let s = Common.read_file file in
     * let _ast = Parse_html.parse_simple_tree (HtmlRaw s) in
     *
     * let (xs, stat) = Parse_erlang.parse file in
     * Common.push2 stat stat_list;
    *)
    try
      let _tree = Parse_html.parse file in
      ()
    with Parse_info.Parsing_error info ->
      pr2 (Parse_info.error_message_info info)
  );
  ()

(*
let test_dump_html_old _file =
  failwith "ocamlnet not supported anymore"
*)
(*
  let s = Common.read_file file in
  let ast = Parse_html.parse_simple_tree (HtmlRaw s) in
  let json = Export_html.json_of_html_tree2 ast in
  let s = Json_out.string_of_json json in
  pr2 s
*)

(*
let test_dump_html file =
  let (ast, _toks) = Parse_html.parse file in
  (* Format.set_margin 400; Format.set_max_indent 400; *)
  let s = Export_html.ml_pattern_string_of_html_tree ast in
  pr2 s

let test_json_html file =
  let (ast, _toks) = Parse_html.parse file in
(*  let s = Export_ast_ml.ml_pattern_string_of_program ast in *)
  let json = Export_html.json_of_html_tree ast in
  let s = Json_out.string_of_json json in
  pr2 s
*)

let test_pp_html file =
  let (ast, _toks) = Parse_html.parse file in
  let s = Pretty_print_html.string_of_html_tree ast in
  pr2 s

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-tokens_html", "   <file>",
  Common.mk_action_1_arg test_tokens_html;
  "-parse_html", "   <files or dirs>",
  Common.mk_action_n_arg test_parse_html;
(*
  "-dump_html", "   <file>",
  Common.mk_action_1_arg test_dump_html;
*)
  "-pp_html", "   <file>",
  Common.mk_action_1_arg test_pp_html;

(*
  "-json_html", "   <file>",
  Common.mk_action_1_arg test_json_html;
  "-json_html_old", "   <file>",
  Common.mk_action_1_arg test_dump_html_old;
*)
]
