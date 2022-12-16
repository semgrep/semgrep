(*s: test_parsing_php.ml *)
open Common

module Flag = Flag_parsing

(*****************************************************************************)
(* Lexing/Parsing *)
(*****************************************************************************)
(*s: test_tokens_php *)
let test_tokens_php file =
  if not (file =~ ".*\\.php")
  then pr2 "warning: seems not a .php file";

  Flag.verbose_lexing := true;
  Flag.verbose_parsing := true;

  let toks = Parse_php.tokens file in
  toks |> List.iter (fun x -> pr2_gen x);
  ()

(*e: test_tokens_php *)
(*s: test_parse_php *)
let test_parse_php xs  =
  let fullxs = Lib_parsing_php.find_source_files_of_dir_or_files xs in

  let fullxs, _skipped_paths =
    match xs with
    | [x] when Common2.is_directory x ->
        let skip_list =
          if Sys.file_exists (x ^ "/skip_list.txt")
          then Skip_code.load (x ^ "/skip_list.txt")
          else []
        in
        Skip_code.filter_files skip_list x fullxs
    | _ -> fullxs, []
  in

  let stat_list = ref [] in
  (*s: initialize -parse_php regression testing hash *)
  let newscore  = Common2.empty_score () in
  (*e: initialize -parse_php regression testing hash *)

  Common2.check_stack_nbfiles (List.length fullxs);

  fullxs |> Console.progress (fun k -> List.iter (fun file ->
    k ();

    let { Parse_info. stat; _ } =
      Common.save_excursion Flag.error_recovery true (fun () ->
        Parse_php.parse file
      )
    in
    Common.push stat stat_list;
    (*s: add stat for regression testing in hash *)
    let s = spf "bad = %d" stat.Parse_info.error_line_count in
    if stat.Parse_info.error_line_count = 0
    then Hashtbl.add newscore file (Common2.Ok)
    else Hashtbl.add newscore file (Common2.Pb s)
    ;
    (*e: add stat for regression testing in hash *)
  ));

  Parse_info.print_parsing_stat_list !stat_list;
  (*s: print regression testing results *)
  Parse_info.print_regression_information ~ext:"php" xs newscore;
  ()
(*e: print regression testing results *)
(*e: test_parse_php *)
(*****************************************************************************)
(* Export *)
(*****************************************************************************)

(*s: test_sexp_php *)
(*x: test_sexp_php *)
(*e: test_sexp_php *)
(*s: test_json_php *)
(*
let test_json_php file =
  let ast = Parse_php.parse_program file in
  let s = Export_ast_php.json_string_of_program ast in
  pr s;
  ()

let test_json_fast_php file =
  let ast = Parse_php.parse_program file in
  let s = Export_ast_php.json_string_of_program_fast ast in
  pr s;
  ()
*)
(*e: test_json_php *)

let test_dump_php file =
  let cst = Parse_php.parse_program file in
  let s = Cst_php.show_program cst in
  pr s

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)
(*s: test_visit_php *)
(*
let test_visit_php file =
  let ast = Parse_php.parse_program file in

  let hooks = { Visitor_php.default_visitor with
    Visitor_php.kinfo = (fun (_k, _) info ->
      let s = Parse_info.str_of_info info in
      pr2 s;
    );

    Visitor_php.kexpr = (fun (k, _) e ->
      match e with
      | Cst_php.Sc _ ->
          pr2 "scalar";
          k e
      | _ -> k e
    );
  } in
  let visitor = Visitor_php.mk_visitor hooks in
  visitor (Ast.Program ast)
*)
(*e: test_visit_php *)
(*
let test_unparse_php file =
  let (ast2, _stat) = Parse_php.parse file in
  let tmpfile = Common.new_temp_file "unparse_php" ".php" in
  let s = Unparse_php.string_of_program_with_comments_using_transfo ast2 in
  Common.write_file ~file:tmpfile s;
  let xs = Common2.unix_diff file tmpfile in
  xs |> List.iter pr2;
  ()

let test_pretty_print_php file =
  let _ast = Parse_php.parse_program file in
  raise Todo
  (* Pretty_print_php.pretty_print_program ast *)

(* note that pfff can now parse XHP files without calling xhpize *)
let test_parse_xhp_with_xhpize file =
  let pp_cmd = "xhpize" in
  let _ast = Parse_php.parse_program ~pp:(Some pp_cmd) file in
  raise Todo

let test_parse_xdebug_expr s =
  let _e = Parse_php.xdebug_expr_of_string s in
  raise Todo
*)

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)
let actions () = [
  (*s: test_parsing_php actions *)
  "-parse_php", "   <file or dir>",
  Common.mk_action_n_arg test_parse_php;
  (*x: test_parsing_php actions *)
(*
    "-visit_php", "   <file>",
      Common.mk_action_1_arg test_visit_php;
*)
  (*x: test_parsing_php actions *)
  (* an alias for -sexp_php *)
(*
    "-json", "   <file> export the AST of file into JSON",
      Common.mk_action_1_arg test_json_php;
    "-json_fast", "   <file> export the AST of file into a compact JSON",
      Common.mk_action_1_arg test_json_fast_php;
*)
  (*x: test_parsing_php actions *)
  (* an alias for -sexp_php *)
  "-dump_php", "   <file>",
  Common.mk_action_1_arg test_dump_php;
  "-dump_php_ml", "   <file>",
  Common.mk_action_1_arg test_dump_php;
  (*x: test_parsing_php actions *)
  (*x: test_parsing_php actions *)
  (*x: test_parsing_php actions *)
  "-tokens_php", "   <file>",
  Common.mk_action_1_arg test_tokens_php;
  (*e: test_parsing_php actions *)
(*
    "-unparse_php", "   <file>",
    Common.mk_action_1_arg test_unparse_php;
    "-pretty_print_php", "   <file>",
    Common.mk_action_1_arg test_pretty_print_php;
    "-parse_xdebug_expr", "   <string>",
    Common.mk_action_1_arg test_parse_xdebug_expr;
    "-parse_xhp_with_xhpize", "   <file>",
    Common.mk_action_1_arg test_parse_xhp_with_xhpize;
*)
]
(*e: test_parsing_php.ml *)
