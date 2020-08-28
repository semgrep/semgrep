(*s: pfff/lang_python/parsing/Test_parsing_python.ml *)
open Common

module Flag = Flag_parsing

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

(*s: function [[Test_parsing_python.test_tokens_python]] *)
let test_tokens_python file = 
  if not (file =~ ".*\\.py") 
  then pr2 "warning: seems not a python file";

  Flag.verbose_lexing := true;
  Flag.verbose_parsing := true;
  Flag.exn_when_lexical_error := true;
  let parsing_mode = Parse_python.Python in

  let toks = Parse_python.tokens parsing_mode file 
      |> Parsing_hacks_python.fix_tokens in
  toks |> List.iter (fun x -> pr2_gen x);
  ()
(*e: function [[Test_parsing_python.test_tokens_python]] *)

(*s: function [[Test_parsing_python.test_parse_python_common]] *)
let test_parse_python_common parsing_mode xs =
  let xs = List.map Common.fullpath xs in

  let fullxs = 
    Lib_parsing_python.find_source_files_of_dir_or_files xs 
    |> Skip_code.filter_files_if_skip_list ~root:xs
  in

  let stat_list = ref [] in
  let newscore  = Common2.empty_score () in
  let ext = "python" in

  fullxs |> Console.progress (fun k -> List.iter (fun file ->
    k();

    let (_xs, stat) =
     Common.save_excursion Flag.error_recovery true (fun () ->
     Common.save_excursion Flag.exn_when_lexical_error false (fun () ->
       Parse_python.parse ~parsing_mode file
    )) in
    Common.push stat stat_list;
    let s = spf "bad = %d" stat.Parse_info.bad in
    if stat.Parse_info.bad = 0
    then Hashtbl.add newscore file (Common2.Ok)
    else Hashtbl.add newscore file (Common2.Pb s)
  ));
  Parse_info.print_parsing_stat_list !stat_list;
  let dirname_opt = 
    match xs with
    | [x] when Common2.is_directory x -> Some (Common.fullpath x)
    | _ -> None
  in
    let score_path = Filename.concat Config_pfff.path "tmp" in
    dirname_opt |> Common.do_option (fun dirname -> 
      let dirname = Common.fullpath dirname in
      pr2 "--------------------------------";
      pr2 "regression testing  information";
      pr2 "--------------------------------";
      let str = Str.global_replace (Str.regexp "/") "__" dirname in
      Common2.regression_testing newscore 
        (Filename.concat score_path
         (spf "score_parsing__%s%s.marshalled" str ext))
    );
  ()
(*e: function [[Test_parsing_python.test_parse_python_common]] *)


(*s: function [[Test_parsing_python.test_dump_python]] *)
let test_dump_python file =
  Common.save_excursion Flag.error_recovery true (fun () ->
  Common.save_excursion Flag.exn_when_lexical_error false (fun () ->
   let ast = Parse_python.parse_program file in
   let s = AST_python.show_program ast in
   pr s
  ))
(*e: function [[Test_parsing_python.test_dump_python]] *)

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

(*s: function [[Test_parsing_python.actions]] *)
let actions () = [
  "-tokens_python", "   <file>", 
  Common.mk_action_1_arg test_tokens_python;
  "-parse_python", "   <files or dirs>", 
  Common.mk_action_n_arg (test_parse_python_common Parse_python.Python);
  "-parse_python2", "   <files or dirs>", 
  Common.mk_action_n_arg (test_parse_python_common Parse_python.Python2);
  "-parse_python3", "   <files or dirs>", 
  Common.mk_action_n_arg (test_parse_python_common Parse_python.Python3);
  "-dump_python", "   <file>", 
  Common.mk_action_1_arg test_dump_python;
]
(*e: function [[Test_parsing_python.actions]] *)
(*e: pfff/lang_python/parsing/Test_parsing_python.ml *)
