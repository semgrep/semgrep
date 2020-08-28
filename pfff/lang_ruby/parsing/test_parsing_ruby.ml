open Common

module PI = Parse_info
module Flag = Flag_parsing
module TH = Token_helpers_ruby

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens file = 
  if not (file =~ ".*\\.rb") 
  then pr2 "warning: seems not a ruby file";
(*
  Flag.verbose_lexing := true;
  Flag.verbose_parsing := true;
  Flag.exn_when_lexical_error := true;
*)

  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  let state = Lexer_parser_ruby.create ("top_lexer", Lexer_ruby.top_lexer) in 

  Parser_ruby_helpers.clear_env ();
  let env = Utils_ruby.default_opt Utils_ruby.StrSet.empty None in
  Parser_ruby_helpers.set_env env ;

  let lexerf = Lexer_ruby.token state in
  let lexerf = fun lexbuf ->
      let rec aux lexbuf = 
        let res = lexerf lexbuf in
        pr2_gen res;
        if TH.is_comment res
        then aux lexbuf
        else res
      in
      aux lexbuf
  in
  let _lst = Parser_ruby.main lexerf lexbuf in
  ()


let test_parse xs =
  let xs = List.map Common.fullpath xs in

  let fullxs = 
    Lib_parsing_ruby.find_source_files_of_dir_or_files xs 
    |> Skip_code.filter_files_if_skip_list ~root:xs
  in

  let stat_list = ref [] in
  let newscore  = Common2.empty_score () in
  let ext = "rb" in

  fullxs |> Console.progress (fun k -> List.iter (fun file ->
    k();

    let (_xs, stat) =
     Common.save_excursion Flag.error_recovery true (fun () ->
     Common.save_excursion Flag.exn_when_lexical_error false (fun () ->
     Common.save_excursion Flag.show_parsing_error true (fun () ->
        Parse_ruby.parse file
    ))) in

    Common.push stat stat_list;
    let s = spf "bad = %d" stat.PI.bad in
    if stat.PI.bad = 0
    then Hashtbl.add newscore file (Common2.Ok)
    else Hashtbl.add newscore file (Common2.Pb s)
  ));
  flush stdout; flush stderr;

  Parse_info.print_parsing_stat_list !stat_list;

  (* todo: could factorize with other *)
  let dirname_opt = 
    match xs with
    | [x] when Common2.is_directory x -> Some (Common.fullpath x)
    | _ -> None
  in
  let score_path = Filename.concat Config_pfff.path "tmp" in
  dirname_opt |> Common.do_option (fun dirname -> 
    pr2 "--------------------------------";
    pr2 "regression testing  information";
    pr2 "--------------------------------";
    let str = Str.global_replace (Str.regexp "/") "__" dirname in
    Common2.regression_testing newscore 
      (Filename.concat score_path
       ("score_parsing__" ^str ^ ext ^ ".marshalled"))
  );

  ()


let test_dump file =
  let ast = Parse_ruby.parse_program file in
  let s = Ast_ruby.show_program ast in
  pr s

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-tokens_ruby", "   <file>", 
  Common.mk_action_1_arg test_tokens;
  "-parse_ruby", "   <files or dirs>", 
  Common.mk_action_n_arg test_parse;
  "-dump_ruby", "   <file>", 
  Common.mk_action_1_arg test_dump;
]
