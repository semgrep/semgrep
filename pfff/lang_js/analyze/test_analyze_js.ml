open Common

(*module Flag = Flag_parsing*)
module PI = Parse_info

(*****************************************************************************)
(* Simple AST *)
(*****************************************************************************)
(* Note that the code below try to be close to what we do in 
 * semgrep -lang js -test_parse_lang.
 * We do a bit more stuff than in pfff -parse_js.
 *)
let test_parse_simple xs =
  let xs = List.map Common.fullpath xs in

  let fullxs = Lib_parsing_js.find_source_files_of_dir_or_files xs
    |> Skip_code.filter_files_if_skip_list ~root:xs
  in

  let stat_list = ref [] in
  let newscore  = Common2.empty_score () in
  let ext = "ast_js" in

  fullxs |> Console.progress (fun k -> List.iter (fun file ->
    k();
    let (stat) = 
    try 
      let (xs, stat) = 
(*
       Common.save_excursion Flag.error_recovery true (fun () ->
       Common.save_excursion Flag.exn_when_lexical_error false (fun () ->
*) ((
                Parse_js.parse (* ~timeout:5 *) file 
       ))
      in
      let cst = 
          match xs with
          | Some cst, _ -> cst
          | None, _ -> failwith "parse error"
      in
      let ast = Ast_js_build.program cst in
      (* just to have a better comparison with 
       * semgrep -lang js -test_parse_lang *)
      let _gen = Js_to_generic.program ast in      
      (* note that there's still Naming_AST.resolve and
       * Constant_propafation.propagate done with -test_parse_lang
       *)
      stat
    with exn ->
      (match exn with
      | Ast_js_build.TodoConstruct (s, tok)
      | Ast_js_build.UnhandledConstruct (s, tok)
        -> 
        pr2 s;
        pr2 (Parse_info.error_message_info tok);
        Parse_info.bad_stat file
      | _ -> Parse_info.bad_stat file
      )
    in
    Common.push stat stat_list;
    let s = spf "bad = %d" stat.PI.bad in
    if stat.PI.bad = 0
    then Hashtbl.add newscore file (Common2.Ok)
    else Hashtbl.add newscore file (Common2.Pb s)
  ));

  PI.print_parsing_stat_list !stat_list;

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


let test_dump_ast file =
  try 
    let cst = Parse_js.parse_program file in
    let ast = 
      Common.save_excursion Ast_js_build.transpile_xml false (fun () ->
      Common.save_excursion Ast_js_build.transpile_pattern false (fun () ->
        Ast_js_build.program cst 
      ))
    in
    let s = Ast_js.show_program ast in
    pr s
   with exn ->
      (match exn with
      | Parse_info.Lexical_error (s, tok)
      | Ast_js_build.TodoConstruct (s, tok)
      | Ast_js_build.UnhandledConstruct (s, tok)
        -> 
        pr2 s;
        pr2 (Parse_info.error_message_info tok);
        raise exn
      | _ -> raise exn
      )

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-parse_ast_js", "   <files or dirs>",
  Common.mk_action_n_arg test_parse_simple;
  "-dump_ast_js", "   <file>",
  Common.mk_action_1_arg test_dump_ast;
]
