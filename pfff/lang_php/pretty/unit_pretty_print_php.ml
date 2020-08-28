open Common

open OUnit

module T = Parser_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let pp_file file =
  let (ast, tokens), _ = Parse_php.parse file in
  let ast = Ast_pp_build.program_with_comments tokens ast in

  let buf = Buffer.create 256 in
  let env = Pretty_print_code.empty (fun s -> Buffer.add_string buf s) in
  Pretty_print.program_env env ast;
  Buffer.contents buf

let pp_string s =
  let file = Parse_php.tmp_php_file_from_string s in
  pp_file file

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)
let unittest = "pretty print php" >::: [
  "basic" >:: (fun () ->

    let content = 
      "function foo() { }" in
    let expect =
"<?php
function foo() {
}
" in
    assert_equal 
      ~msg:"it should put the ending brace on another line"
      expect (pp_string content);
  );

  "comments" >:: (fun () ->
    let content = 
      " function foo() { // first comment\n $x = 1; }" in
    let expect =
"<?php
function foo() {
  // first comment
  $x = 1;
}
" in
    assert_equal 
      ~msg:"it should maintain comments"
      expect (pp_string content);
  );  

  "unparse/pretty-print mix (chunk management)" >:: (fun () ->
    let content =
"
function foo() { }

function bar() { }
"
    in
    let file = Parse_php.tmp_php_file_from_string content in
    let (ast, toks), _ = Parse_php.parse file in
    let chunks = Unparse_pretty_print_mix.split_chunks toks ast in
    (match chunks with
    | [(Unparse_pretty_print_mix.Func def1, toks1);
       (Unparse_pretty_print_mix.Func def2, toks2);
       (Unparse_pretty_print_mix.FinalDef _, _toks3);
      ] ->
        let ast1 = Ast_pp_build.toplevels toks1 [Cst_php.FuncDef def1] in
        let ast2 = Ast_pp_build.toplevels toks2 [Cst_php.FuncDef def2] in

        (match toks1, List.rev toks1 with
        | T.T_OPEN_TAG _::T.TNewline _::T.TNewline _::T.T_FUNCTION _::_,  
          T.TNewline _::T.TCBRACE _::_ -> ()
        | _ -> 
            assert_failure "it should chunk w/ the open tag and ending newline"
        );
        (match ast1 with
        | [Ast_pp.StmtEsthet Ast_pp.Comment "<?php"; 
           Ast_pp.StmtEsthet Ast_pp.Newline; 
           Ast_pp.FuncDef _] -> ()
        | _ ->
            assert_failure (spf "wrong ast1: %s " (Common.dump ast1))
        );

        (match toks2, List.rev toks2 with
        | T.TNewline _::T.T_FUNCTION _::_,  
          T.TNewline _::T.TCBRACE _::_ -> ()
        | _ -> 
            assert_failure "it should chunk w/ the starting and ending newline"
        );
        (* pad: bug, got only a FuncDef even if the tokens contained a 
         * newline.
         *)
        (match ast2 with
        | [Ast_pp.StmtEsthet Ast_pp.Newline; Ast_pp.FuncDef _] -> ()
        | _ ->
            assert_failure (spf "wrong ast2: %s " (Common.dump ast2))
        )
    | _ -> 
        assert_failure (spf "wrong chunks: %s " (Common.dump chunks))
    )
  );


  "regression files" >:: (fun () ->
    let dir = Filename.concat Config_pfff.path "/tests/php/pretty" in
    let files = Common2.glob (spf "%s/*.php" dir) in
    files |> List.iter (fun file ->
      let res = pp_file file in
      assert_equal
        ~msg:(spf "it should correctly pretty print %s" file)
        (Common.read_file file) 
        res
    )
  );
]
