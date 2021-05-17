open Common
open OUnit
open Cst_php

(*****************************************************************************)
(* Sgrep Unit tests *)
(*****************************************************************************)

(* See https://github.com/facebook/pfff/wiki/Sgrep *)

(* run by sgrep -test *)
let sgrep_unittest =
  "sgrep php"
  >::: [
         ( "sgrep features" >:: fun () ->
           (* spec: pattern string, code string (statement), should_match boolean *)
           let triples =
             [
               (* ------------ *)
               (* spacing *)
               (* ------------ *)

               (* basic string match of course *)
               ("foo(1,2);", "foo(1,2);", true);
               ("foo(1,3);", "foo(1,2);", false);
               (* matches even when space differs *)
               ("foo(1,2);", "foo(1,     2);", true);
               (* ------------ *)
               (* metavariables *)
               (* ------------ *)

               (* expression metavariable *)
               ("foo(X);", "foo(1);", true);
               ("foo(X);", "foo(1+1);", true);
               (* metavariable naming conventions *)
               ("foo(X1);", "foo(1);", true);
               ("foo(X1_MISC);", "foo(1);", true);
               ("foo(X_MISC);", "foo(1);", true);
               ("foo(_MISC);", "foo(1);", false);
               (* variable metavariable *)
               ("foo($X);", "foo($var);", true);
               (* lvalue metavariable *)
               ("X->method();", "$this->method();", true);
               ("X->method();", "$this->foo()->method();", true);
               (* "linear" patterns, a la Prolog *)
               ("X && X;", "($a || $b) && ($a || $b);", true);
               ("foo($E, $E);", "foo($x, $x);", true);
               ("foo($E, $E);", "foo($x, $y);", false);
               (* many arguments metavariables *)
               ("foo(MANYARGS);", "foo(1,2,3);", true);
               ("foo(MANYARGS2);", "foo(1,2,3);", true);
               ("foo(MANYARGS);", "foo();", true);
               ("foo(X, MANYARGS);", "foo(1,2,3);", true);
               (* ... also match when there is no additional arguments *)
               ("foo(X, MANYARGS);", "foo(1);", true);
               (* metavariables on function name *)
               ("X(1,2);", "foo(1,2);", true);
               (* metavariables on class name *)
               ("X::foo();", "Ent::foo();", true);
               (* metavariable string for identifiers *)
               ("foo('X');", "foo('a_func');", true);
               (* metavariable on reference arguments *)
               ("foo(X,Y);", "foo(&$a, $b);", true);
               (* metavariable on class name reference *)
               ("new X(...);", "new $dyn();", true);
               ("new X(...);", "new self();", true);
               (* ------------ *)
               (* ... *)
               (* ------------ *)

               (* '...' in funcall *)
               ("foo(...);", "foo();", true);
               ("foo(...);", "foo(1);", true);
               ("foo(...);", "foo(1,2);", true);
               ("foo(X,...);", "foo(1,2);", true);
               (* ... also match when there is no additional arguments *)
               ("foo(X,...);", "foo(1);", true);
               (* TODO: foo(..., 3, ...), foo(1,2,3,4) *)

               (* '...' in arrays *)
               ("foo(X, array(...));", "foo(1, array(2, 3));", true);
               (* '...' in strings *)
               ("foo(\"...\");", "foo(\"a string\");", true);
               ("foo(\"...\");", "foo(\"a string\" . \"another string\");", true);
               (* '...' in new *)
               ("new Foo(...);", "new Foo(1);", true);
               ("new Foo(...);", "new Foo();", true);
               ("new Foo(...);", "new Foo;", true);
               (* ------------ *)
               (* Misc isomorphisms *)
               (* ------------ *)

               (* isomorphism on "keyword" arguments *)
               ("foo(true);", "foo($x=true);", true);
               ("foo(true);", "foo(true);", true);
               (* isomorphisms on trailing comma *)
               ("foo(true);", "foo(true,);", true);
               ("foo(true,);", "foo(true);", true);
               (* interaction of ... and trailing comma *)
               ("foo(A, ...);", "foo(1);", true);
               ("foo(A, ...);", "foo(1,);", true);
               (* we want sgrep/spatch to be case insensitive, like PHP *)
               ("foo(...);", "Foo(true);", true);
               ("Foo(...);", "foo(true);", true);
               ("foo(...);", "Fo0(true);", false);
               (* more complex expressions *)
               ("strstr(...) == false;", "strstr($x)==false;", true);
               (* regexp, PCRE syntax *)
               ("foo('=~/.*CONSTANT/');", "foo('MY_CONSTANT');", true);
               ("foo('=~/.*CONSTANT/');", "foo('MY_CONSTAN');", false);
               (* statements *)
               ("if(X) { foo(); }", "if(true) { foo(); }", true);
               (* ------------ *)
               (* xhp patterns *)
               (* ------------ *)

               (* order does not matter *)
               ( "return <x:frag border=\"1\" foo=\"2\" ></x:frag>;",
                 "return <x:frag foo=\"2\" border=\"1\" ></x:frag>;",
                 true );
               ( "return <x:frag border=\"1\" foo=\"2\" ></x:frag>;",
                 "return <x:frag foo=\"3\" border=\"1\" ></x:frag>;",
                 false );
               (* concrete code can have more fields *)
               ( "return <x:frag border=\"1\"></x:frag>;",
                 "return <x:frag foo=\"2\" border=\"1\" ></x:frag>;",
                 true );
               ("return <x:frag />;", "return <x:frag border=\"1\" />;", true);
               (* concrete code can have a body *)
               ( "return <x:frag border=\"1\"></x:frag>;",
                 "return <x:frag border=\"1\" >this is text</x:frag>;",
                 true );
               (* metavariable on xhp tag *)
               ( "return <X label=\"1\"></X>;",
                 "return <x:frag label=\"1\"></x:frag>;",
                 true );
               (* metavariable on xhp label *)
               ( "return <X Y=\"1\"></X>;",
                 "return <x:frag label=\"1\"></x:frag>;",
                 true );
               (* xhp classes have a different syntax when used in xml context (<tag...)
                * and when used as regular classes (:tag...), but a metavariable should
                * accomodate both syntax
                *)
               (*TODO does not work since the lvalue/expr unif, weird
                     "return <X>{X::foo()}</X>;", "return <x:frag>{:x:frag::foo()}</x:frag>;",
                     true;
               *)
               (* TODO:
                *  Xhp should also match XhpSingleton or optional closing tag
                * "return <x:frag></x:frag>;", "return <x:frag />;", true;
                * "return <x:frag></x:frag>;", "return <x:frag></>;", true;
                *)
             ]
           in
           triples
           |> List.iter (fun (spattern, scode, should_match) ->
                  match
                    (Sgrep_php.parse spattern, Parse_php.any_of_string scode)
                  with
                  | Stmt2 pattern, Stmt2 code ->
                      let matches_with_env =
                        Matching_php.match_st_st pattern code
                      in
                      if should_match then
                        assert_bool
                          (spf "pattern:|%s| should match |%s" spattern scode)
                          (matches_with_env <> [])
                      else
                        assert_bool
                          (spf "pattern:|%s| should not match |%s" spattern
                             scode)
                          (matches_with_env = [])
                  | _ ->
                      assert_failure "parsing problem in sgrep pattern parsing")
         );
         ( "sgrep variable metavars matching" >:: fun () ->
           let pattern = Parse_php.any_of_string "foo($E, $E);" in
           let code = Parse_php.any_of_string "foo($x, $y);" in
           match (pattern, code) with
           | Stmt2 pattern, Stmt2 code ->
               let matches_with_env = Matching_php.match_st_st pattern code in
               assert_bool "it should not match" (matches_with_env = [])
           | _ -> assert_failure "parsing problem in sgrep pattern parsing" );
         (* TODO: should use sgrep_fuzzy for that
            "toplevel sgrep matching" >:: (fun () ->
             (* pattern string, code string *)
             let pairs = [
               "function X(){ return Y(...); }","function foo(){ return bar(); }";
               "function X(...){ return Y(...); }","function foo($x){ return bar(); }";
             ] in
             pairs +> List.iter (fun (spattern, scode) ->
               match Sgrep_php.parse spattern, Parse_php.any_of_string scode with
               | Toplevel pattern, Toplevel code ->
                   let matches_with_env = Matching_php.match_top_top pattern code in
                   assert_bool (spf "pattern:|%s| should match |%s" spattern scode)
                     (matches_with_env <> []);
             | _ ->
                 assert_failure "parsing problem in sgrep pattern parsing"
             )
            );
         *)
       ]

(*****************************************************************************)
(* Spatch Unit tests *)
(*****************************************************************************)

(* See https://github.com/facebook/pfff/wiki/Spatch *)

(* run by spatch -test *)
let spatch_unittest =
  "spatch regressions files" >:: fun () ->
  let testdir = Filename.concat Config_pfff.path "tests/php/spatch/" in
  let expfiles = Common2.glob (testdir ^ "*.exp") in

  expfiles
  |> List.iter (fun expfile ->
         (* todo: this regexp should just be .*? but ocaml regexp do not
          * have the greedy feature :( Also note that expfile is a fullpath
          * so it can contains /, hence this ugly regexp
          *)
         if expfile =~ "\\([a-zA-Z_/]+\\)\\([0-9]*\\)\\.exp$" then (
           let prefix, variant = Common.matched2 expfile in
           let spatchfile = prefix ^ ".spatch" in
           let phpfile = prefix ^ variant ^ ".php" in

           let pattern = Spatch_php.parse spatchfile in
           let resopt =
             try Spatch_php.spatch pattern phpfile
             with Failure s ->
               assert_failure
                 (spf "spatch on %s have resulted in exn = %s" phpfile s)
           in

           let file_res =
             match resopt with
             | None -> phpfile
             | Some s ->
                 let tmpfile = Common.new_temp_file "spatch_test" ".php" in
                 Common.write_file ~file:tmpfile s;
                 tmpfile
           in
           let diff = Common2.unix_diff file_res expfile in
           diff |> List.iter pr;
           if List.length diff > 1 then
             assert_failure
               (spf "spatch %s on %s should have resulted in %s"
                  (Filename.basename spatchfile)
                  (Filename.basename phpfile)
                  (Filename.basename expfile)) )
         else failwith ("wrong format for expfile: " ^ expfile))

(*****************************************************************************)
(* Generic refactorings *)
(*****************************************************************************)
let refactoring_unittest =
  "refactoring php"
  >::: [
         ( "adding return type" >:: fun () ->
           let file_content = "function foo() { }" in
           let refactoring =
             ( Refactoring_code.AddReturnType "int",
               Some
                 {
                   Refactoring_code.file = "";
                   (* line 2 because tmp_php_file_from_string below add a leading
                    * <php\n
                    *)
                   line = 2;
                   col = 9;
                 } )
           in
           let file = Parse_php.tmp_php_file_from_string file_content in
           let ast2, _stat = Parse_php.parse file in
           let res = Refactoring_code_php.refactor [ refactoring ] ast2 in
           assert_equal res "<?php\nfunction foo(): int { }" );
         ( "adding parameter type" >:: fun () ->
           let file_content = "function foo($x) { }" in
           let refactoring =
             (Refactoring_code.AddTypeHintParameter "int", None)
           in
           let file = Parse_php.tmp_php_file_from_string file_content in
           let ast2, _stat = Parse_php.parse file in
           let res = Refactoring_code_php.refactor [ refactoring ] ast2 in
           assert_equal res "<?php\nfunction foo(int $x) { }" );
         ( "adding member type" >:: fun () ->
           let file_content = "class X { private $x; }" in
           let refactoring = (Refactoring_code.AddTypeMember "int", None) in
           let file = Parse_php.tmp_php_file_from_string file_content in
           let ast2, _stat = Parse_php.parse file in
           let res = Refactoring_code_php.refactor [ refactoring ] ast2 in
           assert_equal res "<?php\nclass X { private int $x; }" );
         ( "optionize type" >:: fun () ->
           let file_content = "function foo(int $x) { }" in
           let refactoring = (Refactoring_code.OptionizeTypeParameter, None) in
           let file = Parse_php.tmp_php_file_from_string file_content in
           let ast2, _stat = Parse_php.parse file in
           let res = Refactoring_code_php.refactor [ refactoring ] ast2 in
           assert_equal res "<?php\nfunction foo(?int $x) { }" );
         (* this used to raise a NoOriginTok exn because the name of the lambda
          * is a fakeInfo and it was used in a line/col comparison
          *)
         ( "adding return type and closure" >:: fun () ->
           let file_content = "function foo() { $x = function() { }; }" in
           let refactoring =
             ( Refactoring_code.AddReturnType "int",
               Some { Refactoring_code.file = ""; line = 2; col = 9 } )
           in
           let file = Parse_php.tmp_php_file_from_string file_content in
           let ast2, _stat = Parse_php.parse file in
           let res = Refactoring_code_php.refactor [ refactoring ] ast2 in
           assert_equal res
             "<?php\nfunction foo(): int { $x = function() { }; }" );
         ( "split members" >:: fun () ->
           let file_content = "\nclass X {\n  private $x, $y, $z = 1;\n}" in
           let refactoring = (Refactoring_code.SplitMembers, None) in
           let file = Parse_php.tmp_php_file_from_string file_content in
           let ast2, _stat = Parse_php.parse file in
           let res = Refactoring_code_php.refactor [ refactoring ] ast2 in
           (* TODO: fix weird indentation since switch to new unparse_php algo *)
           assert_equal res
             "<?php\n\n\
              class X {\n\
             \  private $x;\n\
             \  private  $y;\n\
             \  private  $z = 1;\n\
              }" );
         ( "add interface" >:: fun () ->
           let file_content = "class X { }" in
           let refactoring =
             (Refactoring_code.AddInterface (Some "X", "I"), None)
           in

           let file = Parse_php.tmp_php_file_from_string file_content in
           let ast_and_toks = Parse_php.ast_and_tokens file in
           let res =
             Refactoring_code_php.refactor [ refactoring ] ast_and_toks
           in
           assert_equal res "<?php\nclass X implements I { }";

           let file_content = "class X implements B { }" in
           let refactoring =
             (Refactoring_code.AddInterface (Some "X", "I"), None)
           in

           let file = Parse_php.tmp_php_file_from_string file_content in
           let ast_and_toks = Parse_php.ast_and_tokens file in
           let res =
             Refactoring_code_php.refactor [ refactoring ] ast_and_toks
           in
           assert_equal res "<?php\nclass X implements B, I { }" );
         ( "remove interface" >:: fun () ->
           let file_content = "class X implements I { }" in
           let refactoring =
             (Refactoring_code.RemoveInterface (Some "X", "I"), None)
           in
           let file = Parse_php.tmp_php_file_from_string file_content in
           let ast_and_toks = Parse_php.ast_and_tokens file in
           let res =
             Refactoring_code_php.refactor [ refactoring ] ast_and_toks
           in
           assert_equal "<?php\nclass X { }" res;

           let file_content = "class X implements B, I { }" in
           let refactoring =
             (Refactoring_code.RemoveInterface (Some "X", "I"), None)
           in
           let file = Parse_php.tmp_php_file_from_string file_content in
           let ast_and_toks = Parse_php.ast_and_tokens file in
           let res =
             Refactoring_code_php.refactor [ refactoring ] ast_and_toks
           in
           assert_equal "<?php\nclass X implements B { }" res;

           let file_content = "class X implements I, B { }" in
           let refactoring =
             (Refactoring_code.RemoveInterface (Some "X", "I"), None)
           in
           let file = Parse_php.tmp_php_file_from_string file_content in
           let ast_and_toks = Parse_php.ast_and_tokens file in
           let res =
             Refactoring_code_php.refactor [ refactoring ] ast_and_toks
           in
           assert_equal "<?php\nclass X implements B { }" res );
       ]

let unparser_unittest =
  "unparser_php"
  >::: [
         ( "add arguments" >:: fun () ->
           let file_content = "function foo() { $x = printf(\"%s %d\", ); }" in
           let file = Parse_php.tmp_php_file_from_string file_content in
           let ast, toks = Parse_php.ast_and_tokens file in
           toks
           |> List.iter (fun tok ->
                  match tok with
                  | Parser_php.TCPAR info when Parse_info.str_of_info info = ")"
                    ->
                      info.Parse_info.transfo <-
                        Parse_info.AddArgsBefore [ "str"; "1" ]
                  | _ -> ());
           let res =
             Unparse_php.string_of_program_with_comments_using_transfo
               (ast, toks)
           in
           assert_equal
             "<?php\nfunction foo(str, 1) { $x = printf(\"%s %d\", str, 1); }"
             res );
         ( "unparsing xhp" >:: fun () ->
           let file_content = "$x = <div a=\"1\" b=\"2\">foo</div>;" in
           let file = Parse_php.tmp_php_file_from_string file_content in
           let ast, toks = Parse_php.ast_and_tokens file in
           let res =
             Unparse_php.string_of_program_with_comments_using_transfo
               (ast, toks)
           in
           assert_equal "<?php\n$x = <div a=\"1\" b=\"2\">foo</div>;" res );
         ( "string_of_any" >:: fun () ->
           let str = "foo(<div a=\"1\">bar</div>)" in
           let e = Parse_php.expr_of_string str in
           let str2 = Unparse_php.string_of_expr e in
           assert_equal str str2 );
       ]

(*****************************************************************************)
(* Final suite *)
(*****************************************************************************)

let unittest =
  "matcher_php"
  >::: [
         sgrep_unittest;
         spatch_unittest;
         refactoring_unittest;
         unparser_unittest;
       ]
