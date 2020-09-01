open Common
open OUnit

(*****************************************************************************)
(* Sgrep Fuzzy Unit tests *)
(*****************************************************************************)

let sgrep_fuzzy_unittest ~ast_fuzzy_of_string =
  "sgrep-fuzzy features" >:: (fun () ->

    (* spec: pattern string, code string, should_match boolean *)
    let triples = [

      (* ------------ *)
      (* spacing *)
      (* ------------ *)

      (* basic string match of course *)
      "foo(1,2);", "foo(1,2);", true;
      "foo(1,3);", "foo(1,2);", false;
      (* matches even when space or newline differs *)
      "foo(1,2);", "foo(1,     2);", true;
      "foo(1,2);", "foo(1,
                        2);", true;
      (* matches even when have comments in the middle *)
      "foo(1,2);", "foo(1, /* foo */ 2);", true;

      (* ------------ *)
      (* metavariables *)
      (* ------------ *)

      (* for identifiers *)
      "class $X { int x; }", "class Foo { int x; }", true;
      (* for expressions *)
      "foo($X);",  "foo(1);", true;
      "foo($X);",  "foo(1+1);", true;
      (* for lvalues *)
      "$X->method();",  "this->method();", true;
(*TODO      "$X->method();"  ,  "this->foo()->method();", true; *)
(* this will work though: "->method();"  ,  "$this->foo()->method();", true; *)

      (* "linear" patterns, a la Prolog *)
      "$X & $X;", "(a | b) & (a | b);", true;
      "foo($X, $X);", "foo(a, a);", true;
      "foo($X, $X);", "foo(a, b);", false;

      (* many arguments metavariables *)
(*TODO      "foo($MANYARGS);", "foo(1,2,3);", true; *)

      (* metavariable on function name *)
      "$X(1,2);", "foo(1,2);", true;
      (* metavariable on class name *)
      "$X::foo();", "Ent::foo();", true;
      (* metavariable string for identifiers *)
(*TODO      "foo('X');", "foo('a_func');", true; *)
      (* metavariable on reference arguments *)
      "foo($X,$Y);", "foo(&a, b);", true;
      (* metavariable on class name reference *)
      "new $X(...);", "new $dyn();", true;
      "new $X(...);", "new self();", true;

      (* ------------ *)
      (* ... *)
      (* ------------ *)

      (* for stmts *)
      "class Foo { ... }", "class Foo { int x; }", true;

      (* '...' in funcall *)
      "foo(...);", "foo();", true;
      "foo(...);", "foo(1);", true;
      "foo(...);", "foo(1,2);", true;
      "foo($X,...);", "foo(1,2);", true;
      (* ... also match when there is no additional arguments *)
      "foo($X,...);", "foo(1);", true;
      (* TODO: foo(..., 3, ...), foo(1,2,3,4) *)

      (* '...' in arrays *)
      "foo($X, array(...));",  "foo(1, array(2, 3));", true;

      (* '...' in strings *)
(*TODO      "foo(\"...\");", "foo(\"a string\");", true; *)
(*TODO      "foo(\"...\");", "foo(\"a string\" . \"another string\");", true;*)

      (* '...' in new *)
      "new Foo(...);","new Foo(1);", true;
      "new Foo(...);","new Foo();", true;

      (* more complex expressions *)
      "strstr(...) == false;", "strstr(x)==false;", true;

      (* ------------ *)
      (* Misc isomorphisms *)
      (* ------------ *)
(*TODO      "new Foo(...);","new Foo;", true; *)

    ]
    in
    triples |> List.iter (fun (spattern, scode, should_match) ->
      let pattern = ast_fuzzy_of_string spattern in
      let code = ast_fuzzy_of_string scode in
      let matches_with_env = Matching_fuzzy.match_trees_trees pattern code in
      if should_match
      then
        assert_bool (spf "pattern:|%s| should match |%s" spattern scode)
          (matches_with_env <> [])
      else
        assert_bool (spf "pattern:|%s| should not match |%s" spattern scode)
          (matches_with_env = [])
    )
  )


(*****************************************************************************)
(* Spatch Unit tests *)
(*****************************************************************************)

(* See https://github.com/facebook/pfff/wiki/Spatch *)

(* run by spatch -test *)
let spatch_fuzzy_unittest ~ast_fuzzy_of_string ~parse_file =
  "spatch regressions files" >:: (fun () ->

    let testdir = Config_pfff.tests_path "fuzzy/spatch/" in
    let expfiles = Common2.glob (testdir ^ "*.exp") in

    expfiles |> List.iter (fun expfile ->
      (* todo: this regexp should just be .*? but ocaml regexp do not
       * have the greedy feature :( Also note that expfile is a fullpath
       * so it can contains /, hence this ugly regexp
       *)
      if expfile =~ "\\([a-zA-Z_/]+\\)\\([0-9]*\\)\\.exp$" then begin
        let (prefix, variant) = Common.matched2 expfile in
        let spatchfile = prefix ^ ".spatch" in
        let srcfile = prefix ^ variant ^ ".fuzzy" in

        let pattern =
          Spatch_fuzzy.parse
            ~pattern_of_string:ast_fuzzy_of_string
            ~ii_of_pattern:Lib_ast_fuzzy.toks_of_trees
            spatchfile
        in
        let trees, toks =
          parse_file srcfile
        in
        let was_modified = Spatch_fuzzy.spatch pattern trees in
        let resopt =
          if was_modified
          then Some (Lib_unparser.string_of_toks_using_transfo toks)
          else None
        in

        let file_res =
          match resopt with
          | None -> srcfile
          | Some s ->
            let tmpfile = Common.new_temp_file "spatch_test" ".fuzzy" in
            Common.write_file ~file:tmpfile s;
            tmpfile
        in
        let diff = Common2.unix_diff file_res expfile in
        diff |> List.iter pr;
        if List.length diff > 1
        then assert_failure
          (spf "spatch %s on %s should have resulted in %s"
              (Filename.basename spatchfile)
              (Filename.basename srcfile)
              (Filename.basename expfile))
      end
      else failwith ("wrong format for expfile: " ^ expfile)
    )
  )
