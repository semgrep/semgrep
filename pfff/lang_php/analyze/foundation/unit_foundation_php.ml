open Common
open OUnit

module Ast = Cst_php
module A = Annotation_php
module E = Entity_code
module G = Graph_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(*---------------------------------------------------------------------------*)
(* Ast simple *)
(*---------------------------------------------------------------------------*)

let ast_unittest =
  "ast_simple regression files" >:: (fun () ->
    let dir1 = Filename.concat Config_pfff.path "/tests/php/parsing" in
    let dir2 = Filename.concat Config_pfff.path "/tests/php/semantic" in
    let files =
      (Common2.glob (spf "%s/*.php" dir1) @ Common2.glob (spf "%s/*.php" dir2))
    in
    files |> List.iter (fun file ->
      try
        let cst = Parse_php.parse_program file in
        let _ast = Ast_php_build.program cst in
        ()
      with exn ->
        assert_failure (spf "it should correctly parse %s, exn = %s"
                           file (Common.exn_to_s exn))
      )
    )

(*---------------------------------------------------------------------------*)
(* Defs/uses *)
(*---------------------------------------------------------------------------*)

let defs_uses_unittest =
  "defs_uses" >::: [
    "functions uses" >:: (fun () ->
      let file_content = "
foo1();
" in
      let ast = Parse_php.program_of_string file_content in
      let uses = Defs_uses_php.uses_of_any (Ast.Program ast) in
      let uses_strings =
        uses |> List.map (fun (name, _kind) -> Ast.str_of_name name) in
      assert_equal
        (sort ["foo1"])
        (sort uses_strings);
    );

    "classes uses" >:: (fun () ->
      let file_content = "
new Foo1();
if($x instanceof Foo2) { }

echo Foo3::Cst;
echo Foo4::$var;
Foo5::method();
Foo6::$f();

class X1 extends Foo7 { }
class X2 implements Foo8 { }
try { } catch(Foo9 $x) { }
function foo1(Foo10 $x) { }

$x = <x:xhp1></x:xhp1>;
$x = <x:xhp2/>;
" in
      let ast = Parse_php.program_of_string file_content in
      let uses = Defs_uses_php.uses_of_any (Ast.Program ast) in
      let str_of_name = function
        | Ast.XName [Ast.QI (Ast.Name (s, _))] -> s
        | Ast.XName [Ast.QI (Ast.XhpName (xhp_tag, _))] ->
            Common.join ":" xhp_tag
        | _ -> raise Impossible
      in
      let uses_strings =
        uses |> List.map (fun (name, _kind) -> str_of_name name) in

      let classes =
        (Common2.enum 1 10) |> List.map (fun i -> spf "Foo%d" i) in
      let xhp_classes =
        (Common2.enum 1 2) |> List.map (fun i -> spf "x:xhp%d" i) in
      assert_equal
        (sort (classes @ xhp_classes))
        (sort uses_strings);
    );
  ]

(*---------------------------------------------------------------------------*)
(* Tags *)
(*---------------------------------------------------------------------------*)
let tags_unittest =
    "tags_php" >::: [

      "basic tags" >:: (fun () ->
        let file_content = "
            function foo() { }
            class A { }
            interface B { }
            trait C { }
            const CST = 1;
            define('OldCst',1);
            type T = int;
        "
        in
        let tmpfile = Parse_php.tmp_php_file_from_string file_content in
        let tags =
          Tags_php.php_defs_of_files_or_dirs ~verbose:false [tmpfile] in
        (match tags with
        | [file, tags_in_file] ->
            assert_equal tmpfile file;
            let xs = tags_in_file |> List.map (fun x ->
              x.Tags_file.tagname, x.Tags_file.kind
            ) in
            assert_equal ~msg:"it should contain the right entries" [
              "foo", E.Function;
              "A", E.Class;
              "B", E.Class(* E.Interface*);
              "C", E.Class(* E.Trait*);
              "CST", E.Constant;
              "OldCst", E.Constant;
              "T", E.Type;
            ]
            xs
        | _ ->
            assert_failure "The tags should contain only one entry for one file"
        )
      );

      "method tags" >:: (fun () ->
        let file_content = "
           class A {
              function a_method() { }
              function ambiguous_with_function() { }
              function ambiguous_with_another_class() { }
           }
           class B {
              function ambiguous_with_another_class() { }
           }
           function ambiguous_with_function() { }
        " in
        let tmpfile = Parse_php.tmp_php_file_from_string file_content in
        let tags =
          Tags_php.php_defs_of_files_or_dirs ~verbose:false [tmpfile] in
        (match tags with
        | [file, tags_in_file] ->
            assert_equal tmpfile file;
            let xs = tags_in_file |> List.map (fun x ->
              x.Tags_file.tagname, x.Tags_file.kind
            ) in
            (* we now generate two tags per method, one for 'a_method',
             * and one for 'A::a_method', but only if there is not somewhere
             * a function called a_method() or another class with the same
             * method name.
             *)
            assert_equal ~msg:"The tags should contain the right entries" [
              "A", E.Class;
              "A::a_method", E.Method;
              (* this tag is safe to generate, no ambiguity *)
              "a_method", E.Method;
              "A::ambiguous_with_function", E.Method;
              "A::ambiguous_with_another_class", E.Method;
              "B", E.Class;
              "B::ambiguous_with_another_class", E.Method;
              "ambiguous_with_function", E.Function;
            ]
            xs

        | _ ->
            assert_failure "The tags should contain only one entry for one file"
        )
      );

      "magic tags" >:: (fun () ->
        let file_content = "
           class A {
              function yieldSomething() { }
              function prepareSomethingElse() { }
           }
        " in
        let tmpfile = Parse_php.tmp_php_file_from_string file_content in
        let tags =
          Tags_php.php_defs_of_files_or_dirs ~verbose:false [tmpfile] in
        (match tags with
        | [file, tags_in_file] ->
            assert_equal tmpfile file;
            let xs = tags_in_file |> List.map (fun x ->
              x.Tags_file.tagname, x.Tags_file.kind
            ) in
            let desired = [
              "A", E.Class;
              "A::yieldSomething", E.Method;
              "yieldSomething", E.Method;
              "A::genSomething", E.Method;
              "genSomething", E.Method;
              "A::prepareSomething", E.Method;
              "prepareSomething", E.Method;
              "A::getSomething", E.Method;
              "getSomething", E.Method;
              "A::prepareSomethingElse", E.Method;
              "prepareSomethingElse", E.Method;
              "A::genSomethingElse", E.Method;
              "genSomethingElse", E.Method;
            ]
            in
            assert_equal ~msg:"Tags should contain entries for the right magic methods"
              desired
              xs

        | _ ->
            assert_failure "The tags should contain only one entry for one file"
        )
      );

      "xhp tags" >:: (fun () ->
        let file_content = "class :x:foo { }" in
        let tmpfile = Parse_php.tmp_php_file_from_string file_content in
        let tags =
          Tags_php.php_defs_of_files_or_dirs ~verbose:false [tmpfile] in
        let all_tags = tags |> List.map snd |> List.flatten in
        assert_bool
          ~msg:"it should contain an entry for the :x:... classname form"
          (all_tags |> List.exists (fun t ->
            t.Tags_file.tagname =$= ":x:foo"));
        assert_bool
          ~msg:"it should contain an entry for the x:... classname form"
          (all_tags |> List.exists (fun t ->
            t.Tags_file.tagname =$= "x:foo"));
      );
    ]

(*---------------------------------------------------------------------------*)
(* Codegraph *)
(*---------------------------------------------------------------------------*)
let _codegraph_unittest = 
  "codegraph_php" >::: [

    "basic class def/uses" >:: (fun () ->
      let file_content = "
class A {
 public $fld;
 public function test_field() {
   return $this->fld;
 }
}
function test_useA() {
  $o = new A();
}
"
      in
      let tmpfile = Parse_php.tmp_php_file_from_string file_content in
      let (g, _stat) = Graph_code_php.build 
        ~verbose:false ~logfile:"/dev/null" "/tmp" [tmpfile] in

      let src = ("A.$fld", E.Field) in
      let pred = G.pred src G.Use g in
      assert_equal
        ~msg:"it should link the use of a PHP field to its def"
        ["A.test_field", E.Method]
        pred;

      let src = ("A", E.Class) in
      let pred = G.pred src G.Use g in
      assert_equal
        ~msg:"it should link the use of a class to its use"
        ["test_useA", E.Function]
        pred;
    );

    "required xhp field" >:: (fun () ->
      let file_content = "
        class :x:required {
          attribute
          int req_int @required;
}
"
      in
      let tmpfile = Parse_php.tmp_php_file_from_string file_content in
      let (g, _) = Graph_code_php.build 
        ~verbose:false ~logfile:"/dev/null" "/tmp" [tmpfile] in
      let field = (":x:required.req_int=", E.Field) in
      let nodeinfo = G.nodeinfo field g in
      let props = nodeinfo.G.props in
      assert_equal
        ~msg:"it should annotate xhp required field"
        props
        [E.Required]
    );

    "regression files" >:: (fun () ->
      let root = Filename.concat Config_pfff.path "/tests/php/codegraph" in
      let skip_list = Skip_code.load (Filename.concat root "skip_list.txt") in
      let files = 
        Lib_parsing_php.find_source_files_of_dir_or_files [root] 
        |> Skip_code.filter_files skip_list root
        |> Skip_code.reorder_files_skip_errors_last skip_list root
      in
      let is_skip_error_file = Skip_code.build_filter_errors_file skip_list in
      let logfile = Filename.concat root "pfff_test.log" in
      let expected_logfile = Filename.concat root "pfff_test.exp" in
      let (g, _) = Graph_code_php.build 
        ~verbose:false ~readable_file_format:true ~logfile ~is_skip_error_file
        root files in

      let xs = Common2.unix_diff logfile expected_logfile in
      assert_bool
        ~msg:("it should generate the right errors in pfff_test.log, diff = "^
                 (Common.join "\n" xs))
        (null xs);

      let src = (":x:misc.xstr=", E.Field) in
      let pred = G.pred src G.Use g in
      assert_equal
        ~msg:"it should link the use of an xhp attribute to its def"
        pred
        ["xhp_use.php", E.File];

      let src = (":x:super.superstr=", E.Field) in
      let pred = G.pred src G.Use g in
      assert_equal
        ~msg:"it should link the use of an xhp attribute to its def"
        pred
        ["xhp_use.php", E.File];


    )
  ]
(*---------------------------------------------------------------------------*)
(* Annotations *)
(*---------------------------------------------------------------------------*)

let annotation_unittest =
  "annotation_php" >::: [
    "data provider annotations" >:: (fun () ->
      let file_content = "
        class A {
          // @dataProvider provider
          public function foo() { }
          public function provider() { }
          // @dataProvider B::provider2
          public function foo2() { }
          /**
           * @dataProvider provider3
           */
          public function foo3() { }
          /*
           * @dataProvider provider4
           */
          public function foo4() { }
}
"
      in
      let tmpfile = Parse_php.tmp_php_file_from_string file_content in
      let (ast_with_comments, _stat) = Parse_php.parse tmpfile in
      let annots =
        Annotation_php.annotations_of_program_with_comments ast_with_comments
          |> List.map fst
      in
      assert_equal ~msg:"should have the DataProvider annotations"
        (sort [A.DataProvider (A.Method "provider");
               A.DataProvider (A.MethodExternal ("B", "provider2"));
               A.DataProvider (A.Method "provider3");
               A.DataProvider (A.Method "provider4");
        ])
        (sort annots);
    );
  ]

(*---------------------------------------------------------------------------*)
(* Include use/def *)
(*---------------------------------------------------------------------------*)

let include_unittest =
  "include_require" >::: (
      let env = {
        Env_php.global_arrays = Common.hash_of_list [
          "_SERVER", Common.hash_of_list [
            "PHP_ROOT", "/home/foo/www";
          ];
        ];
        Env_php.constants = Common.hash_of_list [];
        Env_php.globals = Common.hash_of_list [];
        Env_php.globals_specials = (fun _s _dir -> None);
      }
      in [
      (* I was previously using Filename.concat in include_require_php.ml
       * which generate paths like a/b//c/foo.php which is annoying
       * and can confuse some of our analysis. Check that no regression
       * on this issue.
       *)
      "resolving path" >:: (fun () ->
        let file = "
        require_once $_SERVER['PHP_ROOT'].'/lib/alerts/alerts.php';
        "
        in
        let ast = Parse_php.program_of_string file in
        let incs = Include_require_php.top_increq_of_program ast in
        match incs with
        | [(_inc_kind,_tok, incexpr)] ->
            let path =
              Include_require_php.resolve_path (env, "/") incexpr in
            assert_equal
              (Some "/home/foo/www/lib/alerts/alerts.php")
              path;

        | _ ->
            assert_failure
              "wrong number of elements returned by increq_of_program"
      );

      ])

(*---------------------------------------------------------------------------*)
(* Final suite *)
(*---------------------------------------------------------------------------*)

let unittest =
  "foundation_php" >::: [
    ast_unittest;
    defs_uses_unittest;
    tags_unittest;
(* TODO    codegraph_unittest; should be in check_generic *)
    annotation_unittest;
    include_unittest;
  ]
