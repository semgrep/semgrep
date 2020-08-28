open Common

module E = Entity_code

open OUnit

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)
let unittest =
 "analyze_js" >::: [

  "commonjs annotations" >:: (fun () ->
      let file_content = "
/**
 * @providesModule my-module
 */
function foo() {}
/**
 * @providesLegacy other-module
 */
function bar() {}
" in
      Common2.with_tmp_file ~str:file_content ~ext:"js" (fun tmpfile ->
        let (ast_and_tokens, _stat) = Parse_js.parse tmpfile in
        let annots = 
          Annotation_js.annotations_of_program_with_comments ast_and_tokens
          |> List.map fst
        in
        assert_equal 
          ~msg:"it should extract @providesModule annotations"
          [Annotation_js.ProvidesModule "my-module";
           Annotation_js.ProvidesLegacy "other-module";
          ]
          annots
      )
    );

   "commonjs tags support" >:: (fun () ->
     let file_content = "
/**
 * @providesModule my-module
 */
function foo() {}
" in
     Common2.with_tmp_file ~str:file_content ~ext:"js" (fun tmpfile ->
       let tags = Tags_js.tags_of_files_or_dirs ~verbose:false [tmpfile] in
       (match tags |> List.map snd |> List.flatten with
       | [{ Tags_file.tagname = "my-module"; kind = E.Module; _}] -> ()
       | _ -> assert_failure "it should extract module names from comments"
       );

       let tmpfile = Common.new_temp_file "unit" "tags" in
       Tags_file.generate_vi_tags_file tmpfile tags;
       let content = Common.read_file tmpfile in
       if content =~ "my-module\t.*\t/\\(.*\\)/;.*"
       then
         let str = Common.matched1 content in
         assert_equal
           ~msg:"it should correctly escape slash in vi tags file"
           "\\/**"
           str
       else assert_failure "it should generate the write vi tags file"
     ));

 "AST js building regression files" >:: (fun () ->
      let dir = Filename.concat Config_pfff.path "/tests/js/parsing" in
      let files = 
        Common2.glob (spf "%s/*.js" dir) @
(*
        Common2.glob (spf "%s/jsx/*.js" dir) @
        Common2.glob (spf "%s/typescript/*.js" dir) @
*)
        []
      in
      files |> List.iter (fun file ->
        try
          let cst = Parse_js.parse_program file in
          Common.save_excursion Ast_js_build.transpile_xml false (fun () ->
          Common.save_excursion Ast_js_build.transpile_pattern false (fun () ->
            Ast_js_build.program cst  |> ignore
          ))
        with 
        | Parse_info.Parsing_error _
        | Parse_info.Lexical_error (_, _)
        | Ast_js_build.TodoConstruct (_, _)
        | Ast_js_build.UnhandledConstruct (_, _) 
        | Common.Impossible
              ->
          assert_failure (spf "it should correctly parse %s" file)
      )
    );
]

