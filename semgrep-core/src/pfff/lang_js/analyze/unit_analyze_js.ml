(* module E = Entity_code *)


(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)
let tests =
  Testutil.pack_tests "analyze_js" [

    "commonjs annotations", (fun () ->
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
        let res = Parse_js.parse tmpfile in
        let annots =
          Annotation_js.annotations_of_program_with_comments res
          |> List.map fst
        in
        Alcotest.(check bool)
          "it should extract @providesModule annotations"
          true
          ([Annotation_js.ProvidesModule "my-module";
            Annotation_js.ProvidesLegacy "other-module";
           ] =
           annots)
      )
    );

    (* TODO need Class_pre_es6
       "commonjs tags support", (fun () ->
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
    *)
  ]
