(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let tests =
  Testutil.pack_tests "analyze_java"
    [ (* now done in semgrep/Test.ml
          "regression files", (fun () ->
            let dir = Config_pfff.tests_path "java/parsing" in
            let files = Common2.glob (spf "%s/*.java" dir) in
            files |> List.iter (fun file ->
              try
                let ast = Parse_java.parse_program file in
                let _gen = Java_to_generic.program ast in
                ()
              with _exn ->
                assert_failure (spf "it should generate a generic AST %s" file)
            )
          );
      *)

      (*
 "tags_java" [

   "basic tags", (fun () ->
     let file_content = "
       package foo;
       class Bar { }
     " in
     let tmpfile = Parse_java.tmp_file_from_string file_content in
     let tags = Tags_java.defs_of_dir_or_file ~verbose:false tmpfile [] in
        (match tags with
        | [file, tags_in_file] ->
            assert_equal tmpfile file;
            let xs = tags_in_file +> List.map (fun x ->
              x.Tags_file.tagname, x.Tags_file.kind
            ) in
            assert_equal ~msg:"it should contain the right 6 entries" [
              "foo.Bar", E.Class E.RegularClass;
            ]
            xs
        | _ ->
            assert_failure "The tags should contain only one entry for one file"
        )
   );
 ]
*) ]
