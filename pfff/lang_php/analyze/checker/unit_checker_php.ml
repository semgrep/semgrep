open Common
open OUnit

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let unittest =
 "checkers_php" >::: [
  "basic checkers" >:: (fun () ->
  let p path = 
     Filename.concat Config_pfff.path 
            (Filename.concat "tests/php/scheck" path)
  in

  let test_files = [
    p "builtins.php";
    p "common.php";

    p "includes.php";

    p "variables.php";
    p "variables_fp.php";
    p "arrays.php";
    p "foreach.php";
    p "edit_distance.php";

    p "functions.php";
    p "static_methods.php";
    p "methods.php";

    p "classes.php";
    p "traits.php";

(*
    p "namespaces.php";
    p "namespaces_uses.php";
*)

    p "cfg.php";
    p "references.php";
    p "xhp.php";
    p "typing.php";

    p "dynamic_bailout.php";

    p "format_string.php";
    p "ternary_if.php";
    p "misc.php";

    p "lint.php";
    p "micro_clones.php";
  ] 
  in

  (* old: Lib_parsing_php.find_php_files_of_dir_or_files [p "/data/php_stdlib"]
   * we need a tests/php/scheck/php_stdlib.php because data/php_stdlib
   * is now mostly auto generated from idl files, and data/php_stdlib
   * contains only pfff internal builtins.
   * data/php_stdlib is not anymore in the pfff repo.
   *)
  let builtin_files = [
    p "php_stdlib.php";
  ]

  in
  let files = builtin_files @ test_files in

  let expected_error_lines = 
     Error_code.expected_error_lines_of_files test_files in

  Error_php._errors := [];
  let verbose = false in

  (* old:
   *  let db = Database_php_build.db_of_files_or_dirs files in
   *  let find_entity = Some (Database_php_build.build_entity_finder db) in
   *)
  let (cg, _stat) = Graph_code_php.build ~verbose (p "") files in
  let find_entity = 
    Some (Entity_php.entity_finder_of_graph_code ~check_dupes:true
             cg (p "")) in
  
  let env = Env_php.mk_env ~php_root:"/" in

  (* run the bugs finders *)
  test_files |> List.iter (fun file ->
    Check_all_php.check_file ~verbose ~find_entity env file;
    Check_classes_php.check_required_field cg file
  );
  if verbose then begin
    !Error_php._errors |> List.iter (fun e -> pr (Error_php.string_of_error e))
  end;
  let actual_errors = !Error_php._errors in
  let actual_error_lines = 
    actual_errors |> List.map (fun err ->
      let info = err.Error_php.loc in
      Parse_info.file_of_info info, Parse_info.line_of_info info
      )
  in
  
  (* diff report
   * TODO: can not factorize yet with Error_code.compare_actual_to_expected
   * because the error type is different (Error_code.error != Error_php.error)
   *)
  let (_common, only_in_expected, only_in_actual) = 
    Common2.diff_set_eff expected_error_lines actual_error_lines in

  only_in_expected |> List.iter (fun (src, l) ->
    pr2 (spf "this one error is missing: %s:%d" src l);
  );
  only_in_actual |> List.iter (fun (src, l) ->
    pr2 (spf "this one error was not expected: %s:%d (%s)" src l
           (!Error_php._errors |> List.find (fun err ->
             let info = err.Error_php.loc in
             src =$= Parse_info.file_of_info info &&
             l   =|= Parse_info.line_of_info info
            ) |> Error_php.string_of_error));
  );
  assert_bool
    ~msg:(spf "it should find all reported errors and no more (%d errors)"
             (List.length (only_in_actual @ only_in_expected)))
    (null only_in_expected && null only_in_actual);
  )
  ]
