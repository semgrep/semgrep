(*s: pfff/lang_GENERIC/parsing/Test_parsing_generic.ml *)
open Common

(*s: constant [[Test_parsing_generic.lang]] *)
let lang = ref ""
(*e: constant [[Test_parsing_generic.lang]] *)

(*s: function [[Test_parsing_generic.test_parse_generic]] *)
let test_parse_generic xs =
  let xs = List.map Common.fullpath xs in
  let files = Common.files_of_dir_or_files_no_vcs_nofilter xs in
  files |> List.iter (fun file ->
    match Lang.langs_of_filename file with
    | [] -> pr2 (spf "skipping %s" file)
    | _x::_xs ->
        Error_code.try_with_print_exn_and_reraise file (fun () ->
          let _ast = Parse_generic.parse_program file in
          ()
        )
  )
(*e: function [[Test_parsing_generic.test_parse_generic]] *)

let test_show_generic file =
  let ast = Parse_generic.parse_program file in
  let s = AST_generic.show_program ast in
  pr2 s

(*s: function [[Test_parsing_generic.test_dump_generic]] *)
let test_dump_generic file =
  let x = Parse_generic.parse_program file in

  let v = Meta_AST.vof_any (AST_generic.Pr x) in
  let s = OCaml.string_of_v v in
  pr2 s
(*e: function [[Test_parsing_generic.test_dump_generic]] *)

(*s: function [[Test_parsing_generic.test_dump_pattern_generic]] *)
let test_dump_pattern_generic file =
  match Lang.lang_of_string_opt !lang with
  | _ when !lang = "" -> failwith "use -lang"
  | Some lang ->
      let s = Common.read_file file in
      Error_code.try_with_print_exn_and_reraise file (fun () ->
        let any = Parse_generic.parse_pattern lang s in
        let s = AST_generic.show_any any in
        pr2 s
      )
  | None -> failwith (spf "unsupported language: %s" !lang)
(*e: function [[Test_parsing_generic.test_dump_pattern_generic]] *)


(*s: function [[Test_parsing_generic.actions]] *)
let actions () = [
  "-parse_generic", " <dirs_or_files>",
  Common.mk_action_n_arg test_parse_generic;
  (*s: [[Test_parsing_generic.actions]] other cases *)
  "-show_ast", " <file>",
  Common.mk_action_1_arg test_show_generic;
  (* now also in sgrep *)
  "-dump_ast", " <file>",
  Common.mk_action_1_arg test_dump_generic;
  (*x: [[Test_parsing_generic.actions]] other cases *)
  "-dump_generic", " <file>",
  Common.mk_action_1_arg test_dump_generic;
  (*x: [[Test_parsing_generic.actions]] other cases *)
  "-dump_pattern", " <file>",
  Common.mk_action_1_arg test_dump_pattern_generic;
  (*e: [[Test_parsing_generic.actions]] other cases *)
]
(*e: function [[Test_parsing_generic.actions]] *)
(*e: pfff/lang_GENERIC/parsing/Test_parsing_generic.ml *)
