open Common
open File.Operators
module PS = Parsing_stat
module Flag = Flag_parsing
module Flag_cpp = Flag_parsing_cpp
module FT = File_type

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_cpp file =
  Flag.verbose_lexing := true;
  Flag.verbose_parsing := true;
  let toks = Parse_cpp.tokens (Parsing_helpers.file file) in
  toks |> List.iter (fun x -> pr2_gen x);
  ()

(* used to be in Lib_parsing_cpp.ml *)
let find_source_files_of_dir_or_files xs =
  File.files_of_dirs_or_files_no_vcs_nofilter xs
  |> List.filter (fun filename ->
         match File_type.file_type_of_file filename with
         | FT.PL (FT.C ("l" | "y")) -> false
         | FT.PL (FT.C _ | FT.Cplusplus _) ->
             (* todo: fix syncweb so don't need this! *)
             not (FT.is_syncweb_obj_file filename)
         | _ -> false)
  |> List_.sort

let test_parse_cpp ?lang xs =
  let xs = File.Path.of_strings xs in
  let fullxs, _skipped_paths =
    find_source_files_of_dir_or_files xs
    |> Skip_code.filter_files_if_skip_list ~root:xs
  in
  Parse_cpp.init_defs !Flag_cpp.macros_h;

  let stat_list = ref [] in
  let newscore = Common2.empty_score () in

  fullxs
  |> Console.progress (fun k ->
         List.iter (fun file ->
             k ();
             let stat =
               try
                 Common.save_excursion Flag.error_recovery true (fun () ->
                     Common.save_excursion Flag.exn_when_lexical_error false
                       (fun () ->
                         let res =
                           match lang with
                           | None -> Parse_cpp.parse file
                           | Some lang -> Parse_cpp.parse_with_lang ~lang file
                         in
                         res.Parsing_result.stat))
               with
               | exn ->
                   (* TODO: be more strict, Common.hd_exn "unexpected empty list" failure, Stack overflow *)
                   pr2 (spf "PB on %s, exn = %s" !!file (Common.exn_to_s exn));
                   Parsing_stat.bad_stat !!file
             in
             Stack_.push stat stat_list;

             let s = spf "bad = %d" stat.PS.error_line_count in
             if stat.PS.error_line_count =|= 0 then
               Hashtbl.add newscore !!file Common2.Ok
             else Hashtbl.add newscore !!file (Common2.Pb s)));

  Parsing_stat.print_recurring_problematic_tokens !stat_list;
  Parsing_stat.print_parsing_stat_list !stat_list;
  Parsing_stat.print_regression_information ~ext:"cpp" xs newscore;

  (* TODO: restore layer generation for errors
     (match xs with
     | [ dirname ] when Common2.is_directory dirname ->
         let layer_file = "/tmp/layer_parse_errors_red_green.json" in
         pr2 (spf "generating parse error layer in %s" layer_file);
         let layer =
           Layer_parse_errors.gen_red_green_layer ~root:dirname !stat_list
         in
         Layer_code.save_layer layer layer_file;

         let layer_file = "/tmp/layer_parse_errors_heatmap.json" in
         pr2 (spf "generating parse error layer in %s" layer_file);
         let layer =
           Layer_parse_errors.gen_heatmap_layer ~root:dirname !stat_list
         in
         Layer_code.save_layer layer layer_file
     | _ -> ());
  *)
  ()

let test_dump_cpp file =
  let file = Fpath.v file in
  Parse_cpp.init_defs !Flag_cpp.macros_h;
  let ast = Parse_cpp.parse_program file in
  let s = Ast_cpp.show_program ast in
  pr s

let test_dump_cpp_full file =
  let file = Fpath.v file in
  Parse_cpp.init_defs !Flag_cpp.macros_h;
  let ast = Parse_cpp.parse_program file in
  let toks = Parse_cpp.tokens (Parsing_helpers.file !!file) in
  let s = Ast_cpp.show_program (* TODO ~precision *) ast in
  pr s;
  toks
  |> List.iter (fun tok ->
         match tok with
         | Parser_cpp.TComment _ii ->
             (* old:
                let v = Meta_parse_info.vof_info_adjustable_precision ii in
                let s = OCaml.string_of_v v in
                pr s
             *)
             failwith "TODO: display comments in test_dump_cpp_full"
         | _ -> ());
  ()

let test_dump_cpp_view file =
  Parse_cpp.init_defs !Flag_cpp.macros_h;
  let toks_orig = Parse_cpp.tokens (Parsing_helpers.file file) in
  let toks =
    toks_orig
    |> List_.exclude (fun x ->
           Token_helpers_cpp.is_comment x || Token_helpers_cpp.is_eof x)
  in
  let extended = toks |> List.map Token_views_cpp.mk_token_extended in
  Parsing_hacks_cpp.find_template_inf_sup extended;

  let multi = Token_views_cpp.mk_multi extended in
  Token_views_context.set_context_tag_multi multi;
  let v = Token_views_cpp.vof_multi_grouped_list multi in
  let s = OCaml.string_of_v v in
  pr s

let test_parse_cpp_fuzzy xs =
  let xs = File.Path.of_strings xs in
  let fullxs, _skipped_paths =
    find_source_files_of_dir_or_files xs
    |> Skip_code.filter_files_if_skip_list ~root:xs
  in
  fullxs
  |> Console.progress (fun k ->
         List.iter (fun file ->
             k ();
             Common.save_excursion Flag_parsing_cpp.strict_lexer true (fun () ->
                 try
                   let _fuzzy = Parse_cpp.parse_fuzzy file in
                   ()
                 with
                 | exn ->
                     pr2
                       (spf "PB with: %s, exn = %s" !!file (Common.exn_to_s exn)))))

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () =
  [
    ("-tokens_cpp", "   <file>", Arg_helpers.mk_action_1_arg test_tokens_cpp);
    ( "-parse_cpp",
      "   <file or dir>",
      Arg_helpers.mk_action_n_arg test_parse_cpp );
    ( "-parse_cpp_c",
      "   <file or dir>",
      Arg_helpers.mk_action_n_arg (test_parse_cpp ~lang:Flag_cpp.C) );
    ( "-parse_cpp_cplusplus",
      "   <file or dir>",
      Arg_helpers.mk_action_n_arg (test_parse_cpp ~lang:Flag_cpp.Cplusplus) );
    ("-dump_cpp", "   <file>", Arg_helpers.mk_action_1_arg test_dump_cpp);
    ( "-dump_cpp_full",
      "   <file>",
      Arg_helpers.mk_action_1_arg test_dump_cpp_full );
    ( "-dump_cpp_view",
      "   <file>",
      Arg_helpers.mk_action_1_arg test_dump_cpp_view );
    ( "-parse_cpp_fuzzy",
      "   <files or dirs>",
      Arg_helpers.mk_action_n_arg test_parse_cpp_fuzzy )
    (*
    ("-dump_cpp_fuzzy", "   <file>", Arg_helpers.mk_action_1_arg test_dump_cpp_fuzzy);
 *);
  ]
