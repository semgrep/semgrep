(* Copyright (C) 2008 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *
 *)
open Common
open File.Operators
module PS = Parsing_stat
module Flag = Flag_parsing
module Ast = Ast_java
module FT = File_type

let find_source_files_of_dir_or_files xs =
  File.files_of_dirs_or_files_no_vcs_nofilter xs
  |> List.filter (fun filename ->
         match File_type.file_type_of_file filename with
         | FT.PL FT.Java -> true
         | _ -> false)
  |> List_.sort

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_parse xs =
  let xs = xs |> File.Path.of_strings |> List.map File.fullpath in

  let fullxs, _skipped_paths =
    find_source_files_of_dir_or_files xs
    |> Skip_code.filter_files_if_skip_list ~root:xs
  in

  let stat_list = ref [] in
  let newscore = Common2.empty_score () in
  let ext = "java" in

  fullxs
  |> Console.progress (fun k ->
         List.iter (fun file ->
             k ();
             let { Parsing_result.stat; _ } =
               try
                 Common.save_excursion Flag.error_recovery true (fun () ->
                     Common.save_excursion Flag.exn_when_lexical_error false
                       (fun () -> Parse_java.parse !!file))
               with
               | exn ->
                   let e = Exception.catch exn in
                   pr2
                     (spf "PB with %s (exn = %s)" !!file (Common.exn_to_s exn));
                   Exception.reraise e
             in
             Stack_.push stat stat_list;
             let s = spf "bad = %d" stat.PS.error_line_count in
             if stat.PS.error_line_count =|= 0 then
               Hashtbl.add newscore !!file Common2.Ok
             else Hashtbl.add newscore !!file (Common2.Pb s)));
  flush stdout;
  flush stderr;

  Parsing_stat.print_parsing_stat_list !stat_list;
  Parsing_stat.print_regression_information ~ext xs newscore;
  ()

let test_lexer file =
  Common.with_open_infile file (fun chan ->
      let lexbuf = Lexing.from_channel chan in
      while true do
        let result = Lexer_java.token lexbuf in
        pr2_gen result;
        if Token_helpers_java.is_eof result then exit 0
      done)

let test_dump file =
  let s =
    if !Flag_parsing.sgrep_mode then
      let ast = Parse_java.any_of_string (Common.read_file file) in
      Ast_java.show_any ast
    else
      let ast = Parse_java.parse_program file in
      Ast_java.show_program ast
  in
  pr s

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () =
  [
    ("-tokens_java", "   <file>", Arg_helpers.mk_action_1_arg test_lexer);
    ("-parse_java", "   <file or dir>", Arg_helpers.mk_action_n_arg test_parse);
    ("-dump_java", "   <file>", Arg_helpers.mk_action_1_arg test_dump);
  ]
