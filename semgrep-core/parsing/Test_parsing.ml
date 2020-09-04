(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common

module PI = Parse_info
module G = AST_generic

(* less: could infer lang from filename *)
let dump_tree_sitter_cst_lang lang file =
   match lang with
   | Lang.Ruby ->
      Tree_sitter_ruby.Parse.file file
      |> Tree_sitter_ruby.CST.dump_tree
   | Lang.Java ->
      Tree_sitter_java.Parse.file file
      |> Tree_sitter_java.CST.dump_tree
   | Lang.Go   ->
      Tree_sitter_go.Parse.file file
      |> Tree_sitter_go.CST.dump_tree
   | Lang.Csharp ->
      Tree_sitter_csharp.Parse.file file
      |> Tree_sitter_csharp.CST.dump_tree
   | Lang.Javascript ->
      Tree_sitter_javascript.Parse.file file
      |> Tree_sitter_javascript.CST.dump_tree

   | _ -> failwith "lang not supported by ocaml-tree-sitter"

let dump_tree_sitter_cst file =
  match Lang.langs_of_filename file with
  | [l] -> dump_tree_sitter_cst_lang l file
  | [] -> failwith (spf "no language detected for %s" file)
  | _::_::_ -> failwith (spf "too many languages detected for %s" file)

let dump_ast_pfff file =
  match Lang.langs_of_filename file with
  | [lang] ->
      let x = Parse_generic.parse_with_lang lang file in
      let v = Meta_AST.vof_any (G.Pr x) in
      let s = OCaml.string_of_v v in
      pr2 s
  | [] -> failwith (spf "no language detected for %s" file)
  | _::_::_ -> failwith (spf "too many languages detected for %s" file)

(* mostly a copy paste of Test_parsing_ruby.test_parse in pfff but using
 * the tree-sitter Ruby parser instead.
 *)
let test_parse_lang verbose lang get_final_files xs =
  let xs = List.map Common.fullpath xs in
  let fullxs = get_final_files xs
      |> Skip_code.filter_files_if_skip_list ~root:xs
    in
  let lang =
    match Lang.lang_of_string_opt lang with
    | Some l -> l
    | None -> failwith "no language specified; use -lang"
  in

  let stat_list = ref [] in
  fullxs |> Console.progress (fun k -> List.iter (fun file ->
    k();
    if verbose then pr2 (spf "processing %s" file);
    let stat =
    (try
       if true
       then begin
          (* use tree-sitter parser and converters *)
          Parse_code.parse_and_resolve_name_use_pfff_or_treesitter lang file
          |> ignore
       end else begin
          (* just the tree-sitter CST parsing  *)
          (* Execute in its own process, so GC bugs will not pop-out here.
           * Slower, but safer for now, otherwise get segfaults probably
           * because of bugs in tree-sitter OCaml bindings.
           *)
           Parallel.backtrace_when_exn := true;
           Parallel.invoke
             (fun file -> dump_tree_sitter_cst_lang lang file)
             file ()
        end;
       PI.correct_stat file
    with exn ->
        pr2 (spf "%s: exn = %s" file (Common.exn_to_s exn));
        PI.bad_stat file
    )
    in
    Common.push stat stat_list;
  ));
  flush stdout; flush stderr;

  Parse_info.print_parsing_stat_list !stat_list;
  ()

let diff_pfff_tree_sitter xs =
  pr2 "NOTE: consider using -full_token_info to get also diff on tokens";
  xs |> List.iter (fun file ->
  match Lang.langs_of_filename file with
  | [lang] ->
    let ast1 = Parse_generic.parse_with_lang lang file in
    let ast2 =
        Common.save_excursion Flag_semgrep.tree_sitter_only true (fun () ->
            Parse_code.just_parse_with_lang lang file
        ) in
    let s1 = AST_generic.show_program ast1 in
    let s2 = AST_generic.show_program ast2 in
    Common2.with_tmp_file ~str:s1 ~ext:"x" (fun file1 ->
    Common2.with_tmp_file ~str:s2 ~ext:"x" (fun file2 ->
      let xs = Common2.unix_diff file1 file2 in
      xs |> List.iter pr2
    ))

  | _ -> failwith (spf "can't detect single language for %s" file)
  )
