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

let logger = Logging.get_logger [__MODULE__]

let dump_and_print_errors dumper (res : 'a Tree_sitter_run.Parsing_result.t) =
  (match res.program with
   | Some cst -> dumper cst
   | None -> failwith "unknown error from tree-sitter parser"
  );
  res.errors |> List.iter (fun err ->
    pr2 (Tree_sitter_run.Tree_sitter_error.to_string ~color:true err)
  )

(* less: could infer lang from filename *)
let dump_tree_sitter_cst_lang lang file =
  match lang with
  | Lang.Ruby ->
      Tree_sitter_ruby.Parse.file file
      |> dump_and_print_errors Tree_sitter_ruby.CST.dump_tree
  | Lang.Java ->
      Tree_sitter_java.Parse.file file
      |> dump_and_print_errors Tree_sitter_java.CST.dump_tree
  | Lang.Go   ->
      Tree_sitter_go.Parse.file file
      |> dump_and_print_errors Tree_sitter_go.CST.dump_tree
  | Lang.Csharp ->
      Tree_sitter_csharp.Parse.file file
      |> dump_and_print_errors Tree_sitter_csharp.CST.dump_tree
  | Lang.Kotlin ->
      Tree_sitter_kotlin.Parse.file file
      |> dump_and_print_errors Tree_sitter_kotlin.CST.dump_tree
  | Lang.Javascript ->
      Tree_sitter_javascript.Parse.file file
      |> dump_and_print_errors Tree_sitter_javascript.CST.dump_tree
  | Lang.Typescript ->
      Tree_sitter_typescript.Parse.file file
      |> dump_and_print_errors Tree_sitter_typescript.CST.dump_tree

  | Lang.C ->
      Tree_sitter_c.Parse.file file
      |> dump_and_print_errors Tree_sitter_c.CST.dump_tree
  | Lang.Cplusplus ->
      Tree_sitter_cpp.Parse.file file
      |> dump_and_print_errors Tree_sitter_cpp.CST.dump_tree

  | _ -> failwith "lang not supported by ocaml-tree-sitter"

let dump_tree_sitter_cst file =
  match Lang.langs_of_filename file with
  | [l] -> dump_tree_sitter_cst_lang l file
  | [] -> failwith (spf "no language detected for %s" file)
  | _::_::_ -> failwith (spf "too many languages detected for %s" file)

let dump_ast_pfff file =
  match Lang.langs_of_filename file with
  | [lang] ->
      let (ast, _stat) = Parse_generic.parse_with_lang lang file in
      let v = Meta_AST.vof_any (G.Pr ast) in
      let s = OCaml.string_of_v v in
      pr2 s
  | [] -> failwith (spf "no language detected for %s" file)
  | _::_::_ -> failwith (spf "too many languages detected for %s" file)

(* mostly a copy paste of Test_parsing_ruby.test_parse in pfff but using
 * the tree-sitter Ruby parser instead.
*)
let test_parse_lang lang get_final_files xs =
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
    logger#info "processing %s" file;
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

let fail_on_error (parsing_res : 'a Tree_sitter_run.Parsing_result.t) =
  match parsing_res.program, parsing_res.errors with
  | Some cst, [] -> cst
  | Some cst, xs when List.length xs <= 2 -> cst
  | _, err :: _ -> raise (Tree_sitter_run.Tree_sitter_error.Error err)
  | None, [] -> failwith "unknown error from tree-sitter parser"

let test_parse_tree_sitter lang xs =
  let lang =
    match Lang.lang_of_string_opt lang with
    | Some l -> l
    | None -> failwith "no language or unsupported language; use correct -lang"
  in
  let xs = List.map Common.fullpath xs in
  let fullxs = Lang.files_of_dirs_or_files lang xs
               |> Skip_code.filter_files_if_skip_list ~root:xs
  in
  let stat_list = ref [] in
  fullxs |> Console.progress (fun k -> List.iter (fun file ->
    k();
    logger#info "processing %s" file;
    let stat =
      (try
         (match lang with
          (* less: factorize with dump_tree_sitter_cst_lang *)
          | Lang.Ruby ->
              Tree_sitter_ruby.Parse.file file
              |> fail_on_error |> ignore
          | Lang.Java ->
              Tree_sitter_java.Parse.file file
              |> fail_on_error |> ignore
          | Lang.Go   ->
              Tree_sitter_go.Parse.file file
              |> fail_on_error |> ignore
          | Lang.Csharp ->
              Tree_sitter_csharp.Parse.file file
              |> fail_on_error |> ignore
          | Lang.Kotlin ->
              Tree_sitter_kotlin.Parse.file file
              |> fail_on_error |> ignore
          | Lang.Javascript ->
              Tree_sitter_javascript.Parse.file file
              |> fail_on_error |> ignore
          | Lang.Typescript ->
              Tree_sitter_typescript.Parse.file file
              |> fail_on_error |> ignore
          | Lang.C ->
              Tree_sitter_c.Parse.file file
              |> fail_on_error |> ignore
          | Lang.Cplusplus ->
              Tree_sitter_cpp.Parse.file file
              |> fail_on_error |> ignore

          | _ -> failwith (spf "lang %s not supported with tree-sitter"
                             (Lang.string_of_lang lang))
         );
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
        let (ast1, _stat1) = Parse_generic.parse_with_lang lang file in
        let ast2 =
          Common.save_excursion Flag_semgrep.tree_sitter_only true (fun () ->
            let {Parse_code. ast; errors; _} =
              Parse_code.just_parse_with_lang lang file in
            if errors <> [] then failwith (spf "problem parsing %s" file);
            ast
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
