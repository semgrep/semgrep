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
module J = JSON
module FT = File_type
module Resp = Semgrep_core_response_t

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let dump_and_print_errors dumper (res : 'a Tree_sitter_run.Parsing_result.t) =
  (match res.program with
  | Some cst -> dumper cst
  | None -> failwith "unknown error from tree-sitter parser");
  res.errors
  |> List.iter (fun err ->
         pr2 (Tree_sitter_run.Tree_sitter_error.to_string ~color:true err))

let fail_on_error (parsing_res : 'a Tree_sitter_run.Parsing_result.t) =
  match (parsing_res.program, parsing_res.errors) with
  | Some cst, [] -> cst
  | Some cst, xs when List.length xs <= 2 -> cst
  | _, err :: _ -> raise (Tree_sitter_run.Tree_sitter_error.Error err)
  | None, [] -> failwith "unknown error from tree-sitter parser"

(*****************************************************************************)
(* Pfff only *)
(*****************************************************************************)

let dump_pfff_ast lang file =
  let ast =
    Common.save_excursion Flag_semgrep.pfff_only true (fun () ->
        let res = Parse_target.just_parse_with_lang lang file in
        res.ast)
  in
  let v = Meta_AST.vof_any (G.Pr ast) in
  let s = OCaml.string_of_v v in
  pr2 s

(*****************************************************************************)
(* Tree-sitter only *)
(*****************************************************************************)

(*
   Inferring the file type from the name doesn't work e.g. '.h' could
   be C or C++, '.py' could be Python 2 or Python 3.
*)
let dump_tree_sitter_cst lang file =
  match lang with
  | Lang.R ->
      Tree_sitter_r.Parse.file file
      |> dump_and_print_errors Tree_sitter_r.CST.dump_tree
  | Lang.Ruby ->
      Tree_sitter_ruby.Parse.file file
      |> dump_and_print_errors Tree_sitter_ruby.CST.dump_tree
  | Lang.Java ->
      Tree_sitter_java.Parse.file file
      |> dump_and_print_errors Tree_sitter_java.CST.dump_tree
  | Lang.Go ->
      Tree_sitter_go.Parse.file file
      |> dump_and_print_errors Tree_sitter_go.CST.dump_tree
  | Lang.Csharp ->
      Tree_sitter_c_sharp.Parse.file file
      |> dump_and_print_errors Tree_sitter_c_sharp.CST.dump_tree
  | Lang.Kotlin ->
      Tree_sitter_kotlin.Parse.file file
      |> dump_and_print_errors Tree_sitter_kotlin.CST.dump_tree
  | Lang.Javascript ->
      (* JavaScript/JSX is a strict subset of TSX *)
      Tree_sitter_tsx.Parse.file file
      |> dump_and_print_errors Tree_sitter_tsx.CST.dump_tree
  | Lang.Typescript ->
      Tree_sitter_typescript.Parse.file file
      |> dump_and_print_errors Tree_sitter_typescript.CST.dump_tree
  | Lang.Lua ->
      Tree_sitter_lua.Parse.file file
      |> dump_and_print_errors Tree_sitter_lua.CST.dump_tree
  | Lang.Rust ->
      Tree_sitter_rust.Parse.file file
      |> dump_and_print_errors Tree_sitter_rust.CST.dump_tree
  | Lang.OCaml ->
      Tree_sitter_ocaml.Parse.file file
      |> dump_and_print_errors Tree_sitter_ocaml.CST.dump_tree
  | Lang.C ->
      Tree_sitter_c.Parse.file file
      |> dump_and_print_errors Tree_sitter_c.CST.dump_tree
  | Lang.Cplusplus ->
      Tree_sitter_cpp.Parse.file file
      |> dump_and_print_errors Tree_sitter_cpp.CST.dump_tree
  | Lang.HTML ->
      Tree_sitter_html.Parse.file file
      |> dump_and_print_errors Tree_sitter_html.CST.dump_tree
  | Lang.Vue ->
      Tree_sitter_vue.Parse.file file
      |> dump_and_print_errors Tree_sitter_vue.CST.dump_tree
  | Lang.PHP ->
      Tree_sitter_php.Parse.file file
      |> dump_and_print_errors Tree_sitter_php.CST.dump_tree
  | Lang.HCL ->
      Tree_sitter_hcl.Parse.file file
      |> dump_and_print_errors Tree_sitter_hcl.CST.dump_tree
  | _ -> failwith "lang not supported by ocaml-tree-sitter"

let test_parse_tree_sitter lang root_paths =
  let paths = Common.map Common.fullpath root_paths in
  let targets = Common.map (fun path -> (path, Find_target.Filterable)) paths in
  let paths, _skipped_paths = Find_target.files_of_dirs_or_files lang targets in
  let stat_list = ref [] in
  paths
  |> Console.progress (fun k ->
         List.iter (fun file ->
             k ();
             logger#info "processing %s" file;
             let stat =
               try
                 (match lang with
                 (* less: factorize with dump_tree_sitter_cst_lang *)
                 | Lang.Ruby ->
                     Tree_sitter_ruby.Parse.file file |> fail_on_error |> ignore
                 | Lang.Java ->
                     Tree_sitter_java.Parse.file file |> fail_on_error |> ignore
                 | Lang.Go ->
                     Tree_sitter_go.Parse.file file |> fail_on_error |> ignore
                 | Lang.Csharp ->
                     Tree_sitter_c_sharp.Parse.file file
                     |> fail_on_error |> ignore
                 | Lang.Kotlin ->
                     Tree_sitter_kotlin.Parse.file file
                     |> fail_on_error |> ignore
                 | Lang.Javascript ->
                     Tree_sitter_tsx.Parse.file file |> fail_on_error |> ignore
                 | Lang.Typescript ->
                     Tree_sitter_typescript.Parse.file file
                     |> fail_on_error |> ignore
                 | Lang.Rust ->
                     Tree_sitter_rust.Parse.file file |> fail_on_error |> ignore
                 | Lang.OCaml ->
                     Tree_sitter_ocaml.Parse.file file
                     |> fail_on_error |> ignore
                 | Lang.C ->
                     Tree_sitter_c.Parse.file file |> fail_on_error |> ignore
                 | Lang.Cplusplus ->
                     Tree_sitter_cpp.Parse.file file |> fail_on_error |> ignore
                 | Lang.HTML ->
                     Tree_sitter_html.Parse.file file |> fail_on_error |> ignore
                 | Lang.Vue ->
                     Tree_sitter_vue.Parse.file file |> fail_on_error |> ignore
                 | Lang.PHP ->
                     Tree_sitter_php.Parse.file file |> fail_on_error |> ignore
                 | Lang.HCL ->
                     Tree_sitter_hcl.Parse.file file |> fail_on_error |> ignore
                 | _ ->
                     failwith
                       (spf "lang %s not supported with tree-sitter"
                          (Lang.string_of_lang lang)));
                 PI.correct_stat file
               with exn ->
                 pr2 (spf "%s: exn = %s" file (Common.exn_to_s exn));
                 PI.bad_stat file
             in
             Common.push stat stat_list));
  Parse_info.print_parsing_stat_list !stat_list;
  ()

(*****************************************************************************)
(* Pfff and tree-sitter parsing *)
(*****************************************************************************)

(*
   Expand the list of files or directories into a list of files in the
   specified language, and return a record for each file.

   This is meant to run the same parsers as semgrep-core does for normal
   semgrep scans.
*)
let parsing_common ?(verbose = true) lang files_or_dirs =
  let timeout_seconds = 10.0 in
  (* Without the use of Memory_limit below, we were getting some
   * 'Fatal error: out of memory' errors in the parsing stat CI job,
   * because of some ulimit -v 4000000 in the run-lang script,
   * which then was aborting the whole job.
   *
   * Note that the mem_limit_mb value below must be less than the
   * ulimit value in run-lang.
   *
   * In fact, it is not clear how much less it must be. For example with:
   * $ ulimit -v 1000000
   * $ semgrep-core -lang c -parsing_stats -json lang/c/tmp/php-php-src/ext/fileinfo/
   * I still get a core dump with a "Fatal error: out of memory",
   * even if I set mem_limit_mb below to 500.
   * Seems like the Gc alarm does not trigger, maybe because there is no
   * major GC cycle triggered before reaching the 1 GB ulimit.
   *
   * So the current workaround is to still use mem_limit_mb here, but
   * remove the 'ulimit -v' in the parsing-stat/run-lang script.
   * In fact, even with that we still get some "out of memory" crash, so
   * I also had to filter big files.
   *
   * TODO: The weird thing is that even with the 1Gb limit below, I often
   * see with 'top' semgrep-core -parsing_stat using more than 2GB.
   * Weird. Maybe the allocation happens in tree-sitter which are out of
   * control of the OCaml GC.
   *)
  let mem_limit_mb = 1000 in
  (* This may help a little getting more major cycle triggered.
   * The default is 80, and Main.set_gc set it at 300. The lower it is
   * and the more major cycles there will be.
   *)
  Gc.set { (Gc.get ()) with Gc.space_overhead = 30 };

  logger#info "running with a timeout of %f.1s" timeout_seconds;
  logger#info "running with a memory limit of %d MiB" mem_limit_mb;

  let paths =
    (* = absolute paths *)
    Common.map Common.fullpath files_or_dirs
  in
  let paths = Common.map (fun path -> (path, Find_target.Filterable)) paths in
  let paths, skipped = Find_target.files_of_dirs_or_files lang paths in
  let stats =
    paths
    |> List.rev_map (fun file ->
           pr2
             (spf "%05.1fs: [%s] processing %s" (Sys.time ())
                (Lang.to_lowercase_alnum lang)
                file);
           let stat =
             try
               match
                 Memory_limit.run_with_memory_limit ~mem_limit_mb (fun () ->
                     Common.set_timeout ~verbose:false
                       ~name:"Test_parsing.parsing_common" timeout_seconds
                       (fun () ->
                         Parse_target
                         .parse_and_resolve_name_use_pfff_or_treesitter lang
                           file))
               with
               | Some res -> res.Parse_target.stat
               | None -> { (PI.bad_stat file) with have_timeout = true }
             with
             | Timeout _ -> assert false
             | exn ->
                 if verbose then
                   pr2 (spf "%s: exn = %s" file (Common.exn_to_s exn));
                 (* bugfix: bad_stat() could actually triggering some
                    Sys_error "Out of memory" when implemented naively,
                    and this exn in the exn handler was stopping the whole job.
                 *)
                 PI.bad_stat file
           in
           stat)
  in
  (stats, skipped)

(*
   Parse files from multiple root folders, each root being considered a
   separate project. Keeping projects separate allows us to spot
   projects with unusual results.

   Timeouts are ignored. Why?
   martin: "changing the timeout or the hardware would change the stats.
   They're meant to reflect syntax support for a language, not how slow
   or fast we are at parsing.

   I think it could be useful to have separate stats for timeouts. It could
   be the percentage of lines of code for which we didn't time out. For the
   reasons stated above, this may vary quite a lot from project to project,
   depending on whether they check in weird files (generated, minified) and
   whether we filter them out.

   The specific situation that prompted me to exclude timeouts was the
   low (~80%) parsing rate for javascript. This was only due to minified
   files. In general it could be due to other problems, and yes, it would
   be nice to find out about timeouts. I think the timeout threshold should
   in seconds/MB or equivalent units, not seconds per file."
*)
let parse_project ~verbose lang name files_or_dirs =
  let stat_list, _skipped = parsing_common ~verbose lang files_or_dirs in
  let stat_list =
    List.filter (fun stat -> not stat.PI.have_timeout) stat_list
  in
  pr2
    (spf "%05.1fs: [%s] done parsing %s" (Sys.time ())
       (Lang.to_lowercase_alnum lang)
       name);
  (name, stat_list)

(* Json doesn't tolerate NaN values, so we use 1 instead. *)
let replace_nan x = if x <> x then 1. else x

let update_parsing_rate (acc : Parsing_stats_t.project_stats) :
    Parsing_stats_t.project_stats =
  {
    acc with
    parsing_rate =
      1. -. (float acc.error_line_count /. float acc.line_count) |> replace_nan;
  }

(*
   Add things up for json reporting: file stats -> project stat
*)
let aggregate_file_stats (results : (string * PI.parsing_stat list) list) :
    Parsing_stats_t.project_stats list =
  Common.map
    (fun (project_name, file_stats) ->
      let acc =
        {
          Parsing_stats_t.name = project_name;
          parsing_rate = nan;
          line_count = 0;
          error_line_count = 0;
          file_count = 0;
          error_file_count = 0;
        }
      in
      let acc =
        List.fold_left
          (fun (acc : Parsing_stats_t.project_stats) (x : PI.parsing_stat) ->
            let success = x.error_line_count = 0 in
            {
              acc with
              Parsing_stats_t.line_count = acc.line_count + x.total_line_count;
              error_line_count = acc.error_line_count + x.error_line_count;
              file_count = acc.file_count + 1;
              error_file_count =
                (acc.error_file_count + if not success then 1 else 0);
            })
          acc file_stats
      in
      update_parsing_rate acc)
    results

(*
   Add things up for json reporting: project stats -> global stat
*)
let aggregate_project_stats lang
    (project_stats : Parsing_stats_t.project_stats list) : Parsing_stats_t.t =
  let open Parsing_stats_t in
  let acc =
    {
      name = "*";
      parsing_rate = nan;
      line_count = 0;
      error_line_count = 0;
      file_count = 0;
      error_file_count = 0;
    }
  in
  let acc =
    List.fold_left
      (fun acc proj ->
        {
          acc with
          line_count = acc.line_count + proj.line_count;
          error_line_count = acc.error_line_count + proj.error_line_count;
          file_count = acc.file_count + proj.file_count;
          error_file_count = acc.error_file_count + proj.error_file_count;
        })
      acc project_stats
  in
  let global = update_parsing_rate acc in
  { language = Lang.to_lowercase_alnum lang; global; projects = project_stats }

let print_json lang results =
  let project_stats = aggregate_file_stats results in
  let stats = aggregate_project_stats lang project_stats in
  let s = Parsing_stats_j.string_of_t stats in
  print_endline (Yojson.Safe.prettify s)

let parse_projects ~verbose lang project_dirs =
  Common.map
    (fun dir ->
      let name = dir in
      parse_project ~verbose lang name [ dir ])
    project_dirs

let parsing_stats lang json project_dirs =
  let stat_list = parse_projects ~verbose:(not json) lang project_dirs in
  if json then print_json lang stat_list
  else
    let flat_stat = Common.map snd stat_list |> List.flatten in
    Parse_info.print_parsing_stat_list flat_stat

let parsing_regressions lang files_or_dirs =
  let _stat_list = parsing_common lang files_or_dirs in
  raise Todo

let diff_pfff_tree_sitter xs =
  pr2 "NOTE: consider using -full_token_info to get also diff on tokens";
  xs
  |> List.iter (fun file ->
         match Lang.langs_of_filename file with
         | [ _lang ] ->
             let ast1 =
               Common.save_excursion Flag_semgrep.pfff_only true (fun () ->
                   Parse_target.parse_program file)
             in
             let ast2 =
               Common.save_excursion Flag_semgrep.tree_sitter_only true
                 (fun () -> Parse_target.parse_program file)
             in
             let s1 = AST_generic.show_program ast1 in
             let s2 = AST_generic.show_program ast2 in
             Common2.with_tmp_file ~str:s1 ~ext:"x" (fun file1 ->
                 Common2.with_tmp_file ~str:s2 ~ext:"x" (fun file2 ->
                     let xs = Common2.unix_diff file1 file2 in
                     xs |> List.iter pr2))
         | _ -> failwith (spf "can't detect single language for %s" file))

(*****************************************************************************)
(* Rule parsing *)
(*****************************************************************************)

let test_parse_rules roots =
  let targets = Common.map (fun path -> (path, Find_target.Filterable)) roots in
  let targets, _skipped_paths =
    Find_target.files_of_dirs_or_files Lang.Yaml targets
  in
  targets
  |> List.iter (fun file ->
         logger#info "processing %s" file;
         let _r = Parse_rule.parse file in
         ());
  logger#info "done test_parse_rules"
