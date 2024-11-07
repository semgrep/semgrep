(* Yoann Padioleau
 *
 * Copyright (C) 2020-2022 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common
open Fpath_.Operators
module PS = Parsing_stat
module G = AST_generic
module J = JSON
module FT = File_type
module Resp = Semgrep_output_v1_t

let tags = Logs_.create_tags [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * TODO: remove all those ~verbose parameter; just use Logs
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

module SMap = Map.Make (String)
module IMap = Map.Make (Int)

(* Global store, because I don't want to pass it through a bunch of func calls.
 *)
let store = ref []

(* This function just processes the exception backtrace to find the first instance where a function
   that is not called "todo" was called.
   It then records the line number and name of the function, adding it to a global store.
*)
let process_exn () =
  let rec process (lines : string list) =
    match lines with
    | [] -> "failure"
    | line :: rest -> (
        (* Sue me. I don't want to write a real lexer and parser. *)
        let tokens = String_.split ~sep:" " line in
        match tokens with
        | "Called" :: "from" :: funcname :: _in :: _file :: _filename :: "line"
          :: linenum :: _ ->
            if String.ends_with ~suffix:"todo" funcname then process rest
            else funcname ^ ":" ^ linenum
        | _ -> process rest)
  in
  let res =
    Printexc.get_backtrace () |> String_.split ~sep:(Str.quote "\n") |> process
  in
  store := res :: !store;
  ()

let print_exn file e =
  let trace = Printexc.get_backtrace () in
  process_exn ();
  UCommon.pr2 (spf "%s: exn = %s\n%s" file (Common.exn_to_s e) trace)

(* This function collects all the function name and line number pairs, and then
   sorts them in descending order of frequency.
   I'm using maps to do this so that I can achieve O(log n) insertion per increment.
*)
let report_counts () =
  let counts =
    List.fold_left
      (fun map x ->
        SMap.update x
          (function
            | None -> Some 1
            | Some x -> Some (x + 1))
          map)
      SMap.empty !store
    |> SMap.to_seq
    |> Seq.map (fun (x, y) -> (y, x))
    |> IMap.of_seq |> IMap.to_rev_seq |> List.of_seq
  in
  (* Report all the statistics. *)
  UCommon.pr2 "\nTODO statistics:";
  List.fold_left
    (fun acc (count, filename) -> acc ^ Common.spf "\n%s -> %d" filename count)
    "" counts
  |> UCommon.pr2

let dump_and_print_errors dump_cst dump_extras
    (res : ('cst, 'extras) Tree_sitter_run.Parsing_result.t) =
  (match res.program with
  | Some cst ->
      dump_cst cst;
      dump_extras res.extras
  | None -> failwith "unknown error from tree-sitter parser");
  res.errors
  |> List.iter (fun err ->
         UCommon.pr2
           (Tree_sitter_run.Tree_sitter_error.to_string ~style:Auto err))

let fail_on_error (parsing_res : ('a, 'extras) Tree_sitter_run.Parsing_result.t)
    =
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
  UCommon.pr2 s

(*****************************************************************************)
(* Tree-sitter only *)
(*****************************************************************************)

(*
   Inferring the file type from the name doesn't work e.g. '.h' could
   be C or C++, '.py' could be Python 2 or Python 3.
*)
let dump_tree_sitter_cst (lang : Lang.t) (file : Fpath.t) : unit =
  match lang with
  | Lang.Clojure ->
      Tree_sitter_clojure.Parse.file !!file
      |> dump_and_print_errors Tree_sitter_clojure.Boilerplate.dump_tree
           Tree_sitter_clojure.Boilerplate.dump_extras
  | Lang.R ->
      Tree_sitter_r.Parse.file !!file
      |> dump_and_print_errors Tree_sitter_r.Boilerplate.dump_tree
           Tree_sitter_r.Boilerplate.dump_extras
  | Lang.Ruby ->
      Tree_sitter_ruby.Parse.file !!file
      |> dump_and_print_errors Tree_sitter_ruby.Boilerplate.dump_tree
           Tree_sitter_ruby.Boilerplate.dump_extras
  | Lang.Java ->
      Tree_sitter_java.Parse.file !!file
      |> dump_and_print_errors Tree_sitter_java.Boilerplate.dump_tree
           Tree_sitter_java.Boilerplate.dump_extras
  | Lang.Go ->
      Tree_sitter_go.Parse.file !!file
      |> dump_and_print_errors Tree_sitter_go.Boilerplate.dump_tree
           Tree_sitter_go.Boilerplate.dump_extras
  | Lang.Csharp ->
      Tree_sitter_c_sharp.Parse.file !!file
      |> dump_and_print_errors Tree_sitter_c_sharp.Boilerplate.dump_tree
           Tree_sitter_c_sharp.Boilerplate.dump_extras
  | Lang.Kotlin ->
      Tree_sitter_kotlin.Parse.file !!file
      |> dump_and_print_errors Tree_sitter_kotlin.Boilerplate.dump_tree
           Tree_sitter_kotlin.Boilerplate.dump_extras
  | Lang.Jsonnet ->
      Tree_sitter_jsonnet.Parse.file !!file
      |> dump_and_print_errors Tree_sitter_jsonnet.Boilerplate.dump_tree
           Tree_sitter_jsonnet.Boilerplate.dump_extras
  | Lang.Solidity ->
      Tree_sitter_solidity.Parse.file !!file
      |> dump_and_print_errors Tree_sitter_solidity.Boilerplate.dump_tree
           Tree_sitter_solidity.Boilerplate.dump_extras
  | Lang.Swift ->
      Tree_sitter_swift.Parse.file !!file
      |> dump_and_print_errors Tree_sitter_swift.Boilerplate.dump_tree
           Tree_sitter_swift.Boilerplate.dump_extras
  | Lang.Js ->
      (* JavaScript/JSX is a strict subset of TSX *)
      Tree_sitter_tsx.Parse.file !!file
      |> dump_and_print_errors Tree_sitter_tsx.Boilerplate.dump_tree
           Tree_sitter_tsx.Boilerplate.dump_extras
  | Lang.Ts ->
      (* Typescript is mostly a subset of TSX *)
      Tree_sitter_tsx.Parse.file !!file
      |> dump_and_print_errors Tree_sitter_tsx.Boilerplate.dump_tree
           Tree_sitter_tsx.Boilerplate.dump_extras
  | Lang.Lua ->
      Tree_sitter_lua.Parse.file !!file
      |> dump_and_print_errors Tree_sitter_lua.Boilerplate.dump_tree
           Tree_sitter_lua.Boilerplate.dump_extras
  | Lang.Rust ->
      Tree_sitter_rust.Parse.file !!file
      |> dump_and_print_errors Tree_sitter_rust.Boilerplate.dump_tree
           Tree_sitter_rust.Boilerplate.dump_extras
  | Lang.Ocaml ->
      Tree_sitter_ocaml.Parse.file !!file
      |> dump_and_print_errors Tree_sitter_ocaml.Boilerplate.dump_tree
           Tree_sitter_ocaml.Boilerplate.dump_extras
  | Lang.C
  | Lang.Cpp ->
      Tree_sitter_cpp.Parse.file !!file
      |> dump_and_print_errors Tree_sitter_cpp.Boilerplate.dump_tree
           Tree_sitter_cpp.Boilerplate.dump_extras
  | Lang.Html ->
      Tree_sitter_html.Parse.file !!file
      |> dump_and_print_errors Tree_sitter_html.Boilerplate.dump_tree
           Tree_sitter_html.Boilerplate.dump_extras
  | Lang.Php ->
      Tree_sitter_php.Parse.file !!file
      |> dump_and_print_errors Tree_sitter_php.Boilerplate.dump_tree
           Tree_sitter_php.Boilerplate.dump_extras
  | Lang.Terraform ->
      Tree_sitter_hcl.Parse.file !!file
      |> dump_and_print_errors Tree_sitter_hcl.Boilerplate.dump_tree
           Tree_sitter_hcl.Boilerplate.dump_extras
  | Lang.Julia ->
      Tree_sitter_julia.Parse.file !!file
      |> dump_and_print_errors Tree_sitter_julia.Boilerplate.dump_tree
           Tree_sitter_julia.Boilerplate.dump_extras
  | Lang.Dart ->
      Tree_sitter_dart.Parse.file !!file
      |> dump_and_print_errors Tree_sitter_dart.Boilerplate.dump_tree
           Tree_sitter_dart.Boilerplate.dump_extras
  | Lang.Cairo ->
      Tree_sitter_cairo.Parse.file !!file
      |> dump_and_print_errors Tree_sitter_cairo.Boilerplate.dump_tree
           Tree_sitter_cairo.Boilerplate.dump_extras
  | Lang.Promql ->
      Tree_sitter_promql.Parse.file !!file
      |> dump_and_print_errors Tree_sitter_promql.Boilerplate.dump_tree
           Tree_sitter_promql.Boilerplate.dump_extras
  | Lang.Protobuf ->
      Tree_sitter_proto.Parse.file !!file
      |> dump_and_print_errors Tree_sitter_proto.Boilerplate.dump_tree
           Tree_sitter_proto.Boilerplate.dump_extras
  | Lang.Python2
  | Lang.Python3
  | Lang.Python ->
      Tree_sitter_python.Parse.file !!file
      |> dump_and_print_errors Tree_sitter_python.Boilerplate.dump_tree
           Tree_sitter_python.Boilerplate.dump_extras
  | Lang.Dockerfile ->
      Tree_sitter_dockerfile.Parse.file !!file
      |> dump_and_print_errors Tree_sitter_dockerfile.Boilerplate.dump_tree
           Tree_sitter_dockerfile.Boilerplate.dump_extras
  | _ -> failwith "lang not supported by ocaml-tree-sitter"

let test_parse_tree_sitter lang root_paths =
  let paths, _skipped_paths =
    Find_targets_old.files_of_dirs_or_files (Some lang) root_paths
  in
  let stat_list = ref [] in
  paths |> Fpath_.to_strings
  |> List.iter (fun file ->
         Logs.info (fun m -> m ~tags "processing %s" file);
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
                 Tree_sitter_c_sharp.Parse.file file |> fail_on_error |> ignore
             | Lang.Kotlin ->
                 Tree_sitter_kotlin.Parse.file file |> fail_on_error |> ignore
             | Lang.Js ->
                 Tree_sitter_tsx.Parse.file file |> fail_on_error |> ignore
             | Lang.Jsonnet ->
                 Tree_sitter_jsonnet.Parse.file file |> fail_on_error |> ignore
             | Lang.Ts ->
                 Tree_sitter_tsx.Parse.file file |> fail_on_error |> ignore
             | Lang.Rust ->
                 Tree_sitter_rust.Parse.file file |> fail_on_error |> ignore
             | Lang.Ocaml ->
                 Tree_sitter_ocaml.Parse.file file |> fail_on_error |> ignore
             | Lang.C
             | Lang.Cpp ->
                 Tree_sitter_cpp.Parse.file file |> fail_on_error |> ignore
             | Lang.Html ->
                 Tree_sitter_html.Parse.file file |> fail_on_error |> ignore
             | Lang.Php ->
                 Tree_sitter_php.Parse.file file |> fail_on_error |> ignore
             | Lang.Terraform ->
                 Tree_sitter_hcl.Parse.file file |> fail_on_error |> ignore
             | Lang.Dart ->
                 Tree_sitter_dart.Parse.file file |> fail_on_error |> ignore
             | Lang.Move_on_sui ->
                 Tree_sitter_move_on_sui.Parse.file file
                 |> fail_on_error |> ignore
             | _ ->
                 failwith
                   (spf "lang %s not supported with tree-sitter"
                      (Lang.to_string lang)));
             Parsing_stat.correct_stat file
           with
           | exn ->
               print_exn file exn;
               Parsing_stat.bad_stat file
         in
         Stack_.push stat stat_list);
  Logs.info (fun m -> m "%s" (Parsing_stat.string_of_stats !stat_list));
  ()

(*****************************************************************************)
(* AST specific dumpers *)
(*****************************************************************************)
(* used to be offered by the ./bin/pfff tool *)
let dump_lang_ast (lang : Lang.t) (file : Fpath.t) : unit =
  match lang with
  | Lang.Ocaml ->
      let (ast : AST_ocaml.program) =
        if !Flag_semgrep.tree_sitter_only then
          let res = Parse_ocaml_tree_sitter.parse file in
          res.program |> List_.optlist_to_list
        else Parse_ml.parse_program file
      in
      let s = AST_ocaml.show_program ast in
      UCommon.pr2 s
  | _else_ ->
      failwith (spf "dumper not supported yet for lang: %s" (Lang.show lang))

(*****************************************************************************)
(* Pfff and tree-sitter parsing *)
(*****************************************************************************)

(*
   Expand the list of files or directories into a list of files in the
   specified language, and return a record for each file.

   This is meant to run the same parsers as semgrep-core does for normal
   semgrep scans.
*)
let parsing_common (caps : < Cap.time_limit ; Cap.memory_limit >)
    ?(verbose = true) lang files_or_dirs =
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

  Logs.info (fun m -> m ~tags "running with a timeout of %f.1s" timeout_seconds);
  Logs.info (fun m ->
      m ~tags "running with a memory limit of %d MiB" mem_limit_mb);

  (* less: use realpath? *)
  let paths = files_or_dirs in
  let paths, skipped =
    Find_targets_old.files_of_dirs_or_files (Some lang) paths
  in
  let stats =
    paths
    |> List.rev_map (fun file ->
           UCommon.pr2
             (spf "%05.1fs: [%s] processing %s" (Sys.time ())
                (Lang.to_capitalized_alnum lang)
                !!file);
           let stat =
             try
               match
                 Memory_limit.run_with_memory_limit
                   (caps :> < Cap.memory_limit >)
                   ~mem_limit_mb
                   (fun () ->
                     Time_limit.set_timeout
                       (caps :> < Cap.time_limit >)
                       ~name:"Test_parsing.parsing_common" timeout_seconds
                       (fun () -> Parse_target.parse_and_resolve_name lang file))
               with
               | Some res ->
                   let ast_stat = AST_stat.stat res.ast in
                   { res.Parsing_result2.stat with ast_stat = Some ast_stat }
               | None ->
                   { (Parsing_stat.bad_stat !!file) with have_timeout = true }
             with
             | Time_limit.Timeout _ -> assert false
             | exn ->
                 if verbose then print_exn !!file exn;
                 (* bugfix: bad_stat() could actually triggering some
                    Sys_error "Out of memory" when implemented naively,
                    and this exn in the exn handler was stopping the whole job.
                 *)
                 Parsing_stat.bad_stat !!file
           in
           if verbose && stat.PS.error_line_count > 0 then
             UCommon.pr2 (spf "FAILED TO FULLY PARSE: %s" stat.PS.filename);
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
let parse_project (caps : < Cap.time_limit ; Cap.memory_limit >) ~verbose lang
    name files_or_dirs =
  let stat_list, _skipped = parsing_common caps ~verbose lang files_or_dirs in
  let stat_list =
    List.filter (fun stat -> not stat.PS.have_timeout) stat_list
  in
  UCommon.pr2
    (spf "%05.1fs: [%s] done parsing %s" (Sys.time ())
       (Lang.to_capitalized_alnum lang)
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
let aggregate_file_stats (results : (string * Parsing_stat.t list) list) :
    Parsing_stats_t.project_stats list =
  List_.map
    (fun (project_name, file_stats) ->
      let acc =
        {
          Parsing_stats_t.name = project_name;
          parsing_rate = nan;
          line_count = 0;
          error_line_count = 0;
          file_count = 0;
          error_file_count = 0;
          total_node_count = 0;
          untranslated_node_count = 0;
        }
      in
      let acc =
        List.fold_left
          (fun (acc : Parsing_stats_t.project_stats) (p : Parsing_stat.t) ->
            let success = p.error_line_count =|= 0 in
            let nodes, todo_nodes =
              match p.ast_stat with
              | Some x -> (x.total_node_count, x.untranslated_node_count)
              | None -> (0, 0)
            in
            {
              acc with
              Parsing_stats_t.line_count = acc.line_count + p.total_line_count;
              error_line_count = acc.error_line_count + p.error_line_count;
              file_count = acc.file_count + 1;
              error_file_count =
                (acc.error_file_count + if not success then 1 else 0);
              total_node_count = acc.total_node_count + nodes;
              untranslated_node_count = acc.untranslated_node_count + todo_nodes;
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
      total_node_count = 0;
      untranslated_node_count = 0;
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
          total_node_count = acc.total_node_count + proj.total_node_count;
          untranslated_node_count =
            acc.untranslated_node_count + proj.untranslated_node_count;
        })
      acc project_stats
  in
  let global = update_parsing_rate acc in
  {
    language = Lang.to_capitalized_alnum lang;
    global;
    projects = project_stats;
  }

let print_json lang results =
  let project_stats = aggregate_file_stats results in
  let stats = aggregate_project_stats lang project_stats in
  let s = Parsing_stats_j.string_of_t stats in
  print_endline (Yojson.Safe.prettify s)

let parse_projects caps ~verbose lang project_dirs =
  project_dirs
  |> List_.map (fun dir ->
         let name = dir in
         parse_project caps ~verbose lang name [ Fpath.v dir ])

let parsing_stats caps ?(json = false) ?(verbose = false) lang project_dirs =
  let stat_list = parse_projects caps ~verbose lang project_dirs in
  report_counts ();
  if json then print_json lang stat_list
  else
    let flat_stat = List.concat_map snd stat_list in
    UCommon.pr (Parsing_stat.string_of_stats flat_stat)

let parsing_regressions caps lang files_or_dirs =
  let _stat_list = parsing_common caps lang files_or_dirs in
  raise Todo

let diff_pfff_tree_sitter xs =
  UCommon.pr2 "NOTE: consider using -full_token_info to get also diff on tokens";
  xs
  |> List.iter (fun file ->
         let ast1 =
           Common.save_excursion Flag_semgrep.pfff_only true (fun () ->
               Parse_target.parse_program file)
         in
         let ast2 =
           Common.save_excursion Flag_semgrep.tree_sitter_only true (fun () ->
               Parse_target.parse_program file)
         in
         let s1 = AST_generic.show_program ast1 in
         let s2 = AST_generic.show_program ast2 in
         UTmp.with_temp_file ~contents:s1 ~suffix:".x" (fun file1 ->
             UTmp.with_temp_file ~contents:s2 ~suffix:".x" (fun file2 ->
                 let xs = Common2.unix_diff !!file1 !!file2 in
                 xs |> List.iter UCommon.pr2)))

(*****************************************************************************)
(* Rule parsing *)
(*****************************************************************************)

let test_parse_rules roots =
  let targets, _skipped_paths =
    Find_targets_old.files_of_dirs_or_files (Some Lang.Yaml) roots
  in
  targets
  |> List.iter (fun file ->
         Logs.info (fun m -> m ~tags "processing %s" !!file);
         let _r = Parse_rule.parse file in
         ());
  Logs.info (fun m -> m ~tags "done test_parse_rules")
