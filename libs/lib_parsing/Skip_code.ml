(* Yoann Padioleau
 *
 * Copyright (C) 2012 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Skipping code using a skip_list.txt config file.
 *
 * It is often useful to skip certain parts of a codebase. Large codebase
 * often contains special code that can not be parsed, that contains
 * dependencies that should not exist, old code that we don't want
 * to analyze, etc.
 *
 * todo: simplify interface in skip_list.txt file? can infer
 * dir or file, and maybe sometimes instead of skip we would like
 * to specify the opposite, what we want to keep, so maybe a simple
 *  +/- syntax would be better.
 * update: actually gitignore supports the ! negative patterns as
 * well as the include extra config.
 *
 * !!This module is deprecated!!! You should prefer the gitignore library
 * to skip files.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* the filename are in readable path format *)
type skip =
  | Dir of Fpath.t
  | File of Fpath.t
  | DirElement of Fpath.t
  | SkipErrorsDir of Fpath.t

(*****************************************************************************)
(* IO *)
(*****************************************************************************)
let load file =
  UFile.Legacy.cat !!file
  |> List_.exclude (fun s -> s =~ "#.*" || s =~ "^[ \t]*$")
  |> List_.map (fun s ->
         match s with
         | _ when s =~ "^dir:[ ]*\\([^ ]+\\)" ->
             Dir (Fpath.v (Common.matched1 s))
         | _ when s =~ "^skip_errors_dir:[ ]*\\([^ ]+\\)" ->
             SkipErrorsDir (Fpath.v (Common.matched1 s))
         | _ when s =~ "^file:[ ]*\\([^ ]+\\)" ->
             File (Fpath.v (Common.matched1 s))
         | _ when s =~ "^dir_element:[ ]*\\([^ ]+\\)" ->
             DirElement (Fpath.v (Common.matched1 s))
         | _ -> failwith ("wrong line format in skip file: " ^ s))

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let filter_files skip_list ~root relative_paths : Fpath.t list * Fpath.t list =
  let skip_files =
    skip_list
    |> List_.filter_map (function
         | File s -> Some s
         | _ -> None)
    |> Hashtbl_.hashset_of_list
  in
  let skip_dirs =
    skip_list
    |> List_.filter_map (function
         | Dir s -> Some s
         | _ -> None)
  in
  let skip_dir_elements =
    skip_list
    |> List_.filter_map (function
         | DirElement s -> Some s
         | _ -> None)
  in
  let skipped_abs_paths = ref [] in
  let relative_paths =
    relative_paths
    |> List.filter (fun rel_path ->
           let path = Fpath_.readable ~root rel_path in
           if
             Hashtbl.mem skip_files path
             || skip_dirs |> List.exists (fun dir -> !!path =~ !!dir ^ ".*")
             || skip_dir_elements
                |> List.exists (fun dir -> !!path =~ ".*/" ^ !!dir ^ "/.*")
           then (
             skipped_abs_paths := path :: !skipped_abs_paths;
             false)
           else true)
  in
  (relative_paths, List.rev !skipped_abs_paths)

(* copy paste of h_version_control/git.ml *)
let find_vcs_root_from_absolute_path file =
  let xs = String_.split ~sep:"/" (Filename.dirname !!file) in
  let xxs = Common2.inits xs in
  xxs |> List.rev
  |> List_.find_some_opt (fun xs ->
         let dir = "/" ^ String.concat "/" xs in
         if
           Sys.file_exists (Filename.concat dir ".git")
           || Sys.file_exists (Filename.concat dir ".hg")
           || false
         then Some (Fpath.v dir)
         else None)

let find_skip_file_from_root root =
  (* TODO: figure out why this doesnt work in windows *)
  if !Common.jsoo then None
  else
    let candidates =
      [
        "skip_list.txt";
        (* fbobjc specific *)
        "Configurations/Sgrep/skip_list.txt";
        (* www specific *)
        "conf/codegraph/skip_list.txt";
      ]
      |> Fpath_.of_strings
    in

    candidates
    |> List_.find_some_opt (fun f ->
           let full = Fpath.append root f in
           if Sys.file_exists !!full then Some full else None)

let filter_files_if_skip_list ~root xs =
  let root =
    match root with
    | [ x ] -> x
    | _ -> (
        match xs with
        | [] -> Fpath.v "/"
        | x :: _ -> (
            match find_vcs_root_from_absolute_path x with
            | Some root -> root
            | None -> Fpath.v "/"))
  in
  match find_skip_file_from_root root with
  | Some skip_file ->
      let skip_list = load skip_file in
      (* nosemgrep: no-logs-in-library *)
      Logs.info (fun m -> m "using skip list in %s" !!skip_file);
      filter_files skip_list root xs
  | None -> (xs, [])

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let build_filter_errors_file skip_list =
  let skip_dirs =
    skip_list
    |> List_.filter_map (function
         | SkipErrorsDir dir -> Some dir
         | _ -> None)
  in
  fun readable ->
    skip_dirs |> List.exists (fun dir -> !!readable =~ "^" ^ !!dir)

let reorder_files_skip_errors_last skip_list root xs =
  let is_file_want_to_skip_error = build_filter_errors_file skip_list in
  let skip_errors, ok =
    xs
    |> List.partition (fun file ->
           let readable = Fpath_.readable ~root file in
           is_file_want_to_skip_error readable)
  in
  ok @ skip_errors
