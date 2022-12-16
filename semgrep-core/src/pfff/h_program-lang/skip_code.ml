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

let logger = Logging.get_logger [__MODULE__]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * It is often useful to skip certain parts of a codebase. Large codebase
 * often contains special code that can not be parsed, that contains
 * dependencies that should not exist, old code that we don't want
 * to analyze, etc.
 *
 * todo: simplify interface in skip_list.txt file? can infer
 * dir or file, and maybe sometimes instead of skip we would like
 * to specify the opposite, what we want to keep, so maybe a simple
 *  +/- syntax would be better.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* the filename are in readable path format *)
type skip =
  | Dir of Common.dirname
  | File of Common.filename
  | DirElement of Common.dirname
  | SkipErrorsDir of Common.dirname

(*****************************************************************************)
(* IO *)
(*****************************************************************************)
let load file =
  Common.cat file
  |> Common.exclude (fun s ->
    s =~ "#.*" || s =~ "^[ \t]*$"
  )
  |> List.map (fun s ->
    match s with
    | _ when s =~ "^dir:[ ]*\\([^ ]+\\)" ->
        Dir (Common.matched1 s)
    | _ when s =~ "^skip_errors_dir:[ ]*\\([^ ]+\\)" ->
        SkipErrorsDir (Common.matched1 s)
    | _ when s =~ "^file:[ ]*\\([^ ]+\\)" ->
        File (Common.matched1 s)
    | _ when s =~ "^dir_element:[ ]*\\([^ ]+\\)" ->
        DirElement (Common.matched1 s)
    | _ -> failwith ("wrong line format in skip file: " ^ s)
  )

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let filter_files skip_list ~root relative_paths : string list * string list =
  let skip_files =
    skip_list |> Common.map_filter (function
      | File s -> Some s
      | _ -> None
    ) |> Common.hashset_of_list
  in
  let skip_dirs =
    skip_list |> Common.map_filter (function
      | Dir s -> Some s
      | _ -> None
    )
  in
  let skip_dir_elements =
    skip_list |> Common.map_filter (function
      | DirElement s -> Some s
      | _ -> None
    )
  in
  let skipped_abs_paths = ref [] in
  let relative_paths =
    relative_paths
    |> List.filter (fun rel_path ->
      let path = Common.readable ~root rel_path in
      if Hashtbl.mem skip_files path
      || (skip_dirs |> List.exists
            (fun dir -> path =~ (dir ^ ".*")))
      || (skip_dir_elements |> List.exists
            (fun dir -> path =~ (".*/" ^ dir ^ "/.*")))
      then (
        skipped_abs_paths := path :: !skipped_abs_paths;
        false
      )
      else
        true
    )
  in
  (relative_paths, List.rev !skipped_abs_paths)


(* copy paste of h_version_control/git.ml *)
let find_vcs_root_from_absolute_path file =
  let xs = Common.split "/" (Common2.dirname file) in
  let xxs = Common2.inits xs in
  xxs |> List.rev |> Common.find_some_opt (fun xs ->
    let dir = "/" ^ Common.join "/" xs in
    if Sys.file_exists (Filename.concat dir ".git") ||
       Sys.file_exists (Filename.concat dir ".hg") ||
       false
    then Some dir
    else None
  )

let find_skip_file_from_root root =
  let candidates = [
    "skip_list.txt";
    (* fbobjc specific *)
    "Configurations/Sgrep/skip_list.txt";
    (* www specific *)
    "conf/codegraph/skip_list.txt";
  ]
  in
  candidates |> Common.find_some_opt (fun f ->
    let full = Filename.concat root f in
    if Sys.file_exists full
    then Some full
    else None
  )

let filter_files_if_skip_list ~root xs =
  let root =
    match root with
    | [x] -> x
    | _ ->
        (match xs with
         | [] -> "/"
         | x::_ ->
             match find_vcs_root_from_absolute_path x with
             | Some root -> root
             | None -> "/"
        )
  in
  match find_skip_file_from_root root with
  | Some skip_file ->
      let skip_list = load skip_file in
      logger#info "using skip list in %s" skip_file;
      filter_files skip_list root xs
  | None ->
      (xs, [])

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let build_filter_errors_file skip_list =
  let skip_dirs =
    skip_list |> Common.map_filter (function
      | SkipErrorsDir dir -> Some dir
      | _ -> None
    )
  in
  (fun readable ->
     skip_dirs |> List.exists (fun dir -> readable =~ ("^" ^ dir))
  )

let reorder_files_skip_errors_last skip_list root xs =
  let is_file_want_to_skip_error = build_filter_errors_file skip_list in
  let (skip_errors, ok) =
    xs |> List.partition (fun file ->
      let readable = Common.readable ~root file in
      is_file_want_to_skip_error readable
    )
  in
  ok @ skip_errors
