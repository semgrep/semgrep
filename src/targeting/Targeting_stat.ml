(* Austin Theriault
 *
 * Copyright (C) 2024, Semgrep, Inc.
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
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* This file contains useful debugging info we can glean from targets.
 * Linux `stat` but more specific to Semgrep. We collect line count, file kind
 *  (regular, symlink etc.), if it was minified, size, if it's a textual file
 *  type and more
 *
 * Try the following with the yojson'd version of annotated_target_list:
 * ```bash
 * pbpaste > annotated_targets.json # paste json
 * jq ".[] | select(.stat.textual | not) | .internal_path" annotated_targets.json # get list of non text files
 * jq "sort_by(.stat.line_count) | .[] | select(.stat.line_count > 4000) | {path: .internal_path, line_count:.stat.line_count}" annotated_targets.json # get list of files w/ line count > 4k
 * jq ".[] | select(.minified) | .internal_path" annotated_targets.json # get list of minified files
 * ```
 *)

(*****************************************************************************)
(* types *)
(*****************************************************************************)

(* TODO? file perms? libmagic? *)
type stat = {
  kind : Unix.file_kind;
      [@to_yojson UFile.file_kind_to_yojson]
      [@of_yojson UFile.file_kind_of_yojson]
  line_count : int;
  minified : bool; (* i.e. very low whitespace but is code *)
  size : int;
  textual : bool;
  type_ : File_type.file_type; [@key "type"]
}
[@@deriving yojson]

(* Similar info to linux's stat, but without things we may not care about
 * (ino, utime etc)
 *)
type annotated_target = {
  internal_path : string; (* here so it's easily jq-able *)
  targets : Target.t list; (* associate the file with its possible targets *)
  stat : stat;
}
[@@deriving yojson]
(* A path with its stat info and associated targets *)

type annotated_target_list = annotated_target list [@@deriving yojson]

let stat_file (file : Fpath.t) =
  let stats = Unix.stat !!file in
  let line_count = List.length (UFile.cat file) in
  let type_ = File_type.file_type_of_file file in
  let textual = File_type.is_textual_file file in
  let minified =
    if textual then
      Common.save_excursion Flag_semgrep.skip_minified_files true (fun () ->
          Result.is_error (Skip_target.is_minified file))
    else false
  in
  (* TODO: iago's is_large_machine_optimized PR (semgrep/semgrep#9992) *)
  {
    kind = stats.st_kind;
    size = stats.st_size;
    line_count;
    minified;
    textual;
    type_;
  }

(*****************************************************************************)
(* Entrypoint *)
(*****************************************************************************)

let annotate_targets (targets : Target.t list) : annotated_target_list =
  let targets_by_path = Assoc.group_by Target.internal_path targets in
  targets_by_path
  |> List_.map (fun (path, targets) ->
         {
           internal_path = Fpath.to_string path;
           stat = stat_file path;
           targets;
         })
