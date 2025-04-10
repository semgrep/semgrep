(* Cooper Pierce
 *
 * Copyright (c) 2024, Semgrep Inc.
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
module Out = Semgrep_output_v1_j

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* See Target.mli for documentation of public items. *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type path = { origin : Origin.t; internal_path_to_content : Fpath.t }
[@@deriving show, eq]

type t = {
  path : path;
  analyzer : Analyzer.t;
  products : Product.t list;
  dependency_source : Dependency_source.t option;
}
[@@deriving show]

(*****************************************************************************)
(* Dumpers *)
(*****************************************************************************)

let pp_debug f (t : t) =
  Format.fprintf f "target file: %s (%s)"
    (t.path.internal_path_to_content |> Fpath.to_string)
    (t.analyzer |> Analyzer.to_string)

(* needed because of some deriving yojson in Targeting_stat.ml *)
let to_yojson (x : t) : Yojson.Safe.t =
  let str = show x in
  (* TODO? could generate a more complex JSON but simpler to abuse show for now*)
  `String str

let of_yojson (_ : Yojson.Safe.t) : (t, string) result =
  failwith "Target.of_yojson is not implemented and should not be needed"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(** [tempfile_of_git_blob sha] is the path to a newly created temporary file
    which contains the contents of the git blob object identified by [sha] *)
let tempfile_of_git_blob sha =
  let contents = sha |> Git_wrapper.cat_file_blob |> Result.get_ok in
  (* TODO: delete this file when done! For this, use 'with_temp_file'. *)
  (* TODO: use CapTmp, but that requires to change lots of callers *)
  let file =
    (* nosemgrep: forbid-tmp *)
    UTmp.new_temp_file ~prefix:"git-blob-"
      ~suffix:(Git_wrapper.hex_of_hash sha)
      ()
  in
  UFile.write_file file contents;
  file

let path_of_origin (origin : Origin.t) : path =
  match origin with
  | File file -> { origin; internal_path_to_content = file }
  | GitBlob { sha; _ } ->
      { origin; internal_path_to_content = tempfile_of_git_blob sha }

(*****************************************************************************)
(* Builders *)
(*****************************************************************************)

let mk_target_origin ?dependency_source analyzer products (origin : Origin.t) :
    t =
  { path = path_of_origin origin; analyzer; products; dependency_source }

(* useful in test context *)
let mk_target_fpath (analyzer : Analyzer.t) (file : Fpath.t) : t =
  let all = Product.all in
  (* TODO: should do the check in the other mk_xxx ? *)
  assert (UFile.is_reg ~follow_symlinks:true file);
  mk_target_origin analyzer all (Origin.File file)

(* useful in test context or DeepScan context *)
let mk_lang_target (lang : Lang.t) (file : Fpath.t) : t =
  mk_target_fpath (Analyzer.of_lang lang) file

(*****************************************************************************)
(* Semgrep_output_v1.target -> Target.t *)
(*****************************************************************************)
(* old: used to be Input_to_core.target -> Target.t *)

let target_location_of_input_to_core
    ({ path; analyzer; products; dependency_source } : Out.code_target) : t =
  mk_target_origin ?dependency_source analyzer products (File path)

let target_of_target (input : Out.target) : t =
  match input with
  | `CodeTarget x -> target_location_of_input_to_core x
  | `DependencySourceTarget _x ->
      failwith
        "Impossible: pysemgrep should not generate DependencySourceTarget"

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

let internal_path (target : t) : Fpath.t =
  let { path = { internal_path_to_content; _ }; _ } = target in
  internal_path_to_content

let origin (target : t) : Origin.t =
  let { path = { origin; _ }; _ } = target in
  origin

let analyzers_of_targets (targets : t list) : Analyzer.t Set_.t =
  List.fold_left
    (fun set target ->
      let a = target.analyzer in
      let analyzers = Analyzer.flatten a in
      List_.fold_right Set_.add analyzers set)
    Set_.empty targets
