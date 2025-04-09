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

type regular = {
  path : path;
  analyzer : Analyzer.t;
  products : Product.t list;
  dependency_source : Dependency_source.t option;
}
[@@deriving show]

type t = Regular of regular | DependencySource of Dependency_source.t
[@@deriving show]

(*****************************************************************************)
(* Dumpers *)
(*****************************************************************************)

let pp_debug_dependency_source f (t : Dependency_source.t) =
  Format.fprintf f "%s" (Dependency_source.show t)

let pp_debug_regular f (t : regular) =
  Format.fprintf f "%s (%s)"
    (t.path.internal_path_to_content |> Fpath.to_string)
    (t.analyzer |> Analyzer.to_string)

let pp_debug f = function
  | Regular t -> Format.fprintf f "target file: %a" pp_debug_regular t
  | DependencySource t ->
      Format.fprintf f "target dependency source: %a" pp_debug_dependency_source
        t

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

let mk_regular ?dependency_source analyzer products (origin : Origin.t) :
    regular =
  { path = path_of_origin origin; analyzer; products; dependency_source }

(* useful in test context *)
let mk_target (analyzer : Analyzer.t) (file : Fpath.t) : t =
  let all = Product.all in
  (* TODO: should do the check in the other mk_xxx ? *)
  assert (UFile.is_reg ~follow_symlinks:true file);
  Regular (mk_regular analyzer all (Origin.File file))

(* useful in test context or DeepScan context *)
let mk_lang_target (lang : Lang.t) (file : Fpath.t) : regular =
  mk_regular (Analyzer.of_lang lang) Product.all (File file)

(*****************************************************************************)
(* Semgrep_output_v1.target -> Target.t *)
(*****************************************************************************)
(* old: used to be Input_to_core.target -> Target.t *)

let code_target_location_of_input_to_core
    ({ path; analyzer; products; dependency_source } : Out.code_target) :
    regular =
  mk_regular ?dependency_source analyzer products (File path)

let target_of_target (input : Out.target) : t =
  match input with
  | `CodeTarget x -> Regular (code_target_location_of_input_to_core x)
  | `DependencySourceTarget x -> DependencySource x

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

let rec internal_path_of_dependency_source (source : Dependency_source.t) :
    Fpath.t =
  match source with
  | ManifestOnly { path; _ } -> path
  | LockfileOnly { path; _ } -> path
  | ManifestLockfile (_, { path; _ }) -> path
  (* TODO: this seems really dubious, what should we do here? *)
  | MultiLockfile (src :: _) -> internal_path_of_dependency_source src
  | MultiLockfile [] -> failwith "MultiLockfile is empty"

let internal_path (target : t) : Fpath.t =
  match target with
  | Regular { path = { internal_path_to_content; _ }; _ } ->
      internal_path_to_content
  | DependencySource src -> internal_path_of_dependency_source src

let origin (target : t) : Origin.t =
  match target with
  | Regular { path = { origin; _ }; _ } -> origin
  | DependencySource src -> Origin.File (internal_path_of_dependency_source src)

let analyzer (target : t) : Analyzer.t option =
  match target with
  | Regular r -> Some r.analyzer
  | DependencySource _ -> None

let analyzers_of_targets (targets : t list) : Analyzer.t Set_.t =
  List.fold_left
    (fun set target ->
      match analyzer target with
      | None -> set
      | Some a ->
          let analyzers = Analyzer.flatten a in
          List_.fold_right Set_.add analyzers set)
    Set_.empty targets
