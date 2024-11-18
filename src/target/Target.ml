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
module Out = Semgrep_output_v1_t
module OutJ = Semgrep_output_v1_j
module In = Input_to_core_t

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* See Target.mli for documentation of public items. *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type path = {
  origin : Origin.t;
  internal_path_to_content : Fpath.t;
      [@to_yojson Fpath_.to_yojson] [@of_yojson Fpath_.of_yojson]
}
[@@deriving show, eq, yojson]

type manifest = { path : path; kind : Manifest_kind.t }
[@@deriving show, yojson]

type lockfile = { path : path; kind : Lockfile_kind.t }
[@@deriving show, yojson]

type dependency_source =
  | ManifestOnly of manifest
  | LockfileOnly of lockfile
  | ManifestAndLockfile of manifest * lockfile

let pp_debug_lockfile f t =
  Format.fprintf f "%s" (t.path.internal_path_to_content |> Fpath.to_string)

(* TODO: Put this somewhere else? *)
let out_product_list_to_yojson product_list =
  `List
    (List_.map
       (* A little redundant *)
         (fun p -> p |> OutJ.string_of_product |> Yojson.Safe.from_string)
       product_list)

(* TODO: Put this somewhere else? *)
let out_product_list_of_yojson yojson =
  match yojson with
  | `List products -> (
      try
        Ok
          (List_.map
             (fun p -> p |> Yojson.Safe.to_string |> OutJ.product_of_string)
             products)
      with
      | e -> Error (Printexc.to_string e))
  | json ->
      Error
        (Printf.sprintf
           "Could not convert to Out.product list expected `List, received %s"
           Yojson.Safe.(to_string json))

type regular = {
  path : path;
  analyzer : Xlang.t;
  products : Out.product list;
      [@to_yojson out_product_list_to_yojson]
      [@of_yojson out_product_list_of_yojson]
  (* TODO: (sca) associate each target with a dependency_source here rather than only a lockfile.
   * We did not do this at the time that we added dependency_source because this code was unused at that point. *)
  lockfile : lockfile option;
}
[@@deriving show, yojson]

let pp_debug_regular f t =
  Format.fprintf f "%s (%s)"
    (t.path.internal_path_to_content |> Fpath.to_string)
    (t.analyzer |> Xlang.to_string)

type t = Regular of regular | Lockfile of lockfile [@@deriving show, yojson]

let pp_debug f = function
  | Regular t -> Format.fprintf f "target file: %a" pp_debug_regular t
  | Lockfile t -> Format.fprintf f "target lockfile: %a" pp_debug_lockfile t

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

let mk_regular ?lockfile analyzer products (origin : Origin.t) : regular =
  { path = path_of_origin origin; analyzer; products; lockfile }

let mk_lockfile kind (origin : Origin.t) : lockfile =
  { path = path_of_origin origin; kind }

let mk_manifest kind (origin : Origin.t) : manifest =
  { path = path_of_origin origin; kind }

let mk_target (xlang : Xlang.t) (file : Fpath.t) : t =
  let all = Product.all in
  (* TODO: should do the check in the other mk_xxx ? *)
  assert (UFile.is_file file);
  Regular (mk_regular xlang all (Origin.File file))

(*****************************************************************************)
(* Input_to_core -> Target *)
(*****************************************************************************)

let lockfile_target_of_input_to_core
    ({ path; lockfile_kind = kind } : In.lockfile_target) : lockfile =
  mk_lockfile kind (File (Fpath.v path))

let code_target_location_of_input_to_core
    ({ path; analyzer; products; lockfile_target } : In.code_target) : regular =
  let lockfile = Option.map lockfile_target_of_input_to_core lockfile_target in
  mk_regular ?lockfile analyzer products (File (Fpath.v path))

let target_of_input_to_core (input : In.target) : t =
  match input with
  | `CodeTarget x -> Regular (code_target_location_of_input_to_core x)
  | `LockfileTarget x -> Lockfile (lockfile_target_of_input_to_core x)

(*****************************************************************************)
(* Semgrep_output -> Target *)
(*****************************************************************************)

let manifest_of_semgrep_output ({ path; kind } : Out.manifest) : manifest =
  mk_manifest kind (File path)

let lockfile_of_semgrep_output ({ path; kind } : Out.lockfile) : lockfile =
  mk_lockfile kind (File path)

let dependency_source_of_semgrep_output (output_source : Out.dependency_source)
    =
  match output_source with
  | ManifestOnlyDependencySource manifest ->
      ManifestOnly (manifest_of_semgrep_output manifest)
  | LockfileOnlyDependencySource lockfile ->
      LockfileOnly (lockfile_of_semgrep_output lockfile)
  | ManifestLockfileDependencySource (manifest, lockfile) ->
      ManifestAndLockfile
        ( manifest_of_semgrep_output manifest,
          lockfile_of_semgrep_output lockfile )

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

let internal_path (target : t) : Fpath.t =
  match target with
  | Regular { path = { internal_path_to_content; _ }; _ }
  | Lockfile { path = { internal_path_to_content; _ }; _ } ->
      internal_path_to_content

let origin (target : t) : Origin.t =
  match target with
  | Regular { path = { origin; _ }; _ }
  | Lockfile { path = { origin; _ }; _ } ->
      origin

let analyzer (target : t) : Xlang.t option =
  match target with
  | Regular r -> Some r.analyzer
  | Lockfile _ -> None
