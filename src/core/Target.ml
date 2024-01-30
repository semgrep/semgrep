(* Cooper Pierce
 *
 * Copyright (c) Semgrep Inc.
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

(* See Target.mli for documentation of public items. *)

type path = { origin : Origin.t; internal_path_to_content : Fpath.t }
[@@deriving show, eq]

type manifest = { path : path; kind : Manifest_kind.t } [@@deriving show]

type lockfile = {
  path : path;
  kind : Lockfile_kind.t;
  manifest : manifest option;
}
[@@deriving show]

type regular = {
  path : path;
  analyzer : Xlang.t;
  products : Semgrep_output_v1_t.product list;
  lockfile : lockfile option;
}
[@@deriving show]

type t = Regular of regular | Lockfile of lockfile [@@deriving show]

(** [git_blob_to_tempfile sha] is the path to a newly created temporary file
    which contains the contents of the git blob object identified by [sha] *)
let git_blob_to_tempfile sha =
  let contents = Git_wrapper.cat_file_blob sha |> Result.get_ok in
  let file =
    UCommon.new_temp_file "git-blob" ([%show: Git_wrapper.sha] sha) |> Fpath.v
  in
  UFile.write_file file contents;
  file

let path_of_origin (origin : Origin.t) : path =
  match origin with
  | File file -> { origin; internal_path_to_content = file }
  | GitBlob { sha; _ } ->
      { origin; internal_path_to_content = git_blob_to_tempfile sha }

let mk_regular ?lockfile analyzer products (origin : Origin.t) : regular =
  { path = path_of_origin origin; analyzer; products; lockfile }

let mk_lockfile ?manifest kind (origin : Origin.t) : lockfile =
  { path = path_of_origin origin; kind; manifest }

let mk_manifest kind (origin : Origin.t) : manifest =
  { path = path_of_origin origin; kind }

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
