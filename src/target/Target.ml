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
module Out = Semgrep_output_v1_t

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

let pp_debug_lockfile f t =
  Format.fprintf f "%s" (t.path.internal_path_to_content |> Fpath.to_string)

type regular = {
  path : path;
  analyzer : Xlang.t;
  products : Out.product list;
  lockfile : lockfile option;
}
[@@deriving show]

let pp_debug_regular f t =
  Format.fprintf f "%s (%s)"
    (t.path.internal_path_to_content |> Fpath.to_string)
    (t.analyzer |> Xlang.to_string)

type t = Regular of regular | Lockfile of lockfile [@@deriving show]

let pp_debug f = function
  | Regular t -> Format.fprintf f "target file: %a" pp_debug_regular t
  | Lockfile t -> Format.fprintf f "target lockfile: %a" pp_debug_lockfile t

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

let mk_target (lang : Lang.t) (file : Fpath.t) : t =
  (* coupling: src/targeting/Product.all *)
  let all = [ `SAST; `SCA; `Secrets ] in
  Regular (mk_regular (Xlang.of_lang lang) all (Origin.File file))
