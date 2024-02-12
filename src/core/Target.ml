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

type target_path = { source : Source.t; internal_path_to_content : Fpath.t }
[@@deriving show]

type manifest = { path : target_path; kind : Manifest_kind.t } [@@deriving show]

type lockfile = {
  path : target_path;
  kind : Lockfile_kind.t;
  manifest : manifest option;
}
[@@deriving show]

type code = {
  path : target_path;
  analyzer : Xlang.t;
  products : Semgrep_output_v1_t.product list;
  lockfile : lockfile option;
}
[@@deriving show]

type t = Code of code | Lockfile of lockfile [@@deriving show]

let target_path_of_source (source : Source.t) : target_path =
  match source with
  | File file -> { source; internal_path_to_content = file }

let code_of_source ?lockfile analyzer products (source : Source.t) : code =
  { path = target_path_of_source source; analyzer; products; lockfile }

let lockfile_of_source ?manifest kind (source : Source.t) : lockfile =
  { path = target_path_of_source source; kind; manifest }

let manifest_of_source kind (source : Source.t) : manifest =
  { path = target_path_of_source source; kind }

let internal_path_to_content (target : t) : Fpath.t =
  match target with
  | Code { path = { internal_path_to_content; _ }; _ }
  | Lockfile { path = { internal_path_to_content; _ }; _ } ->
      internal_path_to_content

let source (target : t) : Source.t =
  match target with
  | Code { path = { source; _ }; _ }
  | Lockfile { path = { source; _ }; _ } ->
      source
