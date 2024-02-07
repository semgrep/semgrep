(* Cooper Pierce
 *
 * Copyright (C) 2023 Semgrep Inc.
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

(* See Target_location.mli for documentation of public items. *)

type manifest = { source : Source.t; file : Fpath.t; kind : Manifest_kind.t }
[@@deriving show]

type lockfile = {
  source : Source.t;
  file : Fpath.t;
  kind : Lockfile_kind.t;
  manifest : manifest option;
}
[@@deriving show]

type code = {
  source : Source.t;
  file : Fpath.t;
  analyzer : Xlang.t;
  products : Semgrep_output_v1_t.product list;
  lockfile : lockfile option;
}
[@@deriving show]

type t = Code of code | Lockfile of lockfile [@@deriving show]

let code_of_source ?lockfile analyzer products (source : Source.t) : code =
  match source with
  | File file -> { source; file; analyzer; products; lockfile }

let lockfile_of_source ?manifest kind (source : Source.t) : lockfile =
  match source with
  | File file -> { source; file; kind; manifest }

let manifest_of_source kind (source : Source.t) : manifest =
  match source with
  | File file -> { source; file; kind }

let file (target : t) : Fpath.t =
  match target with
  | Code { file; _ }
  | Lockfile { file; _ } ->
      file

let source (target : t) : Source.t =
  match target with
  | Code { source; _ }
  | Lockfile { source; _ } ->
      source
