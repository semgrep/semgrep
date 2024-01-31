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

type t = Code of code | Lockfile of lockfile [@@deriving show]

(* The location of a "normal" semgrep target, comprising source code (or for
 * regex/generic, arbitrary text data) to be executed.
 *)
and code = {
  (* The source of the data as is relevant to the user. This could be, e.g., a
   * relative (from the project root or scanning directory) path to a file, a
   * git object and associated information, or anything else a Source.t can
   * designate.
   *
   * This should be used when reporting a location to the user.
   *)
  source : Source.t;
  (* The path to a file which contains the data to be scanned. This could be
   * the same as the source, if the source is a path to a regular file, or it
   * could be a tempfile
   *
   *)
  file : Fpath.t;
  analyzer : Xlang.t;
  products : Semgrep_output_v1_t.product list;
  (* Optional lockfile associated with this target.
   * The association is namely that this target has its dependencies specified
   * by the given lockfile. Core doesn't need to worry about determining these
   * associations; rather the target selection and generation process must
   * resolve these connections as part of generating code targets.
   *)
  lockfile : lockfile option;
}
[@@deriving show]

(* A lockfile to be matched against dependency patterns. *)
and lockfile = {
  source : Source.t;
  file : Fpath.t;
  kind : Lockfile_kind.t;
  manifest : manifest option;
}
[@@deriving show]

and manifest = { source : Source.t; file : Fpath.t; kind : Manifest_kind.t }
[@@deriving show]

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
