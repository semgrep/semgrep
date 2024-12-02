(* Matthew McQuaid
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Similar to Xtarget.ml but for lockfile targets instead of regular targets.
 *
 * See mli for documentation of public items.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type t = {
  target : Lockfile.t;
  manifest : manifest option;
  lazy_content : string lazy_t;
      (** The contents of the lockfile, as a string. *)
  lazy_dependencies : SCA_dependency.t list lazy_t;
      (** The parsed contents of the lockfile, comprising the list of specified
          dependencies and their versions. *)
}

and manifest = {
  target : Manifest.t;
  lazy_content : string lazy_t;
      (** The contents of the manifest, as a string. *)
  lazy_dependencies : SCA_dependency.manifest_dependency list lazy_t;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let resolve_manifest parser (target : Manifest.t) : manifest =
  {
    target;
    lazy_content = lazy (UFile.read_file target.path);
    lazy_dependencies = lazy (parser target.kind target.path);
  }

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let resolve manifest_parser lockfile_parser (lockfile_target : Lockfile.t)
    (manifest_target : Manifest.t option) : t =
  let manifest =
    Option.map (resolve_manifest manifest_parser) manifest_target
  in
  {
    target = lockfile_target;
    manifest;
    lazy_content = lazy (UFile.read_file lockfile_target.path);
    lazy_dependencies =
      lazy (lockfile_parser lockfile_target.kind manifest lockfile_target.path);
  }
