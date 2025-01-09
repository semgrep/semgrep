(* Matthew McQuaid, Yoann Padioleau
 *
 * Copyright (C) 2024 Semgrep Inc.
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
module Log = Log_parsing.Log
module J = JSON

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Parsing a lockfile or manifest file.
 *
 * TODO: port lots of code from cli/src/semdep/parsers/
 *)

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

exception UnsupportedFormat of string
exception WrongFormat of string

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* TODO: reuse recent matthew's parser in src/sca/ in semgrep-pro *)
let parse_npm_package_json (file : Fpath.t) : SCA_dependency.t list =
  (* TODO we should use our own json parser to get the location
   * information.
   *)
  let loc1 = Tok.first_loc_of_file file in
  let tok1 = Tok.first_tok_of_file file in
  let loc = (loc1, loc1) in
  let tokens = lazy [ tok1 ] in

  UChan.with_open_in file (fun chan ->
      let json = JSON.json_of_chan chan in
      match json with
      (* just enough to parse some toy package-lock.json *)
      | J.Object
          [
            ("requires", _);
            ("lockfileVersion", J.Int 1);
            ("dependencies", J.Object deps);
          ] ->
          deps
          |> List_.map (fun (package_name, json) ->
                 match json with
                 | J.Object
                     [
                       ("version", J.String ver);
                       ("resolved", J.String url);
                       ("integrity", J.String _checksum);
                     ] ->
                     SCA_dependency.
                       {
                         package_name;
                         package_version = Parse_SCA_version.parse ver;
                         package_version_string = ver;
                         ecosystem = `Npm;
                         transitivity = `Unknown;
                         url = Uri_.of_string_opt url;
                         loc;
                         tokens;
                       }
                 | _ -> raise (WrongFormat "package-lock.json"))
      | _ -> raise (WrongFormat "package-lock.json"))

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* TODO: add parsers, guard behind semgrep-pro  *)
let parse (kind : Lockfile.kind)
    (_manifest_optTODO : Lockfile_xtarget.manifest option) (file : Fpath.t) :
    SCA_dependency.t list =
  Log.debug (fun m ->
      m "parsing lockfile %s with kind %s" !!file (Lockfile.show_kind kind));
  match kind with
  | NpmPackageLockJson -> parse_npm_package_json file
  | PipRequirementsTxt
  | PoetryLock
  | PipfileLock
  | YarnLock
  | PnpmLock
  | GemfileLock
  | GoMod
  | CargoLock
  | MavenDepTree
  | GradleLockfile
  | ComposerLock
  | NugetPackagesLockJson
  | PubspecLock
  | SwiftPackageResolved
  | MixLock
  | UvLock
  | ConanLock
  | PodfileLock ->
      raise (UnsupportedFormat (Lockfile.show_kind kind))

let parse_manifest :
    Manifest.kind -> Fpath.t -> SCA_dependency.manifest_dependency list =
  function
  (* TODO: add parsers, guard behind semgrep-pro  *)
  | `RequirementsIn
  | `PackageJson
  | `Gemfile
  | `GoMod
  | `CargoToml
  | `PomXml
  | `BuildGradle
  | `SettingsGradle
  | `ComposerJson
  | `NugetManifestJson
  | `PubspecYaml
  | `PackageSwift
  | `MixExs
  | `Pipfile
  | `PyprojectToml
  | `ConanFilePy
  | `ConanFileTxt
  | `Csproj
  | `Podfile ->
      fun _ -> []
