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
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Parse a package version or version constraint
 *
 * TODO: port parts of cli/src/semdep/external/packaging/specifiers.py
 *)

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

(* alt: use Result.t *)
exception Error of string

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Entrypoints *)
(*****************************************************************************)

(* TODO: port part of specifiers.py
 * TODO: pass more context (e.g., a Tok.t) for better Logs.warn or error
 *)
let parse (str : string) : SCA_version.t =
  (* just enough to parse some toy package-lock.json and enough
   * to parse the version part of the version constraints in our SCA rules
   *)
  match str with
  (* "1.2.3" *)
  | _ when str =~ "^\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)$" ->
      let s1, s2, s3 = Common.matched3 str in
      SCA_version.V
        {
          major = int_of_string s1;
          minor = int_of_string s2;
          incrementals = [ int_of_string s3 ];
        }
  (* "40.0" *)
  | _ when str =~ "^\\([0-9]+\\)\\.\\([0-9]+\\)$" ->
      let s1, s2 = Common.matched2 str in
      SCA_version.V
        {
          major = int_of_string s1;
          minor = int_of_string s2;
          incrementals = [];
        }
  (* "40" *)
  | _ when str =~ "^\\([0-9]+\\)$" ->
      let s1 = Common.matched1 str in
      SCA_version.V { major = int_of_string s1; minor = 0; incrementals = [] }
  (* alt: raise (Error (spf "wrong version format for %s" str)) in *)
  | _ ->
      (* nosemgrep: no-logs-in-library *)
      Logs.warn (fun m -> m "unrecognized version format for %s" str);
      SCA_version.Other str

(* TODO: port part of specifiers.py *)
let parse_constraints (s : string) : SCA_pattern.version_constraints =
  let error () = raise (Error (spf "wrong constraint format for %s" s)) in
  (* just enough to parse some toy package-lock.json *)
  (* similar to what we did for ruleid: annots in cli_test/Test_annotation.ml *)
  (* start from " > 1.0.2, < 1.05 " *)
  let s = String.trim s in
  let xs = Str.split_delim (Str.regexp "[ \t]*,[ \t]*") s in
  SCA_pattern.SCA_And
    (xs
    |> List_.map (fun s ->
           (* "> 1.0.2" *)
           let s = String.trim s in
           if s =~ "^\\([=<>]+\\)[ \t]*\\([^ ]+\\)$" then
             let op, ver = Common.matched2 s in
             let op : SCA_pattern.sca_operator =
               match op with
               | "="
               | "==" ->
                   Eq
               | ">=" -> Gte
               | "<=" -> Lte
               | ">" -> Gt
               | "<" -> Lt
               | _ -> error ()
             in
             let version : SCA_version.t = parse ver in
             SCA_pattern.{ op; version }
           else error ()))
