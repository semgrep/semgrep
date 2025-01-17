(* Matthew McQuaid, Yoann Padioleau
 *
 * Copyright (C) 2024-2025 Semgrep Inc.
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
(* Entrypoints *)
(*****************************************************************************)

(* ex: "4.0.0-beta.86" "15.0-rc-1" "14.0-rc1" "2.4.0rc0" "4.0.0-beta"
 *  "2.3.4-p2" "2.0.0-M1" "2.0.0-sp1" ...
 *)
let other_version =
  Pcre2_.regexp ~flags:[ `CASELESS ]
    {|^[0-9]+(\.[0-9]+)*[-.]?(alpha|beta|pre|rc|dev|final|release|incubating|milestone|next|stable|lts|preview|fp|cr|sp|pl|[abempquv])[-.]?[0-9]*$|}

(* ex: "117.veb" "0.5.0b3.dev42" "720.vbe985dd73d66" "1206.v14049fa"
 * "1087.1089.v2f1b_9a_b_040e4" (FYI just 1 SCA rule is using '_' in its version
 * constraint)
 *)
let almost_bailout_version =
  Pcre2_.regexp {|^[0-9]+(\.[0-9]+)*[-.][a-zA-Z0-9._-]+$|}

(* TODO: port part of specifiers.py?
 * TODO: pass more context (e.g., a Tok.t) for better Logs.warn or error
 *)
let parse (str : string) : SCA_version.t =
  (* just enough to parse some toy package-lock.json and enough
   * to parse the version part of the version constraints in our SCA rules.
   *)
  match str with
  (* ex: (most common version format) "1.2.3" *)
  | _ when str =~ "^\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)$" ->
      let s1, s2, s3 = Common.matched3 str in
      SCA_version.V
        {
          major = int_of_string s1;
          minor = int_of_string s2;
          incrementals = [ int_of_string s3 ];
        }
  (* ex: "40.0" *)
  | _ when str =~ "^\\([0-9]+\\)\\.\\([0-9]+\\)$" ->
      let s1, s2 = Common.matched2 str in
      SCA_version.V
        {
          major = int_of_string s1;
          minor = int_of_string s2;
          incrementals = [];
        }
  (* ex: "40" *)
  | _ when str =~ "^\\([0-9]+\\)$" ->
      let s1 = Common.matched1 str in
      SCA_version.V { major = int_of_string s1; minor = 0; incrementals = [] }
  (* ex: "2.2.8.1" *)
  | _ when str =~ "^\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)$"
    ->
      let s1, s2, s3, s4 = Common.matched4 str in
      SCA_version.V
        {
          major = int_of_string s1;
          minor = int_of_string s2;
          incrementals = [ int_of_string s3; int_of_string s4 ];
        }
  | _ ->
      if
        (not (Pcre2_.pmatch_noerr ~rex:other_version str))
        && not (Pcre2_.pmatch_noerr ~rex:almost_bailout_version str)
        (* nosemgrep: no-logs-in-library *)
      then Logs.warn (fun m -> m "unrecognized version format for %s" str);
      (* alt: raise (Error (spf "wrong version format for %s" str)) in *)
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
