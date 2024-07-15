(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 Semgrep Inc.
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
open Fpath_.Operators
module Eq = Equivalence

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let error s = failwith (spf "sgrep_equivalence: wrong format. %s" s)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let parse file =
  let str = UFile.read_file file in
  let yaml_res = Yaml.of_string str in
  match yaml_res with
  | Result.Ok v -> (
      match v with
      | `O [ ("equivalences", `A xs) ] ->
          xs
          |> List_.map (fun v ->
                 match v with
                 | `O xs -> (
                     match Assoc.sort_by_key_lowfirst xs with
                     | [
                      ("id", `String id);
                      ("languages", `A langs);
                      ("pattern", `String str);
                     ] ->
                         let languages =
                           langs
                           |> List_.map (function
                                | `String s -> (
                                    match Lang.of_string_opt s with
                                    | None ->
                                        error (spf "unsupported language: %s" s)
                                    | Some l -> l)
                                | _ ->
                                    error
                                      (spf "expecting a string for languages"))
                         in
                         let lang =
                           match languages with
                           | [] -> error "we need at least one language"
                           | x :: _xs -> x
                         in
                         let left, op, right =
                           let xs =
                             Str.full_split (Str.regexp "<==>\\|==>") str
                           in
                           match xs with
                           | [ Str.Text a; Str.Delim "<==>"; Str.Text b ] ->
                               (a, Eq.Equiv, b)
                           | [ Str.Text a; Str.Delim "==>"; Str.Text b ] ->
                               (a, Eq.Imply, b)
                           | _ ->
                               error
                                 (spf "could not parse the equivalence: %s" str)
                         in
                         let left =
                           match Parse_pattern.parse_pattern lang left with
                           | Ok x -> x
                           | Error s ->
                               error
                                 (spf
                                    "could not parse the left pattern: %s (%s)"
                                    left s)
                           | exception exn ->
                               error
                                 (spf
                                    "could not parse the left pattern: %s (exn \
                                     = %s)"
                                    left (Common.exn_to_s exn))
                         in
                         let right =
                           match Parse_pattern.parse_pattern lang right with
                           | Ok x -> x
                           | Error s ->
                               error
                                 (spf
                                    "could not parse the right pattern: %s (%s)"
                                    right s)
                           | exception exn ->
                               error
                                 (spf
                                    "could not parse the right pattern: %s \
                                     (exn = %s)"
                                    right (Common.exn_to_s exn))
                         in
                         { Eq.id; left; op; right; languages }
                     | _ -> error "wrong equivalence fields")
                 | _ -> error "wrong equivalence fields")
      | _ -> error "missing equivalences entry")
  | Result.Error (`Msg s) ->
      failwith
        (spf "sgrep_equivalence: could not parse %s (error = %s)" !!file s)
