(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* Parsing functions. They will raise exceptions if the input is malformed.

   The anchor is the pattern that matches the path from the git project
   root to the work folder, typically the one containing the gitignore file.

   The default selection mode is Ignore.
*)

type parsed_pattern = {
  selector : Gitignore.path_selector;
  absolute_pattern : Glob.Pattern.t;
  is_negated : bool;
}
(** Output of parsing a single gitignore line. Contains the matcher-ready
    [selector] plus the [absolute_pattern] needed for downstream
    classification and [is_negated] flag. *)

val from_file :
  anchor:Glob.Pattern.t ->
  format:Gitignore.format ->
  source_kind:string ->
  Fpath.t ->
  parsed_pattern list

val from_string :
  anchor:Glob.Pattern.t ->
  name:string ->
  source_kind:string ->
  string ->
  parsed_pattern list

type parse_pattern_result = {
  compiled_pattern : Glob.Match.compiled_pattern;
  absolute_pattern : Glob.Pattern.t;
  is_affected_by_middle_slash_option : bool;
}

(* Lower-level function that can be used to create custom matchers that
   combine multiple patterns.

   middle_slash_anchors_left: true by default as per the Gitignore spec.

   right_anchored: true by default. If false, '/**' will be appended to
   the pattern to match subpaths.
*)
val parse_pattern :
  ?middle_slash_anchors_left:bool ->
  ?right_anchored:bool ->
  source:Glob.Match.loc ->
  left_anchor:Glob.Pattern.t ->
  string ->
  parse_pattern_result
