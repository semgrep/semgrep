(* Martin Jambon
 *
 * Copyright (C) 2023 Semgrep
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Pattern
module Log = Log_glob.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Matching of a glob pattern against a path.
   This is purely syntactic: the file system is not accessed.

   We could use Re.Glob from the ocaml-re library for parsing the patterns
   but it doesn't expose the AST of the glob pattern, and this prevents us
   from making transformations required by gitignore such as treating
   the pattern 'foo/bar' as equivalent to '/foo/bar' but not treat
   'foo' as '/foo'. However, we use ocaml-re to produce the regexp tree
   and then execute it to match a path given as a string.
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type loc = {
  source_name : string;
  source_kind : string option;
  line_number : int;
  line_contents : string;
}

let show_loc x =
  Printf.sprintf "%s, line %i: %s" x.source_name x.line_number x.line_contents

let pp_loc fmt x = Format.pp_print_string fmt (show_loc x)

type compiled_pattern = { source : loc; re : Pcre2_.t }

let string_loc ?(source_name = "<pattern>") ~source_kind pat =
  { source_name; source_kind; line_number = 1; line_contents = pat }

(*****************************************************************************)
(* Compilation of a Glob_pattern.t to a PCRE2 pattern *)
(*****************************************************************************)
(*
   We used to use ocaml-re ('Re' module) to build directly a tree but
   unfortunately, it doesn't support lookhead assertions that we would
   need to match a glob pattern correctly. The issue is that the pattern
   'a/*b' matches 'a/b' but the pattern 'a/*' doesn't match 'a/'.
*)

let add = Buffer.add_string
let addc = Buffer.add_char
let quote_char buf c = add buf (Pcre2_.quote (String.make 1 c))

let translate_frag buf pos (frag : Pattern.segment_fragment) =
  match frag with
  | Char c -> quote_char buf c
  | Char_class { complement; ranges } ->
      if complement then add buf "[^" else addc buf '[';
      ranges
      |> List.iter (fun range ->
             match range with
             | Class_char c -> quote_char buf c
             | Range (a, b) ->
                 quote_char buf a;
                 addc buf '-';
                 quote_char buf b);
      addc buf ']'
  | Question ->
      if pos = 0 then (* leading dot must match literally *)
        add buf "[^/.]"
      else add buf "[^/]"
  | Star ->
      if pos = 0 then (* leading dot must match literally *)
        add buf "(?![.])";
      add buf "[^/]*"

let translate_seg buf (seg : segment_fragment list) =
  match seg with
  | [] -> ()
  | _nonempty_segment ->
      (* lookahead assertion that checks that the path segment is not empty,
         because pattern 'a/*' should not match path 'a/' which has an
         empty trailing segment. *)
      add buf "(?=[^/])";
      List.iteri (translate_frag buf) seg

(* beginning of string *)
let bos = {|\A|}

(* end of string *)
let eos = {|\z|}

let rec translate buf pat =
  match pat with
  | [ Segment seg ] ->
      translate_seg buf seg;
      add buf eos
  | [ Any_subpath ] -> ()
  | Segment seg :: pat ->
      translate_seg buf seg;
      add buf "/+";
      translate buf pat
  | Any_subpath :: pat ->
      add buf "/*(?:[^/]+/+)*";
      translate buf pat
  | [] -> add buf eos

(* Create a pattern that's left-anchored and right-anchored *)
let translate_root pat =
  let buf = Buffer.create 128 in
  add buf bos;
  translate buf pat;
  Buffer.contents buf

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* Compile a pattern into an ocaml-re regexp for fast matching *)
let compile ~source pat =
  let pcre = translate_root pat in
  let re = Pcre2_.regexp pcre in
  { source; re }
[@@profiling "Glob.Match.compile"]

let run matcher path =
  let res = Pcre2_.pmatch_noerr ~rex:matcher.re path in
  (* perf: this gets called a lot. The match-with is expected to make things
     faster by creating a closure for the anonymous function only in debug
     mode. *)
  (* nosemgrep: no-logs-in-library *)
  (match Logs.level () with
  | Some Debug ->
      Log.debug (fun m ->
          m "glob: %S  pcre: %s  path: %S  matches: %B"
            matcher.source.line_contents matcher.re.pattern path res)
  | _ -> ());
  res
[@@profiling "Glob.Match.run"]

let source matcher = matcher.source

let show_compiled_pattern x =
  Printf.sprintf "pattern at %s:\n%s" (show_loc x.source) x.re.pattern

let pp_compiled_pattern fmt x =
  Format.pp_print_string fmt (show_compiled_pattern x)
