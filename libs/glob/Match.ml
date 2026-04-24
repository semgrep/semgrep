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

type conf = { glob_period : bool; right_anchored : bool } [@@deriving show]

let default_conf = { glob_period = false; right_anchored = true }

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

(* Escape a character (char) so that it's interpreted literally
   by PCRE in extended mode. The extended mode recognizes shell-style
   comments (#) and ignores whitespace, so these must be escaped in addition
   to what ocaml-pcre2's quote function does.

   See https://github.com/tobil4sk/pcre2-ocaml/issues/1 for the possible
   addition of such a function to ocaml-pcre2.
*)
let quote_char buf c =
  match c with
  (* white space is: HT (9), LF (10), VT (11), FF (12), CR (13),
       and space (32) *)
  | '\009' .. '\013'
  | ' '
  | '#' ->
      addc buf '\\';
      addc buf c
  | _ -> add buf (Pcre2_.quote (String.make 1 c))

let translate_frag conf buf pos (frag : Pattern.segment_fragment) =
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
      if pos = 0 && not conf.glob_period then
        (* leading period must match literally *)
        add buf "[^/.]"
      else add buf "[^/]"
  | Star ->
      if pos = 0 && not conf.glob_period then
        (* leading period must match literally *)
        add buf "(?![.])";
      add buf "[^/]*"

let translate_seg conf buf (seg : segment_fragment list) =
  match seg with
  | [] -> (* leading slash (of an absolute path) *) ()
  | _nonempty_segment ->
      (* ensure we're not already in the middle of a segment
         = there is no character on the left that's not a slash *)
      add buf {|(?<![^/])|};
      (* lookahead assertion that checks that the path segment is not empty,
         because pattern 'a/*' should not match path 'a/' which has an
         empty trailing segment. *)
      add buf "(?=[^/])";
      List.iteri (translate_frag conf buf) seg

(* generic regexp language
   TODO: put this into a reusable regexp module
*)
let sequence xs = String.concat " " xs
let _choice xs = Printf.sprintf "(?:%s)" (String.concat " | " xs)
let repeat x = Printf.sprintf "(?:%s)*" x
let _repeat1 x = Printf.sprintf "(?:%s)*" x
let optional x = Printf.sprintf "(?:%s)?" x

(* beginning of string *)
let bos = {|\A|}

(* end of path (tolerates trailing slashes) *)
let end_of_path = {|/*\z|}
let segment_separator = {|/+|}

(* possibly empty segment *)
let segment = {|[^/]*|}

(* possibly empty segment not starting with a dot *)
let nodot_segment = {|(?:[^/.][^/]*)?|}

(* Match 0, 1, or more segments *)
let any_subpath ~segment =
  (* separator = one or more consecutive slashes *)
  let separator = {|/+|} in
  optional (sequence [ segment; repeat (sequence [ separator; segment ]) ])

(* The current position is not in the middle of a segment.
   = there's no previous character that's not a slash
     or there's no current character that's not slash *)
let segment_boundary = {|(?: (?<![^/]) | (?![^/]) )|}

(* end of path segment optionally followed by a subpath.
   There may or may not be a slash before this position.
   We need to ensure we're not in the middle of a segment. *)
let end_with_optional_subpath = segment_boundary

(*
   is_start and is_end indicate the position in the pattern for the purpose
   of inserting separators.
*)
let rec translate ?(is_start = false) conf buf pat =
  let is_end =
    match pat with
    | [] -> true
    | _ -> false
  in
  if (not is_start) && not is_end then
    (* must match a separator *)
    add buf segment_separator;
  match pat with
  | Segment seg :: pat ->
      translate_seg conf buf seg;
      translate conf buf pat
  | Any_subpath :: pat ->
      (* 0, 1, or more segments *)
      if conf.glob_period then add buf (any_subpath ~segment)
      else add buf (any_subpath ~segment:nodot_segment);
      (* The slash after a '**' doesn't have to match a slash in the path.
         is_start:true ensures this. *)
      translate ~is_start:true conf buf pat
  | [] ->
      if conf.right_anchored then
        (* anchored right end *)
        add buf end_of_path
      else
        (* end path or continue with /... *)
        add buf end_with_optional_subpath

(* Create a pattern that's left-anchored and right-anchored *)
let translate_root conf pat =
  let buf = Buffer.create 128 in
  add buf bos;
  translate ~is_start:true conf buf pat;
  (* Uncomment to print a readable version of the glob AST.
     It's too much to log in debug level even in tests. *)
  (*
  Logs.info (fun m ->
    m "translate %s" (Pattern.show pat)
  );
  *)
  Buffer.contents buf

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* Compile a pattern into an ocaml-re regexp for fast matching *)
let compile ?(conf = default_conf) ~source pat =
  let pcre = translate_root conf pat in
  (* EXTENDED: needed to ignore the whitespace we put into the PCRE pattern
     for readability *)
  let re = Pcre2_.regexp ~flags:[ `EXTENDED ] pcre in
  { source; re }

let run matcher path =
  let res = Pcre2_.pmatch_noerr ~rex:matcher.re path in
  (* Uncomment to print something in tests when debugging.
     Why not have a permanent log instruction:
     1. This logs a lot for any basic semgrep operation so it's useless
        outside of unit tests.
     2. It's still too much for some tests e.g. in Unit_gitignore.
     3. The Logs_ module is broken at the moment
        (tags are required for Log.debug to do anything).
   *)
  (*
  Logs.info (fun m ->
     m "glob: %S  pcre: %s  path: %S  matches: %B"
       matcher.source.line_contents matcher.re.pattern path res);
  *)
  res

let source matcher = matcher.source

let show_compiled_pattern x =
  Printf.sprintf "pattern at %s:\n%s" (show_loc x.source) x.re.pattern

let pp_compiled_pattern fmt x =
  Format.pp_print_string fmt (show_compiled_pattern x)
