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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* AST of a glob pattern.
 *
 * The main reference for this is:
 *  - https://git-scm.com/docs/gitignore
 *    which itself relies partially on POSIX glob
 *  - https://man7.org/linux/man-pages/man7/glob.7.html (man 7 glob)
 *
 * Examples:
 *  - *.c       # all local files with a '.c' extension
 *  - Thing.ml? # matches 'Thing.ml' as well as 'Thing.mli', 'Thing.mll', etc.
 *  - [a-z0-9]  # matches a single character in these ranges
 *  - /tmp/**   # all valid paths under '/tmp/'
 *
 * alternatives:
 *  -  Re.Glob from the ocaml-re library for parsing the patterns.
 *     But Re.Glob doesn't expose the AST of the glob pattern, and this prevents us
 *     from making transformations required by gitignore such as treating
 *     the pattern 'foo/bar' as equivalent to '/foo/bar' but not treat
 *     'foo' as '/foo'. However, we use ocaml-re in Match.ml
 *  - https://gitlab.com/gasche/path_glob ??
 *  - https://opam.ocaml.org/packages/dune-glob/ ??
 *  - https://github.com/bleepbloopsify/globlon ??
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* A pattern which matches paths. Segments are separated by '/'. *)
type t = segment list

(* A path segment is what represents a simple file name in a directory. *)
and segment =
  | Segment of segment_fragment list
  (* gitignore extension: '**' *)
  | Any_subpath

and segment_fragment =
  | Char of char
  | Char_class of char_class
  (* any character (not a modifier like in regexps) *)
  | Question
  (* any (possibly empty) sequence of characters, but not '/' *)
  | Star

and char_class = {
  (* range complement can be '^' or '!' *)
  complement : bool;
  ranges : char_class_range list;
}

and char_class_range = Class_char of char | Range of char * char
[@@deriving show]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* the pattern that matches '/' *)
let root_pattern = [ Segment []; Segment [] ]

(* remove the leading slash unless it's a trailing slash *)
let remove_leading_slash xs =
  match xs with
  | [ Segment []; Segment [] ] as xs -> xs
  | Segment [] :: xs -> xs
  | xs -> xs

(* remove the trailing slash unless it's a leading slash *)
let remove_trailing_slash xs =
  let rec loop xs =
    match xs with
    | [] -> []
    | [ Segment [] ] ->
        (* ignore trailing slash that's not a leading slash *) []
    | x :: xs -> x :: loop xs
  in
  match xs with
  (* preserve leading slash *)
  | Segment [] :: xs -> Segment [] :: loop xs
  | xs -> loop xs

(* Append the segments, taking care of trailing slashes that prevent
   us from using a plain list append (@). *)
let append (a : t) (b : t) : t =
  remove_trailing_slash a @ remove_leading_slash b

let of_path_segments (segments : string list) : t =
  List_.map
    (fun s ->
      let chars = String.fold_right (fun c acc -> Char c :: acc) s [] in
      Segment chars)
    segments
