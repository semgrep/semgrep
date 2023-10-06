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

type compiled_pattern = { source : loc; re : Re.re }

let string_loc ?(source_name = "<pattern>") ~source_kind pat =
  { source_name; source_kind; line_number = 1; line_contents = pat }

(*****************************************************************************)
(* Compilation of a Glob_pattern.t to Re.t *)
(*****************************************************************************)

let slash = Re.char '/'
let not_slash = Re.compl [ slash ]

let map_frag (frag : Pattern.segment_fragment) : Re.t =
  match frag with
  | Char c -> Re.char c
  | Char_class { complement; ranges } ->
      let cset =
        Common.map
          (fun range ->
            match range with
            | Class_char c -> Re.char c
            | Range (a, b) -> Re.rg a b)
          ranges
      in
      if complement then Re.compl cset else Re.alt cset
  | Question -> not_slash
  | Star -> Re.rep not_slash

let map_seg (seg : segment_fragment list) : Re.t =
  Re.seq (Common.map map_frag seg)

let rec map pat =
  match pat with
  | [ Segment seg ] -> [ map_seg seg; Re.eos ]
  | [ Any_subpath ] -> []
  | Segment seg :: pat -> map_seg seg :: slash :: map pat
  | Any_subpath :: pat -> Re.rep (Re.seq [ Re.rep not_slash; slash ]) :: map pat
  | [] -> [ Re.eos ]

(* Create a pattern that's left-anchored and right-anchored *)
let map_root pat = Re.seq (Re.bos :: map pat)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* Compile a pattern into an ocaml-re regexp for fast matching *)
let compile ~source pat =
  let re = map_root pat |> Re.compile in
  { source; re }
[@@profiling "Glob.Match.compile"]

(* This is used during unit testing. *)
let debug = ref false

let run matcher path =
  let res = Re.execp matcher.re path in
  if !debug then
    (* expensive string concatenation; may not be suitable for logger#debug *)
    Printf.printf "** pattern: %S  path: %S  matches: %B\n"
      matcher.source.line_contents path res;
  res
[@@profiling "Glob.Match.run"]

let source matcher = matcher.source

let show x =
  let re_info =
    Re.pp_re Format.str_formatter x.re;
    Format.flush_str_formatter ()
  in
  Printf.sprintf "pattern at %s:\n%s" (show_loc x.source) re_info
