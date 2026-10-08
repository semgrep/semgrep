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
open Gitignore
module M = Glob.Match

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*
   - eliminate trailing spaces
   - support Windows line endings regardless of current platform

   Gitignore content is byte-oriented (paths need not be valid UTF-8), so we
   split by hand rather than with a (UTF-compiled) regex, which fails on such
   input. This is equivalent to splitting on the regex [ *\r?\n]: each
   newline-terminated line is stripped of one trailing '\r' and then of
   trailing spaces, while any final newline-less fragment is kept verbatim.
*)
let read_lines_from_string str =
  let strip_line_end line =
    String_.trim_cr line |> Base.String.rstrip ~drop:(Char.equal ' ')
  in
  let lines = String.split_on_char '\n' str in
  let last_index = List.length lines - 1 in
  List.mapi
    (fun index line ->
      (* The final field was not terminated by a newline, so keep it verbatim. *)
      if index = last_index then line else strip_line_end line)
    lines

let is_ignored_line = function
  | " "
  | "\t" ->
      true
  | str -> String.starts_with ~prefix:"#" str

(* semgrep-legacy (deprecated)

   Try to parse a line of input as a ':include' instruction
*)
let parse_maybe_include_line =
  let rex = Pcre2_.compile_exn {|^[ \t]*:include[ \t]*([^ \t]*)[ \t]*$|} in
  let parse line : Fpath.t option =
    match Pcre2_.captures rex line with
    | Ok (Some res) ->
        Option.bind (Pcre2_.substring_of_captures res 1) (fun path ->
            Fpath.of_string path |> Result.to_option)
    | Ok None
    | Error _ ->
        None
  in
  parse

let rec contains_nontrailing_slash (pat : Glob.Pattern.t) =
  match pat with
  | Segment [] :: pat -> contains_nontrailing_slash pat
  | [] -> false
  | _nonempty :: (* trailing slash *) [ Segment [] ]
  | [ _nonempty ] ->
      false
  | _nonempty1 :: _nonempty2 :: _ -> true

(* anchored pattern = relative to the work directory only, as opposed to
   being relative to any folder in the subtree.

   middle_slash_anchors_left = Gitignore's standard behavior that makes 'a/b'
   left-anchored just like '/a/b' but not 'a/'.
*)
let is_anchored_pattern ~middle_slash_anchors_left (pat : Glob.Pattern.t) =
  match pat with
  (* /... *)
  | Segment [] :: _ -> true
  (* **/ *)
  | Any_subpath :: _ -> true
  | pat ->
      if middle_slash_anchors_left then contains_nontrailing_slash pat
      else false

let gitignore_glob_conf : M.conf =
  {
    (* Gitignore allows '*' to match dot files unlike e.g. Bash *)
    glob_period = true;
    (* We only match full paths (some of which may be directory paths) *)
    right_anchored = true;
  }

type parsed_pattern = {
  selector : Gitignore.path_selector;
  absolute_pattern : Glob.Pattern.t;
  is_negated : bool;
}

type parse_pattern_result = {
  compiled_pattern : M.compiled_pattern;
  absolute_pattern : Glob.Pattern.t;
  is_affected_by_middle_slash_option : bool;
}

(*
   Parse and compile a gitignore pattern.

   The resulting matcher matches a git path, i.e. a file path relative
   to the git project root.

   left anchor: path of the gitignore file's directory relative to the git
   project root. For example, if the gitignore path is '/foo/.gitignore',
   then the pattern '/bar' will be expanded into '/foo/bar'.
   However a non-anchored pattern such as '*.c' will be expanded into
   '/foo/**/*.c'.
*)
let parse_pattern ?(middle_slash_anchors_left = true) ?(right_anchored = true)
    ~source ~left_anchor str : parse_pattern_result =
  let pat = Glob.Parse.parse_string ~deprecated_absolute_dotslash:true str in
  let is_anchored = is_anchored_pattern ~middle_slash_anchors_left pat in
  let is_anchored_alt =
    is_anchored_pattern
      ~middle_slash_anchors_left:(not middle_slash_anchors_left)
      pat
  in
  let is_affected_by_middle_slash_option = is_anchored <> is_anchored_alt in
  let absolute_pattern =
    if is_anchored then
      (* /foo -> /ppath/to/subdir/foo *)
      Glob.Pattern.append left_anchor pat
    else
      (* foo -> /ppath/to/subdir/**/foo *)
      Glob.Pattern.append left_anchor (Any_subpath :: pat)
  in
  let conf = { gitignore_glob_conf with right_anchored } in
  {
    compiled_pattern = M.compile ~conf ~source absolute_pattern;
    absolute_pattern;
    is_affected_by_middle_slash_option;
  }

let parse_line ~anchor source_name source_kind line_number line_contents =
  if line_contents = "" || is_ignored_line line_contents then None
  else
    let loc : M.loc =
      {
        source_name;
        source_kind = Some source_kind;
        line_number;
        line_contents;
      }
    in
    let is_negated, pattern_str =
      match remove_negator line_contents with
      | None -> (false, line_contents)
      | Some s -> (true, s)
    in
    let { compiled_pattern; absolute_pattern; _ } =
      parse_pattern ~source:loc ~left_anchor:anchor pattern_str
    in
    let matcher (ppath : Ppath.t) =
      match M.run compiled_pattern (Ppath.to_string_fast ppath) with
      | true ->
          if is_negated then Some (Deselected loc) else Some (Selected loc)
      | false -> None
    in
    Some { selector = { loc; matcher }; absolute_pattern; is_negated }

(* semgrep-legacy *)
let get_include_path ~orig_semgrepignore_path relative_include_path =
  let base_dir = Fpath.parent orig_semgrepignore_path in
  (* Preserve the original path components as much as possible to avoid
     possible confusion later *)
  Fpath.(base_dir // relative_include_path)

(*
   Expand lines like ':include foo/bar' into their contents.
   The included file 'foo/bar' must be in pure Gitignore syntax and
   may not contain ':include' directives.
*)
let rec expand_includes ~orig_semgrepignore_path lines =
  let expand_line line =
    match parse_maybe_include_line line with
    | Some relative_include_path ->
        let include_path =
          get_include_path ~orig_semgrepignore_path relative_include_path
        in
        if UFile.is_reg ~follow_symlinks:true include_path then
          include_path |> UFile.read_file |> read_lines_from_string
        else
          (* ignore silently
             (why: git also ignores .gitignore files that are broken
             symlinks) *)
          []
    | None -> [ line ]
  in
  List.concat_map expand_line lines

and from_lines ~allow_include ~anchor ~name ~source_kind ~source_path lines =
  let lines =
    (* Don't allow ':include' when reading exclusion patterns from the
       command line (or not from a file in general) *)
    match source_path with
    | Some orig_semgrepignore_path when allow_include ->
        if allow_include then expand_includes ~orig_semgrepignore_path lines
        else lines
    | Some _
    | None ->
        lines
  in
  List.mapi
    (fun i contents ->
      let linenum = i + 1 in
      parse_line ~anchor name source_kind linenum contents)
    lines
  |> List.filter_map Fun.id

and from_string_gen ~allow_include ~anchor ~name ~source_path ~source_kind str =
  let lines = read_lines_from_string str in
  from_lines ~allow_include ~anchor ~name ~source_path ~source_kind lines

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

and from_string ~anchor ~name ~source_kind str =
  from_string_gen ~allow_include:false ~anchor ~name ~source_path:None
    ~source_kind str

and from_file ~anchor ~format ~source_kind path =
  path |> UFile.read_file
  |> from_string_gen
       ~allow_include:(format = Legacy_semgrepignore)
       ~anchor ~name:(Fpath.to_string path) ~source_path:(Some path)
       ~source_kind
[@@profiling]
