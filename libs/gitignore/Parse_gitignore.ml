open Gitignore
module M = Glob.Match

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let read_lines_from_string =
  (*
     - eliminate trailing spaces
     - support Windows line endings regardless of current platform
  *)
  let sep = Pcre2_.regexp " *\r?\n" in
  fun str ->
    match Pcre2_.split ~rex:sep str with
    | Ok res -> res
    | Error err ->
        (* not sure why it would happen so we let it fail *)
        raise (Pcre2.Error err)

let is_ignored_line =
  let rex = Pcre2_.regexp "^(?:[ \t]$|#.*)$" in
  fun str -> Pcre2_.pmatch_noerr ~rex str

(* semgrep-legacy (deprecated)

   Try to parse a line of input as a ':include' instruction
*)
let parse_maybe_include_line =
  let rex = Pcre2_.regexp {|^[ \t]*:include[ \t]*([^ \t]*)[ \t]*$|} in
  let parse ~orig_semgrepignore_path line : Fpath.t option =
    match Pcre2_.exec ~rex line with
    | Ok (Some res) -> (
        match Pcre2_.get_substring rex res 1 with
        | Ok (Some path) -> (
            match Fpath.of_string path with
            | Ok path ->
                (* nosemgrep: no-logs-in-library *)
                Logs.warn (fun m ->
                    m
                      "Deprecated include directive '%s' in semgrepignore file \
                       '%s'"
                      line
                      (Fpath.to_string orig_semgrepignore_path));
                Some path
            | Error _ -> None)
        | Ok None
        | Error _ ->
            None)
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
   being relative to any folder in the subtree. *)
let is_anchored_pattern (pat : Glob.Pattern.t) =
  match pat with
  (* /... *)
  | Segment [] :: _ -> true
  (* **/ *)
  | Any_subpath :: _ -> true
  | pat -> contains_nontrailing_slash pat

(*
   Parse and compile a gitignore pattern.

   The resulting matcher matches a git path, i.e. a file path relative
   to the git project root.

   anchor: path of the gitignore file's directory relative to the git project
   root. For example, if the gitignore path is '/foo/.gitignore',
   then the pattern '/bar' will be expanded into '/foo/bar'.
   However a non-anchored pattern such as '*.c' will be expanded into
   '/foo/**/*.c'.
*)
let parse_pattern ~source ~anchor str : M.compiled_pattern =
  let pat = Glob.Parse.parse_string str in
  let absolute_pattern =
    if is_anchored_pattern pat then Glob.Pattern.append anchor pat
    else Glob.Pattern.append anchor (Any_subpath :: pat)
  in
  M.compile ~source absolute_pattern

let parse_line ~anchor source_name source_kind line_number line_contents =
  if is_ignored_line line_contents then None
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
    let pattern = parse_pattern ~source:loc ~anchor pattern_str in
    let matcher (ppath : Ppath.t) =
      match M.run pattern (Ppath.to_string_fast ppath) with
      | true ->
          if is_negated then Some (Deselected loc) else Some (Selected loc)
      | false -> None
    in
    Some { loc; matcher }

(* semgrep-legacy *)
let get_include_path ~orig_semgrepignore_path relative_include_path =
  let base_dir = Fpath.parent orig_semgrepignore_path in
  (* Preserve the original path components as much as possible to avoid
     possible confusion later *)
  Fpath.(base_dir // relative_include_path)

(*
   semgrep-legacy

   Expand lines like ':include foo/bar' into their contents.

   This is an legacy feature from semgrep that is now deprecated.

   It will not expand includes recursively to avoid cycles and other
   complications.
*)
let rec expand_includes ~orig_semgrepignore_path lines =
  let expand_line line =
    match parse_maybe_include_line ~orig_semgrepignore_path line with
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
  List_.mapi
    (fun i contents ->
      let linenum = i + 1 in
      parse_line ~anchor name source_kind linenum contents)
    lines
  |> List_.filter_map (fun x -> x)

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
