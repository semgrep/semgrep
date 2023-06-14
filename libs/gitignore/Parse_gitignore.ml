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
  let sep = SPcre.regexp " *\r?\n" in
  fun str ->
    match SPcre.split ~rex:sep str with
    | Ok res -> res
    | Error err ->
        (* not sure why it would happen so we let it fail *)
        raise (Pcre.Error err)

let is_ignored_line =
  let rex = SPcre.regexp "^(?:[ \t]$|#.*)$" in
  fun str -> SPcre.pmatch_noerr ~rex str

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
      match M.run pattern (Ppath.to_string ppath) with
      | true ->
          if is_negated then Some (Deselected loc) else Some (Selected loc)
      | false -> None
    in
    Some { loc; matcher }

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let from_string ~anchor ~name ~kind str =
  let lines = read_lines_from_string str in
  Common.mapi
    (fun i contents ->
      let linenum = i + 1 in
      parse_line ~anchor name kind linenum contents)
    lines
  |> Common.map_filter (fun x -> x)

let from_file ~anchor ~kind path =
  Fpath.to_string path |> Common.read_file
  |> from_string ~anchor ~name:(Fpath.to_string path) ~kind
  [@@profiling]
