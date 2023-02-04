(*
   gitignore syntax, and syntax only.

   https://git-scm.com/docs/gitignore
*)

type loc = {
  (* File name or other source location name useful to a human reader
     in error messages. *)
  source_name : string;
  (* Line number, starting from 1. *)
  line_number : int;
  line_contents : string;
}

type selection_event = Selected of loc | Deselected of loc
type path_selector = { loc : loc; matcher : Fpath.t -> selection_event option }
type t = path_selector list

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

let remove_negator str =
  if String.length str >= 1 && str.[0] = '!' then Some (Str.string_after str 1)
  else None

let parse_pattern _str : Glob_matcher.compiled_pattern =
  (* use Glob_parser + Glob_matcher + some tweaks for gitignore *)
  failwith "todo: parse_pattern"

let parse_line source_name line_number line_contents =
  if is_ignored_line line_contents then None
  else
    let loc = { source_name; line_number; line_contents } in
    let is_negated, pattern_str =
      match remove_negator line_contents with
      | None -> (false, line_contents)
      | Some s -> (true, s)
    in
    let pattern = parse_pattern pattern_str in
    let matcher path =
      match Glob_matcher.run pattern path with
      | true ->
          if is_negated then Some (Deselected loc) else Some (Selected loc)
      | false -> None
    in
    Some { loc; matcher }

let from_string ?(name = "<string>") str =
  let lines = read_lines_from_string str in
  List.mapi
    (fun i contents ->
      let linenum = i + 1 in
      parse_line name linenum contents)
    lines
  |> List.filter_map (fun x -> x)

let from_file fname = Common.read_file fname |> from_string ~name:fname
