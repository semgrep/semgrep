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

type path_selector = { loc : loc; matcher : Fpath.t -> bool }
type t = path_selector list

let read_lines_from_string =
  (*
     - eliminate trailing spaces
     - support Windows line endings regardless of current platform
  *)
  let sep = SPcre.regexp " *\r?\n" in
  fun str -> SPcre.split ~rex:sep str

(*
   How to interpret backslash escapes?

   The spec isn't clear about it. We assume the following, based on
   experimentation:
   - A backslash preceding any character causes the following character
     to be interpreted "literally". What this means depends on the context.
   - Backslashes are used in the glob syntax to protect special characters,
     so we don't eliminate them here.
   - end-of-line (lone) backslashes are ignored.
*)
let unescape_pattern str =
  let len = String.length str in
  let buf = Buffer.create 200 in
  let rec loop pos =
    if pos >= len then ()
    else if pos = len - 1 then Buffer.add_char buf str.[pos]
    else
      let next_pos =
        match str.[pos] with
        | '\\' ->
            Buffer.add_char buf str.[pos + 1];
            pos + 2
        | c ->
            Buffer.add_char buf c;
            pos + 1
      in
      loop next_pos
  in
  loop 0;
  Buffer.contents buf

let is_ignored_line =
  let rex = SPcre.regexp "^(?:[ \t]$|#.*)$" in
  fun str -> SPcre.pmatch_noerr ~rex str

let remove_negator str =
  if String.length str >= 1 && str.[0] = '!' then Some (Str.string_after str 1)
  else None

let parse_pattern _str =
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
    let pattern = unescape_pattern pattern_str |> parse_pattern in
    let matcher str =
      let b = Glob_matcher.run pattern str in
      if is_negated then not b else b
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
