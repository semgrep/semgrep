open Printf

let ansi_highlight s =
  match s with
  | "" -> s
  | s -> ANSITerminal.(sprintf [ Bold; green ] "%s" s)

let make_separator_printer () =
  let is_first = ref true in
  fun () -> if !is_first then is_first := false else print_char '\n'

let print ?(highlight = false)
    ?(print_optional_separator = make_separator_printer ()) src
    (matches : Match.match_ list) =
  let highlight_fun = if highlight then Some ansi_highlight else None in
  let line_prefix =
    match Src_file.source src with
    | File path -> sprintf "%s:" path
    | Stdin
    | String
    | Channel ->
        ""
  in
  List.iter
    (fun (match_ : Match.match_) ->
      print_optional_separator ();
      let start_loc, end_loc = match_.region in
      if !Match.debug then
        printf "match from %s to %s\n" (Loc.show start_loc) (Loc.show end_loc);
      Src_file.lines_of_loc_range ?highlight:highlight_fun ~line_prefix src
        start_loc end_loc
      |> print_string)
    matches

let print_errors ?(highlight = false) errors =
  let error_prefix =
    if highlight then ANSITerminal.(sprintf [ Bold; red ] "%s" "error:")
    else "error:"
  in
  List.iter
    (fun (src, error) ->
      let src_prefix =
        match Src_file.source src with
        | File path -> sprintf "%s: " path
        | Stdin
        | String
        | Channel ->
            ""
      in
      eprintf "%s %s%s\n" error_prefix src_prefix error.Parse_pattern.msg)
    errors

let print_nested_results ?(with_time = false) ?highlight
    ?(print_optional_separator = make_separator_printer ()) doc_matches errors =
  let total_parse_time = ref 0. in
  let total_match_time = ref 0. in
  List.iter
    (fun (src, pat_matches, parse_time, _run_time) ->
      total_parse_time := !total_parse_time +. parse_time;
      List.iter
        (fun (_pat_id, matches, match_time) ->
          total_match_time := !total_match_time +. match_time;
          print ?highlight ~print_optional_separator src matches)
        pat_matches)
    doc_matches;
  if with_time then
    eprintf "parse time: %.6f s\nmatch time: %.6f s\n" !total_parse_time
      !total_match_time;
  print_errors ?highlight errors
