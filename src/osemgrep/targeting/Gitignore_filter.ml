(*
   Support for file tree filtering using the gitignore specification.
*)

let ( / ) = Fpath.( / )

module S = Gitignore_syntax

type level = {
  level_kind : string;
  source_name : string;
  patterns : S.path_selector list;
}

type t = level list

let is_selected (sel_events : S.selection_event list) =
  match sel_events with
  | [] -> false
  | Deselected _ :: _ -> false
  | Selected _ :: _ -> true

(*
   Filter a path, assuming all its parents were deselected
   (= not gitignored).
*)
let select_one acc levels path : S.selection_event list =
  List.fold_left
    (fun acc level ->
      List.fold_left
        (fun acc (path_selector : S.path_selector) ->
          match path_selector.matcher path with
          | Some ((Selected _ | Deselected _) as x) -> x :: acc
          | None -> acc)
        acc level.patterns)
    acc levels

(*
   Filter a path according to gitignore rules, requiring all the parent paths
   to be deselected (not gitignored).
*)
let select levels path =
  let rec loop sel_events path components =
    (* add a component to the path and check if it's gitignored *)
    match components with
    | [] -> sel_events
    | component :: components ->
        let path = path / component in
        let sel_events = select_one sel_events levels path in
        if is_selected sel_events then
          (* stop here, don't go deeper as per gitignore spec *)
          sel_events
        else loop sel_events path components
  in
  let root_path, components =
    match Fpath.segs path with
    | "" :: xs -> (Fpath.(v dir_sep), xs)
    | __else__ ->
        invalid_arg
          ("Gitignore_filter.select: not an absolute path: "
         ^ Fpath.to_string path)
  in
  let sel_events = loop [] root_path components in
  (is_selected sel_events, sel_events)
