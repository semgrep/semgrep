(*
   Support for file tree filtering using the gitignore specification.
*)

module S = Gitignore_syntax

type level = {
  level_kind : string;
  source_name : string;
  patterns : S.path_selector list;
}

type t = {
  project_root: Fpath.t;
  higher_priority_levels: level list;
  gitignore_file_cache: Gitignore_files.t;
  lower_priority_levels: level list;
}

let create
    ?gitignore_filenames
    ?(higher_priority_levels = [])
    ?(lower_priority_levels = [])
    ~project_root
    () =
  {
    project_root;
    higher_priority_levels;
    gitignore_file_cache =
      Gitignore_files.create ?gitignore_filenames ~project_root ();
    lower_priority_levels;
  }

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

let ( / ) a b = a ^ "/" ^ b

(*
   Filter a path according to gitignore rules, requiring all the parent paths
   to be deselected (not gitignored).
*)
let select t ~git_path =
  let rec loop sel_events parent_path components =
    (* add a component to the path and check if it's gitignored *)
    match components with
    | [] -> sel_events
    | component :: components ->
        let additional_level = Gitignore_files.load parent_path in
        let path = parent_path / component in
        let sel_events = select_one sel_events levels path in
        if is_selected sel_events then
          (* stop here, don't go deeper as per gitignore spec *)
          sel_events
        else loop sel_events path components
  in
  let components =
    match String.split_on_char '/' git_path with
    | "" :: xs -> xs
    | __else_ ->
        invalid_arg
          ("Gitignore_filter.select: not an absolute path: "
           ^ git_path)
  in
  let sel_events = loop [] "/" components in
  (is_selected sel_events, sel_events)
