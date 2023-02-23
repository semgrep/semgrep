(*
   Support for file tree filtering using the gitignore specification.
*)

module S = Gitignore_syntax
open Git_path.Ops

type t = {
  project_root : Fpath.t;
  higher_priority_levels : Gitignore_level.t list;
  gitignore_file_cache : Gitignore_files.t;
  lower_priority_levels : Gitignore_level.t list;
}

type status = Not_ignored | Ignored

let create ?gitignore_filenames ?(higher_priority_levels = [])
    ?(lower_priority_levels = []) ~project_root () =
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

let result_of_selection_events sel_events =
  let status = if is_selected sel_events then Ignored else Not_ignored in
  (status, sel_events)

(*
   Filter a path, assuming all its parents were deselected
   (= not gitignored).
*)
let select_one acc levels path : S.selection_event list =
  List.fold_left
    (fun acc (level : Gitignore_level.t) ->
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

   For a given path, we check the acceptability of all its ancestor folders.
   Each time we descend into a folder, we read the .gitignore files in
   that folder which add filters to the existing filters found earlier.
*)
let select t (full_git_path : Git_path.t) =
  (* Middle-priority levels: in-project .gitignore files *)
  let rec loop sel_events levels parent_path components =
    (* add a component to the path and check if it's gitignored *)
    match components with
    | [] -> sel_events
    | component :: components -> (
        (* load local gitignore file *)
        let additional_level =
          Gitignore_files.load t.gitignore_file_cache parent_path
        in
        let levels = levels @ [ additional_level ] in
        (* check whether partial path should be gitignored *)
        let file_path = parent_path / component in
        let sel_events = select_one sel_events levels file_path in
        if is_selected sel_events then
          (* stop here, don't go deeper as per gitignore spec *)
          sel_events
        else
          match components with
          | []
          | [ "" ] ->
              loop sel_events levels file_path components
          | _ :: _ ->
              (* add trailing slash to match directory-only patterns *)
              let dir_path = file_path / "" in
              let sel_events = select_one sel_events levels dir_path in
              if is_selected sel_events then sel_events
              else loop sel_events levels file_path components)
  in
  let components =
    match full_git_path.components with
    | "" :: xs -> xs
    | __else_ ->
        invalid_arg
          ("Gitignore_filter.select: not an absolute path: "
         ^ full_git_path.string)
  in
  let sel_events = [] in
  let sel_events =
    select_one sel_events t.higher_priority_levels full_git_path
  in
  if is_selected sel_events then result_of_selection_events sel_events
  else
    let sel_events = loop [] [] Git_path.root components in
    if is_selected sel_events then result_of_selection_events sel_events
    else
      let sel_events =
        select_one sel_events t.lower_priority_levels full_git_path
      in
      result_of_selection_events sel_events
