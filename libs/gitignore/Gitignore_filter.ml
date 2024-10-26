(*
   Support for file tree filtering using the gitignore specification.
*)
open Gitignore
open Ppath.Operators

let create ?gitignore_filenames ?(higher_priority_levels = [])
    ?(lower_priority_levels = []) ~project_root () =
  {
    project_root;
    higher_priority_levels;
    gitignore_file_cache =
      Gitignore_cache.create ?gitignore_filenames ~project_root ();
    lower_priority_levels;
  }

let is_selected (sel_events : Gitignore.selection_event list) =
  match sel_events with
  | [] -> false
  | Deselected _ :: _ -> false
  | Selected _ :: _ -> true

let result_of_selection_events sel_events =
  let status = if is_selected sel_events then Ignored else Not_ignored in
  (status, sel_events)

(*
   Scan successive precedence levels, stopping as soon as one level
   decides to gitignore (= select) the path.
*)
let rec fold_levels func sel_events levels =
  match levels with
  | [] -> sel_events
  | level :: levels ->
      let sel_events = func sel_events level in
      if is_selected sel_events then
        (* early exit, unlike List.fold_left *)
        sel_events
      else fold_levels func sel_events levels

(*
   Filter a path, assuming all its parents were deselected
   (= not gitignored).
*)
let select_one acc levels path : Gitignore.selection_event list =
  fold_levels
    (fun acc (level : Gitignore.level) ->
      List.fold_left
        (fun acc (path_selector : Gitignore.path_selector) ->
          match path_selector.matcher path with
          | Some ((Selected _ | Deselected _) as x) -> x :: acc
          | None -> acc)
        acc level.patterns)
    acc levels
[@@profiling]

let select_path opt_gitignore_file_cache sel_events levels relative_segments =
  let rec loop sel_events levels parent_path segments =
    (* add a segment to the path and check if it's gitignored *)
    match segments with
    | [] -> sel_events
    | segment :: segments -> (
        let levels =
          match opt_gitignore_file_cache with
          | Some cache -> (
              (* load local gitignore file *)
              match Gitignore_cache.load cache parent_path with
              | Some additional_level -> levels @ [ additional_level ]
              | None -> levels)
          | None -> levels
        in
        (* check whether partial path should be gitignored *)
        let file_path = parent_path / segment in
        let sel_events = select_one sel_events levels file_path in
        let deselected =
          match sel_events with
          | Deselected _ :: _ -> true
          | _ -> false
        in
        if is_selected sel_events then
          (* stop here, don't go deeper as per gitignore spec *)
          sel_events
        else
          match segments with
          | []
          | [ "" ] ->
              loop sel_events levels file_path segments
          (* If a path has been deselected, don't test for dir-only patterns *)
          | _ :: _ when deselected -> loop sel_events levels file_path segments
          | _ :: _ ->
              (* add trailing slash to match directory-only patterns *)
              let dir_path = file_path / "" in
              let sel_events = select_one sel_events levels dir_path in
              if is_selected sel_events then sel_events
              else loop sel_events levels file_path segments)
  in
  loop sel_events levels Ppath.root relative_segments

(*
   Filter a path according to gitignore rules, requiring all the parent paths
   to be deselected (not gitignored).

   For a given path, we check the acceptability of all its ancestor folders.
   Each time we descend into a folder, we read the .gitignore files in
   that folder which add filters to the existing filters found earlier.
*)
let select t (full_git_path : Ppath.t) =
  let sel_events = [] in
  let rel_segments = Ppath.relative_segments full_git_path in
  (* higher levels (command-line)
     and middle levels (gitignore files discovered along the way) *)
  let sel_events =
    select_path (Some t.gitignore_file_cache) sel_events
      t.higher_priority_levels rel_segments
  in
  if is_selected sel_events then result_of_selection_events sel_events
  else
    (* lower levels (other sources of gitignore patterns) *)
    let sel_events =
      select_path None sel_events t.lower_priority_levels rel_segments
    in
    result_of_selection_events sel_events
[@@profiling]
