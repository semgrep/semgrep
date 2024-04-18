(*
   Similar to Gitignore_filter but select paths to be kept rather than ignored.
*)

open Ppath.Operators
module Log = Log_targeting.Log

type t = {
  project_root : Fpath.t;
  glob_matchers : Glob.Match.compiled_pattern list;
  no_match_loc : Glob.Match.loc;
}
[@@deriving show]

let check_nonnegated_pattern str =
  match Gitignore.remove_negator str with
  | None -> ()
  | Some _ -> failwith ("--include patterns cannot be negated: " ^ str)

let create ~project_root patterns =
  List.iter check_nonnegated_pattern patterns;
  let glob_matchers =
    List_.map
      (fun pat ->
        Parse_gitignore.parse_pattern
          ~source:
            (Glob.Match.string_loc ~source_name:"include pattern"
               ~source_kind:(Some "include") pat)
          ~anchor:Glob.Pattern.root_pattern pat)
      patterns
  in
  let no_match_loc =
    Glob.Match.string_loc ~source_name:"include patterns"
      ~source_kind:(Some "include")
      (Printf.sprintf "NOT (%s)" (String.concat " OR " patterns))
  in
  { project_root; glob_matchers; no_match_loc }

(* map + find_opt, stopping as early as possible *)
let rec find_first func xs =
  match xs with
  | [] -> None
  | x :: xs -> (
      match func x with
      | None -> find_first func xs
      | Some _ as res -> res)

(*
   Command line options look like this:

     --include <PATTERN1> --include PATTERN2 SCANNING_ROOT

   This results in (Some [PATTERN1; PATTERN2]). It means that any path
   or subpath in SCANNING_ROOT or one of its children matching PATTERN1
   or PATTERN2 will be selected as valid scanning roots.
   Other paths will be filtered out.

   Example:

    file tree:

    a
    └── b
       ├── c
       └── d

    command: --include b/c a

    paths and subpaths to consider for selection:
    - /a
    - /a/b
    - /a/b/c
    - /a/b/d
    - a
    - a/b
    - a/b/c
    - a/b/d
    - b
    - b/c
    - b/d
    - c
    - d

    selected paths:
    - /a/b/c (via the subpath b/c matching the pattern b/c)

   The 'select' function below receives the path to a file within a project
   rather than a whole file tree. The selection is performed by breaking
   down the path into subpaths and matching them against the patterns.

   The path is selected if any of its subpaths matches any of the include
   patterns.
*)
let select t (full_git_path : Ppath.t) =
  Log.debug (fun m ->
      m "Include_filter.select %s ppath:%s" (show t)
        (Ppath.to_string_for_tests full_git_path));
  let rec scan_segments matcher parent_path segments =
    (* add a segment to the path and check if it's selected *)
    match segments with
    | [] -> None
    | segment :: segments -> (
        (* check whether partial path should be gitignored *)
        let file_path = parent_path / segment in
        if Glob.Match.run matcher (Ppath.to_string_fast file_path) then
          Some (Glob.Match.source matcher)
        else
          match segments with
          | []
          | [ "" ] ->
              None
          | _ :: _ ->
              (* add trailing slash to match directory-only patterns *)
              let dir_path = file_path / "" in
              if Glob.Match.run matcher (Ppath.to_string_fast dir_path) then
                Some (Glob.Match.source matcher)
              else scan_segments matcher file_path segments)
  in
  let rel_segments =
    match Ppath.segments full_git_path with
    | "" :: xs -> xs
    | _ -> assert false
  in
  match
    t.glob_matchers
    |> find_first (fun matcher -> scan_segments matcher Ppath.root rel_segments)
  with
  | None -> (Gitignore.Ignored, [ Gitignore.Selected t.no_match_loc ])
  | Some loc ->
      (* !! Deselected for gitignore = not ignored !! *)
      (Gitignore.Not_ignored, [ Gitignore.Deselected loc ])
