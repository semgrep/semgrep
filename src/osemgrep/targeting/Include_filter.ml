(*
   Similar to Gitignore_filter but select paths to be kept rather than ignored.
*)

open Git_path.Ops

type t = {
  project_root : Fpath.t;
  glob_matchers : Glob_matcher.t list;
  no_match_loc : Glob_matcher.loc;
}

let check_nonnegated_pattern str =
  match Gitignore_syntax.remove_negator str with
  | None -> ()
  | Some _ -> failwith ("--include patterns cannot be negated: " ^ str)

let create ~project_root patterns =
  List.iter check_nonnegated_pattern patterns;
  let glob_matchers =
    Common.map
      (fun pat ->
        Gitignore_syntax.parse_pattern
          ~source:(Glob_matcher.string_loc ~source_name:"include pattern" pat)
          ~anchor:Glob_matcher.root_pattern pat)
      patterns
  in
  let no_match_loc =
    Glob_matcher.string_loc ~source_name:"include patterns"
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
   Each pattern is matched not just against the given path but also against
   its parents:

     Path    /src/a.c
     Pattern /src/      --> will be tested against /src, /src/, and /src/a.c

   If any of the patterns matches on any variant of the path, the
   file is selected.
*)
let select t (full_git_path : Git_path.t) =
  let rec scan_components matcher parent_path components =
    (* add a component to the path and check if it's selected *)
    match components with
    | [] -> None
    | component :: components -> (
        (* check whether partial path should be gitignored *)
        let file_path = parent_path / component in
        if Glob_matcher.run matcher (Git_path.to_string file_path) then
          Some (Glob_matcher.source matcher)
        else
          match components with
          | []
          | [ "" ] ->
              None
          | _ :: _ ->
              (* add trailing slash to match directory-only patterns *)
              let dir_path = file_path / "" in
              if Glob_matcher.run matcher (Git_path.to_string dir_path) then
                Some (Glob_matcher.source matcher)
              else scan_components matcher file_path components)
  in
  let rel_components =
    match full_git_path.components with
    | "" :: xs -> xs
    | __else__ -> assert false
  in
  match
    t.glob_matchers
    |> find_first (fun matcher ->
           scan_components matcher Git_path.root rel_components)
  with
  | None ->
      (Gitignore_filter.Ignored, [ Gitignore_syntax.Selected t.no_match_loc ])
  | Some loc ->
      (* !! Deselected for gitignore = not ignored !! *)
      (Gitignore_filter.Not_ignored, [ Gitignore_syntax.Deselected loc ])
