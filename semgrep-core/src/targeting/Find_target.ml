(*
   Find and filter targets.

   TODO: note that some of the functions below are also present in
   semgrep-python so we might want to move all file-targeting in one
   place.
*)
module In = Input_to_core_t
module Resp = Output_from_core_t

type baseline_handler = TODO
type file_ignore = TODO
type path = string

(*
   Some rules will use 'include' (required_path_patterns) and 'exclude'
   (excluded_path_patterns) to select targets that don't have an extension
   such as 'Dockerfile'. We expect most rules written for a language
   to use the same combination of include/exclude. This allows caching
   across the many rules that target the same language.
*)
type target_cache_key = {
  path : path;
  language : Xlang.t;
  required_path_patterns : string list;
  excluded_path_patterns : string list;
}

type target_cache = (target_cache_key, bool) Hashtbl.t

let deduplicate_list l =
  let tbl = Hashtbl.create 1000 in
  List.filter
    (fun x ->
      if Hashtbl.mem tbl x then false
      else (
        Hashtbl.add tbl x ();
        true))
    l

let select_global_targets ?(includes = []) ?(excludes = []) ~max_target_bytes
    ~respect_git_ignore ?(baseline_handler : baseline_handler option)
    ?(file_ignore : file_ignore option) paths =
  let all_files =
    Common.map (List_files.list_regular_files ~keep_root:true) paths
    |> List.flatten |> deduplicate_list
  in
  ignore includes;
  ignore excludes;
  ignore max_target_bytes (* from the semgrep CLI, not semgrep-core *);
  ignore respect_git_ignore;
  ignore baseline_handler;
  ignore file_ignore;
  all_files

let create_cache () = Hashtbl.create 1000

let match_glob_pattern ~pat path =
  (* TODO *)
  ignore pat;
  ignore path;
  true

let match_a_required_path_pattern required_path_patterns path =
  match required_path_patterns with
  | [] -> (* <grimacing face emoji> *) true
  | pats -> List.exists (fun pat -> match_glob_pattern ~pat path) pats

let match_all_excluded_path_patterns excluded_path_patterns path =
  List.for_all (fun pat -> match_glob_pattern ~pat path) excluded_path_patterns

let filter_target_for_lang ~cache ~language ~required_path_patterns
    ~excluded_path_patterns (file : file_info) =
  let key : target_cache_key =
    {
      path = file.path;
      language;
      required_path_patterns;
      excluded_path_patterns;
    }
  in
  match Hashtbl.find_opt cache key with
  | Some res -> res
  | None ->
      let res =
        match_a_required_path_pattern required_path_patterns file.path
        && match_all_excluded_path_patterns excluded_path_patterns file.path
        && match_language lang file
      in
      Hashtbl.replace cache key res;
      res

let filter_target_for_rule cache (rule : Rule.t) (file : file_info) =
  let langs = rule.languages in
  let { include_; exclude } = rule.paths in
  langs
  |> List.exists (fun language ->
         filter_target_for_lang ~cache ~language
           ~required_path_patterns:include_ ~excluded_path_patterns:exclude file)

let filter_targets_for_rule cache rule files =
  List.filter (filter_target_for_rule cache) files

let sort_targets_by_decreasing_size targets =
  targets
  |> Common.map (fun target -> (target, Common2.filesize target.In.path))
  |> List.sort (fun (_, (a : int)) (_, b) -> compare b a)
  |> Common.map fst

let sort_files_by_decreasing_size files =
  files
  |> Common.map (fun file -> (file, Common2.filesize file))
  |> List.sort (fun (_, (a : int)) (_, b) -> compare b a)
  |> Common.map fst

let files_of_dirs_or_files ?(keep_root_files = true)
    ?(sort_by_decr_size = false) opt_lang roots =
  let explicit_targets, paths =
    if keep_root_files then
      roots
      |> List.partition (fun path ->
             Sys.file_exists path && not (Sys.is_directory path))
    else (roots, [])
  in
  let paths = Common.files_of_dir_or_files_no_vcs_nofilter paths in
  let files, skipped = global_filter paths in
  let paths = Common.map (fun file -> file.path) files in
  let paths = explicit_targets @ paths in
  let sorted_paths =
    if sort_by_decr_size then sort_files_by_decreasing_size paths
    else List.sort String.compare paths
  in
  let sorted_skipped =
    List.sort
      (fun (a : Resp.skipped_target) b -> String.compare a.path b.path)
      skipped
  in
  (sorted_paths, sorted_skipped)
