(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(* Filter target candidates.

   TODO: Handles all file include/exclude logic for semgrep

   filtering each (rule, target) pair can
   become problematic since the number of such pairs is O(number of targets
   * number of rules).
   WORKS? This is why we cache the results of this step.
   This allows reducing the number of rules to the number of different
   languages and patterns used by the rules.

   Partially translated from target_manager.py
*)

(*************************************************************************)
(* Types *)
(*************************************************************************)

(*
   Some rules will use 'include' (required_path_patterns) and 'exclude'
   (excluded_path_patterns) to select targets that don't have an extension
   such as 'Dockerfile'. We expect most rules written for a language
   to use the same combination of include/exclude. This allows caching
   across the many rules that target the same language.
*)
type target_cache_key = {
  path : Fpath.t;
  lang : Xlang.t;
  required_path_patterns : string list;
  excluded_path_patterns : string list;
}

type target_cache = (target_cache_key, bool) Hashtbl.t

(*************************************************************************)
(* Helpers *)
(*************************************************************************)

let create_cache () = Hashtbl.create 1000

let match_glob_pattern ~pat path =
  let regex = Glob.Parse.parse_string pat in
  Glob.Match.run
    Glob.Match.(compile ~source:(string_loc ~source_kind:None pat) regex)
    (Fpath.to_string path)

let match_a_path_pattern path_patterns path =
  match path_patterns with
  | [] -> (* <grimacing face emoji> *) true
  | pats -> List.exists (fun pat -> match_glob_pattern ~pat path) pats

let match_language (xlang : Xlang.t) path =
  match xlang with
  | L (lang, langs) ->
      (* ok if the file appears to be in one of rule's languages *)
      List.exists
        (fun lang -> Guess_lang.inspect_file_p lang path)
        (lang :: langs)
  | LRegex
  | LGeneric ->
      true

(*************************************************************************)
(* Entry point *)
(*************************************************************************)

(* Used by Core_runner.split_jobs_by_language *)
let filter_target_for_lang ~cache ~lang ~required_path_patterns
    ~excluded_path_patterns path =
  let cond () =
    match_a_path_pattern required_path_patterns path
    && (Common.null excluded_path_patterns
       || not (match_a_path_pattern excluded_path_patterns path))
    && match_language lang path
  in
  (* TODO: use Common.memoize *)
  let key : target_cache_key =
    { path; lang; required_path_patterns; excluded_path_patterns }
  in
  match Hashtbl.find_opt cache key with
  | Some res -> res
  | None ->
      let res = cond () in
      Hashtbl.replace cache key res;
      res

(* TODO? useful?
   let filter_target_for_rule cache (rule : Rule.t) path =
     let required_path_patterns, excluded_path_patterns =
       match rule.paths with
       | Some { include_; exclude } -> (include_, exclude)
       | None -> ([], [])
     in
     filter_target_for_lang ~cache ~lang:rule.languages ~required_path_patterns
       ~excluded_path_patterns path

   let filter_targets_for_rule cache rule files =
     List.filter (filter_target_for_rule cache rule) files
*)
