(* Return the set of dependency/pattern pairs that matched *)
let match_dependency_formula :
    Xtarget.lockfile_data ->
    Rule.dependency_formula ->
    Pattern_match.dependency_match list =
 fun _ _ -> failwith "match_dependency_formula"

let match_dependencies xtarget rule =
  match xtarget.Xtarget.lockfile_data with
  | None -> None
  | Some d -> Some (match_dependency_formula d rule.Rule.dependency_formula)

let match_all_dependencies xtarget =
  Common.map (fun rule -> (rule, match_dependencies xtarget rule))

(* TODO: should these actually use a mutable fields for perf reasons?  *)

(* Precondition: dependency_matches is non-empty  *)
let join_pattern_match
    (dependency_matches : Pattern_match.dependency_match list)
    (code_match : Pattern_match.t) =
  dependency_matches
  |> Common.map (fun dm -> { code_match with dependency_match = Some dm })

(* TODO: if the core_result has no matches, but we *do* have dependency matches, create unreachable/lockfile-only matches! *)
let join_core_result
    ((core_result, dependency_matches) : 'a Core_result.match_result * _) =
  match dependency_matches with
  | None -> core_result
  | Some dependency_matches ->
      Core_result.
        {
          core_result with
          matches =
            List.concat_map
              (join_pattern_match dependency_matches)
              core_result.matches;
        }
