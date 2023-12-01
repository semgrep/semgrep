type version_core = { major : int; minor : int; incrementals : int list }
type version = Version of version_core | Other of string

let parse_version =
  let open Angstrom in
  let int =
    take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
    >>| int_of_string
  in
  let dot = char '.' in
  let any_string = take_while1 (function _ -> true) in
  let version =
    let* major = int <* dot in
    let* minor = int <* dot in
    let* incrementals = sep_by1 dot int in
    return @@ Version { major; minor; incrementals }
  in
  version <|> (any_string >>| fun s -> Other s)

let compare_version_core c1 c2 =
  let cmp n m = if n > m then `GT else if n < m then `LT else `EQ in
  let join i j =
    match (i, j) with
    | `GT, _
    | `LT, _ ->
        i
    | `EQ, j -> j
  in
  (c1.major, c2.major) :: (c1.minor, c2.minor)
  :: Common2.zip c1.incrementals c2.incrementals
  |> List.fold_left (fun acc (i, j) -> join acc (cmp i j)) `EQ
(*
let compare v1 v2 =
  match v1, v2 with
    | Version c1, Version c2 -> compare_version_core c1 c2 *)

(* Return the set of dependency/pattern pairs that matched *)
let match_dependency_formula :
    Xtarget.lockfile_data ->
    Rule.dependency_formula ->
    Pattern_match.dependency_match list =
 fun _ _ -> []
(* failwith "match_dependency_formula" *)

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
