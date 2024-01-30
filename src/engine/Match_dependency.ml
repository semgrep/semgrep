type dependency_match_table =
  (Rule_ID.t, Pattern_match.dependency_match list) Hashtbl.t

type version_core = { major : int; minor : int; incrementals : int list }
type version = Version of version_core | Other of string

let version_parser =
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

let constraint_parser =
  let open Angstrom in
  let constr =
    choice
      [
        string "==" *> return `EQ;
        string ">=" *> return `GTE;
        string "<=" *> return `LTE;
        string ">" *> return `GT;
        string "<" *> return `LT;
      ]
  in
  let* c = constr in
  many (char ' ')
  *>
  let* v = version_parser in
  return (c, v)

let parse_version str =
  Angstrom.parse_string ~consume:All version_parser str |> Result.get_ok

let parse_constraint str =
  Angstrom.parse_string ~consume:All constraint_parser str |> Result.get_ok

let compare_version_core c1 c2 =
  let cmp n m = if n > m then `GT else if n < m then `LT else `EQ in
  let rec check = function
    | (i, j) :: is -> (
        match cmp i j with
        | `EQ -> check is
        | `GT -> `GT
        | `LT -> `LT)
    | [] -> `EQ
  in
  (c1.major, c2.major) :: (c1.minor, c2.minor)
  :: Common2.zip c1.incrementals c2.incrementals
  |> check

type cmp = [ `EQ | `GT | `LT ] [@@deriving show]

let check_constraint (c, v) v' =
  match (v, v') with
  | Version v, Version v' -> (
      match c with
      | `EQ -> compare_version_core v' v = `EQ
      | `GTE -> compare_version_core v' v <> `LT
      | `LTE -> compare_version_core v' v <> `GT
      | `GT -> compare_version_core v' v = `GT
      | `LT -> compare_version_core v' v = `LT)
  | _ -> false

let match_dependency_pattern (deps : Supply_chain.dependency list)
    (pat : Rule.dependency_pattern) : Pattern_match.dependency_match list =
  deps
  |> List_.map_filter @@ fun (dep : Supply_chain.dependency) ->
     if
       String.equal dep.package_name pat.package_name
       && check_constraint
            (parse_constraint pat.version_constraint)
            (parse_version dep.package_version)
     then Some (dep, pat)
     else None

(* Return the set of dependency/pattern pairs that matched *)
let match_dependency_formula :
    Lockfile_target.t ->
    Rule.dependency_formula ->
    Pattern_match.dependency_match list =
 fun { lazy_lockfile_ast_and_errors; _ } ->
  List.concat_map (fun pat ->
      match_dependency_pattern (Lazy.force lazy_lockfile_ast_and_errors) pat)

let match_dependencies lockfile_target rule =
  match (lockfile_target, rule.Rule.dependency_formula) with
  | Some d, Some f -> Some (match_dependency_formula d f)
  | _ -> None

let match_all_dependencies lockfile_target =
  List_.map (fun rule -> (rule, match_dependencies lockfile_target rule))

let annotate_pattern_match dep_matches pm =
  match dep_matches with
  | None -> [ pm ]
  | Some dep_matches ->
      (* If there are two, transitive copies of a library, and no direct copies, and it's used in code, we produce TWO reachable matches *)
      dep_matches
      |> List_.map_filter (fun dm ->
             (* TODO: Make this not quadratic
                If the match is on a transitive dep and there's also a match on
                a direct copy of the dep, then do not include it, only use the direct one
                this is what the python code does
             *)
             if
               Supply_chain.(
                 equal_transitivity (fst dm).transitivity Transitive)
               && dep_matches
                  |> List.exists (fun (dep, _) ->
                         Supply_chain.(
                           equal_transitivity dep.transitivity Direct
                           && String.equal dep.package_name
                                (fst dm).package_name))
             then None
             else Some { pm with Pattern_match.dependency_match = Some dm })
