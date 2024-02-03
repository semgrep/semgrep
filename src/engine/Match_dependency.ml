module R = Rule
module PM = Pattern_match
module D = Dependency
module Out = Semgrep_output_v1_t

type dependency_match_table =
  (Rule_ID.t, Pattern_match.dependency_match list) Hashtbl.t

type cmp = [ `EQ | `GT | `LT ]

let compare_version_core c1 c2 =
  let cmp n m : cmp = if n > m then `GT else if n < m then `LT else `EQ in
  let rec check = function
    | (i, j) :: is -> (
        match cmp i j with
        | `EQ -> check is
        | `GT -> `GT
        | `LT -> `LT)
    | [] -> `EQ
  in
  (c1.D.major, c2.D.major) :: (c1.minor, c2.minor)
  :: Common2.zip c1.incrementals c2.incrementals
  |> check

let check_constraint D.{ version; constraint_ } v' =
  match (version, v') with
  | D.Version v, D.Version v' -> (
      match constraint_ with
      | D.Eq -> compare_version_core v' v = `EQ
      | Gte -> compare_version_core v' v <> `LT
      | Lte -> compare_version_core v' v <> `GT
      | Gt -> compare_version_core v' v = `GT
      | Lt -> compare_version_core v' v = `LT)
  | _ -> false

let match_dependency_pattern (deps : Dependency.t list)
    (pat : Rule.dependency_pattern) : Pattern_match.dependency_match list =
  deps
  |> List_.map_filter @@ fun (dep : Dependency.t) ->
     if
       String.equal dep.package_name pat.package_name
       && pat.version_constraints |> fun (And cs) ->
          List.for_all
            (fun constr -> check_constraint constr dep.package_version)
            cs
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
  match rule.Rule.dependency_formula with
  | Some f -> Some (match_dependency_formula lockfile_target f)
  | _ -> None

let match_all_dependencies lockfile_target =
  List_.map (fun rule -> (rule, match_dependencies lockfile_target rule))

let check_rule rule target dependency_formula =
  let _, parse_time =
    Common.with_time (fun () ->
        Lazy.force target.Lockfile_target.lazy_lockfile_ast_and_errors)
  in
  let matches, match_time =
    Common.with_time (fun () ->
        match_dependency_formula target dependency_formula)
  in
  let matches =
    matches
    |> List_.map (fun ((dep, pat) : PM.dependency_match) ->
           PM.
             {
               rule_id =
                 {
                   id = fst rule.R.id;
                   message = rule.R.message;
                   metadata = rule.R.metadata;
                   fix = rule.R.fix;
                   fix_regexp = rule.R.fix_regexp;
                   langs = Xlang.to_langs rule.R.target_analyzer;
                   (* TODO: What should this be? *)
                   pattern_string = "";
                 };
               file = target.lockfile;
               (* TODO: should be pro? Where is this supposed to be set? *)
               engine_kind = `OSS;
               range_loc = dep.Dependency.loc;
               tokens = lazy dep.Dependency.toks;
               env = [];
               taint_trace = None;
               (* TODO: What if I have a secrets rule with a dependency pattern *)
               validation_state = `No_validator;
               severity_override = None;
               metadata_override = None;
               dependency = Some (LockfileOnlyMatch (dep, pat));
             })
  in
  Core_result.make_match_result matches Core_error.ErrorSet.empty
    { Core_profiling.parse_time; match_time; rule_id = fst rule.R.id }

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
               Dependency.(
                 Out.equal_transitivity (fst dm).transitivity `Transitive)
               && dep_matches
                  |> List.exists (fun (dep, _) ->
                         Dependency.(
                           Out.equal_transitivity dep.transitivity `Direct
                           && String.equal dep.package_name
                                (fst dm).package_name))
             then None
             else
               Some
                 {
                   pm with
                   Pattern_match.dependency = Some (CodeAndLockfileMatch dm);
                 })
