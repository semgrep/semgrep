open Common
module R = Rule
module RP = Report
module RM = Range_with_metavars
module PM = Pattern_match
module Out = Semgrep_output_v0_t

(* 
   
def extract_npm_lockfile_hash(s: str) -> Dict[str, List[str]]:
    """
    Go from:
        sha512-aePbxDmcYW++PaqBsJ+HYUFwCdv4LVvdnhBy78E57PIor8/OVvhMrADFFEDh8DHDFRv/O9i3lPhsENjO7QX0+A==
    To:
        sha512,
    """
    algorithm = s.split("-")[0]
    rest = s[len(algorithm) + 1 :]
    decode_base_64 = base64.b64decode(rest)
    return {algorithm: [base64.b16encode(decode_base_64).decode("ascii").lower()]}
*)

(* TODO: turn base64 in base16 *)
let parse_npm_dep_hash str =
  match String.split_on_char '-' str with
    | [alg ; hash] -> [(alg,[hash])]
    | _ -> []

let parse_package_lock str =
  let open Yojson.Basic in
  let json = from_string str in
  let deps = 
      match Util.member "dependencies" json with
        | `Assoc deps -> deps
        | _ ->
          match Util.member "packages" json with
            | `Assoc deps -> deps
            | _ -> []
  in
  let parse_dep (package,dep_json) : Out.parsed_dep option =
    (* TODO: check if the version is valid! *)
    let str_member id json = json |> Util.member id |> Util.to_string_option in
    Option.bind (str_member "version" dep_json) @@ fun version ->
    let resolved_url = str_member "resolved" dep_json in
    let hash = str_member "integrity" dep_json in
    Some (
    Out.{
      package;
      version;
      ecosystem = `Npm;
      allowed_hashes = Option.fold hash ~none:[] ~some:parse_npm_dep_hash ;
      resolved_urls = Option.to_list resolved_url
    })
  in
  List.filter_map parse_dep deps

let parse_lockfile _path : Out.parsed_dep list = []

let semver_match _version _range = true

let match_dependencies (dep_formulas : Out.dep_formula list) (parsed_deps : Out.parsed_dep list) lockfile = 
  let (let*) x f = List.concat_map f x in
  let* dep_formula = dep_formulas in
  let* parsed_dep = parsed_deps in
  if parsed_dep.Out.ecosystem = dep_formula.Out.ecosystem &&
    parsed_dep.Out.package = dep_formula.Out.package &&
    semver_match parsed_dep.Out.version dep_formula.Out.semver_range
  then 
    [Out.{dep_formula ; parsed_dep ; lockfile}] 
  else 
    []


let rec parents path =
  match Filename.dirname path with
    | "." | "/" -> []
    | p -> p :: parents p

let find_lockfile path lockfile_names =
  let rec go = function
    | [] -> None
    | p :: ps ->
    let lockfile =
      find_some_opt (fun lockfile_name -> 
      let lockfile_path = Filename.concat p lockfile_name in
      if Sys.file_exists lockfile_path then Some lockfile_path else None)
      lockfile_names
    in
    match lockfile with
      | None -> go ps
      | Some lockfile -> Some (lockfile,parse_lockfile lockfile)

  in
  go (parents path)

let ecosystem_to_lockfiles : Semgrep_output_v0_t.ecosystem -> string list = function
  | `Npm -> ["package-lock.json";"Yarn.lock"]
  | _ -> []
(* 
let check_rule : 
  Rule.dep_rule ->
  (string -> Pattern_match.t -> unit) ->
  Config_semgrep.t * Equivalence.equivalences ->
  Xtarget.t ->
  Report.rule_profiling Report.match_result =
  fun rule match_hook (default_config, equivs) xtarget ->
    match rule.mode with
      | `Dep {spec = Some (`Search pformula); dep_formulas} as m ->
        let config = rule.R.options ||| default_config in
        let rule_id = fst rule.id in
        let formula = R.formula_of_pformula ~rule_id pformula in
        let ecosystems = List.map (fun Out.{ecosystem ; _} -> ecosystem) dep_formulas |> uniq_by (=) in
        let lockfile_names = List.concat_map ecosystem_to_lockfiles ecosystems in
        let res, _final_ranges =
          Match_search_mode.matches_of_formula (config, equivs) {rule with mode = m} xtarget formula None
        in
        let make_dep_matches (m : Pattern_match.t) =
          match find_lockfile m.file lockfile_names with
            | None -> []
            | Some (lockfile,parsed_deps) -> 
              match match_dependencies dep_formulas parsed_deps lockfile with
                | [] -> []
                | dep_matches ->
                  dep_matches |> List.map (fun dep_match ->
                  let dep_info = Out.{reachable = true ; reachability_rule = true ; dep_match} in
                  {m with dep_info_opt = Some dep_info}
                  )
        in      
        let matches = List.concat_map make_dep_matches res.matches in
        {
          RP.matches =
            matches
            (* dedup similar findings (we do that also in Match_patterns.ml,
             * but different mini-rules matches can now become the same match)
             *)
            |> PM.uniq
            |> before_return (fun v ->
                   v
                   |> List.iter (fun (m : Pattern_match.t) ->
                          let str = spf "with rule %s" rule_id in
                          match_hook str m));
          errors = res.errors;
          extra = res.extra;
        }
      
      | `Dep {spec = _ ; dep_formulas = _} -> failwith "" *)