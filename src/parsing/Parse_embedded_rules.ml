let find s1 s2 loc =
  let re = Str.regexp_string s2 in
  try Str.search_forward re s1 loc with
  | Not_found -> -1

let has_embedded_rules fpath =
  let f = Fpath.to_string fpath in
  let file_contents = Common.read_file f in
  find file_contents "semgrep-embedded:" 0 > -1

(* TODO: find end of comment, then parse everthing after semgrep-embedded up to end of commment. Language dependent?*)
let parse_embedded_rules fpath =
  let f = Fpath.to_string fpath in
  let file_contents = Common.read_file f in
  let parse_embedded_rules_helper _acc prev_rule_loc str =
    let _next_loc = find str "semgrep-embedded:" prev_rule_loc + 1 in
    failwith "unimplemented"
  in

  parse_embedded_rules_helper [] 0 file_contents
