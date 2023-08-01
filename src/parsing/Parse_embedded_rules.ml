let start_string = "embedded-semgrep:"
let end_string = "embedded-semgrep-end"

let find s1 s2 loc =
  let re = Str.regexp_string s2 in
  try Str.search_forward re s1 loc with
  | Not_found -> -1

let has_embedded_rules fpath =
  let f = Fpath.to_string fpath in
  let file_contents = Common.read_file f in
  find file_contents start_string 0 > -1

let parse_embedded_rules fpath =
  let f = Fpath.to_string fpath in
  let file_contents = Common.read_file f in
  let rec parse_embedded_rules_helper (acc_rule, acc_error) prev_loc str =
    let next_loc = find str start_string prev_loc in
    if next_loc == -1 then (acc_rule, acc_error)
    else
      let end_loc = find str end_string (next_loc + 1) in
      let rule_string =
        String.sub str
          (next_loc + String.length start_string)
          (end_loc - (next_loc + String.length start_string))
      in
      print_string rule_string;
      let temp_file = Filename.open_temp_file "temp_rule" ".yaml" in
      Out_channel.output_string (snd temp_file) rule_string;
      Out_channel.close (snd temp_file);
      let path = Fpath.of_string (fst temp_file) in
      match path with
      | Ok p ->
          let rules, errors = Parse_rule.parse_and_filter_invalid_rules p in
          parse_embedded_rules_helper
            (rules @ acc_rule, errors @ acc_error)
            end_loc str
      | Error _ -> failwith "not a correct file path"
  in

  parse_embedded_rules_helper ([], []) 0 file_contents
