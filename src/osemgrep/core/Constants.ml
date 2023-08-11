(*
   translated from constants.py
*)
open Common

let _rules_key = "rules"
let _id_key = "id"
let rule_id_for_dash_e = Rule_ID.of_string "-"

let _please_file_issue_text =
  "An error occurred while invoking the Semgrep engine. Please help us fix \
   this by creating an issue at https://github.com/returntocorp/semgrep"

let default_semgrep_config_name = "semgrep"
let _default_config_file = spf ".%s.yml" default_semgrep_config_name
let _default_config_folder = spf ".%s" default_semgrep_config_name

let _returntocorp_lever_url =
  "https://api.lever.co/v0/postings/returntocorp?mode=json"

let _unsupported_ext_ignore_langs = [ ("generic", "regex") ]
let _comma_separated_list_re = SPcre.regexp {|[,\s]|}
let break_line_width = 80
let break_line_char = '-'
let _break_line = String.make break_line_width break_line_char
