(*
   translated from constants.py
*)

open Printf

let _rules_key = "rules"
let _id_key = "id"
let cli_rule_id = "-"

let _please_file_issue_text =
  "An error occurred while invoking the Semgrep engine. Please help us fix \
   this by creating an issue at https://github.com/returntocorp/semgrep"

let default_semgrep_config_name = "semgrep"
let _default_config_file = sprintf ".%s.yml" default_semgrep_config_name
let _default_config_folder = sprintf ".%s" default_semgrep_config_name
let _default_semgrep_app_config_url = "api/agent/deployments/scans/config"

(* LATER: move to Scan_CLI.default directly *)
let default_timeout = 30 (* seconds *)
let yml_extensions = [ ".yml"; ".yaml" ]
let _yml_suffixes = Common.map (fun ext -> [ ext ]) yml_extensions
let _yml_test_suffixes = Common.map (fun ext -> [ ".test"; ext ]) yml_extensions
let _fixtest_suffix = ".fixed"

let _returntocorp_lever_url =
  "https://api.lever.co/v0/postings/returntocorp?mode=json"

let _unsupported_ext_ignore_langs = [ ("generic", "regex") ]

(* LATER: move out of Constants.ml, those are type defs, not constants ... *)
type output_format =
  | Text
  | Json
  | Gitlab_sast
  | Gitlab_secrets
  | Junit_xml
  | Sarif
  | Emacs
  | Vim
[@@deriving show]

let _output_format_is_json = function
  | Json
  | Sarif ->
      true
  | Text
  | Gitlab_sast
  | Gitlab_secrets
  | Junit_xml
  | Emacs
  | Vim ->
      false

(* coupling: ensure consistency with 'serverity' in 'rule_schema_v1.yaml'
 * LATER: redundant with Rule.severity in semgrep-core.
 *)
type rule_severity = Info | Warning | Error | Inventory | Experiment
(* TOPORT
    @classmethod
    def _missing_(cls: Type[Enum], value: object) -> Enum:
        if not isinstance(value, str):
            raise TypeError(f"invalid rule severity type: {type(value)}")
        for member in cls:
            if member.value.lower() == value:
                return member
        raise ValueError(f"invalid rule severity value: {value}")
*)

let rule_id_re_str = {|(?:[:=][\s]?(?P<ids>([^,\s](?:[,\s]+)?)+))?|}

(*
   Inline 'noqa' implementation modified from flake8:
   https://github.com/PyCQA/flake8/blob/master/src/flake8/defaults.py
   We're looking for items that look like this:
   ' nosem'
   ' nosemgrep: example-pattern-id'
   ' nosem: pattern-id1,pattern-id2'
   ' NOSEMGREP:pattern-id1,pattern-id2'

   * We do not want to capture the ': ' that follows 'nosem'
   * We do not care about the casing of 'nosem'
   * We want a comma-separated list of ids
   * We want multi-language support, so we cannot strictly look for
     Python comments that begin with '# '
   * nosem and nosemgrep should be interchangeable
*)
let nosem_inline_re_str = {| nosem(?:grep)?|} ^ rule_id_re_str
let _nosem_inline_re = SPcre.regexp nosem_inline_re_str ~flags:[ `CASELESS ]

(*
   As a hack adapted from semgrep-agent,
   we assume comment markers are one of these special characters
*)
let _nosem_inline_comment_re =
  SPcre.regexp (sprintf {|[:#/]+%s$|} nosem_inline_re_str) ~flags:[ `CASELESS ]

(*
   A nosemgrep comment alone on its line.
   Since we don't know the comment syntax for the particular language, we
   assume it's enough that there isn't any word or number character before
   'nosemgrep'.
   The following will not match:
     hello(); // nosemgrep
     + 42 // nosemgrep
   The following will match:
     # nosemgrep
     print('nosemgrep');
*)
let _nosem_previous_line_re =
  SPcre.regexp
    ({|^[^a-zA-Z0-9]* nosem(?:grep)?|} ^ rule_id_re_str)
    ~flags:[ `CASELESS ]

let _comma_separated_list_re = SPcre.regexp {|[,\s]|}
let _max_lines_flag_name = "--max-lines-per-finding"
let _default_max_lines_per_finding = 10
let break_line_width = 80
let break_line_char = '-'
let _break_line = String.make break_line_width break_line_char
let _max_chars_flag_name = "--max-chars-per-line"
let _default_max_chars_per_line = 160
let _ellipsis_string = " ... "

(* LATER: move to Scan_CLI.default directly *)
let default_max_target_size = 1_000_000 (* 1 MB *)

(* python: original code:
   class Colors(Enum):
       # these colors come from user's terminal theme
       foreground = 0
       white = 7
       black = 256
       cyan = "cyan"  # for filenames
       green = "green"  # for autofix
       yellow = "yellow"  # TODO: benchmark timing output?
       red = "red"  # for errors
       bright_blue = "bright_blue"  # TODO: line numbers?

       # these colors ignore user's terminal theme
       forced_black = 16  # #000
       forced_white = 231  # #FFF
*)

(* LATER: move to utils/Color.ml separate file; those are not really constants *)
type _color =
  | Foreground
  | White
  | Black
  | Cyan
  | Green
  | Yellow
  | Red
  | Bright_blue
  | Forced_black
  | Forced_white

type color_code = Int of int | String of string

(* What's the encoding? *)
let _encode_color = function
  | Foreground -> Int 0
  | White -> Int 7 (* really? *)
  | Black -> Int 256 (* really? *)
  | Cyan -> String "cyan"
  | Green -> String "green"
  | Yellow -> String "yellow"
  | Red -> String "red"
  | Bright_blue -> String "bright_blue"
  | Forced_black -> Int 16
  | Forced_white -> Int 231
