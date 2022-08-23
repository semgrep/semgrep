module E = Semgrep_error_code
module PI = Parse_info
module Out = Output_from_core_t
module PM = Pattern_match

type pattern_id = int
type id_to_match_results = (pattern_id, PM.t) Hashtbl.t

type xconfig = {
  config : Config_semgrep_t.t;
  equivs : Equivalence.equivalences;
  matching_explanations : bool;
}

type env = {
  xconf : xconfig;
  pattern_matches : id_to_match_results;
  xtarget : Xtarget.t;
  rule : Rule.rule;
  errors : Report.ErrorSet.t ref;
}

val error : env -> string -> unit
val fake_rule_id : int * string -> PM.rule_id

val adjust_xconfig_with_rule_options :
  xconfig -> Config_semgrep_t.t option -> xconfig
