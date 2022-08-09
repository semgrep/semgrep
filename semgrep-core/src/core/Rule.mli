module G = AST_generic
module MV = Metavariable

val logger : Logging.logger

type tok = Parse_info.token_mutable

val pp_tok : Format.formatter -> tok -> unit
val show_tok : tok -> string
val equal_tok : tok -> tok -> bool

val hash_fold_tok :
  Base_internalhash_types.state -> tok -> Base_internalhash_types.state

val hash_tok : tok -> int

type 'a wrap = 'a G.wrap

val pp_wrap :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a wrap -> unit

val show_wrap : (Format.formatter -> 'a -> unit) -> 'a wrap -> string
val equal_wrap : ('a -> 'a -> bool) -> 'a wrap -> 'a wrap -> bool

val hash_fold_wrap :
  (Base_internalhash_types.state -> 'a -> Base_internalhash_types.state) ->
  Base_internalhash_types.state ->
  'a wrap ->
  Base_internalhash_types.state

type 'a loc = { pattern : 'a; t : tok; path : string list }

val pp_loc :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a loc -> unit

val show_loc : (Format.formatter -> 'a -> unit) -> 'a loc -> string
val equal_loc : ('a -> 'a -> bool) -> 'a loc -> 'a loc -> bool

type formula =
  | P of Xpattern.t * inside option
  | And of conjunction
  | Or of tok * formula list
  | Not of tok * formula

and conjunction = {
  tok : tok;
  conjuncts : formula list;
  conditions : (tok * metavar_cond) list;
  focus : (tok * string) list;
}

and inside = Inside

and metavar_cond =
  | CondEval of G.expr
  | CondRegexp of string * Regexp_engine.t * bool
  | CondAnalysis of string * metavar_analysis_kind
  | CondNestedFormula of string * Xlang.t option * formula

and metavar_analysis_kind = CondEntropy | CondReDoS

val pp_formula : Format.formatter -> formula -> unit
val show_formula : formula -> string
val pp_conjunction : Format.formatter -> conjunction -> unit
val show_conjunction : conjunction -> string
val pp_inside : Format.formatter -> inside -> unit
val show_inside : inside -> string
val pp_metavar_cond : Format.formatter -> metavar_cond -> unit
val show_metavar_cond : metavar_cond -> string
val pp_metavar_analysis_kind : Format.formatter -> metavar_analysis_kind -> unit
val show_metavar_analysis_kind : metavar_analysis_kind -> string
val equal_formula : formula -> formula -> bool
val equal_conjunction : conjunction -> conjunction -> bool
val equal_inside : inside -> inside -> bool
val equal_metavar_cond : metavar_cond -> metavar_cond -> bool

val equal_metavar_analysis_kind :
  metavar_analysis_kind -> metavar_analysis_kind -> bool

type formula_old =
  | Pat of Xpattern.t
  | PatNot of tok * Xpattern.t
  | PatInside of Xpattern.t
  | PatOr of tok * formula_old list
  | PatAnd of tok * formula_old list
  | PatWhere of tok * tok * formula_old * where_constraint list

and where_constraint =
  | WhereComparison of {
      tok : tok;
      comparison : AST_generic.expr;
      strip : bool option;
      base : int option;
    }
  | WhereFocus of tok * string
  | WherePattern of tok * string * Xlang.t option * formula_old

and extra =
  | MetavarRegexp of string * Regexp_engine.t * bool
  | MetavarPattern of string * Xlang.t option * formula_old
  | MetavarComparison of metavariable_comparison
  | MetavarAnalysis of string * metavar_analysis_kind

and metavariable_comparison = {
  metavariable : string option;
  comparison : G.expr;
  strip : bool option;
  base : int option;
}

val pp_formula_old : Format.formatter -> formula_old -> unit
val show_formula_old : formula_old -> string
val pp_where_constraint : Format.formatter -> where_constraint -> unit
val show_where_constraint : where_constraint -> string
val pp_extra : Format.formatter -> extra -> unit
val show_extra : extra -> string

val pp_metavariable_comparison :
  Format.formatter -> metavariable_comparison -> unit

val show_metavariable_comparison : metavariable_comparison -> string
val equal_formula_old : formula_old -> formula_old -> bool
val equal_where_constraint : where_constraint -> where_constraint -> bool
val equal_extra : extra -> extra -> bool

val equal_metavariable_comparison :
  metavariable_comparison -> metavariable_comparison -> bool

type pformula = New of formula | Old of formula_old

val pp_pformula : Format.formatter -> pformula -> unit
val show_pformula : pformula -> string
val equal_pformula : pformula -> pformula -> bool

type taint_source = { formula : pformula; label : string; requires : G.expr }

val pp_taint_source : Format.formatter -> taint_source -> unit
val show_taint_source : taint_source -> string
val default_source_label : string
val default_source_requires : G.tok -> G.expr

type taint_sanitizer = { not_conflicting : bool; formula : pformula }

val pp_taint_sanitizer : Format.formatter -> taint_sanitizer -> unit
val show_taint_sanitizer : taint_sanitizer -> string

type taint_propagator = {
  formula : pformula;
  from : string wrap;
  to_ : string wrap;
}

val pp_taint_propagator : Format.formatter -> taint_propagator -> unit
val show_taint_propagator : taint_propagator -> string

type taint_sink = { formula : pformula; requires : G.expr }

val pp_taint_sink : Format.formatter -> taint_sink -> unit
val show_taint_sink : taint_sink -> string
val default_sink_requires : G.tok -> G.expr

type taint_spec = {
  sources : taint_source list;
  propagators : taint_propagator list;
  sanitizers : taint_sanitizer list;
  sinks : taint_sink list;
}

val pp_taint_spec : Format.formatter -> taint_spec -> unit
val show_taint_spec : taint_spec -> string

type extract_reduction = Separate | Concat

val pp_extract_reduction : Format.formatter -> extract_reduction -> unit
val show_extract_reduction : extract_reduction -> string

type extract_spec = {
  pformula : pformula;
  reduce : extract_reduction;
  dst_lang : Xlang.t;
  extract : string;
}

val pp_extract_spec : Format.formatter -> extract_spec -> unit
val show_extract_spec : extract_spec -> string

type severity = Error | Warning | Info | Inventory | Experiment

val pp_severity : Format.formatter -> severity -> unit
val show_severity : severity -> string

type 'mode rule_info = {
  id : rule_id wrap;
  mode : 'mode;
  message : string;
  severity : severity;
  languages : Xlang.t;
  options : Config_semgrep_t.t option;
  equivalences : string list option;
  fix : string option;
  fix_regexp : (Regexp_engine.t * int option * string) option;
  paths : paths option;
  metadata : JSON.t option;
}

and rule_id = string
and paths = { include_ : string list; exclude : string list }

val pp_rule_info :
  (Format.formatter -> 'mode -> unit) ->
  Format.formatter ->
  'mode rule_info ->
  unit

val show_rule_info :
  (Format.formatter -> 'mode -> unit) -> 'mode rule_info -> string

val pp_rule_id : Format.formatter -> rule_id -> unit
val show_rule_id : rule_id -> string
val pp_paths : Format.formatter -> paths -> unit
val show_paths : paths -> string

type search_mode = [ `Search of pformula ]

val pp_search_mode : Format.formatter -> search_mode -> unit
val show_search_mode : search_mode -> string

type taint_mode = [ `Taint of taint_spec ]

val pp_taint_mode : Format.formatter -> taint_mode -> unit
val show_taint_mode : taint_mode -> string

type extract_mode = [ `Extract of extract_spec ]

val pp_extract_mode : Format.formatter -> extract_mode -> unit
val show_extract_mode : extract_mode -> string

type mode =
  [ `Extract of extract_spec | `Search of pformula | `Taint of taint_spec ]

val pp_mode : Format.formatter -> mode -> unit
val show_mode : mode -> string

type search_rule = search_mode rule_info

val pp_search_rule : Format.formatter -> search_rule -> unit
val show_search_rule : search_rule -> string

type taint_rule = taint_mode rule_info

val pp_taint_rule : Format.formatter -> taint_rule -> unit
val show_taint_rule : taint_rule -> string

type extract_rule = extract_mode rule_info

val pp_extract_rule : Format.formatter -> extract_rule -> unit
val show_extract_rule : extract_rule -> string

type rule = mode rule_info

val pp_rule : Format.formatter -> rule -> unit
val show_rule : rule -> string

type t = rule

val pp : Format.formatter -> t -> unit
val show : t -> string

type rules = t list

val pp_rules : Format.formatter -> rules -> unit
val show_rules : rules -> string

val partition_rules :
  rules -> search_rule list * taint_rule list * extract_rule list

val last_matched_rule : rule_id option ref

type invalid_rule_error = invalid_rule_error_kind * rule_id * tok

and invalid_rule_error_kind =
  | InvalidLanguage of rule_id
  | InvalidPattern of rule_id * Xlang.t * rule_id * rule_id list
  | InvalidRegexp of rule_id
  | DeprecatedFeature of rule_id
  | MissingPositiveTermInAnd
  | InvalidOther of rule_id

val pp_invalid_rule_error : Format.formatter -> invalid_rule_error -> unit
val show_invalid_rule_error : invalid_rule_error -> string

val pp_invalid_rule_error_kind :
  Format.formatter -> invalid_rule_error_kind -> unit

val show_invalid_rule_error_kind : invalid_rule_error_kind -> string
val string_of_invalid_rule_error_kind : invalid_rule_error_kind -> string

exception InvalidRule of invalid_rule_error
exception InvalidYaml of string * Parse_info.token_mutable
exception DuplicateYamlKey of string * Parse_info.token_mutable
exception UnparsableYamlException of string
exception ExceededMemoryLimit of string

val string_of_invalid_rule_error : invalid_rule_error -> string
val opt_string_of_exn : exn -> string option
val register_exception_printer : unit -> unit
val visit_new_formula : (Xpattern.t -> unit) -> formula -> unit
val tok_of_formula : formula -> tok
val kind_of_formula : formula -> string
val rewrite_metavar_comparison_strip : AST_generic.expr -> AST_generic.expr
val remove_noop : formula_old -> formula_old
val split_and : formula list -> formula list * formula list

val convert_formula_old :
  ?in_metavariable_pattern:bool -> rule_id:rule_id -> formula_old -> formula

val formula_of_pformula :
  ?in_metavariable_pattern:bool -> rule_id:rule_id -> pformula -> formula
