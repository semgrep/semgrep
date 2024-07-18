(* Yoann Padioleau
 *
 * Copyright (C) 2019-2023 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common
module MV = Metavariable
module OutJ = Semgrep_output_v1_t
open Ppx_hash_lib.Std.Hash.Builtin

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Data structures to represent a Semgrep rule (=~ AST of a rule).
 *
 * See also semgrep-interfaces/rule_schema_v2.atd which specifies the
 * concrete syntax of a rule.
 * See also Mini_rule.ml where formula and many other features disappear.
 *)

(*****************************************************************************)
(* Position information *)
(*****************************************************************************)

(* This is similar to what we do in AST_generic to get precise
 * error location when a rule is malformed (and also to get some
 * special equality and hashing; see the comment for Tok.t_always_equal
 * in Tok.mli)
 *)
type tok = Tok.t_always_equal [@@deriving show, eq, hash]
type 'a wrap = 'a * tok [@@deriving show, eq, hash]

(* To help report pattern errors in the playground *)
type 'a loc = {
  pattern : 'a;
  t : tok;
  path : string list; (* path to pattern in YAML rule *)
}
[@@deriving show, eq]

(*****************************************************************************)
(* Formula (patterns boolean composition) *)
(*****************************************************************************)

(* Classic boolean-logic/set operators with text range set semantic.
 * The main complication is the handling of metavariables and especially
 * negation in the presence of metavariables.
 *
 * less? enforce invariant that Not can only appear in And?
 *
 * We use 'deriving hash' for formula because of the
 * Match_tainting_mode.Formula_tbl formula cache.
 *)
type formula = {
  f : formula_kind;
  (* metavariable-xyz:'s *)
  conditions : (tok * metavar_cond) list;
  (* focus-metavariable:'s *)
  focus : focus_mv_list list;
  (* autofix *)
  fix : string option;
  (* as:'s *)
  (* This is for binding a match to a metavariable, such as the following example:
     @decorator()
     def $FUNC(...):
       ...
     Without the ability to use `as` to bind this match, we would not be able to
     autofix the entire function, decorator included.
  *)
  as_ : string option;
}

and formula_kind =
  | P of Xpattern.t (* a leaf pattern *)
  (* The conjunction must contain at least
     * one positive "term" (unless it's inside a CondNestedFormula, in which
     * case there is not such a restriction).
     * See also split_and().
     * CAVEAT: This is not required in the metavariable-pattern case, such as
       metavariable-pattern:
         metavariable: $MVAR
         patterns:
         - pattern-not: "foo"
  *)
  | And of tok * formula list
  | Or of tok * formula list
  (* There are currently restrictions on where a Not can appear in a formula.
   * It must be inside an And to be intersected with "positive" formula.
   * TODO? Could this change if we were moving to a different range semantic?
   *)
  | Not of tok * formula
  (* pattern: and pattern-inside: are actually slightly different so
   * we need to keep the information around.
   * (see tests/rules/inside.yaml)
   * The same is true for pattern-not and pattern-not-inside
   * (see tests/rules/negation_exact.yaml)
   * todo: try to remove this at some point, but difficult. See
   * https://github.com/returntocorp/semgrep/issues/1218
   *)
  | Inside of tok * formula
  (* alt: Could do this under a `where` (call it something like `also`).
     Preferred this since existing where conditions are predicates on
     metavariables and this is an additional separate formula. Additionally,
     this version opens this up more naturally for negation (not
     straightforward to negate a where clause, but can just use `not` for a
     formula)
  *)
  | Anywhere of tok * formula

and metavar_cond =
  | CondEval of AST_generic.expr (* see Eval_generic.ml *)
  (* todo: at some point we should remove CondRegexp and have just
   * CondEval, but for now there are some
   * differences between using the matched text region of a metavariable
   * (which we use for MetavarRegexp) and using its actual value
   * (which we use for MetavarComparison), which translate to different
   * calls in Eval_generic.ml
   * update: this is also useful to keep separate from CondEval for
   * the "regexpizer" optimizer (see Analyze_rule.ml).
   *)
  | CondRegexp of
      MV.mvar * Xpattern.regexp_string * bool (* constant-propagation *)
  | CondType of
      MV.mvar
      * Xlang.t option
      (* when the type expression is in different lang *)
      * string list (* raw input string saved for regenerating rule yaml *)
      * AST_generic.type_ list
    (* LATER: could parse lazily, like the patterns *)
  | CondAnalysis of MV.mvar * metavar_analysis_kind
  | CondNestedFormula of MV.mvar * Xlang.t option * formula
  | CondName of MV.mvar * metavar_name_kind

and metavar_analysis_kind = CondEntropy | CondEntropyV2 | CondReDoS

(* ugly: adhoc static analysis used in pro. In the long term we should
 * instead improve the engine (e.g., finish steps mode) instead of
 * writing adhoc analysis.
 *)
and metavar_name_kind = DjangoView

(* Represents all of the metavariables that are being focused by a single
   `focus-metavariable`. *)
and focus_mv_list = tok * MV.mvar list [@@deriving show, eq, hash]

(*****************************************************************************)
(* Semgrep_output aliases *)
(*****************************************************************************)

(* This enum type is now defined in semgrep_output_v1.atd.
 * This used to be essentially Error/Warning/Info, but starting from 1.72.0 we
 * want to migrate to Critical/High/Medium/Low/Info as explained in
 * https://linear.app/semgrep/issue/FIND-1240/unified-severity-levels-across-productslocations
 *)
type severity = Semgrep_output_v1_t.match_severity [@@deriving show, eq]

type validation_state = Semgrep_output_v1_t.validation_state
[@@deriving show, eq]

(*****************************************************************************)
(* Taint-specific types *)
(*****************************************************************************)

(* We roll our own Boolean formula type here for convenience, it is simpler to
 * inspect and manipulate, and we can safely use polymorphic 'compare' on it.
 *)
type precondition =
  | PLabel of string
  | PBool of bool
  | PAnd of precondition list
  | POr of precondition list
  | PNot of precondition
[@@deriving show, ord]

type precondition_with_range = {
  precondition : precondition;
  range : (Tok.location * Tok.location) option;
}
[@@deriving show]

type by_side_effect = Only | Yes | No [@@deriving show]

(* The sources/sanitizers/sinks used to be a simple 'formula list',
 * but with taint labels things are bit more complicated.
 *)
type taint_spec = {
  sources : tok * taint_source list;
  sanitizers : (tok * taint_sanitizer list) option;
  sinks : tok * taint_sink list;
  propagators : taint_propagator list;
}

and taint_source = {
  source_id : string;  (** See 'Parse_rule.parse_taint_source'. *)
  source_formula : formula;
  source_exact : bool;
      (** If 'false' (the default), if the formula were e.g. `source(...)`, then the
      * `ok` inside `source(sink(ok))` is considered tainted, and `sink(ok)` is
      * reported. If 'true', only the entire `source(sink(ok))` expression will be
      * considered tainted, and `sink(ok)` is fine. *)
  source_by_side_effect : by_side_effect;
  source_control : bool;
  label : string;
      (* The label to attach to the data.
       * Alt: We could have an optional label instead, allow taint that is not
       * labeled, and allow sinks that work for any kind of taint? *)
  source_requires : precondition_with_range option;
      (* A Boolean expression over taint labels, using Python syntax
       * (see Parse_rule). The operators allowed are 'not', 'or', and 'and'.
       *
       * The expression that is being checked as a source must satisfy this
       * in order to the label to be produced. Note that with 'requires' a
       * taint source behaves a bit like a propagator. *)
}

(* Note that, with taint labels, we can attach a label "SANITIZED" to the
 * data to flag that it has been sanitized... so do we still need sanitizers?
 * I am not sure to be honest, I think we will have to gain some experience in
 * using labels first.
 * Sanitizers do allow you to completely remove taint from data, although I
 * think that can be simulated with labels too. We could translate (internally)
 * `pattern-sanitizers` as `pattern-sources` with a `"__SANITIZED__"` label,
 * and then rewrite the `requires` of all sinks as `(...) not __SANITIZED__`.
 * But not-conflicting sanitizers cannot be simulated that way. That said, I
 * think we should replace not-conflicting sanitizers with some `options:`,
 * because they are a bit confusing to use sometimes.
 *)
and taint_sanitizer = {
  sanitizer_id : string;
  sanitizer_formula : formula;
  sanitizer_exact : bool;
      (** If 'false' (the default), if the formula were e.g. `sanitize(...)`, then
      * the `tainted` inside `sanitize(sink(tainted))` is considered sanitized,
      * and `sink(tainted)` is fine. If 'true', only the entire expression
      * `sanitize(sink(tainted))` will be considered sanitized, and `sink(tainted)`
      * will be reported. *)
  sanitizer_by_side_effect : bool;
  not_conflicting : bool;
      (* If [not_conflicting] is enabled, the sanitizer cannot conflict with
       * a sink or a source (i.e., match the exact same range) otherwise
       * it is filtered out. This allows to e.g. declare `$F(...)` as a
       * sanitizer, to assume that any other function will handle tainted
       * data safely.
       * Without this, `$F(...)` would automatically sanitize any other
       * function call acting as a sink or a source.
       *
       * THINK: In retrospective, I'm not sure this was a good idea.
       * We should add an option to disable the assumption that function
       * calls always propagate taint, and deprecate not-conflicting
       * sanitizers.
       *)
}

and taint_sink = {
  sink_id : string;  (** See 'Parse_rule.parse_taint_sink'. *)
  sink_formula : formula;
  sink_exact : bool;
      (** If 'true' (the default), if the formula were e.g. `sink(...)`, then the
      * `tainted` inside `sink(if tainted then ok1 else ok2)` is not considered a
      * sink, and nothing is reported. If 'false', then every subexpression in
      * `sink(if tainted then ok1 else ok2)` is considered a sink, and we report
      * a finding due to `tainted`. *)
  sink_requires : precondition_with_range option;
      (* A Boolean expression over taint labels. See also 'taint_source'.
       * The sink will only trigger a finding if the data that reaches it
       * has a set of labels attached that satisfies the 'requires'.
       *)
  sink_at_exit : bool;
      (* Whether this sink only applies to instructions at exit positions. *)
  sink_has_focus : bool;
      (* True if the 'sink_formula' has "focus", so it matches something and then
       * focuses on a specific part of the match.
       *
       * See NOTE(sink_has_focus) in module 'Dataflow_tainting' and
       * `Rule.is_formula_with_focus`.
       *)
}

(* e.g. if we want to specify that adding tainted data to a `HashMap` makes
 *  the `HashMap` tainted too, then "formula" could be `(HashMap $H).add($X)`,
 * with "from" being `$X` and "to" being `$H`. So if `$X` is tainted then `$H`
 * will also be marked as tainted.
 *)
and taint_propagator = {
  propagator_id : string;
  propagator_formula : formula;
  propagator_by_side_effect : bool;
  from : MV.mvar wrap;
  to_ : MV.mvar wrap;
  propagator_requires : precondition_with_range option;
      (* A Boolean expression over taint labels. See also 'taint_source'.
       * This propagator will only propagate taint if the incoming taint
       * satisfies the 'requires'.
       *)
  propagator_replace_labels : string list option;
      (* A list of the particular labels of taint to be replaced by
         the propagator.
         Does nothing if [propagator_label] is not also specified.
         If not specified, all kinds are propagated.
      *)
  propagator_label : string option;
      (* If [propagator_label] is specified, then the propagator will
         output taint with the given label.
         Otherwise, it will output taint with the same label as it
         received.
      *)
}
[@@deriving show]

let default_source_label = "__SOURCE__"
let default_source_requires = PBool true
let default_propagator_requires = PBool true

let get_source_precondition { source_requires; _ } =
  match source_requires with
  | None -> default_source_requires
  | Some { precondition; _ } -> precondition

let get_propagator_precondition { propagator_requires; _ } =
  match propagator_requires with
  | None -> default_propagator_requires
  | Some { precondition; _ } -> precondition

let get_sink_requires { sink_requires; _ } =
  match sink_requires with
  | None -> PLabel default_source_label
  | Some { precondition; _ } -> precondition

(* Check if a formula has "focus" (i.e., `focus-metavariable` in syntax 1.0)
 *
 * See 'taint_sink', field 'sink_has_focus'. *)
let is_formula_with_focus (formula : formula) =
  match formula with
  | { focus = _ :: _; _ } -> true
  | __else__ -> false

(*****************************************************************************)
(* Extract mode (semgrep as a preprocessor) *)
(*****************************************************************************)

(* See also Extract.ml for extract mode helpers *)
type extract = {
  formula : formula;
  dst_lang : Xlang.t;
  (* e.g., $...BODY, $CMD *)
  extract : MV.mvar;
  extract_rule_ids : extract_rule_ids option;
  (* map/reduce *)
  transform : extract_transform;
  reduce : extract_reduction;
}

(* SR wants to be able to choose rules to run on.
   Behaves the same as paths, but for rule ids. *)
and extract_rule_ids = {
  required_rules : Rule_ID.t wrap list;
  excluded_rules : Rule_ID.t wrap list;
}

(* Method to transform extracted content:
    - either treat them as a raw string; or
    - transform JSON array into a raw string
*)
and extract_transform = NoTransform | Unquote | ConcatJsonArray

(* Method to combine extracted ranges within a file:
    - either treat them as separate files; or
    - concatentate them together
*)
and extract_reduction = Separate | Concat [@@deriving show]

(*****************************************************************************)
(* secrets mode (Pro-only) *)
(*****************************************************************************)

(* This type encodes a basic HTTP request; mainly used for in the secrets
 * post-processor; such that a basic http request like
 * GET semgrep.dev
 * Auth: ok
 * Type: tau
 * would be represented as
 * {
 *   url     = semgrep.dev/user;
 *   meth    = `GET;
 *   headers =
 *  [
 *    { n = Auth, v = ok};
 *    { n = Type, v = tau};
 *  ]
 * }
 * NOTE: we don't reuse cohttp's abstract type Cohttp.Headers.t; we still need
 * it to not be abstract for metavariable substitution.
 *)

type header = { name : string; value : string } [@@deriving show]
type meth = [ `DELETE | `GET | `POST | `HEAD | `PUT ] [@@deriving show]

(* Used to request additional auth headers are computed and added automatically,
 * e.g., because they depend on other headers and/or body
 *
 * For instance, AWS requires signed requests for non-anonymous requests. This
 * entails generating an HMAC based on the body, current time, and a subset of
 * the headers. For us, the headers and body might be generated by
 * interpolating metavariables, so this isn't something which can just be
 * statically added to the rule easily (current time also presents an issue
 * here). Thus, we need a way to have this be added to the generated HTTP frame.
 *)
type auth =
  (* Adds headers required as described in
   * <https://docs.aws.amazon.com/IAM/latest/UserGuide/create-signed-request.html>
   *)
  | AWS_SIGV4 of {
      secret_access_key : string;
      access_key_id : string;
      service : string;
      region : string;
    }
[@@deriving show]

type aws_request = {
  secret_access_key : string;
  access_key_id : string;
  region : string;
}
[@@deriving show]

(* why is url : string? metavariables (i.e http://$X) are present at parsing; which
 * if parsed with Uri.of_string translates it to http://%24x
 *)
type request = {
  url : string;
  meth : meth;
  headers : header list;
  body : string option;
  auth : auth option;
}
[@@deriving show]

(* Used to match on the returned response of some request *)
type response = {
  return_code : Parsed_int.t;
  regex : Xpattern.regexp_string option;
}
[@@deriving show]

type http_match_clause = {
  status_code : Parsed_int.t option;
  (* Optional. Empty list if not set *)
  headers : header list;
  content : (formula * Xlang.t) option;
}
[@@deriving show]

type http_matcher = {
  match_conditions : http_match_clause list;
  validity : validation_state;
  (* Fields to potentially modify *)
  severity : severity option;
  metadata : JSON.t option;
  message : string option;
}
[@@deriving show]

type validator =
  | HTTP of { request : request; response : http_matcher list }
  | AWS of { request : aws_request; response : http_matcher list }
[@@deriving show]

(*****************************************************************************)
(* Paths *)
(*****************************************************************************)

(* TODO? store also the compiled glob directly? but we preprocess the pattern
 * in Filter_target.filter_paths, so we would need to recompile it anyway,
 * or call Filter_target.filter_paths preprocessing in Parse_rule.ml
 *)
type glob = string (* original string *) * Glob.Pattern.t (* parsed glob *)
[@@deriving show]

(* TODO? should we provide a pattern-filename: Xpattern to combine
 * with other Xpattern instead of adhoc paths: extra field in the rule?
 * TODO? should we remove this field and opt for a more powerful and general
 * Target_selector.t type?
 *)
type paths = {
  (* If not empty, list of file path patterns (globs) that
   * the file path must at least match once to be considered for the rule.
   * Called 'include' in our doc but really it is a 'require'.
   * TODO? use wrap? to also get location of include/require field?
   *)
  require : glob list;
  (* List of file path patterns we want to exclude. *)
  exclude : glob list;
}
[@@deriving show]

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

type fix_regexp = {
  regexp : Xpattern.regexp_string;
  (* Not using Parsed_int here, because we would rather fail early at rule
     parsing time if we have to apply a regexp more times than we can
     represent.
     We also expect to never receive a count that is that big.
  *)
  count : int option;
  replacement : string;
}
[@@deriving show, eq, hash]

(*****************************************************************************)
(* Shared mode definitions *)
(*****************************************************************************)

(* Polymorhic variants used to improve type checking of rules (see below) *)
type search_mode = [ `Search of formula ] [@@deriving show]
type taint_mode = [ `Taint of taint_spec ] [@@deriving show]
type extract_mode = [ `Extract of extract ] [@@deriving show]

(* Steps mode includes rules that use search_mode and taint_mode.
 * Later, if we keep it, we might want to make all rules have steps,
 * but for the experiment this is easier to remove.
 *)
type steps_mode = [ `Steps of step list ] [@@deriving show]

(*****************************************************************************)
(* Steps mode (Pro-only) *)
(*****************************************************************************)
and step = {
  step_mode : mode_for_step;
  step_selector : Target_selector.t option;
  step_analyzer : Xlang.t;
  step_paths : paths option;
}

and mode_for_step = [ search_mode | taint_mode ] [@@deriving show]

(*****************************************************************************)
(* Supply chain *)
(*****************************************************************************)

(* You can only do single layer deep OR *)
type dependency_formula = dependency_pattern list

(* A pattern to match against versions in a lockfile.
   This is not like a regular code pattern! It's description of a range of *versions*.
   For example: ">=1.0.0, <= 2.3.5", which is meant to "match" any version in that interval, e.g. 1.3.5

   Here's a breakdown of how this interacts with normal patterns:
   * Rule has only normal patterns:
      Rule behaves as normal
   * Rule has normal patterns and dependency patterns:
      If *both* match, the rule produces "reachable" findings: code findings annotated with dependency findings
      If only the code patterns match, the rule produces *no* findings
      If only the dependency patterns match, the rule produces "lockfile-only" findings: dependency findings without code findings
   * Rule has only dependency patterns:
      Rule only produces "lockfile-only" findings
*)
and dependency_pattern = {
  ecosystem : Semgrep_output_v1_t.ecosystem;
  package_name : string;
  version_constraints : Dependency.constraint_ast;
}
[@@deriving show, eq]

type sca_mode = [ `SCA of dependency_formula ] [@@deriving show]

(*****************************************************************************)
(* The rule *)
(*****************************************************************************)

type 'mode rule_info = {
  (* --------------------------------- *)
  (* MANDATORY fields *)
  (* --------------------------------- *)
  id : Rule_ID.t wrap;
  mode : 'mode;
  (* Currently a dummy value for extract mode rules *)
  message : string;
  (* Currently a dummy value for extract mode rules *)
  severity : severity;
  (* Note: The two fields target_seletor and target_analyzer below used to
   * be a single 'languages: Xlang.t' field.
   * Indeed, for historical reasons, the 'languages:' field in the
   * YAML file is a list of strings. There is no distinction between
   * target *selection* and target *analysis*. This led to oddities for
   * analyzers that aren't specific to a programming language
   * (e.g., "spacegrep", "regexp").
   *
   * We can now start to decouple file selection from their analysis.
   * For example, we can select Bash files using the predefined
   * rules that inspect the file extension (.sh) or the shebang line (#!)
   * but analyze them using a regexp instead of a regular Semgrep pattern.
   *
   * see also https://www.notion.so/semgrep/What-s-a-language-3f4e75546edd4a0389fa29a24eedb6c0
   *
   * TODO: instead of always deriving those two fields automatically from
   * the 'languages:' field of the rule in Parse_rule.ml, we should add
   * support for an optional new 'target-selectors:' field in the rule syntax.
   *
   * target_selector: How to *select* target files e.g. "files that look
   * like C files". If None, the selector selects all the files that are not
   * ignored by generic mechanisms such as semgrepignore.
   * In a Semgrep rule where a string is expected, the standard way
   * is to use "generic" but "regex" and "none" have the same effect.
   * They all translate into 'None'.
   *
   * Example:
   *
   *   target_selector = Some [Javascript; Typescript];
   *
   * ... selects all the files that can be parsed and analyzed
   * as TypeScript ( *.js, *.ts, *.tsx) since TypeScript is an extension of
   * JavaScript.
   *)
  target_selector : Target_selector.t option;
  (* target_analyzer: How to *analyze* target files. The accompanying
   * patterns are specified elsewhere in the rule.
   *
   * Examples:
   * - "pattern for the C parser using the generic AST" (regular programming
   *   language using a classic Semgrep pattern)
   * - "pattern for Fortran77 or for Fortran90" (two possible parsers)
   * - "spacegrep pattern"
   * - "high-entropy detection" (doesn't use a pattern)
   * - "extract JavaScript snippets from a PDF file" (doesn't use a pattern)
   * This information may have to be extracted from another part of the
   * YAML rule.
   *
   * Example:
   *
   *   target_analyzer = L (Typescript, []);
   *)
  target_analyzer : Xlang.t;
  (* --------------------------------- *)
  (* OPTIONAL fields *)
  (* --------------------------------- *)
  options : Rule_options.t option;
  (* deprecated? or should we resurrect the feature?
   * TODO: if we resurrect the feature, we should parse the string
   *)
  equivalences : string list option;
  (* Optional replacement pattern, a.k.a autofix.
   *
   * Note that the pattern string is passed through String.trim() during rule
   * parsing. This is to handle rules like
   *
   *        pattern: foobar($X)
   *        fix: |
   *            bar($X)
   * which are parsed by the yaml parser as 'fix: "bar($X)\n"', but we don't
   * want the extra newline added by the autofix.
   * history: this used to be done only on the pysemgrep side in rule_match.py
   * but better to do it here so osemgrep behaves like pysemgrep.
   * Note that this trimming is useful only for languages where the AST-based
   * autofix is not supported, in which case we don't parse the fix pattern
   * but perform a basic textual replacement on the fix pattern string.
   *)
  fix : string option;
  fix_regexp : fix_regexp option;
  (* TODO: we should get rid of this and instead provide a more general
   * Xpattern.Filename feature that integrates well with the xpatterns.
   *)
  paths : paths option;
  (* This is not a concrete field in the rule, but it's derived from
   * other fields (e.g., metadata, mode) in Parse_rule.ml
   *)
  product : OutJ.product;
  (* ex: [("owasp", "A1: Injection")] but can be anything.
   * Metadata was (ab)used for the ("interfile", "true") setting, but this
   * is now done via Rule_options instead.
   *)
  metadata : JSON.t option;
  (* TODO(cooper): would be nice to have nonempty but common2 version not nice to work with; no pp for one *)
  validators : validator list option;
  (* Range of Semgrep versions supported by the rule.
   * Note that a rule with these fields may not even be parseable
   * in the current version of Semgrep and wouldn't even reach this point.
   * coupling: unfortunately, rule filtering is currently done in 3 places:
   *  - in Parse_rule.check_version_compatibility() for semgrep-core and osemgrep
   *    since 1.41.0
   *  - in rule_lang.py remove_incompatible_rules_based_on_version() for pysemgrep
   *    since 1.76.0
   *  - in semgrep-app in server/semgrep_app/foundations/scan/providers/config.py
   *    in filter_rules_by_supported_version() since 1.79.0
   * The last one was done because we wanted to start using Critical severity in
   * our rules without having to wait for everybody to switch to 1.76.0
   * (adding version filtering in semgrep-core was not enough; it was still crashing
   *  pysemgrep which was still doing its own rule validation via jsonschema).
   *)
  min_version : Version_info.t option;
  max_version : Version_info.t option;
  dependency_formula : dependency_formula option;
}
[@@deriving show]

(* Step mode includes rules that use search_mode and taint_mode *)
(* Later, if we keep it, we might want to make all rules have steps,
   but for the experiment this is easier to remove *)

type mode = [ search_mode | taint_mode | extract_mode | steps_mode | sca_mode ]
[@@deriving show]

(* the general type *)
type rule = mode rule_info [@@deriving show]

(* aliases *)
type t = rule [@@deriving show]
type rules = rule list [@@deriving show]
type hrules = (Rule_ID.t, t) Hashtbl.t

(* If you know your function accepts only a certain kind of rule,
 * you can use those precise types below.
 *)
type search_rule = search_mode rule_info [@@deriving show]
type taint_rule = taint_mode rule_info [@@deriving show]
type extract_rule = extract_mode rule_info [@@deriving show]
type steps_rule = steps_mode rule_info [@@deriving show]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* old: was t list -> hrules, but nice to allow for more precise hrules *)
let hrules_of_rules (rules : 'mode rule_info list) :
    (Rule_ID.t, 'mode rule_info) Hashtbl.t =
  rules |> List_.map (fun r -> (fst r.id, r)) |> Hashtbl_.hash_of_list

let partition_rules (rules : rules) :
    search_rule list * taint_rule list * extract_rule list * steps_rule list =
  let rec part_rules search taint extract step = function
    | [] -> (List.rev search, List.rev taint, List.rev extract, List.rev step)
    | r :: l -> (
        match r.mode with
        | `Search _ as s ->
            part_rules ({ r with mode = s } :: search) taint extract step l
        | `Taint _ as t ->
            part_rules search ({ r with mode = t } :: taint) extract step l
        | `Extract _ as e ->
            part_rules search taint ({ r with mode = e } :: extract) step l
        | `Steps _ as j ->
            part_rules search taint extract ({ r with mode = j } :: step) l
        | `SCA _ -> part_rules search taint extract step l)
  in

  part_rules [] [] [] [] rules

(* for informational messages *)
let show_id rule = rule.id |> fst |> Rule_ID.to_string

(*****************************************************************************)
(* Error Management *)
(*****************************************************************************)

(* This is used to let the user know which rule the engine was using when
 * a Timeout or OutOfMemory exn occured.
 * TODO: relation with Match_patterns.last_matched_rule?
 *)
let last_matched_rule : Rule_ID.t option ref = ref None

(* Those are recoverable errors; We can just skip the rules containing them.
 * TODO? put in Output_from_core.atd?
 *)
type invalid_rule_error = invalid_rule_error_kind * Rule_ID.t * Tok.t

and invalid_rule_error_kind =
  | InvalidLanguage of string (* the language string *)
  (* TODO: the Parse_info.t for InvalidPattern is not precise for now;
   * it corresponds to the start of the pattern *)
  | InvalidPattern of
      string (* pattern *)
      * Xlang.t
      * string (* exn *)
      * string list (* yaml path *)
  | InvalidRegexp of string (* PCRE error message *)
  | DeprecatedFeature of string (* e.g., pattern-where-python: *)
  | MissingPositiveTermInAnd
  | IncompatibleRule of
      Version_info.t (* this version of Semgrep *)
      * (Version_info.t option (* minimum version supported by this rule *)
        * Version_info.t option (* maximum version *))
  | MissingPlugin of string (* error message *)
  | InvalidOther of string
[@@deriving show]

(* General errors *)
type error_kind =
  | InvalidRule of invalid_rule_error
  | InvalidYaml of string * Tok.t
  | DuplicateYamlKey of string * Tok.t
  | UnparsableYamlException of string
[@@deriving show]

type rules_and_errors = rules * invalid_rule_error list

(* A small module for the type of rule errors.
   Depending on the variant, these may or may not be "recoverable" or "unrecoverable",
   where "recoverable" errors in rule parsing can simply skip the rule, whereas
   "unrecoverable" rule errors will stop engine execution.

   We make this private, because we have since decided to include a file path with
   each rule error, by intercepting the error before it exits rule parsing.

   To clean the code, we establish a singular way to create an error, that being
   `mk_error`, so that we are not too dependent on the definition of the type.
   This also lets us instantiate the `file` at a dummy value uniformly.
*)
module Error : sig
  (* same as below *)
  type error = private {
    (* Some errors are in the YAML file before we can enter a specific rule
       or it could be a rule without an ID. This is why the rule ID is
       optional. *)
    rule_id : Rule_ID.t option;
    (* helpful to have this for error message purposes, as well as
       conversion via functions like `Core_error.error_of_rule_error
    *)
    file : Fpath.t;
    kind : error_kind;
  }

  and t = error [@@deriving show]

  val mk_error : ?rule_id:Rule_ID.t option -> error_kind -> error
  val augment_with_file : Fpath.t -> error -> error
end = struct
  (* same as above *)
  type error = { rule_id : Rule_ID.t option; file : Fpath.t; kind : error_kind }
  and t = error [@@deriving show]

  (*
      You must provide a rule ID for a rule to be reported properly as an invalid
      rule. The argument is not optional because it's important to not forget to
      specify a rule ID whenever possible.
    *)
  (* It's not great that we set this temporary file, but because of how we guard
     `Parse_rule`, this should definitely be populated in any entry point
     functions other than `parse_xpattern` and `parse_fake_xpattern`.

     Alternatively, we could try to provide the `file` at each call-site of
     `mk_error` itself, such as in the `Parse_rule_helpers.env`. This is actually
     a pretty hard refactor, though, as not all of those call-sites have easy access
     to a file or an env.
     For instance, yaml_error calls Rule.Error.mk_error, but yaml_error is
     explicitly called by take_no_env. We can't thread an env through there,
     the point is that it doesn't take an env, so we would need to do a broader
     refactor. This will do to start, as I anticipate we probably won't be
     refactoring this module to add more entry points at any time.
  *)
  let mk_error ?(rule_id = None) kind = { rule_id; file = Fpath_.no_file; kind }

  (* for intercepting an error before it leaves `Parse_rule`, by augmenting it with
     the file path
  *)
  let augment_with_file file error = { error with file }
end

(*
   Determine if an error can be skipped. This is for presumably well-formed
   rules that aren't compatible with the current version of semgrep
   and shouldn't cause a failure.
*)
let is_skippable_error (kind : invalid_rule_error_kind) =
  match kind with
  | InvalidLanguage _
  | InvalidPattern _
  | InvalidRegexp _
  | DeprecatedFeature _
  | MissingPositiveTermInAnd
  | InvalidOther _ ->
      false
  | IncompatibleRule _
  | MissingPlugin _ ->
      true

(*****************************************************************************)
(* String-of *)
(*****************************************************************************)

let string_of_invalid_rule_error_kind = function
  | InvalidLanguage language -> spf "invalid language %s" language
  | InvalidRegexp message -> spf "invalid regex %s" message
  (* coupling: this is actually intercepted in
   * Semgrep_error_code.exn_to_error to generate a PatternParseError instead
   * of a RuleParseError *)
  | InvalidPattern (pattern, xlang, message, _yaml_path) ->
      spf
        "Invalid pattern for %s: %s\n\
         ----- pattern -----\n\
         %s\n\
         ----- end pattern -----\n"
        (Xlang.to_string xlang) message pattern
  | MissingPositiveTermInAnd ->
      "you need at least one positive term (not just negations or conditions)"
  | DeprecatedFeature s -> spf "deprecated feature: %s" s
  | IncompatibleRule (cur, (Some min_version, None)) ->
      spf "This rule requires upgrading Semgrep from version %s to at least %s"
        (Version_info.to_string cur)
        (Version_info.to_string min_version)
  | IncompatibleRule (cur, (None, Some max_version)) ->
      spf
        "This rule is no longer supported by Semgrep. The last compatible \
         version was %s. This version of Semgrep is %s"
        (Version_info.to_string max_version)
        (Version_info.to_string cur)
  | IncompatibleRule (cur, (Some min_version, Some max_version)) ->
      spf
        "This rule requires a version of Semgrep within [%s, %s] but we're \
         using version %s"
        (Version_info.to_string min_version)
        (Version_info.to_string max_version)
        (Version_info.to_string cur)
  | IncompatibleRule (_, (None, None)) -> assert false
  | MissingPlugin msg -> msg
  | InvalidOther s -> s

let string_of_invalid_rule_error ((kind, rule_id, pos) : invalid_rule_error) =
  spf "invalid rule %s, %s: %s"
    (Rule_ID.to_string rule_id)
    (Tok.stringpos_of_tok pos)
    (string_of_invalid_rule_error_kind kind)

let string_of_error (error : Error.t) : string =
  match error.kind with
  | InvalidRule x -> string_of_invalid_rule_error x
  | InvalidYaml (msg, pos) ->
      spf "invalid YAML, %s: %s" (Tok.stringpos_of_tok pos) msg
  | DuplicateYamlKey (key, pos) ->
      spf "invalid YAML, %s: duplicate key %S" (Tok.stringpos_of_tok pos) key
  | UnparsableYamlException s ->
      (* TODO: what's the string s? *)
      spf "unparsable YAML: %s" s

(*****************************************************************************)
(* Visitor/extractor *)
(*****************************************************************************)
(* currently used in Check_rule.ml metachecker *)
(* OK, this is only a little disgusting, but...
   Evaluation order means that we will only visit children after parents.
   So we keep a reference cell around, and set it to true whenever we descend
   under an inside.
   That way, pattern leaves underneath an Inside/Anywhere will properly be
   paired with a true boolean.
*)
let visit_new_formula func formula =
  let bref = ref false in
  let rec visit_new_formula func formula =
    match formula.f with
    | P p -> func p ~inside:!bref
    | Anywhere (_, formula)
    | Inside (_, formula) ->
        Common.save_excursion bref true (fun () ->
            visit_new_formula func formula)
    | Not (_, x) -> visit_new_formula func x
    | Or (_, xs)
    | And (_, xs) ->
        xs |> List.iter (visit_new_formula func)
  in
  visit_new_formula func formula

(* used by the metachecker for precise error location *)
let tok_of_formula = function
  | And (t, _)
  | Or (t, _)
  | Inside (t, _)
  | Anywhere (t, _)
  | Not (t, _) ->
      t
  | P p -> snd p.pstr

let kind_of_formula = function
  | P _ -> "pattern"
  | Or _
  | And _
  | Inside _
  | Anywhere _
  | Not _ ->
      "formula"

let rec formula_of_mode (mode : mode) =
  match mode with
  | `Search formula -> [ formula ]
  | `Taint { sources = _, sources; sanitizers; sinks = _, sinks; propagators }
    ->
      List_.map (fun src -> src.source_formula) sources
      @ (match sanitizers with
        | None -> []
        | Some (_, sanitizers) ->
            List_.map (fun sanitizer -> sanitizer.sanitizer_formula) sanitizers)
      @ List_.map (fun sink -> sink.sink_formula) sinks
      @ List_.map (fun prop -> prop.propagator_formula) propagators
  | `Extract { formula; extract = _; _ } -> [ formula ]
  | `Steps steps ->
      List.concat_map
        (fun step -> formula_of_mode (step.step_mode :> mode))
        steps
  | `SCA _ -> []

let xpatterns_of_rule rule =
  let formulae = formula_of_mode rule.mode in
  let xpat_store = ref [] in
  let visit xpat ~inside:_ = xpat_store := xpat :: !xpat_store in
  List.iter (visit_new_formula visit) formulae;
  !xpat_store

let mk_formula ?(fix = None) ?(focus = []) ?(conditions = []) ?(as_ = None) kind
    =
  { f = kind; focus; conditions; fix; as_ }

let f kind = mk_formula kind

(*****************************************************************************)
(* Converters *)
(*****************************************************************************)

let selector_and_analyzer_of_xlang (xlang : Xlang.t) :
    Target_selector.t option * Xlang.t =
  match xlang with
  | LRegex
  | LAliengrep
  | LSpacegrep ->
      (None, xlang)
  | L (lang, other_langs) -> (Some (lang :: other_langs), xlang)

(* return list of "positive" x list of Not *)
let split_and (xs : formula list) : formula list * (tok * formula) list =
  xs
  |> Either_.partition (fun e ->
         match e.f with
         (* positives *)
         | P _
         | And _
         | Inside _
         | Anywhere _
         | Or _ ->
             Left e
         (* negatives *)
         | Not (tok, f) -> Right (tok, f))

(* create a fake rule when we only have a pattern and language.
 * This is used when someone calls `semgrep -e print -l python`
 *)

let rule_of_formula ?(fix = None) (xlang : Xlang.t) (formula : formula) : rule =
  let fk = Tok.unsafe_fake_tok "" in
  let target_selector, target_analyzer = selector_and_analyzer_of_xlang xlang in
  {
    id = (Rule_ID.dash_e, fk);
    mode = `Search formula;
    fix;
    (* default values *)
    min_version = None;
    max_version = None;
    message =
      (match formula with
      | { f = P xpat; focus = []; conditions = []; fix = None; as_ = None } ->
          fst xpat.Xpattern.pstr
      | _ -> "simple search rule");
    severity = `Error;
    target_selector;
    target_analyzer;
    options = None;
    equivalences = None;
    fix_regexp = None;
    paths = None;
    metadata = None;
    validators = None;
    product = `SAST;
    dependency_formula = None;
  }

let rule_of_xpattern ?(fix = None) (xlang : Xlang.t) (xpat : Xpattern.t) : rule
    =
  rule_of_formula xlang ~fix (f (P xpat))

(* TODO(dinosaure): Currently, on the Python side, we remove the metadatas and
   serialise the rule into JSON format, then produce the hash from this
   serialisation. However, there is no way (yet?) to serialise OCaml [Rule.t]s
   into JSON format. It exists, however, a path where we should be able to
   serialize a [Rule.t] via [ppx] (or by hands). It requires more work because
   we must have a way to serialize all types required by [Rule.t] but the
   propagation of a [ppx] such as [ppx_deriving.show] demonstrates that it's
   possible to implement such function.

   Actually, we tried to use [Marshal] and produce a hash from the /marshalled/
   output but [Rule.t] contains some custom blocks that the [Marshal] module can
   not handle.

   Currently, we did the choice to **only** hash the [ID.t] of the given rule
   which is clearly not enough comparing to the Python code. But, again, we can
   improve that by serialize everything and compute a hash from it. *)
let sha256_of_rule rule =
  Digestif.SHA256.digest_string (Rule_ID.to_string (fst rule.id))
