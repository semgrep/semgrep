(* Yoann Padioleau
 *
 * Copyright (C) 2019-2022 r2c
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
module G = AST_generic
module MV = Metavariable

let logger = Logging.get_logger [ __MODULE__ ]

open Ppx_hash_lib.Std.Hash.Builtin

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Data structures to represent a Semgrep rule (=~ AST of a rule).
 *
 * See also Mini_rule.ml where formula and many other features disappear.
 *)

(*****************************************************************************)
(* Position information *)
(*****************************************************************************)

(* This is similar to what we do in AST_generic to get precise
 * error location when a rule is malformed.
 *)
type tok = AST_generic.tok [@@deriving show, eq, hash]
type 'a wrap = 'a AST_generic.wrap [@@deriving show, eq, hash]

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
 *)
type formula =
  | P of Xpattern.t (* a leaf pattern *)
  | And of tok * conjunction
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

(* Represents all of the metavariables that are being focused by a single
   `focus-metavariable`. *)
and focus_mv_list = tok * MV.mvar list

(* The conjunction must contain at least
 * one positive "term" (unless it's inside a CondNestedFormula, in which
 * case there is not such a restriction).
 * See also split_and().
 *)
and conjunction = {
  (* pattern-inside:'s and pattern:'s *)
  conjuncts : formula list;
  (* metavariable-xyz:'s *)
  conditions : (tok * metavar_cond) list;
  (* focus-metavariable:'s *)
  focus : focus_mv_list list;
}

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
  | CondAnalysis of MV.mvar * metavar_analysis_kind
  | CondNestedFormula of MV.mvar * Xlang.t option * formula

and metavar_analysis_kind = CondEntropy | CondReDoS
[@@deriving show, eq, hash]

type paths = {
  (* not regexp but globs *)
  include_ : string list;
  exclude : string list;
}
[@@deriving show]

(*****************************************************************************)
(* Taint-specific types *)
(*****************************************************************************)

(* The sources/sanitizers/sinks used to be a simple 'formula list',
 * but with taint labels things are bit more complicated.
 *)
type taint_spec = {
  sources : tok * taint_source list;
  sanitizers : taint_sanitizer list;
  sinks : tok * taint_sink list;
  propagators : taint_propagator list;
}

and taint_source = {
  source_formula : formula;
  source_by_side_effect : bool;
  label : string;
      (* The label to attach to the data.
       * Alt: We could have an optional label instead, allow taint that is not
       * labeled, and allow sinks that work for any kind of taint? *)
  source_requires : AST_generic.expr;
      (* A Boolean expression over taint labels, using Python syntax.
       * The operators allowed are 'not', 'or', and 'and'. The expression is
       * evaluated using the `Eval_generic` machinery.
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
  sanitizer_formula : formula;
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
  sink_requires : tok * AST_generic.expr option;
      (* A Boolean expression over taint labels. See also 'taint_source'.
       * The sink will only trigger a finding if the data that reaches it
       * has a set of labels attached that satisfies the 'requires'.
       *)
}

(* e.g. if we want to specify that adding tainted data to a `HashMap` makes
 *  the `HashMap` tainted too, then "formula" could be `(HashMap $H).add($X)`,
 * with "from" being `$X` and "to" being `$H`. So if `$X` is tainted then `$H`
 * will also be marked as tainted.
 *)
and taint_propagator = {
  propagator_formula : formula;
  propagator_by_side_effect : bool;
  from : MV.mvar wrap;
  to_ : MV.mvar wrap;
  propagator_requires : AST_generic.expr;
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
let default_source_requires tok = G.L (G.Bool (true, tok)) |> G.e
let default_propagator_requires tok = G.L (G.Bool (true, tok)) |> G.e

let get_sink_requires { sink_requires = tok, expr; _ } =
  match expr with
  | None -> G.N (G.Id ((default_source_label, tok), G.empty_id_info ())) |> G.e
  | Some expr -> expr

(*****************************************************************************)
(* Extract mode (semgrep as a preprocessor) *)
(*****************************************************************************)

type extract_spec = {
  formula : formula;
  reduce : extract_reduction;
  dst_lang : Xlang.t;
  (* e.g., $...BODY, $CMD *)
  extract : MV.mvar;
  transform : extract_transform;
}

(* Method to combine extracted ranges within a file:
    - either treat them as separate files; or
    - concatentate them together
*)
and extract_reduction = Separate | Concat [@@deriving show]

(* Method to transform extracted content:
    - either treat them as a raw string; or
    - transform JSON array into a raw string
*)
and extract_transform = NoTransform | Unquote | ConcatJsonArray
[@@deriving show]

(*****************************************************************************)
(* Join mode *)
(*****************************************************************************)

type mode_for_join = Search of formula | Taint of taint_spec [@@deriving show]

type join_info = {
  formula : mode_for_join;
  languages : Xlang.t;
  paths : paths option;
}
[@@deriving show]

type join_spec = join_info list [@@deriving show]

(*****************************************************************************)
(* The rule *)
(*****************************************************************************)

type rule_id = string [@@deriving show]

type 'mode rule_info = {
  (* MANDATORY fields *)
  id : rule_id wrap;
  mode : 'mode;
  message : string; (* Currently a dummy value for extract mode rules *)
  severity : severity; (* Currently a dummy value for extract mode rules *)
  languages : Xlang.t;
  (* OPTIONAL fields *)
  options : Config_semgrep.t option;
  (* deprecated? todo: parse them *)
  equivalences : string list option;
  fix : string option;
  fix_regexp : (Xpattern.regexp_string * int option * string) option;
  paths : paths option;
  (* ex: [("owasp", "A1: Injection")] but can be anything *)
  metadata : JSON.t option;
}

(* TODO? just reuse Error_code.severity *)
and severity = Error | Warning | Info | Inventory | Experiment
[@@deriving show]

(* Polymorhic variants used to improve type checking of rules (see below) *)
type search_mode = [ `Search of formula ] [@@deriving show]
type taint_mode = [ `Taint of taint_spec ] [@@deriving show]
type extract_mode = [ `Extract of extract_spec ] [@@deriving show]
type join_mode = [ `Join of join_spec ] [@@deriving show]

type mode = [ search_mode | taint_mode | extract_mode | join_mode ]
[@@deriving show]

(* If you know your function accepts only a certain kind of rule,
 * you can use those precise types below.
 *)
type search_rule = search_mode rule_info [@@deriving show]
type taint_rule = taint_mode rule_info [@@deriving show]
type extract_rule = extract_mode rule_info [@@deriving show]
type join_rule = join_mode rule_info [@@deriving show]

(* the general type *)
type rule = mode rule_info [@@deriving show]

(* aliases *)
type t = rule [@@deriving show]
type rules = rule list [@@deriving show]
type hrules = (rule_id, t) Hashtbl.t

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let hrules_of_rules (rules : t list) : hrules =
  rules |> Common.map (fun r -> (fst r.id, r)) |> Common.hash_of_list

let partition_rules (rules : rules) :
    search_rule list * taint_rule list * extract_rule list * join_rule list =
  let rec part_rules search taint extract join = function
    | [] -> (List.rev search, List.rev taint, List.rev extract, List.rev join)
    | r :: l -> (
        match r.mode with
        | `Search _ as s ->
            part_rules ({ r with mode = s } :: search) taint extract join l
        | `Taint _ as t ->
            part_rules search ({ r with mode = t } :: taint) extract join l
        | `Extract _ as e ->
            part_rules search taint ({ r with mode = e } :: extract) join l
        | `Join _ as j ->
            part_rules search taint extract ({ r with mode = j } :: join) l)
  in
  part_rules [] [] [] [] rules

(*****************************************************************************)
(* Error Management *)
(*****************************************************************************)

(* This is used to let the user know which rule the engine was using when
 * a Timeout or OutOfMemory exn occured.
 *)
let last_matched_rule : rule_id option ref = ref None

(* Those are recoverable errors; We can just skip the rules containing them.
 * TODO? put in Output_from_core.atd?
 *)
type invalid_rule_error = invalid_rule_error_kind * rule_id * Tok.t

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
  | InvalidOther of string
[@@deriving show]

(* General errors *)
type error =
  | InvalidRule of invalid_rule_error
  | InvalidYaml of string * Tok.t
  | DuplicateYamlKey of string * Tok.t
  | UnparsableYamlException of string

(* can't use Error because it's used for severity *)
exception Err of error

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
  | InvalidOther s -> s

let string_of_invalid_rule_error ((kind, rule_id, pos) : invalid_rule_error) =
  spf "invalid rule %s, %s: %s" rule_id (Tok.stringpos_of_tok pos)
    (string_of_invalid_rule_error_kind kind)

let string_of_error (error : error) : string =
  match error with
  | InvalidRule x -> string_of_invalid_rule_error x
  | InvalidYaml (msg, pos) ->
      spf "invalid YAML, %s: %s" (Tok.stringpos_of_tok pos) msg
  | DuplicateYamlKey (key, pos) ->
      spf "invalid YAML, %s: duplicate key %S" (Tok.stringpos_of_tok pos) key
  | UnparsableYamlException s ->
      (* TODO: what's the string s? *)
      spf "unparsable YAML: %s" s

(*
   Exception printers for Printexc.to_string.
*)
let opt_string_of_exn (exn : exn) =
  match exn with
  | Err x -> Some (string_of_error x)
  | _else_ -> None

(* To be called by the application's main().
 * TODO? why not evaluate it now like let () = Printexc.register_printer ...?
 *)
let register_exception_printer () = Printexc.register_printer opt_string_of_exn

(*****************************************************************************)
(* Visitor/extractor *)
(*****************************************************************************)
(* currently used in Check_rule.ml metachecker *)
(* OK, this is only a little disgusting, but...
   Evaluation order means that we will only visit children after parents.
   So we keep a reference cell around, and set it to true whenever we descend
   under an inside.
   That way, pattern leaves underneath an Inside will properly be paired with
   a true boolean.
*)
let visit_new_formula f formula =
  let bref = ref false in
  let rec visit_new_formula f formula =
    match formula with
    | P p -> f p !bref
    | Inside (_, formula) ->
        Common.save_excursion bref true (fun () -> visit_new_formula f formula)
    | Not (_, x) -> visit_new_formula f x
    | Or (_, xs)
    | And (_, { conjuncts = xs; _ }) ->
        xs |> List.iter (visit_new_formula f)
  in
  visit_new_formula f formula

(* used by the metachecker for precise error location *)
let tok_of_formula = function
  | And (t, _) -> t
  | Or (t, _)
  | Not (t, _) ->
      t
  | P p -> snd p.pstr
  | Inside (t, _) -> t

let kind_of_formula = function
  | P _ -> "pattern"
  | Or _
  | And _
  | Inside _
  | Not _ ->
      "formula"

(*****************************************************************************)
(* Converters *)
(*****************************************************************************)

(* return list of "positive" x list of Not *)
let split_and (xs : formula list) : formula list * (tok * formula) list =
  xs
  |> Common.partition_either (fun e ->
         match e with
         (* positives *)
         | P _
         | And _
         | Inside _
         | Or _ ->
             Left e
         (* negatives *)
         | Not (tok, f) -> Right (tok, f))

(* create a fake rule when we only have a pattern and language.
 * This is used when someone calls `semgrep -e print -l python`
 *)
let rule_of_xpattern (xlang : Xlang.t) (xpat : Xpattern.t) : rule =
  let fk = Tok.unsafe_fake_tok "" in
  {
    id = ("-e", fk);
    mode = `Search (P xpat);
    (* alt: could put xpat.pstr for the message *)
    message = "";
    severity = Error;
    languages = xlang;
    options = None;
    equivalences = None;
    fix = None;
    fix_regexp = None;
    paths = None;
    metadata = None;
  }
