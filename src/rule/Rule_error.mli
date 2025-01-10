type invalid_rule = invalid_rule_kind * Rule_ID.t * Tok.t

and invalid_rule_kind =
  | InvalidLanguage of string (* the language string *)
  | InvalidPattern of
      string (* pattern *)
      * Analyzer.t
      * string (* exn *)
      * string list (* yaml path *)
  | InvalidRegexp of string (* PCRE error message *)
  | DeprecatedFeature of string (* e.g., pattern-where-python: *)
  | MissingPositiveTermInAnd
  | IncompatibleRule of
      Semver_.t (* this version of Semgrep *)
      * (Semver_.t option (* minimum version supported by this rule *)
        * Semver_.t option (* maximum version *))
  | MissingPlugin of string (* error message *)
  | InvalidOther of string
[@@deriving show]

type rules_and_invalid = Rule.rules * invalid_rule list

(* General errors *)
type error_kind =
  | InvalidRule of invalid_rule
  | InvalidYaml of string * Tok.t
  | DuplicateYamlKey of string * Tok.t
  | UnparsableYamlException of string
[@@deriving show]

(* Depending on the variant, these may or may not be "recoverable" or
   "unrecoverable", where "recoverable" errors in rule parsing can simply skip
   the rule, whereas "unrecoverable" rule errors will stop engine execution.

   We make this private, because we have since decided to include a file path
   with each rule error, by intercepting the error before it exits rule parsing.

   To clean the code, we establish a singular way to create an error, that being
   `mk_error`, so that we are not too dependent on the definition of the type.
   This also lets us instantiate the `file` at a dummy value uniformly.
*)

type t = private {
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
[@@deriving show]

(* builder *)
val mk_error : ?rule_id:Rule_ID.t -> error_kind -> t
val augment_with_file : Fpath.t -> t -> t

(* filter *)
val is_skippable_error : invalid_rule_kind -> bool

(* string of *)
val string_of_error : t -> string
val string_of_invalid_rule : invalid_rule -> string
val string_of_invalid_rule_kind : invalid_rule_kind -> string
