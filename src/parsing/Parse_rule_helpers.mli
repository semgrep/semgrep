(*
   Types and functions for translating YAML and specifically a Semgrep rule
   into something else.
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type key = string Rule.wrap

(* TODO: make this definition private or completely opaque? *)
type env = {
  id : Rule_ID.t;
  target_analyzer : Analyzer.t;
  in_metavariable_pattern : bool;
  path : string list;
  options_key : key option;
  options : Rule_options_t.t option;
}

type dict = private {
  (* Do not use directly Hashtbl.find_opt on this field!
   * Use instead dict_take_opt() or take_opt_no_env() otherwise
   * warn_if_remaining_unparsed_fields() will not work.
   * This is mutated!
   *)
  h : (string, key * AST_generic.expr) Hashtbl.t;
  first_tok : Rule.tok;
}

(*****************************************************************************)
(* Error Management *)
(*****************************************************************************)

(* will return InvalidRule (InvalidOther _) errors *)
val error : Rule_ID.t -> Tok.t -> string -> ('a, Rule_error.t) result
val error_at_key : Rule_ID.t -> key -> string -> ('a, Rule_error.t) result

(* will return InvalidYaml error *)
val yaml_error : Tok.t -> string -> ('a, Rule_error.t) result
val yaml_error_at_expr : AST_generic.expr -> string -> ('a, Rule_error.t) result
val yaml_error_at_key : key -> string -> ('a, Rule_error.t) result

val error_at_opt_key :
  Rule_ID.t -> key option -> string -> ('a, Rule_error.t) result

val error_at_expr :
  Rule_ID.t -> AST_generic.expr -> string -> ('a, Rule_error.t) result

val try_and_raise_invalid_pattern_if_error :
  env ->
  string * Tok.t ->
  (unit -> ('a, Rule_error.t) Result.t) ->
  ('a, Rule_error.t) Result.t

val parse_pattern_with_rule_error :
  env ->
  string * Tok.t ->
  ?rule_options:Rule_options_t.t ->
  Lang.t ->
  string ->
  (Pattern.t, Rule_error.t) result

val check_that_dict_is_empty : dict -> (unit, Rule_error.t) Result.t
val warn_if_remaining_unparsed_fields : Rule_ID.t -> dict -> unit

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

val generic_to_json :
  Rule_ID.t -> key -> AST_generic.expr -> (JSON.t, Rule_error.t) Result.t

val read_string_wrap :
  AST_generic.expr_kind -> (string * AST_generic.tok) option

(*****************************************************************************)
(* Dict helper methods *)
(*****************************************************************************)

val dict_take_opt : dict -> string -> (key * AST_generic.expr) option

(* Look up an optional key in a dict and remove it. *)
val take_opt :
  dict ->
  env ->
  (env -> key -> AST_generic.expr -> ('a, Rule_error.t) Result.t) ->
  string ->
  ('a option, Rule_error.t) Result.t

(* Look up a key in a dict and remove it.
   If the key doesn't exist:
   - if no default is provided, an error is created,
   - otherwise, the default is returned.
*)
val take_key :
  ?default:'a ->
  dict ->
  env ->
  (env -> key -> AST_generic.expr -> ('a, Rule_error.t) Result.t) ->
  string ->
  ('a, Rule_error.t) Result.t

val fold_dict :
  (string -> key * AST_generic.expr -> 'a -> 'a) -> dict -> 'a -> 'a

(*****************************************************************************)
(* Parsing methods for before env is created *)
(*****************************************************************************)

val take_opt_no_env :
  dict ->
  (key -> AST_generic.expr -> ('a, Rule_error.t) Result.t) ->
  string ->
  ('a option, Rule_error.t) Result.t

val take_no_env :
  dict ->
  (key -> AST_generic.expr -> ('a, Rule_error.t) Result.t) ->
  string ->
  ('a, Rule_error.t) Result.t

val parse_dict_no_env :
  string -> AST_generic.expr -> (dict, Rule_error.t) Result.t

val parse_string_wrap_no_env :
  key -> AST_generic.expr -> (string * Tok.t, Rule_error.t) Result.t

val parse_rule_id_no_env :
  key -> AST_generic.expr -> (Rule_ID.t * Tok.t, Rule_error.t) Result.t

val parse_string_wrap_list_no_env :
  key -> AST_generic.expr -> ((string * Tok.t) list, Rule_error.t) Result.t

(*****************************************************************************)
(* Parsers for basic types *)
(*****************************************************************************)

(* used to be called 'yaml_to_dict', renamed for consistency *)
val parse_dict : env -> key -> AST_generic.expr -> (dict, Rule_error.t) Result.t

val parse_list :
  env ->
  key ->
  (env -> AST_generic.expr -> ('a, Rule_error.t) Result.t) ->
  AST_generic.expr ->
  ('a list, Rule_error.t) Result.t

val parse_listi :
  env ->
  key ->
  (env -> AST_generic.expr -> ('a, Rule_error.t) Result.t) ->
  AST_generic.expr ->
  ('a list, Rule_error.t) Result.t

val parse_string_wrap :
  env ->
  key ->
  AST_generic.expr ->
  (string * AST_generic.tok, Rule_error.t) result

val parse_string :
  env -> key -> AST_generic.expr -> (string, Rule_error.t) result

val parse_bool : env -> key -> AST_generic.expr -> (bool, Rule_error.t) result

val parse_int :
  env -> key -> AST_generic.expr -> (Parsed_int.t, Rule_error.t) result

val parse_int_strict :
  env -> key -> AST_generic.expr -> (int, Rule_error.t) result

val parse_string_wrap_list :
  (env -> key -> string -> ('a, Rule_error.t) Result.t) ->
  env ->
  key ->
  AST_generic.expr ->
  (('a * AST_generic.tok) list, Rule_error.t) Result.t

(* This handles sum types using the same flexible syntax as used
   in YAML by others (e.g. GitHub Actions workflow config files). It is
   relatively manageable with JSON Schema and less so with ATD but doable
   using so-called adapters. They translate to OCaml variants.

   This first form is compact, being just a string ('bar'):

     foo: bar

   It is a shorthand for:

     foo:
       kind: bar

   The long form allows specifying properties, options, etc.:

     foo:
       kind: bar
       description: blah
*)
val parse_variant :
  ?kind_field_name:string ->
  env ->
  key ->
  AST_generic.expr ->
  (string * dict option, Rule_error.t) result

(*****************************************************************************)
(* Parsers for basic types *)
(*****************************************************************************)

val parse_rule_id :
  env -> key -> AST_generic.expr -> (Rule_ID.t * Tok.t, Rule_error.t) Result.t

val parse_http_method :
  env ->
  key ->
  AST_generic.expr ->
  ([> `DELETE | `GET | `HEAD | `POST | `PUT ], Rule_error.t) Result.t

val parse_auth :
  env -> key -> AST_generic.expr -> (Rule.auth, Rule_error.t) Result.t

val parse_str_or_dict :
  env ->
  AST_generic.expr ->
  ((AST_generic.ident, dict) Either.t, Rule_error.t) Result.t

(*****************************************************************************)
(* Other *)
(*****************************************************************************)

val parse_regexp : env -> string * Tok.t -> (string, Rule_error.t) result

val parse_python_expression :
  env -> key -> string -> (AST_generic.expr, Rule_error.t) result

val parse_metavar_cond :
  env -> key -> string -> (AST_generic.expr, Rule_error.t) result
