(* Yoann Padioleau, Emma Jin
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
open File.Operators
module J = JSON
module FT = File_type
module R = Rule
module XP = Xpattern
module MR = Mini_rule
module G = AST_generic
module Set = Set_
module MV = Metavariable

let logger = Logging.get_logger [ __MODULE__ ]

(* TODO? make it a flag? *)
let use_ojsonnet = true

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Parsing a Semgrep rule, including complex pattern formulas.
 *
 * See also the JSON schema for a rule in rule_schema_v1.yaml.
 *
 * history: we used to parse a semgrep rule by simply using the basic API of
 * the OCaml 'yaml' library. This API allows converting a yaml file into
 * a simple and compact JSON.t value.
 * However, this JSON.t value did not contain any location information, which
 * made it hard to report errors in a YAML rule. This is why we switched
 * to the low-level API of the 'yaml' library that returns a stream
 * of tokens with location information. We actually used first that low-level
 * API to return the generic AST of a yaml file, to add support for
 * YAML in semgrep (allowing semgrep rules on any YAML files).
 * See the Yaml_to_generic.parse_rule function. We then (ab)used this function
 * to also parse a semgrep rule (which is a yaml file) in this file.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type key = string R.wrap

type env = {
  (* id of the current rule (needed by some exns) *)
  id : Rule_ID.t;
  (* analyzer of the current rule (needed by parse_pattern) *)
  target_analyzer : Xlang.t;
  (* whether we are underneath a `metavariable-pattern` *)
  in_metavariable_pattern : bool;
  (* emma: save the path within the yaml file for each pattern
   * (this will allow us to later report errors in playground basic mode)
   *)
  path : string list;
  (* for producing decent error messages when checking the validity
     of the options section that was parsed as a whole from JSON *)
  options_key : key option;
  options : Rule_options_t.t option;
}

(* Parsing generic dictionaries creates a mutable Hashtbl and consumes the
 * fields as they are processed.
 * todo? use a Map instead?
 *)
type dict = {
  (* !this is mutated! *)
  h : (string, key * AST_generic.expr) Hashtbl.t;
  (* for error reports on missing fields *)
  first_tok : R.tok;
}

(*****************************************************************************)
(* Error Management *)
(*****************************************************************************)

let yaml_error t s = Rule.raise_error None (InvalidYaml (s, t))

let yaml_error_at_expr (e : G.expr) s =
  yaml_error (AST_generic_helpers.first_info_of_any (G.E e)) s

let yaml_error_at_key (key : key) s = yaml_error (snd key) s

let error rule_id t s =
  Rule.raise_error (Some rule_id) (InvalidRule (InvalidOther s, rule_id, t))

let error_at_key rule_id (key : key) s = error rule_id (snd key) s

let error_at_opt_key rule_id (opt_key : key option) s =
  match opt_key with
  | Some key -> error_at_key rule_id key s
  | None -> failwith (spf "Internal error (no location provided): %s" s)

let error_at_expr rule_id (e : G.expr) s =
  error rule_id (AST_generic_helpers.first_info_of_any (G.E e)) s

let pcre_error_to_string s exn =
  let message =
    match exn with
    | Pcre.Partial -> "String only matched the pattern partially"
    | BadPartial ->
        "Pattern contains items that cannot be used together with partial \
         matching."
    | BadPattern (msg, pos) -> spf "%s at position %d" msg pos
    | BadUTF8 -> "UTF8 string being matched is invalid"
    | BadUTF8Offset ->
        "Gets raised when a UTF8 string being matched with offset is invalid."
    | MatchLimit ->
        "Maximum allowed number of match attempts with\n\
        \                      backtracking or recursion is reached during \
         matching."
    | RecursionLimit -> "Recursion limit reached"
    | WorkspaceSize -> "Workspace array size reached"
    | InternalError msg -> spf "Internal error: %s" msg
  in
  spf "'%s': %s" s message

let try_and_raise_invalid_pattern_if_error (env : env) (s, t) f =
  try f () with
  | (Time_limit.Timeout _ | UnixExit _) as e -> Exception.catch_and_reraise e
  (* TODO: capture and adjust pos of parsing error exns instead of using [t] *)
  | exn ->
      let error_kind : R.invalid_rule_error_kind =
        InvalidPattern (s, env.target_analyzer, Common.exn_to_s exn, env.path)
      in
      Rule.raise_error (Some env.id) (InvalidRule (error_kind, env.id, t))

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Why do we need this generic_to_json function? Why would we want to convert
 * to JSON when we actually did lots of work to convert the YAML/JSON
 * in the generic AST to get proper error location. This is because
 * the 'metadata' field in Rule.ml is JSON.
 *)
let generic_to_json rule_id (key : key) ast =
  let rec aux x =
    match x.G.e with
    | G.L (Null _) -> J.Null
    | G.L (Bool (b, _)) -> J.Bool b
    | G.L (Float (Some f, _)) -> J.Float f
    | G.L (Int (Some i, _)) -> J.Int i
    | G.L (String (_, (s, _), _)) ->
        (* should use the unescaped string *)
        J.String s
    | G.Container (Array, (_, xs, _)) -> J.Array (xs |> Common.map aux)
    | G.Container (Dict, (_, xs, _)) ->
        J.Object
          (xs
          |> Common.map (fun x ->
                 match x.G.e with
                 | G.Container
                     ( G.Tuple,
                       (_, [ { e = L (String (_, (k, _), _)); _ }; v ], _) ) ->
                     (* should use the unescaped string *)
                     (k, aux v)
                 | _ ->
                     error_at_expr rule_id x
                       ("Expected key/value pair in " ^ fst key ^ " dictionary"))
          )
    | G.Alias (_alias, e) -> aux e
    | _ ->
        Common.pr2 (G.show_expr_kind x.G.e);
        error_at_expr rule_id x "Unexpected generic representation of yaml"
  in
  aux ast

let read_string_wrap e =
  match e with
  | G.L (String (_, (value, t), _)) ->
      (* should use the unescaped string *)
      Some (value, t)
  | G.N (Id ((value, t), _)) -> Some (value, t)
  | _ -> None

(*****************************************************************************)
(* Dict helper methods *)
(*****************************************************************************)

let yaml_to_dict_helper opt_rule_id error_fun_f error_fun_d
    (enclosing : string R.wrap) (rule : G.expr) : dict =
  match rule.G.e with
  (* note that the l/r are actually populated by yaml_to_generic, even
   * though there is no proper corresponding token
   *)
  | G.Container (Dict, (l, fields, _r)) ->
      let dict = Hashtbl.create 10 in
      fields
      |> List.iter (fun field ->
             match field.G.e with
             | G.Container
                 ( G.Tuple,
                   (_, [ { e = L (String (_, (key_str, t), _)); _ }; value ], _)
                 ) ->
                 (* Those are actually silently ignored by many YAML parsers
                  * which just consider the last key/value as the final one.
                  * This was a source of bugs in semgrep rules where people
                  * thought you could enter multiple metavariables under one
                  * metavariable-regex.
                  *)
                 if Hashtbl.mem dict key_str then
                   Rule.raise_error opt_rule_id
                     (DuplicateYamlKey
                        (spf "duplicate key '%s' in dictionary" key_str, t));
                 Hashtbl.add dict key_str ((key_str, t), value)
             | _ -> error_fun_f field "Not a valid key value pair");
      { h = dict; first_tok = l }
  | _ -> error_fun_d rule ("each " ^ fst enclosing ^ " should be a dictionary")

(* Mutates the Hashtbl! *)
let take_opt (dict : dict) (env : env) (f : env -> key -> G.expr -> 'a)
    (key_str : string) : 'a option =
  Option.map
    (fun (key, value) ->
      let res = f env key value in
      Hashtbl.remove dict.h key_str;
      res)
    (Hashtbl.find_opt dict.h key_str)

(* Mutates the Hashtbl! *)
let take (dict : dict) (env : env) (f : env -> key -> G.expr -> 'a)
    (key_str : string) : 'a =
  match take_opt dict env f key_str with
  | Some res -> res
  | None -> error env.id dict.first_tok ("Missing required field " ^ key_str)

let fold_dict f dict x = Hashtbl.fold f dict.h x

let yaml_to_dict (env : env) enclosing =
  yaml_to_dict_helper (Some env.id) (error_at_expr env.id)
    (error_at_expr env.id) enclosing

(*****************************************************************************)
(* Parsing methods for before env is created *)
(*****************************************************************************)

(* Mutates the Hashtbl! *)
let take_opt_no_env (dict : dict) (f : key -> G.expr -> 'a) (key_str : string) :
    'a option =
  Option.map
    (fun (key, value) ->
      let res = f key value in
      Hashtbl.remove dict.h key_str;
      res)
    (Hashtbl.find_opt dict.h key_str)

(* Mutates the Hashtbl! *)
let take_no_env (dict : dict) (f : key -> G.expr -> 'a) (key_str : string) : 'a
    =
  match take_opt_no_env dict f key_str with
  | Some res -> res
  | None -> yaml_error dict.first_tok ("Missing required field " ^ key_str)

let yaml_to_dict_no_env =
  yaml_to_dict_helper None yaml_error_at_expr yaml_error_at_expr

let parse_string_wrap_no_env (key : key) x =
  match read_string_wrap x.G.e with
  | Some (value, t) -> (value, t)
  | None -> yaml_error_at_key key ("Expected a string value for " ^ fst key)

let parse_list_no_env (key : key) f x =
  match x.G.e with
  | G.Container (Array, (_, xs, _)) -> Common.map f xs
  | _ -> yaml_error_at_key key ("Expected a list for " ^ fst key)

let parse_string_wrap_list_no_env (key : key) e =
  let extract_string = function
    | { G.e = G.L (String (_, (value, t), _)); _ } -> (value, t)
    | _ ->
        yaml_error_at_key key
          ("Expected all values in the list to be strings for " ^ fst key)
  in
  parse_list_no_env key extract_string e

(*****************************************************************************)
(* Parsers for basic types *)
(*****************************************************************************)

let parse_string_wrap env (key : key) x =
  match read_string_wrap x.G.e with
  | Some (value, t) -> (value, t)
  | None -> error_at_key env.id key ("Expected a string value for " ^ fst key)

(* TODO: delete at some point, should use parse_string_wrap instead *)
let parse_string env (key : key) x = parse_string_wrap env key x |> fst

let method_ env (key : key) x =
  let meth = parse_string env key x in
  match meth with
  | "DELETE" -> `DELETE
  | "GET" -> `GET
  | "HEAD" -> `HEAD
  | "POST" -> `POST
  | "PUT" -> `PUT
  | _ -> error_at_key env.id key ("non-supported HTTP method: " ^ meth)

let parse_auth env (key : key) x : Rule.auth =
  let auth = yaml_to_dict env key x in
  match take auth env parse_string "type" with
  | "sigv4" ->
      let secret_access_key = take auth env parse_string "secret_access_key" in
      let access_key_id = take auth env parse_string "access_key_id" in
      let region = take auth env parse_string "region" in
      let service = take auth env parse_string "service" in
      AWS_SIGV4 { secret_access_key; access_key_id; region; service }
  | auth_ty ->
      error_at_key env.id key
        ("Unknown authorization type requested to be added: " ^ auth_ty)

let parse_list env (key : key) f x =
  match x.G.e with
  | G.Container (Array, (_, xs, _)) -> Common.map (f env) xs
  | _ -> error_at_key env.id key ("Expected a list for " ^ fst key)

let parse_listi env (key : key) f x =
  let get_component i x =
    let env = { env with path = string_of_int i :: env.path } in
    f env x
  in
  match x.G.e with
  | G.Container (Array, (_, xs, _)) -> Common.mapi get_component xs
  | _ -> error_at_key env.id key ("Expected a list for " ^ fst key)

let parse_string_wrap_list conv env (key : key) e =
  let extract_string_wrap env = function
    | { G.e = G.L (String (_, (value, t), _)); _ } -> (conv value, t)
    | _ ->
        error_at_key env.id key
          ("Expected all values in the list to be strings for " ^ fst key)
  in
  parse_list env key extract_string_wrap e

let parse_bool env (key : key) x =
  match x.G.e with
  | G.L (String (_, ("true", _), _)) -> true
  | G.L (String (_, ("false", _), _)) -> false
  | G.L (Bool (b, _)) -> b
  | _x -> error_at_key env.id key (spf "parse_bool for %s" (fst key))

let parse_int env (key : key) x =
  match x.G.e with
  | G.L (Int (Some i, _)) -> i
  | G.L (String (_, (s, _), _)) -> (
      try int_of_string s with
      | Failure _ -> error_at_key env.id key (spf "parse_int for %s" (fst key)))
  | G.L (Float (Some f, _)) ->
      let i = int_of_float f in
      if float_of_int i =*= f then i else error_at_key env.id key "not an int"
  | _x -> error_at_key env.id key (spf "parse_int for %s" (fst key))

let parse_str_or_dict env (value : G.expr) : (G.ident, dict) Either.t =
  match value.G.e with
  | G.L (String (_, (value, t), _)) ->
      (* should use the unescaped string *)
      Either.Left (value, t)
  | G.L (Float (Some n, t)) ->
      if Float.is_integer n then Left (string_of_int (Float.to_int n), t)
      else Left (string_of_float n, t)
  | G.N (Id ((value, t), _)) -> Left (value, t)
  | G.Container (Dict, _) ->
      Right (yaml_to_dict env ("<TODO>", G.fake "<TODO>") value)
  | _ ->
      error_at_expr env.id value
        "Wrong field for a pattern, expected string or dictionary"

(* env: general data about the current rule
 * key: the word `focus-metavariable` from the original rule.
 * x: the AST expression for the values in the rule that are under
 *    `focus-metavariable`.
 *)
let parse_focus_mvs env (key : key) (x : G.expr) =
  match x.e with
  | G.N (G.Id ((s, _), _))
  | G.L (String (_, (s, _), _)) ->
      [ s ]
  | G.Container (Array, (_, mvs, _)) ->
      Common.map (fun mv -> fst (parse_string_wrap env key mv)) mvs
  | _ ->
      error_at_key env.id key
        ("Expected a string or a list of strings for " ^ fst key)

(*****************************************************************************)
(* Parsers for core fields (languages:, severity:) *)
(*****************************************************************************)

let parse_language ~id ((s, t) as _lang) : Lang.t =
  match Lang.of_string_opt s with
  | None -> Rule.raise_error (Some id) (InvalidRule (InvalidLanguage s, id, t))
  | Some lang -> (
      (* Raise a rule error if a plugin (e.g. Apex) is missing. *)
      (* TODO: find a better place to check for this?
         Note that we don't want to delay this until target parsing time
         which is lazy and may not take place due to optimizations. *)
      match Parsing_plugin.check_if_missing lang with
      | Ok () -> lang
      | Error msg ->
          Rule.raise_error (Some id) (InvalidRule (MissingPlugin msg, id, t)))

(*
   This list specifies target selection and possible pattern parsers.
   Constraints:
   - target selection is either generic or a list of programming languages
     that can be detected by file extensions and other quick lookups.
   - target analysis is done by one of the generic analyzers (regex,
     spacegrep, aliengrep, ...) or by trying to parse the semgrep pattern
     as one of several programming languages.
   This decouples target selection from pattern parsing.

   TODO: note that there's a few places in this file where we use
   Xlang.of_string which allows "spacegrep" and "aliengrep" so
   this might lead to inconsistencies as here we allow just "generic".
*)
let parse_languages ~id (options : Rule_options_t.t) langs :
    Target_selector.t option * Xlang.t =
  match langs with
  | [ (("none" | "regex"), _t) ] -> (None, LRegex)
  | [ ("generic", _t) ] -> (
      (* The generic mode now uses one of two possible engines.
         For now, we keep the name "generic" for both and use an option
         to choose one engine or the other. *)
      match options.generic_engine with
      | `Spacegrep -> (None, LSpacegrep)
      | `Aliengrep -> (None, LAliengrep))
  | xs -> (
      let rule_id, _ = id in
      let langs = xs |> Common.map (parse_language ~id:rule_id) in
      match langs with
      | [] ->
          Rule.raise_error (Some rule_id)
            (InvalidRule
               (InvalidOther "we need at least one language", fst id, snd id))
      | x :: xs -> (Some langs, L (x, xs)))

let parse_severity ~id (s, t) : Rule.severity =
  match s with
  | "ERROR" -> `Error
  | "WARNING" -> `Warning
  | "INFO" -> `Info
  | "INVENTORY" -> `Inventory
  | "EXPERIMENT" -> `Experiment
  | s ->
      Rule.raise_error (Some id)
        (InvalidRule
           ( InvalidOther
               (spf "Bad severity: %s (expected ERROR, WARNING or INFO)" s),
             id,
             t ))

(*****************************************************************************)
(* Parsers for extra (metavar-xxx:, fix:, etc.) *)
(*****************************************************************************)

let parse_python_expression env key s =
  try
    let lang = Lang.Python in
    (* todo? use lang in env? *)
    match Parse_pattern.parse_pattern lang ~print_errors:false s with
    | AST_generic.E e -> e
    | _ -> error_at_key env.id key "not a Python expression"
  with
  | (Time_limit.Timeout _ | UnixExit _) as e -> Exception.catch_and_reraise e
  | exn -> error_at_key env.id key ("exn: " ^ Common.exn_to_s exn)

let parse_metavar_cond env key s = parse_python_expression env key s

let rec parse_type env key (str, tok) =
  match env.target_analyzer with
  | Xlang.L (lang, _) ->
      let str = wrap_type_expr env key lang str in
      try_and_raise_invalid_pattern_if_error env (str, tok) (fun () ->
          Parse_pattern.parse_pattern lang ~print_errors:false str)
      |> unwrap_type_expr env key lang
  | Xlang.LRegex
  | Xlang.LSpacegrep
  | Xlang.LAliengrep ->
      error_at_key env.id key
        "`type` is not supported with regex, spacegrep or aliengrep."

and wrap_type_expr env key lang str =
  match Parse_metavariable_type.wrap_type_expr lang str with
  | Some x -> x
  | None ->
      error_at_key env.id key
        ("`metavariable-type` is not supported for " ^ Lang.show lang)

and unwrap_type_expr env key lang expr =
  match Parse_metavariable_type.unwrap_type_expr lang expr with
  | Some x -> x
  | None ->
      error_at_key env.id key
        ("Failed to unwrap the type expression." ^ G.show_any expr)

let parse_regexp env (s, t) =
  (* We try to compile the regexp just to make sure it's valid, but we store
   * the raw string, see notes attached to 'Xpattern.xpattern_kind'. *)
  try
    (* calls `pcre.compile` *)
    Metavariable.mvars_of_regexp_string s
    |> List.iter (fun mvar ->
           if not (Metavariable.is_metavar_name mvar) then
             logger#warning
               "Found invalid metavariable capture group name `%s` for regexp \
                `%s` -- no binding produced"
               mvar s);
    s
  with
  | Pcre.Error exn ->
      Rule.raise_error (Some env.id)
        (InvalidRule (InvalidRegexp (pcre_error_to_string s exn), env.id, t))

let parse_fix_regex (env : env) (key : key) fields =
  let fix_regex_dict = yaml_to_dict env key fields in
  let (regex : string R.wrap) =
    take fix_regex_dict env parse_string_wrap "regex"
  in
  let (replacement : string) =
    take fix_regex_dict env parse_string "replacement"
  in
  let (count_opt : int option) =
    take_opt fix_regex_dict env parse_int "count"
  in
  (parse_regexp env regex, count_opt, replacement)

let parse_equivalences env key value =
  let parse_equivalence env equiv =
    match equiv.G.e with
    | G.Container
        ( Dict,
          ( _,
            [
              {
                e =
                  Container
                    ( Tuple,
                      ( _,
                        [
                          { e = L (String (_, ("equivalence", t), _)); _ };
                          value;
                        ],
                        _ ) );
                _;
              };
            ],
            _ ) ) ->
        parse_string env ("equivalence", t) value
    | _ ->
        error_at_expr env.id equiv
          "Expected `equivalence: $X` for each equivalences list item"
  in
  parse_list env key parse_equivalence value

let parse_paths env key value =
  let paths_dict = yaml_to_dict env key value in
  (* TODO: should imitate parse_string_wrap_list *)
  let parse_glob_list env (key : key) e =
    let extract_string env = function
      | { G.e = G.L (String (_, (value, _), _)); _ } -> (
          try (value, Glob.Parse.parse_string value) with
          | Glob.Lexer.Syntax_error _ ->
              error_at_key env.id key ("Invalid glob for " ^ fst key))
      | _ ->
          error_at_key env.id key
            ("Expected all values in the list to be globs for " ^ fst key)
    in
    parse_list env key extract_string e
  in
  let inc_opt, exc_opt =
    ( take_opt paths_dict env parse_glob_list "include",
      take_opt paths_dict env parse_glob_list "exclude" )
  in
  (* alt: we could use report_unparsed_fields(), but better to raise an error for now
     to be compatible with pysemgrep *)
  if Hashtbl.length paths_dict.h > 0 then
    error_at_key env.id key
      "Additional properties are not allowed (only 'include' and 'exclude' are \
       supported)";
  { R.require = optlist_to_list inc_opt; exclude = optlist_to_list exc_opt }

(*****************************************************************************)
(* Check the validity of the Aliengrep options *)
(*****************************************************************************)

(* Aliengrep word characters must be single-byte characters for now. *)
let word_chars_of_strings env xs =
  xs
  |> Common.map (function
       | "" -> error_at_opt_key env.id env.options_key "Empty opening brace"
       | x when String.length x =|= 1 (* = *) -> x.[0]
       | long ->
           error_at_opt_key env.id env.options_key
             (spf "Multibyte word characters aren't supported: %S" long))

(* Aliengrep brace pairs are specified as strings because ATD doesn't have
   a char type and because they could be multibyte UTF-8-encoded characters.
   For now, aliengrep only supports single-byte characters. *)
let brace_pairs_of_string_pairs env xs =
  xs
  |> Common.map (fun (open_, close) ->
         let opening_char =
           match open_ with
           | "" -> error_at_opt_key env.id env.options_key "Empty opening brace"
           | x -> x.[0]
         in
         let closing_char =
           match close with
           | "" -> error_at_opt_key env.id env.options_key "Empty closing brace"
           | x -> x.[0]
         in
         if String.length open_ > 1 then
           error_at_opt_key env.id env.options_key
             (spf "Multibyte opening braces aren't supported: %S" open_);
         if String.length close > 1 then
           error_at_opt_key env.id env.options_key
             (spf "Multibyte closing braces aren't supported: %S" close);
         (opening_char, closing_char))

let aliengrep_conf_of_options (env : env) : Aliengrep.Conf.t =
  let options = Option.value env.options ~default:Rule_options.default_config in
  let default = Aliengrep.Conf.default_multiline_conf in
  let caseless = options.generic_caseless in
  let multiline = options.generic_multiline in
  let word_chars =
    default.word_chars
    @ word_chars_of_strings env options.generic_extra_word_characters
  in
  let brackets =
    let base_set =
      match options.generic_braces with
      | None -> default.brackets
      | Some string_pairs -> brace_pairs_of_string_pairs env string_pairs
    in
    let extra_braces =
      brace_pairs_of_string_pairs env options.generic_extra_braces
    in
    base_set @ extra_braces
  in
  { caseless; multiline; word_chars; brackets }

let parse_options rule_id (key : key) value =
  let s = J.string_of_json (generic_to_json rule_id key value) in
  let options =
    Common.save_excursion Atdgen_runtime.Util.Json.unknown_field_handler
      (fun _src_loc field_name ->
        (* for forward compatibility, better to not raise an exn and just
         * ignore the new fields.
         * TODO: we should use a warning/logging infra to report
         * this in the JSON to the semgrep wrapper and user.
         *)
        (*raise (InvalidYamlException (spf "unknown option: %s" field_name))*)
        pr2 (spf "WARNING: unknown option: %s" field_name))
      (fun () -> Rule_options_j.t_of_string s)
  in
  (options, Some key)

(*****************************************************************************)
(* Parser for xpattern *)
(*****************************************************************************)

(* less: could move in a separate Parse_xpattern.ml *)
let parse_rule_xpattern env (str, tok) =
  match env.target_analyzer with
  | Xlang.L (lang, _) ->
      (* opti: parsing Semgrep patterns lazily improves speed significantly.
       * Parsing p/default goes from 13s to just 0.2s, mostly because
       * p/default contains lots of ruby rules which are currently very
       * slow to parse. Still, even if there was no Ruby rule, it's probably
       * still worth the optimization.
       * The disadvantage of parsing lazily is that
       * parse errors in the pattern are detected only later, when
       * the rule/pattern is actually needed. In practice we have pretty
       * good error management and error recovery so the error should
       * find its way to the JSON error field anyway.
       *)
      let lpat =
        lazy
          ((* we need to raise the right error *)
           try_and_raise_invalid_pattern_if_error env (str, tok) (fun () ->
               Parse_pattern.parse_pattern lang ~print_errors:false
                 ~rule_options:env.options str))
      in
      XP.mk_xpat (XP.Sem (lpat, lang)) (str, tok)
  | Xlang.LRegex ->
      XP.mk_xpat (XP.Regexp (parse_regexp env (str, tok))) (str, tok)
  | Xlang.LSpacegrep -> (
      let src = Spacegrep.Src_file.of_string str in
      match Spacegrep.Parse_pattern.of_src src with
      | Ok ast -> XP.mk_xpat (XP.Spacegrep ast) (str, tok)
      (* TODO: use R.Err exn instead? *)
      | Error err -> failwith err.msg)
  | Xlang.LAliengrep ->
      let conf = aliengrep_conf_of_options env in
      let pat = Aliengrep.Pat_compile.from_string conf str in
      XP.mk_xpat (XP.Aliengrep pat) (str, tok)

(* TODO: note that the [pattern] string and token location [t] given to us
 * by the YAML parser do not correspond exactly to the content
 * in the YAML file. If the pattern is on a single line, as in
 *    pattern: foo($X)
 * then everything is fine, but if it's on multiple lines as in
 *    pattern: |
 *       foo($X);
 *       bar($X);
 * The pattern string will contain "foo($X);\nbar($X);\n" without any
 * indentation and the token location [t] will actually be the location
 * of the leading "|", so we need to recompute location by reparsing
 * the YAML file and look at the indentation there.
 *
 * TODO: adjust pos with Map_AST.mk_fix_token_locations and
 * Parse_info.adjust_info_wrt_base t
 *)

let parse_xpattern_expr env e =
  let s, t =
    match read_string_wrap e.G.e with
    | Some (s, t) -> (s, t)
    | None ->
        error_at_expr env.id e
          ("Expected a string value for " ^ Rule_ID.to_string env.id)
  in

  (* emma: This is for later, but note that start and end_ are currently the
   * same (each pattern is only associated with one token). This might be
   * really annoying to change (we need to compute an accurate end_, but
   * the string given to us by the yaml parser has tabs removed).
   * Will include a note to this effect when I make my
   * "add ranges to patterns" PR.
   *)

  (* let start, end_ = Visitor_AST.range_of_any (G.E e) in
     let _s_range =
       (PI.mk_info_of_loc start, PI.mk_info_of_loc end_)
       (* TODO put in *)
     in *)
  try_and_raise_invalid_pattern_if_error env (s, t) (fun () ->
      parse_rule_xpattern env (s, t))

(*****************************************************************************)
(* Parser for old (but current) formula *)
(*****************************************************************************)

(* This was in Rule.ml before and represent the old (but still current)
 * way to write metavariable conditions.
 *)
(* extra conditions, usually on metavariable content *)
type extra =
  | MetavarRegexp of MV.mvar * Xpattern.regexp_string * bool
  | MetavarType of MV.mvar * Xlang.t option * string list * G.type_ list
  | MetavarPattern of MV.mvar * Xlang.t option * Rule.formula
  | MetavarComparison of metavariable_comparison
  | MetavarAnalysis of MV.mvar * Rule.metavar_analysis_kind
(* old: | PatWherePython of string, but it was too dangerous.
 * MetavarComparison is not as powerful, but safer.
 *)

(* See also engine/Eval_generic.ml *)
and metavariable_comparison = {
  metavariable : MV.mvar option;
  comparison : AST_generic.expr;
  (* I don't think those are really needed; they can be inferred
   * from the values *)
  strip : bool option;
  base : int option;
}

(* Substitutes `$MVAR` with `int($MVAR)` in cond. *)
(* This now changes all such metavariables. We expect in most cases there should
   just be one, anyways.
*)
let rewrite_metavar_comparison_strip cond =
  let visitor =
    object (_self : 'self)
      inherit [_] AST_generic.map_legacy as super

      method! visit_expr env e =
        (* apply on children *)
        let e = super#visit_expr env e in
        match e.G.e with
        | G.N (G.Id ((s, tok), _idinfo)) when Metavariable.is_metavar_name s ->
            let py_int = G.Id (("int", tok), G.empty_id_info ()) in
            G.Call (G.N py_int |> G.e, Tok.unsafe_fake_bracket [ G.Arg e ])
            |> G.e
        | _ -> e
    end
  in
  visitor#visit_expr () cond

(* TODO: Old stuff that we can't kill yet. *)
let find_formula_old env (rule_dict : dict) : key * G.expr =
  let find key_str = Hashtbl.find_opt rule_dict.h key_str in
  match
    ( find "pattern",
      find "pattern-either",
      find "patterns",
      find "pattern-regex" )
  with
  | None, None, None, None ->
      error env.id rule_dict.first_tok
        "Expected one of `pattern`, `pattern-either`, `patterns`, \
         `pattern-regex` to be present"
  | Some (key, value), None, None, None
  | None, Some (key, value), None, None
  | None, None, Some (key, value), None
  | None, None, None, Some (key, value) ->
      (key, value)
  | _ ->
      error env.id rule_dict.first_tok
        "Expected only one of `pattern`, `pattern-either`, `patterns`, or \
         `pattern-regex`"

let rec parse_formula_old_from_dict (env : env) (rule_dict : dict) : R.formula =
  let formula = parse_pair_old env (find_formula_old env rule_dict) in
  (* sanity check *)
  (* bTODO: filter out unconstrained nots *)
  formula

and parse_pair_old env ((key, value) : key * G.expr) : R.formula =
  let env = { env with path = fst key :: env.path } in
  let parse_listi env (key : key) f x =
    match x.G.e with
    | G.Container (Array, (_, xs, _)) -> Common.mapi f xs
    | _ -> error_at_key env.id key ("Expected a list for " ^ fst key)
  in
  let get_pattern str_e = parse_xpattern_expr env str_e in
  (* We use this for lists (patterns and pattern-either) as well as things which
     can be both base patterns and pairs.
     The former does not allow base patterns to appear, so we add an `allow_string`
     parameter.
  *)
  let get_formula ?(allow_string = false) env x =
    match (parse_str_or_dict env x, x.G.e) with
    | Left (value, t), _ when allow_string ->
        R.P (parse_rule_xpattern env (value, t))
    | Left (s, _), _ ->
        error_at_expr env.id x
          (Common.spf
             "Strings not allowed at this position, maybe try `pattern: %s`" s)
    | _, G.Container (Dict, (_, entries, _)) -> (
        match entries with
        | [
         {
           e =
             Container
               (Tuple, (_, [ { e = L (String (_, key, _)); _ }; value ], _));
           _;
         };
        ] ->
            parse_pair_old env (key, value)
        | __else__ ->
            error_at_expr env.id x
              "Expected object with only one entry -- did you forget a hyphen?")
    | _ -> error_at_expr env.id x "Received invalid Semgrep pattern"
  in
  let get_nested_formula_in_list env i x =
    let env = { env with path = string_of_int i :: env.path } in
    get_formula env x
  in
  let s, t = key in
  match s with
  | "pattern" -> R.P (get_pattern value)
  | "pattern-not" -> R.Not (t, get_formula ~allow_string:true env value)
  | "pattern-inside" -> R.Inside (t, get_formula ~allow_string:true env value)
  | "pattern-not-inside" ->
      R.Not (t, R.Inside (t, get_formula ~allow_string:true env value))
  | "pattern-either" ->
      R.Or (t, parse_listi env key (get_nested_formula_in_list env) value)
  | "patterns" ->
      let parse_pattern i expr =
        match parse_str_or_dict env expr with
        | Left (s, _t) ->
            error_at_expr env.id value
              (Common.spf
                 "Strings are not valid under `patterns`: did you mean to \
                  write `pattern: %s` instead?"
                 s)
        | Right dict -> (
            let find key_str = Hashtbl.find_opt dict.h key_str in
            let process_extra extra =
              match extra with
              | MetavarRegexp (mvar, regex, b) -> R.CondRegexp (mvar, regex, b)
              | MetavarType (mvar, xlang_opt, s, t) ->
                  R.CondType (mvar, xlang_opt, s, t)
              | MetavarPattern (mvar, xlang_opt, formula) ->
                  R.CondNestedFormula (mvar, xlang_opt, formula)
              | MetavarComparison { comparison; strip; _ } ->
                  R.CondEval
                    (match strip with
                    (* TODO *)
                    | Some true -> rewrite_metavar_comparison_strip comparison
                    | _ -> comparison)
              | MetavarAnalysis (mvar, kind) -> R.CondAnalysis (mvar, kind)
            in
            match
              ( find "focus-metavariable",
                find "metavariable-analysis",
                find "metavariable-regex",
                find "metavariable-type",
                find "metavariable-pattern",
                find "metavariable-comparison" )
            with
            | None, None, None, None, None, None ->
                Left3 (get_nested_formula_in_list env i expr)
            | Some (((_, t) as key), value), None, None, None, None, None ->
                Middle3 (t, parse_focus_mvs env key value)
            | None, Some (key, value), None, None, None, None
            | None, None, Some (key, value), None, None, None
            | None, None, None, Some (key, value), None, None
            | None, None, None, None, Some (key, value), None
            | None, None, None, None, None, Some (key, value) ->
                Right3 (snd key, parse_extra env key value |> process_extra)
            | _ ->
                error_at_expr env.id expr
                  "should not happen, expected\n\
                  \            one of `metavariable-analysis`, \
                   `metavariable-regex`, or `metavariable-comparison`")
      in
      let conjuncts, focus, conditions =
        Common.partition_either3
          (fun x -> x)
          (parse_listi env key parse_pattern value)
      in
      let pos, _ = R.split_and conjuncts in
      if pos =*= [] && not env.in_metavariable_pattern then
        Rule.raise_error (Some env.id)
          (InvalidRule (MissingPositiveTermInAnd, env.id, t));
      R.And (t, { conjuncts; focus; conditions })
  | "pattern-regex" ->
      let x = parse_string_wrap env key value in
      let xpat = XP.mk_xpat (Regexp (parse_regexp env x)) x in
      R.P xpat
  | "pattern-not-regex" ->
      let x = parse_string_wrap env key value in
      let xpat = XP.mk_xpat (Regexp (parse_regexp env x)) x in
      R.Not (t, R.P xpat)
  | "focus-metavariable"
  | "metavariable-analysis"
  | "metavariable-regex"
  | "metavariable-type"
  | "metavariable-pattern"
  | "metavariable-comparison" ->
      error_at_key env.id key "Must occur directly under a patterns:"
  | "pattern-where-python" ->
      Rule.raise_error (Some env.id)
        (InvalidRule (DeprecatedFeature (fst key), env.id, t))
  (* fix suggestions *)
  | "metavariable-regexp" ->
      error_at_key env.id key
        (spf "unexpected key %s, did you mean metavariable-regex" (fst key))
  (* These keys are handled in Python *)
  (* TODO really we should either remove these keys before sending
     the rules or handle them in OCaml, this is not good *)
  | "r2c-internal-patterns-from" -> failwith "TODO"
  | _ -> error_at_key env.id key (spf "unexpected key %s" (fst key))

(* This is now mutually recursive because of metavariable-pattern: which can
 * contain itself a formula! *)
and parse_extra (env : env) (key : key) (value : G.expr) : extra =
  match fst key with
  | "metavariable-analysis" ->
      let mv_analysis_dict = yaml_to_dict env key value in
      let metavar = take mv_analysis_dict env parse_string "metavariable" in
      let analyzer = take mv_analysis_dict env parse_string "analyzer" in
      let kind =
        match analyzer with
        | "entropy" -> R.CondEntropy
        | "entropy_v2" -> R.CondEntropyV2
        | "redos" -> R.CondReDoS
        | other -> error_at_key env.id key ("Unsupported analyzer: " ^ other)
      in
      MetavarAnalysis (metavar, kind)
  | "metavariable-regex" ->
      let mv_regex_dict =
        try yaml_to_dict env key value with
        | R.Error { kind = DuplicateYamlKey (msg, t); _ } ->
            error env.id t (msg ^ ". You should use multiple metavariable-regex")
      in
      let metavar, regexp, const_prop =
        ( take mv_regex_dict env parse_string "metavariable",
          take mv_regex_dict env parse_string_wrap "regex",
          take_opt mv_regex_dict env parse_bool "constant-propagation" )
      in
      MetavarRegexp
        ( metavar,
          parse_regexp env regexp,
          match const_prop with
          | Some b -> b
          | None -> false )
  | "metavariable-type" ->
      let mv_type_dict = yaml_to_dict env key value in
      let metavar = take mv_type_dict env parse_string "metavariable" in
      let type_strs =
        take_opt mv_type_dict env parse_string_wrap "type" |> Option.to_list
      in
      let type_strs =
        type_strs
        @ (take_opt mv_type_dict env
             (parse_string_wrap_list (fun x -> x))
             "types"
          |> Option.to_list |> List.flatten)
      in
      if type_strs =*= [] then
        error env.id mv_type_dict.first_tok
          "Missing required field: type or types";
      let env', opt_xlang =
        match take_opt mv_type_dict env parse_string "language" with
        | Some s ->
            let xlang = Xlang.of_string ~rule_id:(Rule_ID.to_string env.id) s in
            let env' =
              {
                id = env.id;
                target_analyzer = xlang;
                in_metavariable_pattern = env.in_metavariable_pattern;
                path = "metavariable-type" :: "metavariable" :: env.path;
                options_key = None;
                options = None;
              }
            in
            (env', Some xlang)
        | ___else___ -> (env, None)
      in
      let ts = type_strs |> Common.map (parse_type env' key) in
      MetavarType (metavar, opt_xlang, type_strs |> Common.map fst, ts)
  | "metavariable-pattern" ->
      let mv_pattern_dict = yaml_to_dict env key value in
      let metavar = take mv_pattern_dict env parse_string "metavariable" in
      let env', opt_xlang =
        match take_opt mv_pattern_dict env parse_string "language" with
        | Some s ->
            let xlang = Xlang.of_string ~rule_id:(Rule_ID.to_string env.id) s in
            let env' =
              {
                id = env.id;
                target_analyzer = xlang;
                in_metavariable_pattern = env.in_metavariable_pattern;
                path = "metavariable-pattern" :: "metavariable" :: env.path;
                options_key = None;
                options = None;
              }
            in
            (env', Some xlang)
        | ___else___ -> (env, None)
      in
      let env' = { env' with in_metavariable_pattern = true } in
      let formula_old =
        parse_pair_old env' (find_formula_old env mv_pattern_dict)
      in
      MetavarPattern (metavar, opt_xlang, formula_old)
  | "metavariable-comparison" ->
      let mv_comparison_dict = yaml_to_dict env key value in
      let metavariable, comparison, strip, base =
        ( take_opt mv_comparison_dict env parse_string "metavariable",
          take mv_comparison_dict env parse_string "comparison",
          take_opt mv_comparison_dict env parse_bool "strip",
          take_opt mv_comparison_dict env parse_int "base" )
      in
      let comparison = parse_metavar_cond env key comparison in
      (match (metavariable, strip) with
      | None, Some true ->
          error_at_key env.id key
            (fst key
           ^ ": 'metavariable' field is missing, but it is mandatory if \
              'strip: true'")
      | __else__ -> ());
      MetavarComparison { metavariable; comparison; strip; base }
  | _ -> error_at_key env.id key ("wrong parse_extra field: " ^ fst key)

(*****************************************************************************)
(* Parser for new formula *)
(*****************************************************************************)

let formula_keys =
  [ "pattern"; "all"; "any"; "regex"; "taint"; "not"; "inside" ]

let find_formula env (rule_dict : dict) : key * G.expr =
  match find_some_opt (Hashtbl.find_opt rule_dict.h) formula_keys with
  | None ->
      error env.id rule_dict.first_tok
        ("Expected one of " ^ String.concat "," formula_keys ^ " to be present")
  | Some (key, value) -> (key, value)

(* intermediate type used for processing 'where' *)
type principal_constraint = Ccompare | Cfocus | Cmetavar | Canalyzer

let find_constraint dict =
  fold_dict
    (fun s ((_, tok), _) -> function
      | Some res -> Some res
      | None -> (
          match s with
          | "comparison" -> Some (tok, Ccompare)
          | "focus" -> Some (tok, Cfocus)
          | "analyzer" -> Some (tok, Canalyzer)
          (* This can appear in a metavariable-analysis as well, but it shouldn't
             matter since this case occurs after the `analyzer` one. *)
          | "metavariable" -> Some (tok, Cmetavar)
          | _ -> None))
    dict None

let rec parse_formula_from_dict (env : env) (rule_dict : dict) : R.formula =
  let formula = parse_pair env (find_formula env rule_dict) in
  (* sanity check *)
  (* bTODO: filter out unconstrained nots *)
  formula

and parse_formula env (value : G.expr) : R.formula =
  (* First, try to parse as a string *)
  match parse_str_or_dict env value with
  | Left (s, t) ->
      (* emma: This is for later, but note that start and end_ are currently the
         * same (each pattern is only associated with one token). This might be
         * really annoying to change (we need to compute an accurate end_, but
         * the string given to us by the yaml parser has tabs removed).
         * Will include a note to this effect when I make my
         * "add ranges to patterns" PR.
      *)

      (* let start, end_ = Visitor_AST.range_of_any (G.E e) in
         let _s_range =
           (PI.mk_info_of_loc start, PI.mk_info_of_loc end_)
           (* TODO put in *)
         in *)
      R.P
        (try parse_rule_xpattern env (s, t) with
        | (Time_limit.Timeout _ | UnixExit _) as e ->
            Exception.catch_and_reraise e
        (* TODO: capture and adjust pos of parsing error exns instead of using [t] *)
        | exn ->
            Rule.raise_error (Some env.id)
              (InvalidRule
                 ( InvalidPattern
                     (s, env.target_analyzer, Common.exn_to_s exn, env.path),
                   env.id,
                   t )))
  (* If that doesn't work, it should be a key-value pairing.
   *)
  | Right dict -> (
      (* This is ugly, but here's why. *)
      (* First, we need to figure out if there's a `where`. *)
      let where_formula =
        take_opt dict env (fun _env key value -> (key, value)) "where"
      in
      match where_formula with
      (* If there's a `where`, then there must be one key left, the other of which is the
         pattern. *)
      | _ when Hashtbl.length dict.h <> 1 ->
          error env.id dict.first_tok
            "Expected exactly one key of `pattern`, `all`, `any`, `regex`, \
             `not`, or `inside`"
      (* Otherwise, use the where formula if it exists, to modify the formula we know must exist. *)
      | None -> parse_pair env (find_formula env dict)
      | Some (((_, t) as key), value) ->
          parse_pair env (find_formula env dict)
          |> constrain_where env (t, t) key value)

and produce_constraint env dict tok indicator =
  match indicator with
  | Ccompare ->
      (* comparison: ...
         [strip: ...]
         [base: ...]
      *)
      let ((s, t) as compare_key) =
        take dict env parse_string_wrap "comparison"
      in
      let cond = parse_metavar_cond env compare_key s in
      (* This base is pretty unnecessary. *)
      let strip, _base =
        ( take_opt dict env parse_bool "strip",
          take_opt dict env parse_int "base" )
      in
      let cond =
        (* if strip=true we rewrite the condition and insert Python's `int`
            * function to parse the integer value of mvar. *)
        match strip with
        | Some true -> rewrite_metavar_comparison_strip cond
        | _ -> cond
      in
      [ Left (t, R.CondEval cond) ]
  | Cfocus ->
      (* focus: ...
       *)
      let mv_list = take dict env parse_focus_mvs "focus" in
      [ Right (tok, mv_list) ]
  | Canalyzer ->
      (* metavariable: ...
         analyzer: ...
      *)
      let metavar, t = take dict env parse_string_wrap "metavariable" in
      let analyzer, analyze_t = take dict env parse_string_wrap "analyzer" in
      let kind =
        match analyzer with
        | "entropy" -> R.CondEntropy
        | "redos" -> R.CondReDoS
        | other ->
            error_at_key env.id ("analyzer", analyze_t)
              ("Unsupported analyzer: " ^ other)
      in
      [ Left (t, CondAnalysis (metavar, kind)) ]
  | Cmetavar ->
      (* metavariable: ...
         [<pattern-pair>]
         [type: ...]
         [language: ...]
      *)
      let metavar, t = take dict env parse_string_wrap "metavariable" in
      let env', opt_xlang =
        match take_opt dict env parse_string "language" with
        | Some s ->
            let xlang = Xlang.of_string ~rule_id:(Rule_ID.to_string env.id) s in
            let env' =
              {
                env with
                target_analyzer = xlang;
                path = "metavariable-pattern" :: "metavariable" :: env.path;
              }
            in
            (env', Some xlang)
        | ___else___ -> (env, None)
      in
      let pat =
        match find_some_opt (Hashtbl.find_opt dict.h) formula_keys with
        | Some ps -> (
            let env' = { env' with in_metavariable_pattern = true } in
            let formula = parse_pair env' ps in
            match formula with
            | R.P { pat = Xpattern.Regexp regexp; _ } ->
                (* TODO: always on by default *)
                [ Left (t, R.CondRegexp (metavar, regexp, true)) ]
            | _ -> [ Left (t, CondNestedFormula (metavar, opt_xlang, formula)) ]
            )
        | None -> []
      in
      let type_strs =
        take_opt dict env parse_string_wrap "type" |> Option.to_list
      in
      let type_strs =
        type_strs
        @ (take_opt dict env (parse_string_wrap_list (fun x -> x)) "types"
          |> Option.to_list |> List.flatten)
      in
      let typ =
        match type_strs with
        | ts :: _ ->
            [
              Left
                ( snd ts,
                  R.CondType
                    ( metavar,
                      opt_xlang,
                      type_strs |> Common.map fst,
                      type_strs |> Common.map (parse_type env (metavar, t)) ) );
            ]
        | _ -> []
      in
      List.flatten [ pat; typ ]

and constrain_where env (t1, _t2) where_key (value : G.expr) formula : R.formula
    =
  let env = { env with path = "where" :: env.path } in
  (* TODO: first token, or principal token? *)
  let parse_where_pair env (where_value : G.expr) =
    let dict = yaml_to_dict env where_key where_value in
    match find_constraint dict with
    | Some (tok, indicator) -> produce_constraint env dict tok indicator
    | _ -> error_at_expr env.id value "Wrong where constraint fields"
  in
  (* TODO *)
  let conditions, focus =
    parse_listi env where_key parse_where_pair value
    |> List.flatten
    |> Common.partition_either (fun x -> x)
  in
  let tok, conditions, focus, conjuncts =
    (* If the modified pattern is also an `And`, collect the conditions and focus
        and fold them together.
    *)
    match formula with
    | And (tok, { conjuncts; conditions = conditions2; focus = focus2 }) ->
        (tok, conditions @ conditions2, focus @ focus2, conjuncts)
    (* Otherwise, we consider the modified pattern a degenerate singleton `And`.
    *)
    | _ -> (t1, conditions, focus, [ formula ])
  in
  R.And (tok, { conjuncts; conditions; focus })

and parse_pair env ((key, value) : key * G.expr) : R.formula =
  let env = { env with path = fst key :: env.path } in
  let get_string_pattern str_e = parse_xpattern_expr env str_e in
  let s, t = key in
  match s with
  | "pattern" -> R.P (get_string_pattern value)
  | "not" -> R.Not (t, parse_formula env value)
  | "inside" -> R.Inside (t, parse_formula env value)
  | "all" ->
      let conjuncts = parse_listi env key parse_formula value in
      let pos, _ = R.split_and conjuncts in
      if pos =*= [] && not env.in_metavariable_pattern then
        Rule.raise_error (Some env.id)
          (InvalidRule (MissingPositiveTermInAnd, env.id, t));
      R.And (t, { conjuncts; focus = []; conditions = [] })
  | "any" -> R.Or (t, parse_listi env key parse_formula value)
  | "regex" ->
      let x = parse_string_wrap env key value in
      let xpat = XP.mk_xpat (Regexp (parse_regexp env x)) x in
      R.P xpat
  | _ -> error_at_key env.id key (spf "unexpected key %s" (fst key))

(*****************************************************************************)
(* Parsers for taint *)
(*****************************************************************************)

let requires_expr_to_precondition env key e =
  let invalid_requires () =
    error_at_key env.id key
      "Invalid `requires' expression, it must be a Python Boolean expression \
       over labels (any valid Python identifier) using operators `not', `or' \
       and `and'."
  in
  let rec expr_to_precondition e =
    match e.G.e with
    | G.L (G.Bool (v, _)) -> R.PBool v
    | G.N (G.Id ((str, _), _)) when Metavariable.is_metavar_name str ->
        error_at_key env.id key
          ("Invalid `requires' expression, metavariables cannot be used as \
            labels: " ^ str)
    | G.N (G.Id ((str, _), _)) -> R.PLabel str
    | G.Call ({ e = G.IdSpecial (G.Op G.Not, _); _ }, (_, [ Arg e1 ], _)) ->
        PNot (expr_to_precondition e1)
    | G.Call ({ e = G.IdSpecial (G.Op op, _); _ }, (_, args, _)) -> (
        match (op, args_to_precondition args) with
        | G.And, xs -> R.PAnd xs
        | G.Or, xs -> R.POr xs
        | __else__ -> invalid_requires ())
    | __else__ -> invalid_requires ()
  and args_to_precondition args =
    match args with
    | [] -> []
    | G.Arg e :: args' -> expr_to_precondition e :: args_to_precondition args'
    | _ :: _args' -> invalid_requires ()
  in
  expr_to_precondition e

let parse_taint_requires env key x =
  let s = parse_string env key x in
  let e = parse_python_expression env key s in
  let range = AST_generic_helpers.range_of_any_opt (E e) in
  { R.precondition = requires_expr_to_precondition env key e; range }

(* TODO: can add a case where these take in only a single string *)
let parse_taint_source ~(is_old : bool) env (key : key) (value : G.expr) :
    Rule.taint_source =
  let parse_from_dict dict f =
    let source_by_side_effect =
      take_opt dict env parse_bool "by-side-effect"
      |> Option.value ~default:false
    in
    let source_control =
      take_opt dict env parse_bool "control" |> Option.value ~default:false
    in
    let label =
      take_opt dict env parse_string "label"
      |> Option.value ~default:R.default_source_label
    in
    let source_requires = take_opt dict env parse_taint_requires "requires" in
    let source_formula = f env dict in
    {
      R.source_formula;
      source_by_side_effect;
      source_control;
      label;
      source_requires;
    }
  in
  if is_old then
    let dict = yaml_to_dict env key value in
    parse_from_dict dict parse_formula_old_from_dict
  else
    match parse_str_or_dict env value with
    | Left value ->
        let source_formula = R.P (parse_rule_xpattern env value) in
        {
          source_formula;
          source_by_side_effect = false;
          source_control = false;
          label = R.default_source_label;
          source_requires = None;
        }
    | Right dict -> parse_from_dict dict parse_formula_from_dict

let parse_taint_propagator ~(is_old : bool) env (key : key) (value : G.expr) :
    Rule.taint_propagator =
  let f =
    if is_old then parse_formula_old_from_dict else parse_formula_from_dict
  in
  let parse_from_dict dict f =
    let propagator_by_side_effect =
      take_opt dict env parse_bool "by-side-effect"
      |> Option.value ~default:true
    in
    let from = take dict env parse_string_wrap "from" in
    let to_ = take dict env parse_string_wrap "to" in
    let propagator_requires =
      take_opt dict env parse_taint_requires "requires"
    in
    let propagator_label = take_opt dict env parse_string "label" in
    let propagator_replace_labels =
      take_opt dict env
        (fun env key v ->
          parse_listi env key (fun env v -> parse_string env key v) v)
        "replace-labels"
    in
    let propagator_formula = f env dict in
    {
      R.propagator_formula;
      propagator_by_side_effect;
      from;
      to_;
      propagator_requires;
      propagator_replace_labels;
      propagator_label;
    }
  in
  let dict = yaml_to_dict env key value in
  parse_from_dict dict f

let parse_taint_sanitizer ~(is_old : bool) env (key : key) (value : G.expr) =
  let parse_from_dict dict f =
    let sanitizer_by_side_effect =
      take_opt dict env parse_bool "by-side-effect"
      |> Option.value ~default:false
    in
    let not_conflicting =
      take_opt dict env parse_bool
        (if is_old then "not_conflicting" else "not-conflicting")
      |> Option.value ~default:false
    in
    let sanitizer_formula = f env dict in
    { sanitizer_formula; sanitizer_by_side_effect; R.not_conflicting }
  in
  if is_old then
    let dict = yaml_to_dict env key value in
    parse_from_dict dict parse_formula_old_from_dict
  else
    match parse_str_or_dict env value with
    | Left value ->
        let sanitizer_formula = R.P (parse_rule_xpattern env value) in
        {
          sanitizer_formula;
          sanitizer_by_side_effect = false;
          R.not_conflicting = false;
        }
    | Right dict -> parse_from_dict dict parse_formula_from_dict

let parse_taint_sink ~(is_old : bool) env (key : key) (value : G.expr) :
    Rule.taint_sink =
  let sink_id = String.concat ":" env.path in
  let parse_from_dict dict f =
    let sink_requires = take_opt dict env parse_taint_requires "requires" in
    let sink_formula = f env dict in
    { R.sink_id; sink_formula; sink_requires }
  in
  if is_old then
    let dict = yaml_to_dict env key value in
    parse_from_dict dict parse_formula_old_from_dict
  else
    match parse_str_or_dict env value with
    | Left value ->
        let sink_formula = R.P (parse_rule_xpattern env value) in
        { sink_id; sink_formula; sink_requires = None }
    | Right dict -> parse_from_dict dict parse_formula_from_dict

let parse_taint_pattern env key (value : G.expr) =
  let dict = yaml_to_dict env key value in
  let parse_specs parse_spec env key x =
    ( snd key,
      parse_listi env key
        (fun env -> parse_spec env (fst key ^ "list item", snd key))
        x )
  in
  let sources, propagators_opt, sanitizers_opt, sinks =
    ( take dict env (parse_specs (parse_taint_source ~is_old:false)) "sources",
      take_opt dict env
        (parse_specs (parse_taint_propagator ~is_old:false))
        "propagators",
      take_opt dict env
        (parse_specs (parse_taint_sanitizer ~is_old:false))
        "sanitizers",
      take dict env (parse_specs (parse_taint_sink ~is_old:false)) "sinks" )
  in
  `Taint
    {
      R.sources;
      propagators =
        (* optlist_to_list *)
        (match propagators_opt with
        | None -> []
        | Some (_, xs) -> xs);
      sanitizers = sanitizers_opt;
      sinks;
    }

(*****************************************************************************)
(* Parsers for extract mode *)
(*****************************************************************************)

let parse_extract_dest ~id lang : Xlang.t =
  match lang with
  | ("none" | "regex"), _ -> LRegex
  | ("generic" | "spacegrep"), _ -> LSpacegrep
  | "aliengrep", _ -> LAliengrep
  | lang -> L (parse_language ~id lang, [])

let parse_extract_reduction ~id (s, t) =
  match s with
  | "concat" -> R.Concat
  | "separate" -> R.Separate
  | s ->
      Rule.raise_error (Some id)
        (InvalidRule
           ( InvalidOther
               (spf "Bad extract reduction: %s (expected concat or separate)" s),
             id,
             t ))

let parse_extract_transform ~id (s, t) =
  match s with
  | "no_transform" -> R.NoTransform
  | "unquote_string" -> R.Unquote
  | "concat_json_string_array" -> R.ConcatJsonArray
  | s ->
      Rule.raise_error (Some id)
        (InvalidRule
           ( InvalidOther
               (spf
                  "Bad extract transform: %s (expected unquote_string or \
                   concat_json_string_array)"
                  s),
             id,
             t ))

let parse_rules_to_run_with_extract env key value =
  let ruleids_dict = yaml_to_dict env key value in
  let inc_opt, exc_opt =
    ( take_opt ruleids_dict env
        (parse_string_wrap_list Rule_ID.of_string)
        "include",
      take_opt ruleids_dict env
        (parse_string_wrap_list Rule_ID.of_string)
        "exclude" )
  in
  (* alt: we could use report_unparsed_fields(), but better to raise an error for now
     to be compatible with pysemgrep *)
  if Hashtbl.length ruleids_dict.h > 0 then
    error_at_key env.id key
      "Additional properties are not allowed (only 'include' and 'exclude' are \
       supported)";
  {
    R.required_rules = optlist_to_list inc_opt;
    excluded_rules = optlist_to_list exc_opt;
  }

(*****************************************************************************)
(* Parsers used by step mode as well as general rules *)
(*****************************************************************************)

let parse_search_fields env rule_dict =
  let formula =
    take_opt rule_dict env (fun env _ expr -> parse_formula env expr) "match"
  in
  match formula with
  | Some formula -> `Search formula
  | None -> `Search (parse_pair_old env (find_formula_old env rule_dict))

let parse_taint_fields env rule_dict =
  let parse_specs parse_spec env key x =
    ( snd key,
      parse_listi env key
        (fun env -> parse_spec env (fst key ^ "list item", snd key))
        x )
  in
  match Hashtbl.find_opt rule_dict.h "taint" with
  | Some (key, value) -> parse_taint_pattern env key value
  | __else__ ->
      let sources, propagators_opt, sanitizers_opt, sinks =
        ( take rule_dict env
            (parse_specs (parse_taint_source ~is_old:true))
            "pattern-sources",
          take_opt rule_dict env
            (parse_specs (parse_taint_propagator ~is_old:true))
            "pattern-propagators",
          take_opt rule_dict env
            (parse_specs (parse_taint_sanitizer ~is_old:true))
            "pattern-sanitizers",
          take rule_dict env
            (parse_specs (parse_taint_sink ~is_old:true))
            "pattern-sinks" )
      in
      `Taint
        {
          sources;
          propagators =
            (* optlist_to_list *)
            (match propagators_opt with
            | None -> []
            | Some (_, xs) -> xs);
          sanitizers = sanitizers_opt;
          sinks;
        }

(*****************************************************************************)
(* Parsers for step mode *)
(*****************************************************************************)

let parse_step_fields env key (value : G.expr) : R.step =
  let rd = yaml_to_dict env key value in
  let languages = take_no_env rd parse_string_wrap_list_no_env "languages" in
  (* No id, so error at the steps key
     TODO error earlier *)
  let rule_options =
    (* TODO: this is annoying and refers to the global options which may be
       incorrect anyway -> support an 'options' field next to 'languages'
       in the step object? *)
    Option.value env.options ~default:Rule_options.default_config
  in
  let step_id_str, tok = key in
  let id =
    (Rule_ID.of_string (* TODO: is this really a rule ID? *) step_id_str, tok)
  in
  let step_selector, step_analyzer =
    parse_languages ~id rule_options languages
  in
  let env = { env with target_analyzer = step_analyzer } in
  let step_paths = take_opt rd env parse_paths "paths" in
  let mode_opt = take_opt rd env parse_string_wrap "mode" in
  let has_taint_key = Option.is_some (Hashtbl.find_opt rd.h "taint") in
  let step_mode =
    match (mode_opt, has_taint_key) with
    | None, false
    | Some ("search", _), false -> (
        match parse_search_fields env rd with
        | `Search formula -> `Search formula
        | _else_ -> raise Common.Impossible)
    | _, true
    | Some ("taint", _), _ -> (
        match parse_taint_fields env rd with
        | `Taint formula -> `Taint formula
        | _else_ -> raise Common.Impossible)
    | Some key, _ ->
        error_at_key env.id key
          (spf
             "Unexpected value for mode, should be 'search' or 'taint', not %s"
             (fst key))
  in
  { step_selector; step_analyzer; step_paths; step_mode }

let parse_steps env key (value : G.expr) : R.step list =
  let parse_step step = parse_step_fields env key step in
  match value.G.e with
  | G.Container (Array, (_, xs, _)) -> Common.map parse_step xs
  | _ -> error_at_key env.id key ("Expected a list for " ^ fst key)

(*****************************************************************************)
(* Parsers for secrets mode *)
(*****************************************************************************)

let parse_validity env key x : Rule.validation_state =
  match x.G.e with
  | G.L (String (_, ("valid", _), _)) -> `Confirmed_valid
  | G.L (String (_, ("invalid", _), _)) -> `Confirmed_invalid
  | _x -> error_at_key env.id key (spf "parse_validity for %s" (fst key))

let parse_http_request env key value : Rule.request =
  let req = yaml_to_dict env key value in
  let url = take req env parse_string "url" in
  let meth = take req env method_ "method" in
  let headers : Rule.header list =
    take req env yaml_to_dict "headers" |> fun { h; _ } ->
    Hashtbl.fold
      (fun name value lst ->
        { Rule.name; value = parse_string env (fst value) (snd value) } :: lst)
      h []
  in
  let body = take_opt req env parse_string "body" in
  let auth = take_opt req env parse_auth "auth" in
  { url; meth; headers; body; auth }

let parse_http_matcher_clause key env value : Rule.http_match_clause =
  let clause = yaml_to_dict env key value in
  let status_code = take_opt clause env parse_int "status-code" in
  let headers =
    take_opt clause env
      (fun env key ->
        parse_list env key (fun env x : Rule.header ->
            let hd = yaml_to_dict env key x in
            let name = take hd env parse_string "name" in
            let value = take hd env parse_string "value" in
            { name; value }))
      "headers"
  in
  let content = take_opt clause env yaml_to_dict "content" in
  match (status_code, headers, content) with
  | None, None, None -> failwith "ffff"
  | _ ->
      {
        status_code;
        headers = Option.value ~default:[] headers;
        content =
          Option.map
            (fun content ->
              ( parse_pair_old env (find_formula_old env content),
                Option.map (Xlang.of_string ~rule_id:(Rule_ID.to_string env.id))
                @@ take_opt content env parse_string "language"
                |> Option.value ~default:Xlang.LAliengrep ))
            content;
      }

let parse_http_matcher key env value : Rule.http_matcher =
  let matcher = yaml_to_dict env key value in
  let match_conditions =
    take matcher env
      (fun env key -> parse_list env key (parse_http_matcher_clause key))
      "match"
  in
  let result = take matcher env yaml_to_dict "result" in
  let validity = take result env parse_validity "validity" in
  let message = take_opt result env parse_string "message" in
  let severity =
    take_opt result env parse_string_wrap "severity"
    |> Option.map @@ parse_severity ~id:env.id
  in
  let metadata = take_opt_no_env result (generic_to_json env.id) "metadata" in
  { match_conditions; validity; message; severity; metadata }

let parse_http_response env key value : Rule.http_matcher list =
  parse_list env key (parse_http_matcher key) value

let parse_http_validator env key value : Rule.validator =
  let validator_dict = yaml_to_dict env key value in
  let request = take validator_dict env parse_http_request "request" in
  let response = take validator_dict env parse_http_response "response" in
  HTTP { request; response }

let parse_validator key env value =
  let rd = yaml_to_dict env key value in
  let http = take_opt rd env parse_http_validator "http" in
  match http with
  | Some validator -> validator
  | None ->
      error_at_key env.id key
        ("No reconigzed validator (e.g., 'http') at " ^ fst key)

let parse_validators env key value =
  parse_list env key (parse_validator key) value

(* NOTE: For old secrets / postprocessors syntax. *)
let parse_secrets_fields env rule_dict : R.secrets =
  let secrets : R.formula list =
    take rule_dict env
      (fun env key expr ->
        parse_list env key
          (fun env dict_pair ->
            yaml_to_dict env key dict_pair
            |> find_formula_old env |> parse_pair_old env)
          expr)
      "postprocessor-patterns"
  in
  let req = take rule_dict env yaml_to_dict "request" in
  let res = take rule_dict env yaml_to_dict "response" in
  let url = take req env parse_string "url" in
  let meth = take req env method_ "method" in
  let headers : Rule.header list =
    take req env yaml_to_dict "headers" |> fun { h; _ } ->
    Hashtbl.fold
      (fun name value lst ->
        { Rule.name; value = parse_string env (fst value) (snd value) } :: lst)
      h []
  in
  let body = take_opt req env parse_string "body" in
  let auth = take_opt req env parse_auth "auth" in
  let return_code = take res env parse_int "return_code" in
  let regex = take_opt res env parse_string "pattern-regex" in
  {
    secrets;
    request = { url; meth; headers; body; auth };
    response = { return_code; regex };
  }

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
let parse_mode env mode_opt (rule_dict : dict) : R.mode =
  (* We do this because we should only assume that we have a search mode rule
     if there is not a `taint` key present in the rule dict.
  *)
  let has_taint_key = Option.is_some (Hashtbl.find_opt rule_dict.h "taint") in
  match (mode_opt, has_taint_key) with
  | None, false
  | Some ("search", _), false ->
      parse_search_fields env rule_dict
  | _, true
  | Some ("taint", _), _ ->
      parse_taint_fields env rule_dict
  | Some ("extract", _), _ ->
      let formula = parse_pair_old env (find_formula_old env rule_dict) in
      let dst_lang =
        take rule_dict env parse_string_wrap "dest-language"
        |> parse_extract_dest ~id:env.id
      in
      (* TODO: determine fmt---string with interpolated metavars? *)
      let extract = take rule_dict env parse_string "extract" in
      let extract_rule_ids =
        take_opt rule_dict env parse_rules_to_run_with_extract "dest-rules"
      in
      let transform =
        take_opt rule_dict env parse_string_wrap "transform"
        |> Option.map (parse_extract_transform ~id:env.id)
        |> Option.value ~default:R.NoTransform
      in
      let reduce =
        take_opt rule_dict env parse_string_wrap "reduce"
        |> Option.map (parse_extract_reduction ~id:env.id)
        |> Option.value ~default:R.Separate
      in
      `Extract
        { formula; dst_lang; extract_rule_ids; extract; reduce; transform }
  (* TODO: change this mode name to something more descriptive + not intentionally
   * ambigous sometime later.
   *)
  | Some ("semgrep_internal_postprocessor", _), _ ->
      `Secrets (parse_secrets_fields env rule_dict)
  (* TODO? should we use "mode: steps" instead? *)
  | Some ("step", _), _ ->
      let steps = take rule_dict env parse_steps "steps" in
      `Steps steps
  | Some key, _ ->
      error_at_key env.id key
        (spf
           "Unexpected value for mode, should be 'search', 'taint', 'extract', \
            or 'step', not %s"
           (fst key))

(* sanity check there are no remaining fields in rd *)
let report_unparsed_fields rd =
  (* those were not "consumed" *)
  Hashtbl.remove rd.h "pattern";
  Hashtbl.remove rd.h "patterns";
  match Common.hash_to_list rd.h with
  | [] -> ()
  | xs ->
      (* less: we could return an error, but better to be fault-tolerant
       * to futur extensions to the rule format
       *)
      xs
      |> List.iter (fun (k, _v) ->
             logger#warning "skipping unknown field: %s" k)

let parse_version key value =
  let str, tok = parse_string_wrap_no_env key value in
  match Version_info.of_string str with
  | Some version -> (version, tok)
  | None ->
      yaml_error_at_key key
        ("Expected a version of the form X.Y.Z for " ^ fst key)

let incompatible_version ?min_version ?max_version rule_id tok =
  Rule.raise_error (Some rule_id)
    (InvalidRule
       ( IncompatibleRule (Version_info.version, (min_version, max_version)),
         rule_id,
         tok ))

let check_version_compatibility rule_id ~min_version ~max_version =
  (match min_version with
  | None -> ()
  | Some (mini, tok) ->
      if not (Version_info.compare mini Version_info.version <= 0) then
        incompatible_version ?min_version:(Some mini) rule_id tok);
  match max_version with
  | None -> ()
  | Some (maxi, tok) ->
      if not (Version_info.compare Version_info.version maxi <= 0) then
        incompatible_version ?max_version:(Some maxi) rule_id tok

let parse_one_rule ~rewrite_rule_ids (t : G.tok) (i : int) (rule : G.expr) :
    Rule.t =
  let rd = yaml_to_dict_no_env ("rules", t) rule in
  (* We need a rule ID early to produce useful error messages. *)
  let rule_id_str, tok = take_no_env rd parse_string_wrap_no_env "id" in
  let rule_id = Rule_ID.of_string rule_id_str in
  let rule_id : Rule_ID.t =
    match rewrite_rule_ids with
    | None -> rule_id
    | Some f -> f rule_id
  in
  let id = (rule_id, tok) in
  (* We need to check for version compatibility before attempting to interpret
     the rule. *)
  let min_version = take_opt_no_env rd parse_version "min-version" in
  let max_version = take_opt_no_env rd parse_version "max-version" in
  check_version_compatibility rule_id ~min_version ~max_version;

  let languages = take_no_env rd parse_string_wrap_list_no_env "languages" in
  let options_opt, options_key =
    match take_opt_no_env rd (parse_options rule_id) "options" with
    | None -> (None, None)
    | Some (options, options_key) -> (Some options, options_key)
  in
  let options = Option.value options_opt ~default:Rule_options.default_config in
  let target_selector, target_analyzer =
    parse_languages ~id options languages
  in
  let env =
    {
      id = rule_id;
      target_analyzer;
      in_metavariable_pattern = false;
      path = [ string_of_int i; "rules" ];
      options_key;
      options = options_opt;
    }
  in
  let mode_opt = take_opt rd env parse_string_wrap "mode" in
  let mode = parse_mode env mode_opt rd in
  let message, severity =
    match mode with
    | `Extract _ -> ("", ("INFO", Tok.unsafe_fake_tok ""))
    | _ ->
        ( take rd env parse_string "message",
          take rd env parse_string_wrap "severity" )
  in
  let metadata_opt = take_opt_no_env rd (generic_to_json rule_id) "metadata" in
  let fix_opt = take_opt rd env parse_string "fix" in
  let fix_regex_opt = take_opt rd env parse_fix_regex "fix-regex" in
  let paths_opt = take_opt rd env parse_paths "paths" in
  let equivs_opt = take_opt rd env parse_equivalences "equivalences" in
  let validators_opt = take_opt rd env parse_validators "validators" in
  report_unparsed_fields rd;
  {
    R.id;
    min_version = Option.map fst min_version;
    max_version = Option.map fst max_version;
    message;
    target_selector;
    target_analyzer;
    severity = parse_severity ~id:env.id severity;
    mode;
    (* optional fields *)
    metadata = metadata_opt;
    fix = fix_opt;
    fix_regexp = fix_regex_opt;
    paths = paths_opt;
    equivalences = equivs_opt;
    options = options_opt;
    validators = validators_opt;
  }

let parse_generic_ast ?(error_recovery = false) ?(rewrite_rule_ids = None)
    (file : Fpath.t) (ast : AST_generic.program) :
    Rule.rules * Rule.invalid_rule_error list =
  let t, rules =
    match ast with
    | [ { G.s = G.ExprStmt (e, _); _ } ] -> (
        match e.G.e with
        | Container
            ( Dict,
              ( _,
                [
                  {
                    e =
                      Container
                        ( Tuple,
                          ( _,
                            [
                              { e = L (String (_, ("rules", _), _)); _ }; rules;
                            ],
                            _ ) );
                    _;
                  };
                ],
                _ ) ) -> (
            match rules.G.e with
            | G.Container (G.Array, (l, rules, _r)) -> (l, rules)
            | _ ->
                yaml_error_at_expr rules
                  "expected a list of rules following `rules:`")
        (* it's also ok to not have the toplevel rules:, anyway we never
         * used another toplevel key
         *)
        | G.Container (G.Array, (l, rules, _r)) -> (l, rules)
        | _ ->
            let loc = Tok.first_loc_of_file !!file in
            yaml_error (Tok.tok_of_loc loc)
              "missing rules entry as top-level key")
    | [] ->
        (* an empty rules file returns an empty list of rules *)
        (Tok.(tok_of_loc (first_loc_of_file !!file)), [])
    | _ -> assert false
    (* yaml_to_generic should always return a ExprStmt *)
  in
  let xs =
    rules
    |> Common.mapi (fun i rule ->
           try Left (parse_one_rule ~rewrite_rule_ids t i rule) with
           | Rule.Error { kind = InvalidRule ((kind, ruleid, _) as err); _ }
             when error_recovery || R.is_skippable_error kind ->
               let s = Rule.string_of_invalid_rule_error_kind kind in
               logger#warning "skipping rule %s, error = %s"
                 (Rule_ID.to_string ruleid) s;
               Right err)
  in
  Common.partition_either (fun x -> x) xs

(* We can't call just Yaml_to_generic.program below because when we parse
 * YAML Semgrep rules, we preprocess unicode characters differently.
 * We also need to translate Parse_info.Other_error exn in
 * (Rule.Err (Rule.InvalidYaml)) exn.
 * Note that we can't generate a Rule.Err in Yaml_to_generic directly
 * because we don't want parsing/other/ to depend on core/.
 *)
let parse_yaml_rule_file file =
  let str = Common.read_file file in
  try Yaml_to_generic.parse_yaml_file file str with
  | Parsing_error.Other_error (s, t) ->
      Rule.raise_error None (InvalidYaml (s, t))

let parse_file ?error_recovery ?(rewrite_rule_ids = None) file =
  let ast =
    match FT.file_type_of_file file with
    | FT.Config FT.Json ->
        (* in a parsing-rule context, we don't want the parsed strings by
         * Parse_json.parse_program to remain escaped. For example with this
         * JSON rule:
         * { "rules": [ {
         *       "id": "x",
         *       "message": "",
         *       "languages": ["python"],
         *       "severity": "WARNING",
         *       "pattern": "\"hello\""
         *     }
         *   ]
         * }
         * we want the pattern in the generic AST of the rule to contain the
         * string '"hello"', without the antislash, otherwise
         * Parse_python.parse_any will fail parsing it.
         *
         * Note that we didn't have this problem before when we were using
         * Yojson to parse a JSON rule, because Yojson correctly unescaped
         * and returned the "final string".
         *
         * Note that this is handled correctly by Yaml_to_generic.parse_rule
         * below.
         *)
        Json_to_generic.program ~unescape_strings:true
          (Parse_json.parse_program !!file)
    | FT.Config FT.Jsonnet ->
        if use_ojsonnet then
          let ast = Parse_jsonnet.parse_program file in
          (* Note that here we do not support registry-aware import;
           * those are defined in osemgrep/.../Rule_fetching.ml where
           * we use Network.get functions. Thus, semgrep-core -dump_rule
           * will not work with registry-aware import either.
           * Use osemgrep --dump-config instead.
           *)
          let core = Desugar_jsonnet.desugar_program file ast in
          let value_ = Eval_jsonnet.eval_program core in
          Manifest_jsonnet_to_AST_generic.manifest_value value_
        else
          Common2.with_tmp_file ~str:"parse_rule" ~ext:"json" (fun tmpfile ->
              let cmd = spf "jsonnet -J vendor %s -o %s" !!file tmpfile in
              let n = Sys.command cmd in
              if n <> 0 then failwith (spf "error executing %s" cmd);
              let ast = Parse_json.parse_program tmpfile in
              Json_to_generic.program ~unescape_strings:true ast)
    | FT.Config FT.Yaml -> parse_yaml_rule_file ~is_target:true !!file
    | _else_ ->
        logger#error "wrong rule format, only JSON/YAML/JSONNET are valid";
        logger#info "trying to parse %s as YAML" !!file;
        parse_yaml_rule_file ~is_target:true !!file
  in
  parse_generic_ast ?error_recovery ~rewrite_rule_ids file ast

(*****************************************************************************)
(* Main Entry point *)
(*****************************************************************************)

let parse_and_filter_invalid_rules ?rewrite_rule_ids file =
  parse_file ~error_recovery:true ?rewrite_rule_ids file
[@@profiling]

let parse_xpattern xlang (str, tok) =
  let env =
    {
      id = Rule_ID.of_string "anon-pattern";
      target_analyzer = xlang;
      in_metavariable_pattern = false;
      path = [];
      options_key = None;
      options = None;
    }
  in
  parse_rule_xpattern env (str, tok)

(*****************************************************************************)
(* Useful for tests *)
(*****************************************************************************)

let parse file =
  let xs, _skipped = parse_file ~error_recovery:false file in
  (* The skipped rules include Apex rules and other rules that are always
     skippable. *)
  xs

(*****************************************************************************)
(* Valid rule filename checks *)
(*****************************************************************************)
(* Those functions could be in a separate file *)

(* alt: could define
 * type yaml_kind = YamlRule | YamlTest | YamlFixed | YamlOther
 *)
let is_test_yaml_file filepath =
  (* .test.yaml files are YAML target files rather than config files! *)
  let filepath = !!filepath in
  Filename.check_suffix filepath ".test.yaml"
  || Filename.check_suffix filepath ".test.yml"
  || Filename.check_suffix filepath ".test.fixed.yaml"
  || Filename.check_suffix filepath ".test.fixed.yml"

let is_valid_rule_filename filename =
  match File_type.file_type_of_file filename with
  (* ".yml" or ".yaml" *)
  | FT.Config FT.Yaml -> not (is_test_yaml_file filename)
  (* old: we were allowing Jsonnet before, but better to skip
   * them for now to avoid adding a jsonnet dependency in our docker/CI
   * FT.Config (FT.Json FT.Jsonnet) when not unit_testing -> true
   *)
  | _else_ -> false
