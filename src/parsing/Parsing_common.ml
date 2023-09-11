(* Yoann Padioleau, Emma Jin, Brandon Wu
 *
 * Copyright (C) 2023 r2c
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
module R = Rule
module G = AST_generic
module J = JSON

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type key = string R.wrap

type env = {
  (* id of the current rule (needed by some exns) *)
  id : Rule_ID.t;
  (* languages of the current rule (needed by parse_pattern) *)
  languages : Rule.languages;
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
      Rule.raise_error (Some env.id)
        (InvalidRule
           ( InvalidPattern
               (s, env.languages.target_analyzer, Common.exn_to_s exn, env.path),
             env.id,
             t ))

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
