(* Yoann Padioleau, Emma Jin, Brandon Wu
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
module R = Rule
module G = AST_generic
module J = JSON

let tags = Logs_.create_tags [ __MODULE__ ]

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
 * This is useful for warn_if_remaining_unparsed_fields() below.
 *)
type dict = {
  (* Do not use directly Hashtbl.find_opt on this field!
   * Use instead dict_take_opt() or tak_opt_no_env() otherwise
   * warn_if_remaining_unparsed_fields() will not work.
   * This is mutated!
   *)
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
    | Pcre2.Partial -> "String only matched the pattern partially"
    | BadPattern (msg, pos) -> spf "%s at position %d" msg pos
    | BadUTF -> "UTF8 string being matched is invalid"
    | BadUTFOffset ->
        "Gets raised when a UTF8 string being matched with offset is invalid."
    | MatchLimit ->
        "Maximum allowed number of match attempts with\n\
        \                      backtracking or recursion is reached during \
         matching."
    | DepthLimit -> "Recursion limit reached"
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

let check_that_dict_is_empty (dict : dict) =
  if Hashtbl.length dict.h > 0 then
    let remaining_keys =
      dict.h |> Hashtbl.to_seq_keys |> List.of_seq |> List.sort String.compare
      |> String.concat ", "
    in
    yaml_error dict.first_tok
      ("Unknown or duplicate properties found in YAML object: " ^ remaining_keys)

(* sanity check there are no remaining fields in rd (rule dictionnary) *)
let warn_if_remaining_unparsed_fields (rule_id : Rule_ID.t) (rd : dict) : unit =
  (* less: we could return an error, but better to be fault-tolerant
   * to futur extensions to the rule format
   *)
  rd.h |> Hashtbl_.hash_to_list
  |> List.iter (fun (k, _v) ->
         Logs.warn (fun m ->
             m ~tags "Skipping unknown field '%s' in rule %s" k
               (Rule_ID.to_string rule_id)))

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
    | G.L (Int pi) -> (
        match Parsed_int.to_int_opt pi with
        | None ->
            UCommon.pr2 (G.show_expr_kind x.G.e);
            error_at_expr rule_id x "no value for generic integer"
        | Some i -> J.Int i)
    | G.L (String (_, (s, _), _)) ->
        (* should use the unescaped string *)
        J.String s
    | G.Container (Array, (_, xs, _)) -> J.Array (xs |> List_.map aux)
    | G.Container (Dict, (_, xs, _)) ->
        J.Object
          (xs
          |> List_.map (fun x ->
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
        UCommon.pr2 (G.show_expr_kind x.G.e);
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
    (enclosing_obj_name : string) (rule : G.expr) : dict =
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
  | _ ->
      error_fun_d rule ("each " ^ enclosing_obj_name ^ " should be a dictionary")

(* Lookup a key from dict and remove it. *)
let dict_take_opt (dict : dict) (key_str : string) : (key * G.expr) option =
  let tbl = dict.h in
  let res = Hashtbl.find_opt tbl key_str in
  Hashtbl.remove tbl key_str;
  res

(* Mutates the Hashtbl! *)
let take_opt (dict : dict) (env : env) (f : env -> key -> G.expr -> 'a)
    (key_str : string) : 'a option =
  Option.map (fun (key, value) -> f env key value) (dict_take_opt dict key_str)

(* Mutates the Hashtbl! *)
let take_key (dict : dict) (env : env) (f : env -> key -> G.expr -> 'a)
    (key_str : string) : 'a =
  match take_opt dict env f key_str with
  | Some res -> res
  | None -> error env.id dict.first_tok ("Missing required field " ^ key_str)

let fold_dict f dict x = Hashtbl.fold f dict.h x

let yaml_to_dict (env : env) (enclosing_obj_name : string G.wrap) =
  yaml_to_dict_helper (Some env.id) (error_at_expr env.id)
    (error_at_expr env.id) (fst enclosing_obj_name)

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

let yaml_to_dict_no_env enclosing_obj_name expr =
  yaml_to_dict_helper None yaml_error_at_expr yaml_error_at_expr
    enclosing_obj_name expr

let parse_string_wrap_no_env (key : key) x =
  match read_string_wrap x.G.e with
  | Some (value, t) -> (value, t)
  | None -> yaml_error_at_key key ("Expected a string value for " ^ fst key)

let parse_list_no_env (key : key) f x =
  match x.G.e with
  | G.Container (Array, (_, xs, _)) -> List_.map f xs
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
  match take_key auth env parse_string "type" with
  | "sigv4" ->
      let secret_access_key =
        take_key auth env parse_string "secret_access_key"
      in
      let access_key_id = take_key auth env parse_string "access_key_id" in
      let region = take_key auth env parse_string "region" in
      let service = take_key auth env parse_string "service" in
      AWS_SIGV4 { secret_access_key; access_key_id; region; service }
  | auth_ty ->
      error_at_key env.id key
        ("Unknown authorization type requested to be added: " ^ auth_ty)

let parse_list env (key : key) f x =
  match x.G.e with
  | G.Container (Array, (_, xs, _)) -> List_.map (f env) xs
  | _ -> error_at_key env.id key ("Expected a list for " ^ fst key)

let parse_listi env (key : key) f x =
  let get_component i x =
    let env = { env with path = string_of_int i :: env.path } in
    f env x
  in
  match x.G.e with
  | G.Container (Array, (_, xs, _)) -> List_.mapi get_component xs
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
  | G.L (Int pi) -> pi
  | G.L (String (_, (s, _), _)) -> (
      match Parsed_int.of_string_opt s with
      | Some i -> i
      | None -> error_at_key env.id key (spf "parse_int for %s" (fst key)))
  | G.L (Float (Some f, _)) -> (
      let i = Parsed_int.of_float f in
      match Parsed_int.to_float_opt i with
      | Some f' when f =*= f' -> i
      | _ -> error_at_key env.id key "not an int")
  | _x -> error_at_key env.id key (spf "parse_int for %s" (fst key))

(* "Strict", because this function demands we can represent the integer inside. *)
let parse_int_strict env (key : key) x =
  match x.G.e with
  | G.L (Int (Some i64, _)) -> Int64.to_int i64
  | G.L (String (_, (s, _), _)) -> (
      match Parsed_int.of_string_opt s with
      | Some (Some i64, _) -> Int64.to_int i64
      | _ -> error_at_key env.id key (spf "parse_int for %s" (fst key)))
  | G.L (Float (Some f, _)) -> (
      let pi = Parsed_int.of_float f in
      match (pi, Parsed_int.to_float_opt pi) with
      | (Some i64, _), Some f' when f =*= f' -> Int64.to_int i64
      | _ -> error_at_key env.id key "not an int")
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

(*****************************************************************************)
(* Other *)
(*****************************************************************************)

let parse_regexp env (s, t) =
  (* We try to compile the regexp just to make sure it's valid, but we store
   * the raw string, see notes attached to 'Xpattern.xpattern_kind'. *)
  try
    (* calls `pcre.compile` *)
    Metavariable.mvars_of_regexp_string s
    |> List.iter (fun mvar ->
           if not (Metavariable.is_metavar_name mvar) then
             Logs.warn (fun m ->
                 m ~tags
                   "Found invalid metavariable capture group name `%s` for \
                    regexp `%s` -- no binding produced"
                   mvar s));
    s
  with
  | Pcre2.Error exn ->
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

let parse_metavar_cond env key s = parse_python_expression env key s
