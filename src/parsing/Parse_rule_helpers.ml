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

(* alt: use a separate Logs src "semgrep.parsing.rule" *)
module Log = Log_parsing.Log

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type key = string R.wrap

type env = {
  (* id of the current rule (needed by some exns) *)
  id : Rule_ID.t;
  (* analyzer of the current rule (needed by parse_pattern) *)
  target_analyzer : Analyzer.t;
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
   * Use instead dict_take_opt() or take_opt_no_env() otherwise
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

let yaml_error t s = Error (Rule_error.mk_error (InvalidYaml (s, t)))

let yaml_error_at_expr (e : G.expr) s =
  yaml_error (AST_generic_helpers.first_info_of_any (G.E e)) s

let yaml_error_at_key (key : key) s = yaml_error (snd key) s

let error (rule_id : Rule_ID.t) (t : Tok.t) (s : string) :
    ('a, Rule_error.t) result =
  Error
    (Rule_error.mk_error ~rule_id (InvalidRule (InvalidOther s, rule_id, t)))

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

let try_and_raise_invalid_pattern_if_error (env : env) (s, t)
    (f : unit -> ('a, Rule_error.t) Result.t) : ('a, Rule_error.t) Result.t =
  try f () with
  | (Time_limit.Timeout _ | UnixExit _) as e -> Exception.catch_and_reraise e
  (* TODO: capture and adjust pos of parsing error exns instead of using [t] *)
  | exn ->
      let error_kind : Rule_error.invalid_rule_kind =
        InvalidPattern (s, env.target_analyzer, Common.exn_to_s exn, env.path)
      in
      Error
        (Rule_error.mk_error ~rule_id:env.id
           (InvalidRule (error_kind, env.id, t)))

let parse_pattern_with_rule_error env (str, t) ?rule_options lang s =
  Parse_pattern.parse_pattern ?rule_options lang s
  |> Result.map_error (fun s ->
         (* NOTE: copied from try_and_raise_invalid_pattern_if_error while we
            migrate. Intended to do the same. *)
         let error_kind : Rule_error.invalid_rule_kind =
           InvalidPattern (str, env.target_analyzer, s, env.path)
         in
         Rule_error.mk_error ~rule_id:env.id
           (InvalidRule (error_kind, env.id, t)))

let check_that_dict_is_empty (dict : dict) : (unit, Rule_error.t) Result.t =
  if Hashtbl.length dict.h > 0 then
    let remaining_keys =
      dict.h |> Hashtbl.to_seq_keys |> List.of_seq |> List.sort String.compare
      |> String.concat ", "
    in
    yaml_error dict.first_tok
      ("Unknown or duplicate properties found in YAML object: " ^ remaining_keys)
  else Ok ()

(* sanity check there are no remaining fields in rd (rule dictionnary) *)
let warn_if_remaining_unparsed_fields (rule_id : Rule_ID.t) (rd : dict) : unit =
  (* less: we could return an error, but better to be fault-tolerant
   * to future extensions to the rule format
   *)
  rd.h |> Hashtbl_.hash_to_list
  |> List.iter (fun (k, _) ->
         (* nosemgrep: no-logs-in-library *)
         Logs.warn (fun m ->
             m "Skipping unknown field '%s' in rule %a" k Rule_ID.pp rule_id))

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Why do we need this generic_to_json function? Why would we want to convert
 * to JSON when we actually did lots of work to convert the YAML/JSON
 * in the generic AST to get proper error location. This is because
 * the 'metadata' field in Rule.ml is JSON.
 *)
let generic_to_json rule_id (key : key) ast : (J.t, Rule_error.t) Result.t =
  let rec aux x =
    match x.G.e with
    | G.L (Null _) -> Ok J.Null
    | G.L (Bool (b, _)) -> Ok (J.Bool b)
    | G.L (Float (Some f, _)) -> Ok (J.Float f)
    | G.L (Int pi) -> (
        match Parsed_int.to_int_opt pi with
        | None ->
            Log.err (fun m ->
                m "no value for integer %s" (G.show_expr_kind x.G.e));
            let t = AST_generic_helpers.first_info_of_any (G.E x) in
            error rule_id t "no value for generic integer"
        | Some i -> Ok (J.Int i))
    | G.L (String (_, (s, _), _)) ->
        (* should use the unescaped string *)
        Ok (J.String s)
    | G.Container (Array, (_, xs, _)) ->
        let/ xs = Base.Result.all (List_.map aux xs) in
        Ok (J.Array xs)
    | G.Container (Dict, (_, xs, _)) ->
        let/ xs =
          Base.Result.all
            (List_.map
               (fun x ->
                 match x.G.e with
                 | G.Container
                     ( G.Tuple,
                       (_, [ { e = L (String (_, (k, _), _)); _ }; v ], _) ) ->
                     (* should use the unescaped string *)
                     let/ v = aux v in
                     Ok (k, v)
                 | _ ->
                     let t = AST_generic_helpers.first_info_of_any (G.E x) in
                     error rule_id t
                       ("Expected key/value pair in " ^ fst key ^ " dictionary"))
               xs)
        in
        Ok (J.Object xs)
    | G.Alias (_alias, e) -> aux e
    | _ ->
        Log.err (fun m -> m "Unexpected yaml: %s" (G.show_expr_kind x.G.e));
        let t = AST_generic_helpers.first_info_of_any (G.E x) in
        error rule_id t "Unexpected generic representation of yaml"
  in
  let/ result = aux ast in
  Ok result

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

let parse_dict_helper opt_rule_id error_fun_f error_fun_d
    (enclosing_obj_name : string) (rule : G.expr) :
    (dict, Rule_error.t) Result.t =
  match rule.G.e with
  | G.Container (Dict, (l, fields, _r)) ->
      let dict = Hashtbl.create 10 in
      let result =
        fields
        |> List.fold_left
             (fun acc field ->
               let/ () = acc in
               match field.G.e with
               | G.Container
                   ( G.Tuple,
                     ( _,
                       [ { e = L (String (_, (key_str, t), _)); _ }; value ],
                       _ ) ) ->
                   if Hashtbl.mem dict key_str then
                     Error
                       (Rule_error.mk_error ?rule_id:opt_rule_id
                          (DuplicateYamlKey
                             (spf "duplicate key '%s' in dictionary" key_str, t)))
                   else (
                     Hashtbl.add dict key_str ((key_str, t), value);
                     Ok ())
               | _ -> error_fun_f field "Not a valid key value pair")
             (Ok ())
      in
      let/ () = result in
      Ok { h = dict; first_tok = l }
  | _ ->
      error_fun_d rule ("each " ^ enclosing_obj_name ^ " should be a dictionary")

(* Lookup a key from dict and remove it. *)
let dict_take_opt (dict : dict) (key_str : string) : (key * G.expr) option =
  let tbl = dict.h in
  let res = Hashtbl.find_opt tbl key_str in
  Hashtbl.remove tbl key_str;
  res

(* Mutates the Hashtbl! *)
let take_opt (dict : dict) (env : env)
    (f : env -> key -> G.expr -> ('a, Rule_error.t) Result.t) (key_str : string)
    : ('a option, Rule_error.t) Result.t =
  match dict_take_opt dict key_str with
  | Some (k, v) -> f env k v |> Result.map (fun x -> Some x)
  | None -> Ok None

(* Mutates the Hashtbl! *)
let take_key ?default (dict : dict) (env : env)
    (f : env -> key -> G.expr -> ('a, Rule_error.t) Result.t) (key_str : string)
    : ('a, Rule_error.t) Result.t =
  let/ res = take_opt dict env f key_str in
  match res with
  | Some res -> Ok res
  | None -> (
      match default with
      | Some x -> Ok x
      | None -> error env.id dict.first_tok ("Missing required field " ^ key_str)
      )

let fold_dict f dict x = Hashtbl.fold f dict.h x

let parse_dict (env : env) (enclosing_obj_name : string G.wrap) expr =
  parse_dict_helper (Some env.id) (error_at_expr env.id) (error_at_expr env.id)
    (fst enclosing_obj_name) expr

(*****************************************************************************)
(* Parsing methods for before env is created *)
(*****************************************************************************)

(* Mutates the Hashtbl! *)
let take_opt_no_env (dict : dict)
    (f : key -> G.expr -> ('a, Rule_error.t) Result.t) (key_str : string) :
    ('a option, Rule_error.t) Result.t =
  match Hashtbl.find_opt dict.h key_str with
  | Some (key, value) ->
      let result = f key value in
      Hashtbl.remove dict.h key_str;
      result |> Result.map (fun x -> Some x)
  | None -> Ok None

(* Mutates the Hashtbl! *)
let take_no_env (dict : dict) (f : key -> G.expr -> ('a, Rule_error.t) Result.t)
    (key_str : string) : ('a, Rule_error.t) Result.t =
  let/ res = take_opt_no_env dict f key_str in
  match res with
  | Some res -> Ok res
  | None -> yaml_error dict.first_tok ("Missing required field " ^ key_str)

let parse_dict_no_env enclosing_obj_name expr : (dict, Rule_error.t) Result.t =
  parse_dict_helper None yaml_error_at_expr yaml_error_at_expr
    enclosing_obj_name expr

let parse_string_wrap_no_env (key : key) x :
    (string * Tok.t, Rule_error.t) Result.t =
  match read_string_wrap x.G.e with
  | Some x -> Ok x
  | None -> yaml_error_at_key key ("Expected a string value for " ^ fst key)

let parse_rule_id_no_env (key : key) x :
    (Rule_ID.t * Tok.t, Rule_error.t) Result.t =
  let/ str, tok = parse_string_wrap_no_env key x in
  match Rule_ID.of_string_opt str with
  | Some x -> Ok (x, tok)
  | None ->
      yaml_error_at_key key
        ("Expected a valid rule ID at " ^ fst key ^ ". Got " ^ str)

let parse_list_no_env (key : key) f x : ('a list, Rule_error.t) Result.t =
  match x.G.e with
  | G.Container (Array, (_, xs, _)) ->
      let rec aux acc = function
        | [] -> Ok (List.rev acc)
        | x :: xs ->
            let/ res = f x in
            aux (res :: acc) xs
      in
      aux [] xs
  | _ -> yaml_error_at_key key ("Expected a list for " ^ fst key)

let parse_string_wrap_list_no_env (key : key) e :
    ((string * Tok.t) list, Rule_error.t) Result.t =
  let extract_string = function
    | { G.e = G.L (String (_, (value, t), _)); _ } -> Ok (value, t)
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
  | Some (value, t) -> Ok (value, t)
  | None -> error_at_key env.id key ("Expected a string value for " ^ fst key)

(* TODO: delete at some point, should use parse_string_wrap instead *)
let parse_string env (key : key) x =
  parse_string_wrap env key x |> Result.map fst

let parse_list env (key : key) f x =
  match x.G.e with
  | G.Container (Array, (_, xs, _)) -> List_.map (f env) xs |> Base.Result.all
  | _ -> error_at_key env.id key ("Expected a list for " ^ fst key)

let parse_listi env (key : key) f x =
  let get_component i x =
    let env = { env with path = string_of_int i :: env.path } in
    f env x
  in
  match x.G.e with
  | G.Container (Array, (_, xs, _)) ->
      List_.mapi get_component xs |> Base.Result.all
  | _ -> error_at_key env.id key ("Expected a list for " ^ fst key)

let parse_string_wrap_list conv env (key : key) e =
  let extract_string_wrap env = function
    | { G.e = G.L (String (_, (value, t), _)); _ } ->
        let/ x = conv env key value in
        Ok (x, t)
    | _ ->
        error_at_key env.id key
          ("Expected all values in the list to be strings for " ^ fst key)
  in
  parse_list env key extract_string_wrap e

let parse_bool env (key : key) x =
  match x.G.e with
  | G.L (String (_, ("true", _), _)) -> Ok true
  | G.L (String (_, ("false", _), _)) -> Ok false
  | G.L (Bool (b, _)) -> Ok b
  | _x -> error_at_key env.id key (spf "parse_bool for %s" (fst key))

let parse_int env (key : key) x =
  match x.G.e with
  | G.L (Int pi) -> Ok pi
  | G.L (String (_, (s, _), _)) -> (
      match Parsed_int.of_string_opt s with
      | Some i -> Ok i
      | None -> error_at_key env.id key (spf "parse_int for %s" (fst key)))
  | G.L (Float (Some f, _)) -> (
      let i = Parsed_int.of_float f in
      match Parsed_int.to_float_opt i with
      | Some f' when f =*= f' -> Ok i
      | _ -> error_at_key env.id key "not an int")
  | _x -> error_at_key env.id key (spf "parse_int for %s" (fst key))

(* "Strict", because this function demands we can represent the integer inside. *)
let parse_int_strict env (key : key) x =
  match x.G.e with
  | G.L (Int (Some i64, _)) -> Ok (Int64.to_int i64)
  | G.L (String (_, (s, _), _)) -> (
      match Parsed_int.of_string_opt s with
      | Some (Some i64, _) -> Ok (Int64.to_int i64)
      | _ -> error_at_key env.id key (spf "parse_int for %s" (fst key)))
  | G.L (Float (Some f, _)) -> (
      let pi = Parsed_int.of_float f in
      match (pi, Parsed_int.to_float_opt pi) with
      | (Some i64, _), Some f' when f =*= f' -> Ok (Int64.to_int i64)
      | _ -> error_at_key env.id key "not an int")
  | _x -> error_at_key env.id key (spf "parse_int for %s" (fst key))

(* Either an object with one required string field or just a string.
   See mli for examples. *)
let parse_variant ?(kind_field_name = "kind") env (key : key) (expr : G.expr) =
  match expr.e with
  | G.L (String (_, (kind, _), _)) -> Ok (kind, None)
  | _ ->
      let/ dict = parse_dict env key expr in
      let/ kind = take_key dict env parse_string kind_field_name in
      Ok (kind, Some dict)

(*****************************************************************************)
(* Parsers for specialized types *)
(*****************************************************************************)

let parse_rule_id env (key : key) x : (Rule_ID.t * Tok.t, Rule_error.t) Result.t
    =
  let/ str, tok = parse_string_wrap_no_env key x in
  match Rule_ID.of_string_opt str with
  | Some x -> Ok (x, tok)
  | None ->
      error_at_key env.id key ("Expected a valid rule ID. Instead got " ^ str)

let parse_http_method env (key : key) x =
  let/ meth = parse_string env key x in
  match meth with
  | "DELETE" -> Ok `DELETE
  | "GET" -> Ok `GET
  | "HEAD" -> Ok `HEAD
  | "POST" -> Ok `POST
  | "PUT" -> Ok `PUT
  | _ -> error_at_key env.id key ("non-supported HTTP method: " ^ meth)

let parse_auth env (key : key) x : (Rule.auth, Rule_error.t) Result.t =
  let/ auth = parse_dict env key x in
  let/ type_ = take_key auth env parse_string "type" in
  match type_ with
  | "sigv4" ->
      let/ secret_access_key =
        take_key auth env parse_string "secret_access_key"
      in
      let/ access_key_id = take_key auth env parse_string "access_key_id" in
      let/ region = take_key auth env parse_string "region" in
      let/ service = take_key auth env parse_string "service" in
      Ok (R.AWS_SIGV4 { secret_access_key; access_key_id; region; service })
  | auth_ty ->
      error_at_key env.id key
        ("Unknown authorization type requested to be added: " ^ auth_ty)

let parse_str_or_dict env (value : G.expr) :
    ((G.ident, dict) Either.t, Rule_error.t) Result.t =
  match value.G.e with
  | G.L (String (_, (value, t), _)) ->
      (* should use the unescaped string *)
      Ok (Left (value, t))
  | G.L (Float (Some n, t)) ->
      Ok
        (if Float.is_integer n then
           (* Needed since otherwise 1.0 would be printed as 1. *)
           Left (string_of_int (Float.to_int n), t)
         else Left (string_of_float n, t))
  | G.N (Id ((value, t), _)) -> Ok (Left (value, t))
  | G.Container (Dict, _) ->
      let/ x = parse_dict env ("<TODO>", G.fake "<TODO>") value in
      Ok (Either.Right x)
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
    Mvar.mvars_of_regexp_string s
    |> List.iter (fun mvar ->
           if not (Mvar.is_metavar_name mvar) then
             Log.warn (fun m ->
                 m
                   "Found invalid metavariable capture group name `%s` for \
                    regexp `%s` -- no binding produced"
                   mvar s));
    Ok s
  with
  | Pcre2.Error exn ->
      Error
        (Rule_error.mk_error ~rule_id:env.id
           (InvalidRule (InvalidRegexp (pcre_error_to_string s exn), env.id, t)))

let parse_python_expression env key s =
  try
    let lang = Lang.Python in
    (* todo? use lang in env? *)
    match Parse_pattern.parse_pattern lang s with
    | Ok (AST_generic.E e) -> Ok e
    | Error s -> error_at_key env.id key s
    | _ -> error_at_key env.id key "not a Python expression"
  with
  | (Time_limit.Timeout _ | UnixExit _) as e -> Exception.catch_and_reraise e
  | exn -> error_at_key env.id key ("exn: " ^ Common.exn_to_s exn)

let parse_metavar_cond env key s = parse_python_expression env key s
