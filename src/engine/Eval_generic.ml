(* Yoann Padioleau
 *
 * Copyright (C) 2019-2022 Semgrep Inc.
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
module J = JSON
module Log = Log_engine.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A simple interpreter for a simple subset of the generic AST.
 *
 * This can be used to safely execute a subset of pattern-where-python:
 * expressions, and to handle metavariable-comparison:.
 * It is safe because OCaml itself does not provide any 'eval()',
 * so we just execute code that we can explicitely handle.
 *
 * related work:
 * - https://github.com/google/cel-spec by Google
 * - https://github.com/facebook/Haxl by Facebook (was called FXL before)
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type value =
  | Bool of bool
  | Int of int64
  | Float of float
  | String of string (* string without the enclosing '"' *)
  | List of value list
  (* default case where we don't really have good builtin operations.
   * This should be a AST_generic.any once parsed.
   * See JSON_report.json_metavar().
   *)
  | AST of string (* any AST, e.g., "x+1" *)
(* less: Id of string (* simpler to merge with AST *) *)
[@@deriving show]

type env = {
  mvars : (MV.mvar, value) Hashtbl.t;
  constant_propagation : bool;
  file : Fpath.t;
      (** The file that we are currently matching the AST of. We need this so that
        `eval` can call the `Metavariable_regex` logic, whcih needs to produce
        matches that have position data localized to the originating file,
        rather than the originating match.
      *)
}

(* we restrict ourselves to simple expressions for now *)
type code = AST_generic.expr

exception NotHandled of code
exception NotInEnv of Metavariable.mvar

(*****************************************************************************)
(* JSON Parsing *)
(*****************************************************************************)
let metavar_of_json s : J.t -> value = function
  | J.Int i -> Int (Int64.of_int i)
  | J.Bool b -> Bool b
  | J.String s -> String s
  | J.Float f -> Float f
  | _ -> failwith (spf "wrong format for metavar %s" s)

(* JSON format used internally in semgrep-python for metavariable-comparison.
 * metavariable-comparison is actually now handled directly in semgrep-core,
 * so this format is not used anymore in semgrep-python, but we still
 * use it for some of our regression tests in tests/eval/.
 *)
let parse_json (file : string) : env * code =
  let json = UChan.with_open_in (Fpath.v file) JSON.json_of_chan in
  match json with
  | J.Object xs -> (
      match Assoc.sort_by_key_lowfirst xs with
      | [
       ("code", J.String code);
       ("language", J.String lang);
       ("metavars", J.Object xs);
      ] ->
          let lang =
            try Hashtbl.find Lang.lang_map lang with
            | Not_found -> failwith (spf "unsupported language %s" lang)
          in
          (* less: could also use Parse_pattern *)
          let code =
            match Parse_pattern.parse_pattern lang code with
            | Ok (G.E e) -> e
            | _ -> failwith "only expressions are supported"
          in
          let metavars =
            xs |> List_.map (fun (s, json) -> (s, metavar_of_json s json))
          in
          let env =
            {
              mvars = Hashtbl_.hash_of_list metavars;
              constant_propagation = true;
              file = Fpath.v file;
            }
          in
          (env, code)
      | _ -> failwith "wrong json format")
  | _ -> failwith "wrong json format"

(*****************************************************************************)
(* Reporting *)
(*****************************************************************************)

(* alt: could use exit code, or return JSON *)
let print_result (xopt : value option) =
  match xopt with
  (* nosem *)
  | None -> UCommon.pr "NONE"
  | Some v -> (
      match v with
      (* nosem *)
      | Bool b -> UCommon.pr (string_of_bool b)
      (* allow to abuse int to encode boolean ... ugly C tradition *)
      (* nosem *)
      | Int 0L -> UCommon.pr (string_of_bool false)
      (* nosem *)
      | Int _ -> UCommon.pr (string_of_bool true)
      (* nosem *)
      | _ -> UCommon.pr "NONE")
[@@action]

(*****************************************************************************)
(* Eval algorithm *)
(*****************************************************************************)

(* Helper function to convert string date to Epoch time, currently supports only yyyy-mm-dd format *)
let string_to_date s code : value =
  let yyyy_mm_dd = String.split_on_char '-' s in
  match yyyy_mm_dd with
  | [ y; m; d ] -> (
      match (int_of_string_opt y, int_of_string_opt m, int_of_string_opt d) with
      | Some yv, Some mv, Some dv when CalendarLib.Date.is_valid_date yv mv dv
        ->
          (*ok to put in arbitrary values for last 3 arguments since they are ignored *)
          let time : Unix.tm =
            {
              tm_sec = 0;
              tm_min = 0;
              tm_hour = 0;
              tm_mday = dv;
              tm_mon = mv;
              tm_year = yv - 1900;
              tm_wday = 0;
              tm_yday = 0;
              tm_isdst = false;
            }
          in
          Float (fst (Unix.mktime time))
      | _ -> raise (NotHandled code))
  | _ -> raise (NotHandled code)

(*
   Helper function to convert string duration into milliseconds for comparison.
   See https://prometheus.io/docs/prometheus/latest/querying/basics/#time-durations
   We do accept more duration strings here then prometheus which is okay.
*)
let string_duration_to_milliseconds s code : value =
  let int_of_string s =
    if s = "" then raise (NotHandled code) else int_of_string s
  in
  let l = String.length s in
  let rec loop (x, v) i =
    if i >= l then if x = "" then v else raise (NotHandled code)
    else
      let c = String.get s i in
      match c with
      | '0' .. '9' -> loop (x ^ (String.get s i |> String.make 1), v) (i + 1)
      | 's' ->
          let d = int_of_string x in
          loop ("", v + (d * 1000)) (i + 1)
      | 'm' ->
          let d = int_of_string x in
          if i < l - 1 then
            match String.get s (i + 1) with
            | 's' -> loop ("", v + d) (i + 2)
            | _ -> loop ("", v + (d * 60 * 1000)) (i + 1)
          else loop ("", v + (d * 60 * 1000)) (i + 1)
      | 'h' ->
          let d = int_of_string x in
          loop ("", v + (d * 60 * 60 * 1000)) (i + 1)
      | 'd' ->
          let d = int_of_string x in
          loop ("", v + (d * 24 * 60 * 60 * 1000)) (i + 1)
      | 'w' ->
          let d = int_of_string x in
          loop ("", v + (d * 7 * 24 * 60 * 60 * 1000)) (i + 1)
      | 'y' ->
          let d = int_of_string x in
          loop ("", v + (d * 365 * 24 * 60 * 60 * 1000)) (i + 1)
      | _ -> raise (NotHandled code)
  in
  if s = "" then raise (NotHandled code)
  else Int (Int64.of_int (loop ("", 0) 0))

let value_of_lit ~code x : value =
  match x with
  | G.Bool (b, _t) -> Bool b
  | G.String (_, (s, _t), _) -> String s
  (* big integers or floats can't be evaluated (Int (None, ...)) *)
  | G.Int (Some i, _) -> Int i
  | G.Float (Some f, _t) -> Float f
  | _ -> raise (NotHandled code)

let eval_regexp_matches ?(base_offset = 0) ~file ~regexp:re str =
  (* alt: take the text range of the metavariable in the original file,
     * and enforce e1 can only be an Id metavariable.
     * alt: let s = value_to_string v in
     * to convert anything in a string before using regexps on it
  *)
  let regexp = Pcre_.regexp ~flags:[ `ANCHORED ] re in
  Xpattern_match_regexp.regexp_matcher ~base_offset
    Xpattern_match_regexp.pcre_regex_functions str file regexp
[@@alert "-deprecated"]

let rec eval env code =
  match code.G.e with
  | G.L x -> value_of_lit ~code x
  | G.N (G.Id ((_, _), { id_svalue = { contents = Some (G.Lit lit) }; _ }))
  (* coupling: Constant_propagation.eval *)
  | G.Call
      ( { e = G.N (G.Id (("!dockerfile_expand!", _), _)); _ },
        ( _,
          [
            G.Arg
              {
                e =
                  G.N
                    (G.Id
                      ( (_, _),
                        { id_svalue = { contents = Some (G.Lit lit) }; _ } ));
                _;
              };
          ],
          _ ) )
  (* TERRAFORM:
     The propagated value is stored in the `id_svalue` of the DotAccess, for Terraform.
     We need to add this case here, or else we can't do metavariable analysis on an
     identifier which contains a propagated value, because identifiers look like
     DotAccesses in Terraform.
  *)
  | G.DotAccess
      ( { e = G.N (Id ((("local" | "var"), _), _)); _ },
        _,
        FN (Id (_, { id_svalue = { contents = Some (Lit lit); _ }; _ })) )
    when env.constant_propagation ->
      value_of_lit ~code lit
  | G.Call ({ e = IdSpecial (ConcatString op, _); _ }, (_, args, _)) ->
      String (eval_concat_string_op env code op args)
  | G.N (G.Id ((s, _t), _idinfo))
    when Mvar.is_metavar_name s || Mvar.is_metavar_ellipsis s -> (
      try Hashtbl.find env.mvars s with
      | Not_found ->
          Log.warn (fun m -> m "could not find a value for %s in env" s);
          raise (NotInEnv s))
  (* Python int() operator *)
  | G.Call ({ e = G.N (G.Id (("int", _), _)); _ }, (_, [ Arg e ], _)) -> (
      let v = eval env e in
      match v with
      | Int _ -> v
      | String s -> (
          match Parsed_int.of_string_opt s with
          | Some (Some i, _) -> Int i
          | _ -> raise (NotHandled code))
      | __else__ -> raise (NotHandled code))
  | G.Call ({ e = G.IdSpecial (G.Op op, _t); _ }, (_, args, _)) ->
      let values =
        args
        |> List_.map (function
             | G.Arg e -> eval env e
             | _ -> raise (NotHandled code))
      in
      eval_op op values code
  | G.Container (G.List, (_, xs, _)) ->
      let vs = List_.map (eval env) xs in
      List vs
  (* Emulate Python str just enough *)
  | G.Call ({ e = G.N (G.Id (("str", _), _)); _ }, (_, [ G.Arg e ], _)) ->
      let v = eval env e in
      String (eval_str env ~code v)
  (* Convert string to date *)
  | G.Call ({ e = G.N (G.Id (("strptime", _), _)); _ }, (_, [ Arg e ], _)) -> (
      let v = eval env e in
      match v with
      | String s -> string_to_date s code
      | __else__ -> raise (NotHandled code))
  | G.Call ({ e = G.N (G.Id (("today", _), _)); _ }, (_, _, _)) ->
      Float (Unix.time ())
  (* Convert prometheus duration strings to integer milliseconds*)
  | G.Call
      ( { e = G.N (G.Id (("parse_promql_duration", _), _)); _ },
        (_, [ Arg e ], _) ) -> (
      let v = eval env e in
      match v with
      | String s -> string_duration_to_milliseconds s code
      | __else__ -> raise (NotHandled code))
  (* Emulate Python re.match just enough *)
  | G.Call
      ( {
          e =
            G.DotAccess
              ( { e = G.N (G.Id (("re", _), _)); _ },
                _,
                FN (Id (("match", _), _)) );
          _;
        },
        (_, [ G.Arg { e = G.L (G.String (_, (re, _), _)); _ }; G.Arg e ], _) )
    -> (
      match eval env e with
      | String str ->
          let v : value =
            match eval_regexp_matches ~file:env.file ~regexp:re str with
            | [] -> Bool false
            | _ -> Bool true
          in
          Log.debug (fun m ->
              m "regexp %s on %s return %s" re str (show_value v));
          v
      | _ -> raise (NotHandled e))
  | _ -> raise (NotHandled code)

and eval_concat_string_op env code _op args =
  (* The op appears to not be important. We can just concatenate all the
     arguments.
  *)
  args |> List_.map (eval_concat_string_element env code) |> String.concat ""

and eval_concat_string_element env code arg =
  match arg with
  | G.Arg { e = L (G.String (_, (s, _), _)); _ } -> s
  | G.Arg
      {
        e = G.Call ({ e = IdSpecial (ConcatString op, _); _ }, (_, args, _));
        _;
      } ->
      eval_concat_string_op env code op args
  | G.Arg
      {
        e =
          Call
            ( { e = IdSpecial (InterpolatedElement, _); _ },
              (_, [ G.Arg elem ], _) );
        _;
      }
  | G.Arg elem ->
      let v = eval env elem in
      eval_str env ~code v
  | _ -> raise (NotHandled code)

and eval_op op values code =
  match (op, values) with
  | _op, [ AST _; _ ]
  | _op, [ _; AST _ ] ->
      (* To compare `AST` values one needs to explicitly use the `str()`
       * function! Otherwise we would introduce regressions. *)
      raise (NotHandled code)
  | G.And, [ Bool b1; Bool b2 ] -> Bool (b1 && b2)
  | G.Not, [ Bool b1 ] -> Bool (not b1)
  | G.Or, [ Bool b1; Bool b2 ] -> Bool (b1 || b2)
  | G.Gt, [ Int i1; Int i2 ] -> Bool (i1 > i2)
  | G.Gt, [ Float i1; Float i2 ] -> Bool (i1 > i2)
  | G.Gt, [ Int i1; Float i2 ] -> Bool (Int64.to_float i1 > i2)
  | G.Gt, [ Float i1; Int i2 ] -> Bool (i1 > Int64.to_float i2)
  | G.GtE, [ Int i1; Int i2 ] -> Bool (i1 >= i2)
  | G.GtE, [ Float i1; Float i2 ] -> Bool (i1 >= i2)
  | G.GtE, [ Int i1; Float i2 ] -> Bool (Int64.to_float i1 >= i2)
  | G.GtE, [ Float i1; Int i2 ] -> Bool (i1 >= Int64.to_float i2)
  | G.Lt, [ Int i1; Int i2 ] -> Bool (i1 < i2)
  | G.Lt, [ Float i1; Float i2 ] -> Bool (i1 < i2)
  | G.Lt, [ Int i1; Float i2 ] -> Bool (Int64.to_float i1 < i2)
  | G.Lt, [ Float i1; Int i2 ] -> Bool (i1 < Int64.to_float i2)
  | G.LtE, [ Int i1; Int i2 ] -> Bool (i1 <= i2)
  | G.LtE, [ Float i1; Float i2 ] -> Bool (i1 <= i2)
  | G.LtE, [ Int i1; Float i2 ] -> Bool (Int64.to_float i1 <= i2)
  | G.LtE, [ Float i1; Int i2 ] -> Bool (i1 <= Int64.to_float i2)
  | G.Div, [ Int i1; Int i2 ] -> Int Int64_.(i1 / i2)
  | G.Div, [ Float i1; Float i2 ] -> Float (i1 /. i2)
  | G.Div, [ Int i1; Float i2 ] -> Float (Int64.to_float i1 /. i2)
  | G.Div, [ Float i1; Int i2 ] -> Float (i1 /. Int64.to_float i2)
  | G.Minus, [ Int i1 ] -> Int (Int64.neg i1)
  | G.Minus, [ Float i1 ] -> Float (-.i1)
  | G.Minus, [ Int i1; Int i2 ] -> Int Int64_.(i1 - i2)
  | G.Minus, [ Float i1; Float i2 ] -> Float (i1 -. i2)
  | G.Minus, [ Int i1; Float i2 ] -> Float (Int64.to_float i1 -. i2)
  | G.Minus, [ Float i1; Int i2 ] -> Float (i1 -. Int64.to_float i2)
  | G.Mod, [ Int i1; Int i2 ] -> Int Int64_.(i1 mod i2)
  | G.Mod, [ Float i1; Float i2 ] -> Float (Float.rem i1 i2)
  | G.Mod, [ Int i1; Float i2 ] -> Float (Float.rem (Int64.to_float i1) i2)
  | G.Mod, [ Float i1; Int i2 ] -> Float (Float.rem i1 (Int64.to_float i2))
  | G.Mult, [ Int i1; Int i2 ] -> Int Int64_.(i1 * i2)
  | G.Mult, [ Float i1; Float i2 ] -> Float (i1 *. i2)
  | G.Mult, [ Int i1; Float i2 ] -> Float (Int64.to_float i1 *. i2)
  | G.Mult, [ Float i1; Int i2 ] -> Float (i1 *. Int64.to_float i2)
  | G.Plus, [ Int i1 ] -> Int i1
  | G.Plus, [ Float i1 ] -> Float i1
  | G.Plus, [ Int i1; Int i2 ] -> Int Int64_.(i1 + i2)
  | G.Plus, [ Float i1; Float i2 ] -> Float (i1 +. i2)
  | G.Plus, [ Int i1; Float i2 ] -> Float (Int64.to_float i1 +. i2)
  | G.Plus, [ Float i1; Int i2 ] -> Float (i1 +. Int64.to_float i2)
  | G.Pow, [ Int i1; Int i2 ] -> Int (Int64_.power i1 i2)
  | G.Pow, [ Float i1; Int i2 ] -> Float (i1 ** Int64.to_float i2)
  | G.Pow, [ Int i1; Float i2 ] -> Float (Int64.to_float i1 ** i2)
  | G.Pow, [ Float i1; Float i2 ] -> Float (i1 ** i2)
  | G.BitNot, [ Int i1 ] -> Int (Int64.lognot i1)
  | G.BitAnd, [ Int i1; Int i2 ] -> Int (Int64.logand i1 i2)
  | G.BitOr, [ Int i1; Int i2 ] -> Int (Int64.logor i1 i2)
  | G.BitXor, [ Int i1; Int i2 ] -> Int (Int64.logxor i1 i2)
  | G.Eq, [ Int v1; Float v2 ] -> Bool (Int64.to_float v1 =*= v2)
  | G.Eq, [ Float v1; Int v2 ] -> Bool (v1 =*= Int64.to_float v2)
  (* TODO? dangerous use of polymorphic =*= ? *)
  | G.Eq, [ v1; v2 ] -> Bool (v1 =*= v2)
  | G.NotEq, [ Int v1; Float v2 ] -> Bool (Int64.to_float v1 <> v2)
  | G.NotEq, [ Float v1; Int v2 ] -> Bool (v1 <> Int64.to_float v2)
  (* TODO? dangerous use of polymorphic <> ? *)
  | G.NotEq, [ v1; v2 ] -> Bool (v1 <> v2)
  | G.In, [ v1; v2 ] -> (
      match (v1, v2) with
      | _, List xs -> Bool (List.mem v1 xs)
      | String v1, String v2 ->
          Bool (Common2.string_match_substring (Str.regexp_string v1) v2)
      | _ -> Bool false)
  | G.NotIn, [ v1; v2 ] -> (
      match (v1, v2) with
      | _, List _
      | String _, String _ ->
          (* Just negate the "in" *)
          eval_op G.Not [ eval_op G.In values code ] code
      | _ -> Bool false)
  (* less: it would be better to show the actual values not handled,
   * rather than the code, because this may differ as the code does not
   * contained the resolved content of metavariables *)
  | _ -> raise (NotHandled code)

and eval_str _env ~code v =
  let str =
    match v with
    | Bool b -> string_of_bool b
    | Int i -> Int64.to_string i
    | Float f -> string_of_float f
    | String s -> s
    | AST s -> s
    | List _ -> raise (NotHandled code)
  in
  str

(*****************************************************************************)
(* Env builders *)
(*****************************************************************************)

let text_of_binding mvar mval =
  match mval with
  | MV.Text (text, _, _) ->
      (* Note that `text` may be produced by constant folding, in which
       * case we will not have range info. *)
      Some text
  (* There are a few places in Generic_vs_generic where we build artificial
   * code on-the-fly (e.g., a Name from an ImportedEntity), in which case the
   * tokens in this code should not be used to get the string content
   * of the code. Unfortunately, a metavariable can be bound to such
   * code.
   * In that case, it's better to pretty print the code rather than using
   * Visitor_AST.range_of_any_opt and Range.contents_at_range below.
   *
   * The 'not is_hidden' guard is to avoid to pretty print
   * artificial identifiers such as "builtin__include" in PHP that
   * we generate during parsing.
   * TODO: get rid of the ugly __builtin__ once we've fixed
   * ast_php.ml and removed the builtin() function.
   *
   * TODO: handle also MV.Name, MV.E of DotAccess; maybe use
   * Pretty_print/Ugly_print to factorize work.
   *)
  | MV.Id ((s, _tok), Some { id_flags; _ })
    when (not (s =~ "^__builtin.*")) && not (IdFlags.is_hidden !id_flags) ->
      Some s
  | MV.Id ((s, _tok), None) when not (s =~ "^__builtin.*") -> Some s
  | _ -> (
      let any = MV.mvalue_to_any mval in
      match AST_generic_helpers.range_of_any_opt any with
      | None ->
          (* TODO: Report a warning to the user? *)
          Log.warn (fun m ->
              m "We lack range info for metavariable %s: %s" mvar
                (G.show_any any));
          None
      | Some (min, max) ->
          let file = min.Tok.pos.file in
          let range = Range.range_of_token_locations min max in
          Some (Range.content_at_range file range))

let string_of_binding mvar mval =
  let* x = text_of_binding mvar mval in
  Some (mvar, (AST x : value))

let bindings_to_env (config : Rule_options.t) ~file bindings =
  let constant_propagation = config.constant_propagation in
  let mvars =
    bindings
    |> List_.filter_map (fun (mvar, mval) ->
           let try_bind_to_exp e =
             try
               Some
                 ( mvar,
                   eval
                     { mvars = Hashtbl.create 0; constant_propagation; file }
                     e )
             with
             | NotHandled _
             | NotInEnv _ ->
                 (* These are expressions like `x` or `os.getenv("FOO")` that cannot
                  * be evaluated. Previously we just filtered out all these cases, but
                  * in some cases it's interesting to make comparisons based on the
                  * string representation of these expressions. For example, given
                  * $X and $Y binding to two code variables we may want to check
                  * whether both code variables have the same name (even if they are
                  *  in fact different variables). So, if we can obtain such a
                  * string representation, we add it to the environment here. *)
                 string_of_binding mvar mval
           in
           match mval with
           (* this way we can leverage the constant propagation analysis
              * in metavariable-comparison: too! This simplifies some rules.
           *)
           | MV.Id (i, Some id_info) ->
               try_bind_to_exp (G.e (G.N (G.Id (i, id_info))))
           | MV.E e -> try_bind_to_exp e
           | MV.Text (s, _, _) -> Some (mvar, String s)
           | x -> string_of_binding mvar x)
    |> Hashtbl_.hash_of_list
  in

  { mvars; constant_propagation; file }

let bindings_to_env_just_strings (config : Rule_options.t) ~file xs =
  let mvars =
    xs
    |> List_.filter_map (fun (mvar, mval) -> string_of_binding mvar mval)
    |> Hashtbl_.hash_of_list
  in

  { mvars; constant_propagation = config.constant_propagation; file }

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* for testing purpose, via semgrep-core -test_eval *)
let test_eval file =
  try
    let env, code = parse_json file in
    let res = eval env code in
    print_result (Some res)
  with
  | NotHandled e ->
      Log.warn (fun m -> m "expr not handled in test_eval: %s" (G.show_expr e));
      raise (NotHandled e)

(* We need to swallow most exns in eval_bool(). This is because the
 * metavariable-comparison code in [e] may be valid
 * (expressions we can handle), but the metavariables may bind to complex
 * expressions, or have the wrong types, which would generate a NotHandled.
 *
 * For example, in the Python insecure-file-permissions rule, we use
 * a complex set of patterns with the very general
 * 'os.$METHOD($FILE, $BITS)' and metavariable-comparison '$BITS <= 0o650',
 * which is ANDed with a metavariable-pattern to restrict later what
 * $METHOD can be. Unfortunately that means the first pattern can match
 * code like `os.getenv('a', 'defaulta')` where $BITS will bind
 * to a string literal, which can't be compared with '<= 0o650'.
 *)
let eval_opt env e =
  try Some (eval env e) with
  (* this can happen when a metavar is bound to a complex expression,
   * in which case it's filtered in bindings_to_env(), in which case
   * it generates a NotInEnv when we run eval with such an environment.
   *)
  | NotInEnv _ -> None
  | NotHandled e ->
      Log.err (fun m -> m "NotHandled: %s" (G.show_expr e));
      None

let eval_bool env e facts bindings =
  let res = eval_opt env e in
  match res with
  | Some (Bool b) -> b
  | Some res -> (
      Log.err (fun m -> m "not a boolean: %s" (show_value res));
      (* facts_satisfy_e is just a stub, but we intend to prioritize the results of
       * eval_bool, so if eval_opt returns a bool, that will be the source of truth.
       * otherwise (i.e. if eval_opt fails), we will resort to pattern when (if the
       * pro flag is enabled).
       *
       * TODO: eventually we should merge this into eval_opt. consider the condition
       * $X == 0 && $Y > 0, maybe $X == 0 needs const-prop info to be resolved,
       * and $Y > 0 needs the facts. perhaps instead of raise (NotHandled code) in
       * eval_op, we can first try to find a fact that implies $X > 0 or its negation.
       *)
      match !Dataflow_when.hook_facts_satisfy_e with
      | None -> false
      | Some facts_satisfy_e -> facts_satisfy_e bindings facts e)
  | None -> (
      Log.err (fun m -> m "got exn during eval_bool");
      match !Dataflow_when.hook_facts_satisfy_e with
      | None -> false
      | Some facts_satisfy_e -> facts_satisfy_e bindings facts e)
