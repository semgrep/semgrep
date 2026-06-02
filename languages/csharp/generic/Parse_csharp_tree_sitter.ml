(* Sjoerd Langkemper
 *
 * Copyright (c) 2021, 2024 Semgrep Inc.
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
open Either_
open Fpath_.Operators
module CST = Tree_sitter_c_sharp.CST
module H = Parse_tree_sitter_helpers
open AST_generic
module G = AST_generic
module H2 = AST_generic_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Csharp parser using tree-sitter-lang/semgrep-charp and converting
 * directly to AST_generic.ml
 *
 * TODO:
 *  - lots of TODO in this file ... it parses correctly (no exn raised),
 *    but lots of constructs are not converted correctly to the generic AST
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = unit H.env

let token = H.token
let str = H.str
let fb = Tok.unsafe_fake_bracket

(* less: we should check we consume all constraints *)
let type_parameters_with_constraints (tparams : type_parameters option)
    constraints : type_parameters option =
  match tparams with
  (* TODO: possible to have constraints without tparams? *)
  | None -> None
  | Some (lt, tparams, gt) ->
      let tparams' =
        tparams
        |> List.map (function
          (* we do not generate those semgrep constructs for now in
           * semgrep-java, we parse Java patterns in the Java pfff parser *)
          | TParamEllipsis _ -> raise Impossible
          | OtherTypeParam (x, anys) ->
              (* TODO: add constraints *)
              OtherTypeParam (x, anys)
          | TP tparam -> (
              let with_constraints =
                constraints
                |> List.find_opt (fun (id, _xs) -> fst id = fst tparam.tp_id)
              in
              match with_constraints with
              | Some (_id, xs) ->
                  let _more_constraintsTODO, more_bounds =
                    xs |> Either_.partition (fun x -> x)
                  in
                  TP { tparam with tp_bounds = more_bounds @ tparam.tp_bounds }
              | None -> TP tparam))
      in
      Some (lt, tparams', gt)

let var_def_stmt (attrs : attribute list)
    (decls : (entity * variable_definition) list) (sc : Tok.t) : G.stmt =
  let stmts =
    decls
    |> List.map (fun (ent, def) ->
        let ent = { ent with attrs = ent.attrs @ attrs } in
        (ent, def))
    |> H2.add_semicolon_to_last_var_def_and_convert_to_stmts sc
  in
  G.stmt1 stmts

(* TODO? integrate in AST_generic at some point? or extend
 * AST_generic.comprehension to support also the Group/Into/Join/OrderBy?
 *)
type linq_query_part =
  (* comprehension/SQL like *)
  | From of tok * (type_ option * ident) * expr
  | Select of tok * expr
  | Where of tok * expr
  (* PL like *)
  | Let of tok * ident * expr
  | Into of tok * ident * linq_query_part list
  (* SQL like *)
  | Group of tok * expr * expr
  | Join of tok * (type_ option * ident) * expr * expr * expr * ident option
  | OrderBy of tok * (expr * direction) list

and direction = Ascending | Descending

let param_from_lambda_params lambda_params =
  match lambda_params with
  | [] -> failwith "empty lambda_params"
  | [ id ] ->
      Param
        {
          pname = Some id;
          ptype = None;
          pdefault = None;
          pattrs = [];
          pinfo = empty_id_info ();
        }
  | ids ->
      let ids = List.map (fun id -> PatId (id, empty_id_info ())) ids in
      ParamPattern (PatTuple (fb ids))

(* create lambda lambda_params -> expr *)
let create_lambda lambda_params expr =
  let fparams =
    Tok.unsafe_fake_bracket [ param_from_lambda_params lambda_params ]
  in
  Lambda
    {
      fkind = (Arrow, fake "=>");
      fparams;
      frettype = None;
      fbody = FBExpr expr;
    }
  |> G.e

(* create lambda (lambda_params, ident) -> (lambda_params..., ident) *)
let create_join_result_lambda lambda_params ident =
  let p1 = param_from_lambda_params lambda_params in
  let p2 = Param (param_of_id ident) in
  let fparams = fb [ p1; p2 ] in
  let ids =
    lambda_params @ [ ident ]
    |> List.map (fun id -> N (Id (id, empty_id_info ())) |> G.e)
  in
  let expr = G.Container (G.Tuple, fb ids) |> G.e in
  Lambda
    {
      fkind = (Arrow, fake "=>");
      fparams;
      frettype = None;
      fbody = FBExpr expr;
    }
  |> G.e

(* create a new lambda in the form
 * base_expr.funcname(lambda_params => expr)
 *)
let call_lambda base_expr funcname tok funcs =
  (* let funcs = exprs |> List.map (fun expr -> create_lambda lambda_params expr) in *)
  let args = funcs |> List.map (fun func -> Arg func) in
  (* We use fake:true because the funcname is a fake identifier
   * that actually does not occur in the target code.
   *)
  let idinfo = empty_id_info ~fake:true () in
  let method_ =
    DotAccess (base_expr, tok, FN (Id ((funcname, tok), idinfo))) |> G.e
  in
  Call (method_, fb args) |> G.e

let rec call_orderby base_expr lambda_params tok orderings =
  match orderings with
  | [] -> base_expr
  | ht :: tl ->
      let expr, dir = ht in
      let funcname =
        match dir with
        | Ascending -> "OrderBy"
        | Descending -> "OrderByDescending"
      in
      let func = create_lambda lambda_params expr in
      let base_expr = call_lambda base_expr funcname tok [ func ] in
      call_orderby base_expr lambda_params tok tl

let rec linq_remainder_to_expr (query : linq_query_part list) (base_expr : expr)
    (lambda_params : ident list) =
  match query with
  | [] -> base_expr
  | ht :: tl -> (
      match ht with
      | Select (tok, expr) ->
          let func = create_lambda lambda_params expr in
          let base_expr = call_lambda base_expr "Select" tok [ func ] in
          linq_remainder_to_expr tl base_expr lambda_params
      | Where (tok, expr) ->
          let func = create_lambda lambda_params expr in
          let base_expr = call_lambda base_expr "Where" tok [ func ] in
          linq_remainder_to_expr tl base_expr lambda_params
      | Let (tok, ident, expr) ->
          (* base_expr.Select(lambda_params -> (lambda_params..., expr))
           * and add ident to lambda_params
           *)
          let ids =
            List.map
              (fun id -> N (Id (id, empty_id_info ())) |> G.e)
              lambda_params
          in
          let expr = Container (Tuple, fb (ids @ [ expr ])) |> G.e in
          let func = create_lambda lambda_params expr in
          let base_expr = call_lambda base_expr "Select" tok [ func ] in
          let lambda_params = lambda_params @ [ ident ] in
          linq_remainder_to_expr tl base_expr lambda_params
      | Group (tok, vals, key) ->
          (* base_expr.GroupBy(lambda_params -> key, lambda_params -> vals)
           * and clear lambda_params
           *)
          let key_func = create_lambda lambda_params key in
          let val_func = create_lambda lambda_params vals in
          let base_expr =
            call_lambda base_expr "GroupBy" tok [ key_func; val_func ]
          in
          linq_remainder_to_expr tl base_expr []
      | Into (_tok, ident, remainder) ->
          (* TODO can we throw away tl? *)
          linq_remainder_to_expr remainder base_expr [ ident ]
      | From (tok, (_type, ident), expr) ->
          (* base_expr.SelectMany(lambda_params -> expr, (lambda_params, col) -> (lambda_params, col))
           * and add ident to lambda_params
           *)
          let sel_func = create_lambda lambda_params expr in
          let res_func = create_join_result_lambda lambda_params ident in
          let base_expr =
            call_lambda base_expr "SelectMany" tok [ sel_func; res_func ]
          in
          let lambda_params = lambda_params @ [ ident ] in
          linq_remainder_to_expr tl base_expr lambda_params
      | OrderBy (tok, orderings) ->
          let base_expr = call_orderby base_expr lambda_params tok orderings in
          linq_remainder_to_expr tl base_expr lambda_params
      | Join (tok, (_type, ident), enum, left, right, None) ->
          (* base_expr.Join(enum, lambda_params -> left, ident -> right, (lambda_params, ident) -> (lambda_params, ident))
           * and add ident to lambda_params
           *)
          let left_func = create_lambda lambda_params left in
          let right_func = create_lambda [ ident ] right in
          let res_func = create_join_result_lambda lambda_params ident in
          let base_expr =
            call_lambda base_expr "Join" tok
              [ enum; left_func; right_func; res_func ]
          in
          let lambda_params = lambda_params @ [ ident ] in
          linq_remainder_to_expr tl base_expr lambda_params
      | Join (tok, (_type, ident), enum, left, right, Some into) ->
          (* base_expr.GroupJoin(enum, lambda_params -> left, ident -> right, (lambda_params, into) -> (lambda_params, into))
                * and add into to lambda_params
          *)
          let left_func = create_lambda lambda_params left in
          let right_func = create_lambda [ ident ] right in
          let res_func = create_join_result_lambda lambda_params into in
          let base_expr =
            call_lambda base_expr "GroupJoin" tok
              [ enum; left_func; right_func; res_func ]
          in
          let lambda_params = lambda_params @ [ into ] in
          linq_remainder_to_expr tl base_expr lambda_params)

let linq_to_expr (from : linq_query_part) (body : linq_query_part list) =
  match from with
  | From (_, (_type, id), collection) ->
      linq_remainder_to_expr body collection [ id ]
  | _ -> raise Impossible

let new_index_from_end tok expr =
  let name =
    H2.name_of_ids [ ("System", fake "System"); ("Index", fake "Index") ]
  in
  let index = TyN name |> G.t in
  New
    ( tok,
      index,
      empty_id_info (),
      fb [ Arg expr; Arg (L (Bool (true, fake "true")) |> G.e) ] )
  |> G.e

module List = struct
  include List

  (* not available in 4.09 *)
  let concat_map f xs = List.map f xs |> List_.flatten
end

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-java/Boilerplate.ml *)

(**
   Boilerplate to be used as a template when mapping the csharp CST
   to another type of tree.
*)

let todo_expr _env tok = G.OtherExpr (("CSharpTodo", tok), []) |> G.e
let todo_stmt _env tok = G.OtherStmt (G.OS_Todo, [ G.Tk tok ]) |> G.s
let todo_pat _env tok = G.OtherPat (("Todo", tok), [])
let todo_attr _env tok = G.OtherAttribute (("Todo", tok), [])
let todo_type _env tok = G.OtherType (("Todo", tok), []) |> G.t

let _opt_semi (env : env) (v1 : CST.optional_semi) =
  (* Because of compromises we had to make in the grammar, to make
     it not take forever to build, opt_semi is not actually an option.
     We have no choice but to analyze the enclosed token and check
     if it's a semicolon or not.
  *)
  match snd v1 with
  | ";" -> Some (token env v1)
  | __else__ -> None

let map_anon_choice_static_d4628a0 (env : env)
    (x : CST.anon_choice_static_d4628a0) =
  match x with
  | `Static tok -> KeywordAttr (Static, token env tok)
  | `Async tok -> KeywordAttr (Async, token env tok)

(* Extract the underlying token from an `escape_sequence`. The
   semgrep-c-sharp extension broadens `escape_sequence` to also accept
   any `\.` so semgrep can parse real-world C# strings that Roslyn
   would reject (e.g. Windows paths inside non-verbatim string
   literals: `"\\share\images"`). Both choices land here as a token. *)
let escape_sequence_tok (x : CST.escape_sequence) =
  match x with
  | `Tok_choice_pat_8eb8081 t -> t
  | `Tok_pat_245b2d0 t -> t

let escape_sequence (env : env) (x : CST.escape_sequence) =
  String (fb (str env (escape_sequence_tok x)))

(* The new grammar inlines the assignment operator as an anonymous
   polymorphic variant inside assignment_expression; there is no longer a
   named CST.assignment_operator type. We let OCaml infer the input type. *)
let assignment_operator (env : env) x : operator wrap =
  match x with
  | `EQ tok -> (Eq, token env tok) (* "=" *)
  | `PLUSEQ tok -> (Plus, token env tok) (* "+=" *)
  | `DASHEQ tok -> (Minus, token env tok) (* "-=" *)
  | `STAREQ tok -> (Mult, token env tok) (* "*=" *)
  | `SLASHEQ tok -> (Div, token env tok) (* "/=" *)
  | `PERCEQ tok -> (Mod, token env tok) (* "%=" *)
  | `AMPEQ tok -> (BitAnd, token env tok) (* "&=" *)
  | `HATEQ tok -> (BitXor, token env tok) (* "^=" *)
  | `BAREQ tok -> (BitOr, token env tok) (* "|=" *)
  (* https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/operators/bitwise-and-shift-operators#right-shift-operator-
     https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/operators/bitwise-and-shift-operators#unsigned-right-shift-operator-
  *)
  | `LTLTEQ tok -> (LSL, token env tok) (* "<<=" *)
  | `GTGTEQ tok -> (ASR, token env tok) (* ">>=" *)
  | `GTGTGTEQ tok -> (LSR, token env tok) (* ">>>=" *)
  | `QMARKQMARKEQ tok -> (Nullish, token env tok)

(* "??=" *)

let boolean_literal (env : env) (x : CST.boolean_literal) =
  match x with
  | `True tok -> Bool (true, token env tok) (* "true" *)
  | `False tok -> Bool (false, token env tok)

(* "false" *)

let predefined_type (env : env) (tok : CST.predefined_type) =
  G.ty_builtin (str env tok)

let verbatim_string_literal (env : env) (tok : CST.verbatim_string_literal) =
  G.String (fb (str env tok))

(* verbatim_string_literal *)

let attribute_target_specifier (env : env)
    ((v1, v2) : CST.attribute_target_specifier) =
  let v1 =
    match v1 with
    | `Field tok -> token env tok (* "field" *)
    | `Event tok -> token env tok (* "event" *)
    | `Meth tok -> token env tok (* "method" *)
    | `Param tok -> token env tok (* "param" *)
    | `Prop tok -> token env tok (* "property" *)
    | `Ret tok -> token env tok (* "return" *)
    | `Type_599dcce tok -> token env tok (* "type" *)
    | `Type_2a87ba5 tok -> token env tok (* "typevar" *)
  in
  let v2 =
    token env v2
    (* ":" *)
  in
  (v1, v2)

(* note that there's no octal literal in C# so no need for
 * H.int_of_string_c_octal_opt
 *)
let integer_literal (env : env) (tok : CST.integer_literal) =
  let s, t = str env tok in
  (* integer_literal *)
  G.Int (Parsed_int.parse (s, t))

(* The new grammar inlines the operator into operator_declaration as an
   anonymous polymorphic variant. C# 14 also adds compound-assignment
   operator overloads (PLUSEQ, DASHEQ, ...). We accept the broader set. *)
let overloadable_operator (env : env) x =
  match x with
  | `BANG tok -> str env tok (* "!" *)
  | `TILDE tok -> str env tok (* "~" *)
  | `PLUSPLUS tok -> str env tok (* "++" *)
  | `DASHDASH tok -> str env tok (* "--" *)
  | `True tok -> str env tok (* "true" *)
  | `False tok -> str env tok (* "false" *)
  | `PLUS tok -> str env tok (* "+" *)
  | `DASH tok -> str env tok (* "-" *)
  | `STAR tok -> str env tok (* "*" *)
  | `SLASH tok -> str env tok (* "/" *)
  | `PERC tok -> str env tok (* "%" *)
  | `HAT tok -> str env tok (* "^" *)
  | `BAR tok -> str env tok (* "|" *)
  | `AMP tok -> str env tok (* "&" *)
  | `LTLT tok -> str env tok (* "<<" *)
  | `GTGT tok -> str env tok (* ">>" *)
  | `GTGTGT tok -> str env tok (* ">>>" *)
  | `EQEQ tok -> str env tok (* "==" *)
  | `BANGEQ tok -> str env tok (* "!=" *)
  | `GT tok -> str env tok (* ">" *)
  | `LT tok -> str env tok (* "<" *)
  | `GTEQ tok -> str env tok (* ">=" *)
  | `LTEQ tok -> str env tok (* "<=" *)
  | `PLUSEQ tok -> str env tok (* "+=" *)
  | `DASHEQ tok -> str env tok (* "-=" *)
  | `STAREQ tok -> str env tok (* "*=" *)
  | `SLASHEQ tok -> str env tok (* "/=" *)
  | `PERCEQ tok -> str env tok (* "%=" *)
  | `HATEQ tok -> str env tok (* "^=" *)
  | `BAREQ tok -> str env tok (* "|=" *)
  | `AMPEQ tok -> str env tok (* "&=" *)
  | `LTLTEQ tok -> str env tok (* "<<=" *)
  | `GTGTEQ tok -> str env tok (* ">>=" *)
  | `GTGTGTEQ tok -> str env tok (* ">>>=" *)

let modifier (env : env) (x : CST.modifier) =
  (* TODO these should all be KeywordAttr, but pfff doesn't know about all keywords *)
  match x with
  | `Abst tok -> KeywordAttr (Abstract, token env tok) (* "abstract" *)
  | `Async tok -> KeywordAttr (Async, token env tok) (* "async" *)
  | `Const tok -> KeywordAttr (Const, token env tok) (* "const" *)
  | `Extern tok -> KeywordAttr (Extern, token env tok) (* "extern" *)
  | `Fixed tok -> unhandled_keywordattr (str env tok)
  | `Inte tok -> unhandled_keywordattr (str env tok)
  | `New tok -> unhandled_keywordattr (str env tok)
  | `Over tok -> unhandled_keywordattr (str env tok)
  | `Part tok -> unhandled_keywordattr (str env tok)
  | `Priv tok -> KeywordAttr (Private, token env tok) (* "private" *)
  | `Prot tok -> KeywordAttr (Protected, token env tok) (* "protected" *)
  | `Public tok -> KeywordAttr (Public, token env tok) (* "public" *)
  | `Read tok -> KeywordAttr (Const, token env tok) (* "readonly" *)
  | `Requ tok -> unhandled_keywordattr (str env tok) (* "required" *)
  | `File tok -> unhandled_keywordattr (str env tok) (* "file" *)
  | `Sealed tok ->
      (* TODO we map Sealed to Final here, is that OK? *)
      KeywordAttr (Final, token env tok)
      (* "sealed" *)
  | `Static tok -> KeywordAttr (Static, token env tok) (* "static" *)
  | `Unsafe tok -> unhandled_keywordattr (str env tok)
  | `Virt tok -> unhandled_keywordattr (str env tok)
  | `Vola tok -> KeywordAttr (Volatile, token env tok)

(* "volatile" *)

let interpolation_format_clause (env : env)
    ((v1, v2) : CST.interpolation_format_clause) =
  let _v1 =
    token env v1
    (* ":" *)
  in
  let v2 =
    token env v2
    (* pattern "[^}\"]+" *)
  in
  v2

(* The new grammar collapses verbatim/raw/regular interpolated-string text
   into a single `interpolation_string_content` token. Escape sequences
   live as their own variant on `interpolated_string_content`. *)
let interpolated_verbatim_string_text (env : env)
    (tok : CST.interpolation_string_content) =
  String (fb (str env tok))

let interpolated_raw_string_text (env : env)
    (tok : CST.interpolation_string_content) =
  String (fb (str env tok))

let _map_anon_choice_ref_eec35e8 (env : env) (x : CST.anon_choice_ref_eec35e8) =
  match x with
  | `Ref tok -> (* "ref" *) token env tok
  | `Out tok -> (* "out" *) token env tok
  | `In tok -> (* "in" *) token env tok

let real_literal (env : env) (tok : CST.real_literal) =
  let s, t =
    str env tok
    (* real_literal *)
  in
  G.Float (float_of_string_opt s, t)

let _contextual_keywords env x =
  match x with
  | `Alias x (* "alias" *)
  | `Asce x (* "ascending" *)
  | `By x (* "by" *)
  | `Desc x (* "descending" *)
  | `Equals x (* "equals" *)
  | `File x (* "file" *)
  | `From x (* "from" *)
  | `Global x (* "global" *)
  | `Group x (* "group" *)
  | `Into x (* "into" *)
  | `Join x (* "join" *)
  | `Let x (* "let" *)
  | `Notn x (* "notnull" *)
  | `On x (* "on" *)
  | `Orde x (* "orderby" *)
  | `Scoped x (* "scoped" *)
  | `Select x (* "select" *)
  | `Unma x (* "unmanaged" *)
  | `Var x (* "var" *)
  | `When x (* "when" *)
  | `Where x (* "where" *)
  | `Yield x (* "yield" *)
  | `Add x (* "add" *)
  | `Get x (* "get" *)
  | `Remove x (* "remove" *)
  | `Set x (* "set" *)
  | `Dyna x (* "dynamic" *)
  | `Nameof x (* "nameof" *) ->
      str env x

(* The old grammar tagged contextual keywords as `Cont_keywos`. In the
   post-C#-14 grammar the same set (with additions) lives under
   `reserved_identifier`. Constructors `Type_599dcce`/`Type_2a87ba5`
   disambiguate "type"/"typevar" since both used to collide on `Type`. *)
let reserved_identifier (env : env) (x : CST.reserved_identifier) : ident =
  match x with
  | `Alias x | `Asce x | `By x | `Desc x | `Equals x | `Field x | `File x
  | `From x | `Global x | `Group x | `Into x | `Join x | `Let x | `Meth x
  | `Notn x | `On x | `Orde x | `Param x | `Prop x | `Scoped x | `Select x
  | `Type_599dcce x | `Type_2a87ba5 x | `Unma x | `Var x | `When x | `Where x
  | `Yield x ->
      str env x

let identifier (env : env) (tok : CST.identifier) : ident =
  match tok with
  | `Choice_id_tok (`Id_tok tok) -> str env tok
  | `Choice_id_tok (`Rese_id x) -> reserved_identifier env x
  | `Semg_meta tok -> str env tok

(* The new grammar collapses regular interpolated-string text into
   `interpolation_string_content`; escape sequences live as their own
   `Esc_seq` variant on `interpolated_string_content`. *)
let interpolated_string_text (env : env) (tok : CST.interpolation_string_content) =
  String (fb (str env tok))

(* escape_sequence *)

let rec variable_designation (env : env) (x : CST.variable_designation) =
  match x with
  | `Disc tok -> PatWildcard (token env tok) (* "_" *)
  | `Paren_var_desi (v1, v2, v3) ->
      let v1 =
        token env v1
        (* "(" *)
      in
      let v2 =
        match v2 with
        | Some (v1, v2) ->
            let v1 = variable_designation env v1 in
            let v2 =
              List.map
                (fun (v1, v2) ->
                  let _v1 =
                    token env v1
                    (* "," *)
                  in
                  let v2 = variable_designation env v2 in
                  v2)
                v2
            in
            v1 :: v2
        | None -> []
      in
      let v3 =
        token env v3
        (* ")" *)
      in
      PatTuple (v1, v2, v3)
  | `Id tok -> PatId (identifier env tok, empty_id_info ())

(* identifier *)

let join_into_clause (env : env) ((v1, v2) : CST.join_into_clause) =
  let _v1 =
    token env v1
    (* "into" *)
  in
  let v2 =
    identifier env v2
    (* identifier *)
  in
  v2

(* `identifier_or_global` was folded into the unified `identifier` rule:
   "global" is now one of the `reserved_identifier` alternatives, so an
   `identifier` already accepts it. Both helpers degenerate to `identifier`. *)
let identifier_or_global (env : env) (x : CST.identifier) = identifier env x

let identifier_or_global_qualifier (env : env) (x : CST.identifier) : G.ident =
  identifier env x

let rec tuple_pattern (env : env) ((v1, v2, v3, v4) : CST.tuple_pattern) =
  let v1 =
    token env v1
    (* "(" *)
  in
  let v2 = anon_choice_id_c036834 env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let _v1 =
          token env v1
          (* "," *)
        in
        let v2 = anon_choice_id_c036834 env v2 in
        v2)
      v3
  in
  let v4 =
    token env v4
    (* ")" *)
  in
  PatTuple (v1, v2 :: v3, v4)

and anon_choice_id_c036834 (env : env) (x : CST.anon_choice_id_c036834) =
  match x with
  | `Id tok ->
      let id =
        identifier env tok
        (* identifier *)
      in
      PatId (id, empty_id_info ())
  | `Disc tok ->
      let tok =
        token env tok
        (* "_" *)
      in
      PatWildcard tok
  | `Tuple_pat x -> tuple_pattern env x

(* `name_colon` and `name_equals` are no longer named CST types in the
   post-C#-14 grammar; both are inlined as `(identifier, ":" | "=")`
   tuples on attribute_argument and a few other rules. The helpers below
   accept the inlined shape and discard the punctuation token, matching
   the old behavior. *)
let name_colon (env : env) ((v1, v2) : CST.identifier * Tree_sitter_run.Token.t) =
  let v1 = identifier env v1 in
  let _v2 = token env v2 (* ":" *) in
  v1

let name_equals (env : env) ((v1, v2) : CST.identifier * Tree_sitter_run.Token.t) =
  let v1 = identifier env v1 in
  let _v2 =
    token env v2
    (* "=" *)
  in
  v1

let literal (env : env) (x : CST.literal) : literal =
  match x with
  | `Null_lit tok -> G.Null (token env tok) (* "null" *)
  | `Bool_lit x -> boolean_literal env x
  | `Char_lit (v1, v2, v3) ->
      let v1 =
        token env v1
        (* "'" *)
      in
      let s, t =
        match v2 with
        | `Char_lit_content tok -> str env tok (* pattern "[^'\\\\]" *)
        | `Esc_seq tok -> str env (escape_sequence_tok tok)
        (* escape_sequence *)
      in
      let v3 =
        token env v3
        (* "'" *)
      in
      Char (s, Tok.combine_toks v1 [ t; v3 ])
  | `Real_lit tok -> real_literal env tok (* real_literal *)
  | `Int_lit tok -> integer_literal env tok (* integer_literal *)
  | `Str_lit (v1, v2, v3, _v4TODO) ->
      (* v4 is the "string literal encoding" *)
      let l =
        token env v1
        (* "\"" *)
      in
      let xs =
        List.map
          (fun x ->
            match x with
            | `Str_lit_content tok -> str env tok (* pattern "[^\"\\\\\\n]+" *)
            | `Esc_seq tok -> str env (escape_sequence_tok tok)
            (* escape_sequence *))
          v2
      in
      let r = token env v3 in
      G.String (G.string_ (l, xs, r))
  | `Verb_str_lit tok -> verbatim_string_literal env tok
  | `Raw_str_lit (v1, v2, v3, v4_encoding) ->
      (* C# 11 raw string literal: """ <content> """[u8 encoding]?
         Take the content as the string body and combine the opening
         delimiter, closing delimiter, and optional `u8` suffix into a
         single bracket span so source-location info covers the whole
         literal. *)
      let l = token env v1 in
      let content = str env v2 in
      let r =
        let close = token env v3 in
        match v4_encoding with
        | Some enc -> Tok.combine_toks close [ token env enc ]
        | None -> close
      in
      G.String (l, content, r)

(* verbatim_string_literal *)

(* Synthesize a public field declaration from a primary-constructor
 * parameter. Per the C# 12 spec
 * https://learn.microsoft.com/en-us/dotnet/csharp/whats-new/tutorials/primary-constructors
 * primary-constructor parameters are surfaced as auto-captured members on
 * the type. Modeling them as `public field` instead of properties
 * (getter/setter) keeps the Generic AST simple and is sufficient for taint
 * propagation through `instance.x` reads — the fidelity loss
 * (field-vs-property distinction) doesn't matter to rule writers because
 * the matcher already treats both alike. *)
let parameter_to_field (p : G.parameter) : G.stmt option =
  match p with
  | G.Param { pname = Some n; ptype; pdefault; pattrs; _ } ->
      let public = G.KeywordAttr (G.Public, fake "public") in
      let entity = G.basic_entity ~attrs:(public :: pattrs) n in
      let def = G.VarDef { vinit = pdefault; vtype = ptype; vtok = G.no_sc } in
      Some (G.DefStmt (entity, def) |> G.s)
  | _ -> None

(* "void" *)
let rec type_pattern (env : env) (x : CST.type_pattern) = type_ env x

and variable_declaration (env : env) ((v1, v2, v3) : CST.variable_declaration) :
    (entity * variable_definition) list =
  let v1 = local_variable_type env v1 in
  let v2 = variable_declarator env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let _v1 =
          token env v1
          (* "," *)
        in
        let v2 = variable_declarator env v2 in
        v2)
      v3
  in
  let decls = v2 :: v3 in
  List.map
    (fun (ent, vardef) ->
      (ent, { vinit = vardef.vinit; vtype = v1; vtok = G.no_sc }))
    decls

and interpolation_alignment_clause (env : env)
    ((v1, v2) : CST.interpolation_alignment_clause) =
  let _v1 =
    token env v1
    (* "," *)
  in
  let v2 = expression env v2 in
  v2

and parenthesized_expression (env : env)
    ((_v1, v2, _v3) : CST.parenthesized_expression) =
  (* Due to ignoring the parentheses, an expression matched
     by Semgrep which translates to this tree-sitter construct
     will not encompass the parentheses.
     It seems this is standard practice though, as we also
     ignore parentheses in our C, CPP, and Java parsers.
     Possible TODO:
  *)
  non_lvalue_expression env v2

(* The new grammar makes postfix_unary a `(expression, operator)` tuple
   instead of three separate `Exp_*` variants. *)
and postfix_unary_expression (env : env) ((v1, v2) : CST.postfix_unary_expression) =
  let v1 = expression env v1 in
  match v2 with
  | `PLUSPLUS tok ->
      let t = token env tok (* "++" *) in
      Call (Special (IncrDecr (Incr, Postfix), t) |> G.e, fb [ Arg v1 ]) |> G.e
  | `DASHDASH tok ->
      let t = token env tok (* "--" *) in
      Call (Special (IncrDecr (Decr, Postfix), t) |> G.e, fb [ Arg v1 ]) |> G.e
  | `BANG tok ->
      let t = token env tok (* "!" *) in
      Call (Special (Op NotNullPostfix, t) |> G.e, fb [ Arg v1 ]) |> G.e

and when_clause (env : env) ((v1, v2) : CST.when_clause) =
  let _v1 =
    token env v1
    (* "when" *)
  in
  let v2 = expression env v2 in
  v2

and relational_pattern (env : env) (x : CST.relational_pattern) =
  match x with
  | `LT_exp (v1, v2) ->
      let v1 =
        token env v1
        (* "<" *)
      in
      let _v2 = expression env v2 in
      todo_pat env v1
  | `LTEQ_exp (v1, v2) ->
      let v1 =
        token env v1
        (* "<=" *)
      in
      let _v2 = expression env v2 in
      todo_pat env v1
  | `GT_exp (v1, v2) ->
      let v1 =
        token env v1
        (* ">" *)
      in
      let _v2 = expression env v2 in
      todo_pat env v1
  | `GTEQ_exp (v1, v2) ->
      let v1 =
        token env v1
        (* ">=" *)
      in
      let _v2 = expression env v2 in
      todo_pat env v1

and binary_expression (env : env) (x : CST.binary_expression) : G.expr =
  match x with
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "&&" *)
      in
      let v3 = expression env v3 in
      Call (Special (Op And, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "||" *)
      in
      let v3 = expression env v3 in
      Call (Special (Op Or, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_GTGT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* ">>" *)
      in
      let v3 = expression env v3 in
      Call (Special (Op ASR, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_GTGTGT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* ">>>" *)
      in
      let v3 = expression env v3 in
      Call (Special (Op LSR, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_LTLT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "<<" *)
      in
      let v3 = expression env v3 in
      Call (Special (Op LSL, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "&" *)
      in
      let v3 = expression env v3 in
      Call (Special (Op BitAnd, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "^" *)
      in
      let v3 = expression env v3 in
      Call (Special (Op BitXor, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "|" *)
      in
      let v3 = expression env v3 in
      Call (Special (Op BitOr, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_PLUS_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "+" *)
      in
      let v3 = expression env v3 in
      Call (Special (Op Plus, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_DASH_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "-" *)
      in
      let v3 = expression env v3 in
      Call (Special (Op Minus, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_STAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "*" *)
      in
      let v3 = expression env v3 in
      Call (Special (Op Mult, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_SLASH_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "/" *)
      in
      let v3 = expression env v3 in
      Call (Special (Op Div, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_PERC_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "%" *)
      in
      let v3 = expression env v3 in
      Call (Special (Op Mod, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_LT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "<" *)
      in
      let v3 = expression env v3 in
      Call (Special (Op Lt, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_LTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "<=" *)
      in
      let v3 = expression env v3 in
      Call (Special (Op LtE, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "==" *)
      in
      let v3 = expression env v3 in
      Call (Special (Op Eq, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_BANGEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "!=" *)
      in
      let v3 = expression env v3 in
      Call (Special (Op NotEq, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_GTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* ">=" *)
      in
      let v3 = expression env v3 in
      Call (Special (Op GtE, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_GT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* ">" *)
      in
      let v3 = expression env v3 in
      Call (Special (Op Gt, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_QMARKQMARK_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "??" *)
      in
      let v3 = expression env v3 in
      Call (Special (Op Nullish, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e

and block (env : env) ((v1, v2, v3) : CST.block) : stmt =
  let v1 =
    token env v1
    (* "{" *)
  in
  let v2 = List.map (statement env) v2 in
  let v3 =
    token env v3
    (* "}" *)
  in
  G.Block (v1, v2, v3) |> G.s

and variable_declarator (env : env) ((v1, v2, v3) : CST.variable_declarator) =
  let v1, pattern =
    match v1 with
    | `Id tok -> (identifier env tok, None) (* identifier *)
    | `Tuple_pat x ->
        let tok, _, _, _ = x in
        let id = (G.special_multivardef_pattern, token env tok) in
        let pat = Some (tuple_pattern env x) in
        (id, pat)
  in
  let _v2TODO = Option.map (element_binding_expression env) v2 in
  let v3 = Option.map (equals_value_clause env) v3 in
  let vinit =
    match (pattern, v3) with
    | Some pat, Some init -> Some (LetPattern (pat, init) |> G.e)
    | _ -> v3
  in
  let ent = basic_entity v1 in
  let vardef = { vinit; vtype = None; vtok = G.no_sc } in
  (ent, vardef)

(* The new grammar models `record with { id = expr, ... }` as a
   `with_body` rule: a brace-wrapped, optional, comma-separated list of
   `with_initializer = (identifier * "=" * expression_element)`. We map
   it to the same `G.field list` shape the rest of the mapper expects. *)
and with_initializer_expression (env : env) ((_l, items, _trailing, _r) : CST.with_body)
    : G.field list =
  match items with
  | None -> []
  | Some (first, rest) ->
      let first = with_initializer env first in
      let rest =
        List.map
          (fun (_comma, init) -> with_initializer env init)
          rest
      in
      first :: rest

and with_initializer (env : env)
    ((v1, v2, v3) : CST.with_initializer) : G.field =
  let v1 = identifier env v1 in
  let _v2 = token env v2 (* "=" *) in
  let v3 = expression env v3 in
  G.basic_field v1 (Some v3) None

(* The new grammar collapses prefix_unary_expression into
   `(operator_choice * expression)`. The `AMP_exp` (address-of) form
   moved to a separate `address_of_expression` rule, so it is no longer
   one of the variants. *)
and prefix_unary_expression (env : env)
    ((op, exp) : CST.prefix_unary_expression) =
  let v2 = expression env exp in
  match op with
  | `BANG tok ->
      let t = token env tok (* "!" *) in
      Call (Special (Op Not, t) |> G.e, fb [ Arg v2 ]) |> G.e
  | `PLUS tok ->
      let t = token env tok (* "+" *) in
      Call (Special (Op Plus, t) |> G.e, fb [ Arg v2 ]) |> G.e
  | `PLUSPLUS tok ->
      let t = token env tok (* "++" *) in
      Call (Special (IncrDecr (Incr, Prefix), t) |> G.e, fb [ Arg v2 ]) |> G.e
  | `DASH tok ->
      let t = token env tok (* "-" *) in
      Call (Special (Op Minus, t) |> G.e, fb [ Arg v2 ]) |> G.e
  | `DASHDASH tok ->
      let t = token env tok (* "--" *) in
      Call (Special (IncrDecr (Decr, Prefix), t) |> G.e, fb [ Arg v2 ]) |> G.e
  | `HAT tok ->
      let t = token env tok (* "^" *) in
      new_index_from_end t v2
  | `TILDE tok ->
      let t = token env tok (* "~" *) in
      Call (Special (Op BitNot, t) |> G.e, fb [ Arg v2 ]) |> G.e

and name (env : env) (x : CST.name) : G.name =
  match x with
  | `Alias_qual_name (v1, v2, v3) ->
      let v1 = identifier_or_global_qualifier env v1 in
      let _v2 =
        token env v2
        (* "::" *)
      in
      let v3 = simple_name env v3 in
      H2.name_of_ids_with_opt_typeargs [ (v1, None); v3 ]
  | `Qual_name (v1, v2, v3) ->
      let v1 = name env v1 in
      let _v2 =
        token env v2
        (* "." *)
      in
      let v3 = simple_name env v3 in
      H2.add_id_opt_type_args_to_name v1 v3
  | `Simple_name x -> H2.name_of_ids_with_opt_typeargs [ simple_name env x ]

and type_parameter (env : env) ((v1, v2, v3) : CST.type_parameter) :
    G.type_parameter =
  let v1 = List.concat_map (attribute_list_ env) v1 in
  let v2 =
    match v2 with
    | Some (`In tok) -> Some (Contravariant, token env tok) (* "in" *)
    | Some (`Out tok) -> Some (Covariant, token env tok) (* "out" *)
    | None -> None
  in
  let v3 =
    identifier env v3
    (* identifier *)
  in
  G.tparam_of_id v3 ~tp_attrs:v1 ?tp_variance:v2

(* The old `element_binding_expression` rule was just a
   `bracketed_argument_list`; the new grammar drops the named alias. *)
and element_binding_expression (env : env) (x : CST.bracketed_argument_list) =
  let open_br, args, close_br = bracketed_argument_list env x in
  let exprs = List.map H2.argument_to_expr args in
  (open_br, exprs, close_br)

and nullable_type (env : env) ((v1, v2) : CST.nullable_type) =
  let t = nullable_base_type env v1 in
  let tquestion = (* "?" *) token env v2 in
  TyQuestion (t, tquestion) |> G.t

and _object_creation_type (env : env) (x : CST.object_creation_type) =
  match x with
  | `Name x ->
      let n = name env x in
      TyN n |> G.t
  | `Null_type x -> nullable_type env x
  | `Pred_type tok -> predefined_type env tok

and nullable_base_type (env : env) (x : CST.nullable_base_type) : G.type_ =
  match x with
  | `Array_type x -> array_type env x
  | `Name x ->
      let n = name env x in
      TyN n |> G.t
  | `Tuple_type x -> tuple_type env x
  | `Pred_type tok -> predefined_type env tok

and array_type (env : env) ((v1, v2) : CST.array_type) =
  let v1 = array_base_type env v1 in
  let v2 = array_rank_specifier env v2 in
  let open_br, exps, close_br = v2 in
  let rec jag exps t =
    match exps with
    | [] -> TyArray ((open_br, None, close_br), t) |> G.t
    | [ e ] -> TyArray ((open_br, e, close_br), t) |> G.t
    | e :: tl -> jag tl (TyArray ((open_br, e, close_br), t) |> G.t)
  in
  (* TODO correct order? *)
  jag exps v1

and array_base_type (env : env) (x : CST.array_base_type) : G.type_ =
  match x with
  | `Array_type x -> array_type env x
  | `Name x -> type_name env x
  | `Null_type x -> nullable_type env x
  | `Poin_type x -> pointer_type env x
  | `Func_poin_type x -> function_pointer_type env x
  | `Pred_type tok -> predefined_type env tok (* predefined_type *)
  | `Tuple_type x -> tuple_type env x

and interpolated_verbatim_string_content (env : env)
    (x : CST.interpolated_verbatim_string_content) =
  match x with
  | `Interp_str_content tok ->
      L (interpolated_verbatim_string_text env tok) |> G.e
  | `Interp x -> Tok.unbracket (interpolation env x)

and interpolated_raw_string_content (env : env)
    (x : CST.interpolated_raw_string_content) =
  match x with
  | `Interp_str_content tok ->
      L (interpolated_raw_string_text env tok) |> G.e
  | `Interp x -> Tok.unbracket (interpolation env x)

and array_rank_specifier (env : env) ((v1, v2, v3) : CST.array_rank_specifier) =
  let v1 =
    token env v1
    (* "[" *)
  in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = Option.map (expression env) v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let _v1 =
                token env v1
                (* "," *)
              in
              let v2 = Option.map (expression env) v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 =
    token env v3
    (* "]" *)
  in
  (* TODO we could give each expression brackets, instead of using the same brackets for all expressions *)
  (v1, v2, v3)

and argument (env : env) (x : CST.argument) : G.argument =
  match x with
  | `Opt_id_COLON_opt_choice_ref_choice_exp (v1, v2, v3) -> (
      (* `name_colon` is now inlined as `(identifier, ":")`. *)
      let v1 = Option.map (name_colon env) v1 in
      let _v2TODO =
        match v2 with
        | Some x -> (
            match x with
            | `Ref tok -> Some (token env tok) (* "ref" *)
            | `Out tok -> Some (token env tok) (* "out" *)
            | `In tok -> Some (token env tok) (* "in" *))
        | None -> None
      in
      let v3 =
        match v3 with
        | `Exp x -> expression env x
        | `Decl_exp x -> declaration_expression env x
      in
      match v1 with
      | None -> G.Arg v3
      | Some id -> G.ArgKwd (id, v3))
  | `Semg_vari_meta v1 ->
      let id = str env v1 in
      G.Arg (N (H2.name_of_id id) |> G.e)

and initializer_expression (env : env)
    ((v1, v2, v3, v4) : CST.initializer_expression) : expr list G.bracket =
  let v1 =
    token env v1
    (* "{" *)
  in
  let v2 = anon_opt_exp_elem_rep_interp_alig_clause_48c2617 env v2 in
  let _v3 =
    Option.map (token env) v3
    (* "," *)
  in
  let v4 =
    token env v4
    (* "}" *)
  in
  (v1, v2, v4)

and switch_expression_arm (env : env)
    ((v1, v2, v3, v4) : CST.switch_expression_arm) =
  let v1 = pattern env v1 in
  let v2 =
    match v2 with
    | Some x -> PatWhen (v1, when_clause env x)
    | None -> v1
  in
  let _v3 =
    token env v3
    (* "=>" *)
  in
  let v4 = expression env v4 in
  G.case_of_pat_and_expr (v2, v4)

(* tuple_expression collapsed from a 6-tuple
   `(open, arg, ",", arg, (comma, arg) list, close)` to a 4-tuple
   `(open, arg, (comma, arg) list, close)`. *)
and tuple_expression (env : env) ((v1, v2, v3, v4) : CST.tuple_expression) =
  let v1 = token env v1 (* "(" *) in
  let v2 = argument env v2 in
  let v3 =
    List.map
      (fun (comma, a) ->
        let _ = token env comma in
        argument env a)
      v3
  in
  let v4 = token env v4 (* ")" *) in
  let exprs = List.map H2.argument_to_expr (v2 :: v3) in
  Container (Tuple, (v1, exprs, v4)) |> G.e

and query_body (env : env) ((v1, v2, v3) : CST.query_body) =
  (* `query_continuation` was inlined into query_body in the new grammar.
     The third tuple element is a list of (into-tok, id, query_clauses,
     select_or_group) chains, each of which we re-emit as a single Into
     LinqQueryPart so the downstream comprehension translation keeps
     working unchanged. *)
  let v1 = List.map (query_clause env) v1 in
  let v2 = select_or_group_clause env v2 in
  let v3 =
    List.map
      (fun (into_tok, id, clauses, sel) ->
        let into_tok = token env into_tok in
        let id = identifier env id in
        let clauses = List.map (query_clause env) clauses in
        let sel = select_or_group_clause env sel in
        Into (into_tok, id, clauses @ [ sel ]))
      v3
  in
  v1 @ [ v2 ] @ v3

(* catch_clause collapsed from 4-tuple
   `(catch, declaration?, filter?, block)` to 3-tuple
   `(catch, [Catch_decl|Catch_filter_clause] list, block)` —
   the declaration and the filter share one list now. *)
and catch_clause (env : env) ((v1, v2, v3) : CST.catch_clause) =
  let v1 = token env v1 (* "catch" *) in
  let exn =
    List.fold_left
      (fun acc x ->
        match x with
        | `Catch_decl decl -> catch_declaration env decl
        | `Catch_filter_clause f ->
            let _filterTODO = catch_filter_clause env f in
            acc)
      (CatchPattern (PatWildcard (fake "_")))
      v2
  in
  let v3 = block env v3 in
  (v1, exn, v3)

and ordering (env : env) ((v1, v2) : CST.ordering) =
  let v1 = expression env v1 in
  let v2 =
    match v2 with
    | Some x -> (
        match x with
        | `Asce _tok -> Ascending (* "ascending" *)
        | `Desc _tok -> Descending (* "descending" *))
    | None -> Ascending
  in
  (v1, v2)

and interpolated_string_content (env : env)
    (x : CST.interpolated_string_content) =
  match x with
  | `Interp_str_content tok ->
      L (interpolated_string_text env tok) |> G.e
  | `Esc_seq tok -> L (escape_sequence env tok) |> G.e
  | `Interp x -> Tok.unbracket (interpolation env x)

(* checked_expression collapsed: the checked/unchecked keyword is now a
   choice on the first tuple element. *)
and checked_expression (env : env)
    ((kw, _lp, v3, _rp) : CST.checked_expression) =
  let label, v1 =
    match kw with
    | `Chec tok -> ("Checked", token env tok)
    | `Unch tok -> ("Unchecked", token env tok)
  in
  let v3 = expression env v3 in
  OtherExpr ((label, v1), [ E v3 ]) |> G.e

and pred_type env x =
  (* e.g. `int` in `int.maxValue` *)
  let id = str env x in
  N (Id (id, empty_id_info ())) |> G.e

and default_expression env (v1, v2) =
  let v1 =
    token env v1
    (* "default" *)
  in
  let v2 =
    match v2 with
    | Some (v1, v2, v3) ->
        let v1 =
          token env v1
          (* "(" *)
        in
        let v2 = ArgType (type_pattern env v2) in
        let v3 =
          token env v3
          (* ")" *)
        in
        (v1, [ v2 ], v3)
    | None -> fb []
  in
  (* old: was a New *)
  let e = G.OtherExpr (("Default", v1), []) |> G.e in
  Call (e, v2) |> G.e

and size_of_expression env (v1, v2, v3, v4) =
  let v1 =
    token env v1
    (* "sizeof" *)
  in
  let v2 =
    token env v2
    (* "(" *)
  in
  let v3 = type_pattern env v3 in
  let v4 =
    token env v4
    (* ")" *)
  in
  Call (Special (Sizeof, v1) |> G.e, (v2, [ ArgType v3 ], v4)) |> G.e

and type_of_expression env (v1, v2, v3, v4) =
  let v1 =
    token env v1
    (* "typeof" *)
  in
  let v2 =
    token env v2
    (* "(" *)
  in
  let v3 = type_pattern env v3 in
  let v4 =
    token env v4
    (* ")" *)
  in
  Call (Special (Typeof, v1) |> G.e, (v2, [ ArgType v3 ], v4)) |> G.e

and member_access_expression env (v1, v2, v3) =
  let v1 =
    match v1 with
    | `Exp x -> expression env x
    | `Pred_type x -> pred_type env x
    | `Name x -> N (name env x) |> G.e
  in
  let v2 =
    match v2 with
    | `DOT tok -> token env tok (* "." *)
    | `DASHGT tok -> token env tok
    (* "->" *)
  in
  let v3 = simple_name env v3 in
  let n = H2.name_of_ids_with_opt_typeargs [ v3 ] in
  G.DotAccess (v1, v2, G.FN n) |> G.e

and invocation_expression env (v1, v2) =
  let v1 = expression env v1 in
  let v2 = argument_list env v2 in
  G.Call (v1, v2) |> G.e

and cast_expression env (v1, v2, v3, v4) =
  let v1 =
    token env v1
    (* "(" *)
  in
  let v2 = type_pattern env v2 in
  let _v3 =
    token env v3
    (* ")" *)
  in
  let v4 = expression env v4 in
  Cast (v2, v1, v4) |> G.e

and simple_name_expression env x =
  N (H2.name_of_ids_with_opt_typeargs [ simple_name env x ]) |> G.e

and deep_ellipsis (env : env) ((v1, v2, v3) : CST.deep_ellipsis) =
  let v1 = token env v1 in
  let v2 = expression env v2 in
  let v3 = token env v3 in
  DeepEllipsis (v1, v2, v3) |> G.e

and member_access_ellipsis (env : env)
    ((v1, v2, v3) : CST.member_access_ellipsis_expression) =
  let e =
    match v1 with
    | `Exp x -> expression env x
    | `Pred_type x -> pred_type env x
    | `Name x -> N (name env x) |> G.e
  in
  let _tdot =
    match v2 with
    | `DOT tok -> token env tok (* "." *)
    | `DASHGT tok -> token env tok
    (* "->" *)
  in
  let tdots = token env v3 in
  DotAccessEllipsis (e, tdots) |> G.e

and typed_metavariable (env : env) ((v1, v2, v3, v4) : CST.typed_metavariable) =
  let lp = (* "(" *) token env v1 in
  let ty = type_pattern env v2 in
  let id = (* semgrep_metavariable *) str env v3 in
  let _rp = (* ")" *) token env v4 in
  TypedMetavar (id, lp, ty) |> G.e

and expression (env : env) (x : CST.expression) : G.expr =
  match x with
  | `Non_lvalue_exp v1 -> non_lvalue_expression env v1
  | `Lvalue_exp v1 -> lvalue_expression env v1
  (* semgrep: *)
  | `Ellips v1 -> Ellipsis (token env v1) |> G.e
  | `Deep_ellips v1 -> deep_ellipsis env v1
  | `Member_access_ellips_exp v1 -> member_access_ellipsis env v1
  | `Typed_meta v1 -> typed_metavariable env v1

and lvalue_expression (env : env) (x : CST.lvalue_expression) : G.expr =
  match x with
  | `This tok ->
      let t = token env tok (* "this" *) in
      N (IdSpecial ((This, t), G.empty_id_info ())) |> G.e
  | `Member_access_exp x -> member_access_expression env x
  | `Tuple_exp x -> tuple_expression env x
  | `Simple_name x -> simple_name_expression env x
  | `Elem_access_exp (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = element_binding_expression env v2 in
      let open_br, _exprsTODO, close_br = v2 in
      (* TODO we map multidim arrays as jagged arrays when creating arrays, with as tuples here. *)
      ArrayAccess (v1, (open_br, Container (Tuple, v2) |> G.e, close_br)) |> G.e
  | `Brac_arg_list x ->
      Container (Tuple, element_binding_expression env x) |> G.e
  | `Poin_indi_exp (v1, v2) -> DeRef (token env v1, lvalue_expression env v2) |> G.e
  | `Paren_lvalue_exp (_v1, v2, _v3) -> lvalue_expression env v2
  | `Cond_access_exp x -> conditional_access_expression env x

and conditional_access_expression (env : env)
    ((v1, v2, v3) : CST.conditional_access_expression) =
  let v1 = expression env v1 in
  let v2 = token env v2 (* "?" *) in
  let inner =
    G.Call (G.Special (G.Op G.Elvis, v2) |> G.e, fb [ G.Arg v1 ]) |> G.e
  in
  match v3 with
  | `Brac_arg_list x ->
      let x = element_binding_expression env x in
      let open_br, _, close_br = x in
      ArrayAccess (inner, (open_br, Container (Tuple, x) |> G.e, close_br))
      |> G.e
  | `Member_bind_exp (x1, x2) ->
      let x1 = token env x1 (* "." *) in
      let x2 = simple_name env x2 in
      let n = H2.name_of_ids_with_opt_typeargs [ x2 ] in
      DotAccess (inner, x1, FN n) |> G.e

and expression_statement_expression (env : env)
    (x : CST.expression_statement_expression) =
  match x with
  | `Assign_exp (v1, v2, v3) ->
      let v1 = lvalue_expression env v1 in
      let v2 = assignment_operator env v2 in
      let v3 = expression env v3 in
      AssignOp (v1, v2, v3) |> G.e
  | `Invo_exp x -> invocation_expression env x
  | `Post_un_exp x -> postfix_unary_expression env x
  | `Prefix_un_exp x -> prefix_unary_expression env x
  | `Await_exp (v1, v2) ->
      let v1 =
        token env v1
        (* "await" *)
      in
      let v2 = expression env v2 in
      Await (v1, v2) |> G.e
  | `Obj_crea_exp (v1, v2, v3, v4) ->
      let v1 =
        token env v1
        (* "new" *)
      in
      (* `object_creation_type` was a narrow Name/Null/Pred choice;
         the new grammar accepts any `type_pattern` (= `type_`) here. *)
      let v2 = type_ env v2 in
      let v3 =
        match v3 with
        | Some x -> argument_list env x
        | None -> fb []
      in
      let v4 =
        match v4 with
        | Some x -> initializer_expression env x
        | None -> fb []
      in
      let lp, v3', rp = v3 in
      let args = (lp, v3' @ [ Arg (Container (Tuple, v4) |> G.e) ], rp) in
      New (v1, v2, empty_id_info (), args) |> G.e
  | `Paren_exp x -> parenthesized_expression env x

and non_lvalue_expression (env : env) (x : CST.non_lvalue_expression) : G.expr =
  match x with
  | `Anon_meth_exp (v1, v2, v3, v4) ->
      let _v1TODO = List.map (map_anon_choice_static_d4628a0 env) v1 in
      let tdelegate =
        token env v2
        (* "delegate" *)
      in
      let fparams =
        match v3 with
        | Some x -> parameter_list env x
        | None -> fb []
      in
      let v4 = block env v4 in
      Lambda
        {
          fkind = (LambdaKind, tdelegate);
          fparams;
          frettype = None;
          fbody = G.FBStmt v4;
        }
      |> G.e
  | `Anon_obj_crea_exp (v1, v2, v3, v4, v5) ->
      let v1 =
        token env v1
        (* "new" *)
      in
      let v2 =
        token env v2
        (* "{" *)
      in
      let v3 =
        match v3 with
        | Some (v1, v2) ->
            let v1 = anonymous_object_member_declarator env v1 in
            let v2 =
              List.map
                (fun (v1, v2) ->
                  let _v1 =
                    token env v1
                    (* "," *)
                  in
                  let v2 = anonymous_object_member_declarator env v2 in
                  v2)
                v2
            in
            v1 :: v2
        | None -> []
      in
      let _v4 =
        Option.map (token env) v4
        (* "," *)
      in
      let v5 =
        token env v5
        (* "}" *)
      in
      AnonClass
        {
          ckind = (Class, v1);
          cextends = [];
          cimplements = [];
          cmixins = [];
          cparams = fb [];
          cbody = (v2, v3, v5);
        }
      |> G.e
  | `Array_crea_exp (v1, v2, v3) ->
      let v1 =
        token env v1
        (* "new" *)
      in
      let v2 = array_type env v2 in
      let v3 =
        match v3 with
        | Some x -> initializer_expression env x
        | None -> fb []
      in
      let lb, _, rb = v3 in
      let args = (lb, [ Arg (G.Container (G.Tuple, v3) |> G.e) ], rb) in
      New (v1, v2, empty_id_info (), args) |> G.e
  | `As_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "as" *)
      in
      let v3 = type_ env v3 in
      (* TODO `as` is really a conditional cast *)
      Cast (v3, v2, v1) |> G.e
  | `Base tok ->
      (* `base` was renamed from `Base_exp` to the bare `Base` token in the
         non_lvalue_expression choice. *)
      let x = token env tok (* "base" *) in
      N (IdSpecial ((Super, x), G.empty_id_info ())) |> G.e
  | `Bin_exp x -> binary_expression env x
  | `Cast_exp x -> cast_expression env x
  | `Chec_exp x -> checked_expression env x
  | `Cond_access_exp x -> conditional_access_expression env x
  | `Cond_exp (v1, v2, v3, v4, v5) ->
      let v1 = expression env v1 in
      let _v2 =
        token env v2
        (* "?" *)
      in
      let v3 = expression env v3 in
      let _v4 =
        token env v4
        (* ":" *)
      in
      let v5 = expression env v5 in
      Conditional (v1, v3, v5) |> G.e
  | `Defa_exp x -> default_expression env x
  | `Impl_array_crea_exp (v1, v2, v3, v4, v5) ->
      let _v1TODO =
        token env v1
        (* "new" *)
      in
      let _v2 =
        token env v2
        (* "[" *)
      in
      let _v3 = List.map (token env) (* "," *) v3 in
      let _v4 =
        token env v4
        (* "]" *)
      in
      let v5 = initializer_expression env v5 in
      Container (Array, v5) |> G.e
  | `Impl_obj_crea_exp (v1, v2, v3) ->
      let v1 =
        token env v1
        (* "new" *)
      in
      let v2 = argument_list env v2 in
      let v3 =
        match v3 with
        | Some x -> initializer_expression env x
        | None -> fb []
      in
      let lp, v2', rp = v2 in
      let args = (lp, v2' @ [ Arg (Container (Tuple, v3) |> G.e) ], rp) in
      (* old: was New *)
      let e = G.OtherExpr (("NewNoType", v1), []) |> G.e in
      Call (e, args) |> G.e
  | `Impl_stac_exp (v1, v2, v3, v4) ->
      let v1 =
        token env v1
        (* "stackalloc" *)
      in
      let _v2 =
        token env v2
        (* "[" *)
      in
      let _v3 =
        token env v3
        (* "]" *)
      in
      let _v4 = initializer_expression env v4 in
      todo_expr env v1
  | `Init_exp x -> Container (Tuple, initializer_expression env x) |> G.e
  | `Inte_str_exp x -> interpolated_string_expression env x
  | `Is_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "is" *)
      in
      let v3 = type_ env v3 in
      Call (Special (Instanceof, v2) |> G.e, fb [ Arg v1; ArgType v3 ]) |> G.e
  | `Is_pat_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let _v2 =
        token env v2
        (* "is" *)
      in
      let v3 = pattern env v3 in
      LetPattern (v3, v1) |> G.e
  | `Lambda_exp (init, arrow, body) ->
      (* lambda_expression is now (init, =>, body) where init bundles
         attributes/static-async/return-type/parameters. *)
      let v1_attrs, v2_static_async, v3_rettype, v4_params = init in
      let _v1TODO = List.concat_map (attribute_list_ env) v1_attrs in
      let _v2TODO = List.map (map_anon_choice_static_d4628a0 env) v2_static_async in
      let v3 = Option.map (type_pattern env) v3_rettype in
      let v4 =
        match v4_params with
        | `Param_list x -> parameter_list env x
        | `Id id ->
            (* simple single-parameter lambda `x => ...` *)
            let id = identifier env id in
            fb [ Param (param_of_id id) ]
        | `Impl_param_list_with_modifs xs -> implicit_parameter_list env xs
      in
      let v5 = (* "=>" *) token env arrow in
      let v6 =
        match body with
        | `Blk x -> G.FBStmt (block env x)
        | `Exp x -> G.FBExpr (expression env x)
      in
      Lambda { fkind = (Arrow, v5); fparams = v4; frettype = v3; fbody = v6 }
      |> G.e
  | `Make_exp (v1, v2, v3, v4) ->
      let v1 =
        token env v1
        (* "__makeref" *)
      in
      let _v2 =
        token env v2
        (* "(" *)
      in
      let v3 = expression env v3 in
      let _v4 =
        token env v4
        (* ")" *)
      in
      Ref (v1, v3) |> G.e
  | `Query_exp (v1, v2) ->
      let v1 = from_clause env v1 in
      let v2 = query_body env v2 in
      linq_to_expr v1 v2
  | `Range_exp (v1, v2, v3) ->
      let fake_zero = L (Int Parsed_int.fake_zero) |> G.e in
      let v1 =
        match v1 with
        | Some x -> expression env x
        | None -> fake_zero
      in
      let v2 =
        token env v2
        (* ".." *)
      in
      let v3 =
        match v3 with
        | Some x -> expression env x
        | None -> new_index_from_end v2 fake_zero
      in
      Call (Special (Op Range, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Ref_exp (v1, v2) ->
      let v1 =
        token env v1
        (* "ref" *)
      in
      let v2 = expression env v2 in
      G.Ref (v1, v2) |> G.e
  | `Reft_exp (v1, v2, v3, v4) ->
      let v1 =
        token env v1
        (* "__reftype" *)
      in
      let v2 =
        token env v2
        (* "(" *)
      in
      let v3 = expression env v3 in
      let v4 =
        token env v4
        (* ")" *)
      in
      Call
        (Special (Typeof, v1) |> G.e, (v2, [ Arg (DeRef (v1, v3) |> G.e) ], v4))
      |> G.e
  | `Refv_exp (v1, v2, v3, v4, v5, v6) ->
      let v1 =
        token env v1
        (* "__refvalue" *)
      in
      let _v2 =
        token env v2
        (* "(" *)
      in
      let v3 = expression env v3 in
      let _v4 =
        token env v4
        (* "," *)
      in
      let _v5 = type_pattern env v5 in
      let _v6 =
        token env v6
        (* ")" *)
      in
      DeRef (v1, v3) |> G.e
  | `Sizeof_exp x -> size_of_expression env x
  | `Stac_exp (v1, v2, v3) ->
      let v1 =
        token env v1
        (* "stackalloc" *)
      in
      let _v2 = array_type env v2 in
      let _v3 =
        match v3 with
        | Some x -> initializer_expression env x
        | None -> fb [ todo_expr env v1 ]
      in
      todo_expr env v1
  | `Switch_exp (v1, v2, body) ->
      (* switch_expression collapsed to `(expr, "switch", switch_expression_body)`
         where the body holds the brace tokens and arm list. *)
      let v1 = expression env v1 in
      let v2 = token env v2 (* "switch" *) in
      let _lbrace, arms, _trailing, _rbrace = body in
      let arms =
        match arms with
        | None -> []
        | Some (first, rest) ->
            let first = switch_expression_arm env first in
            let rest =
              List.map
                (fun (_comma, a) -> switch_expression_arm env a)
                rest
            in
            first :: rest
      in
      (* TODO: use Switch instead? *)
      let st = G.Switch (v2, Some (G.Cond v1), arms) |> G.s in
      G.stmt_to_expr st
  | `Throw_exp (v1, v2) ->
      let v1 =
        token env v1
        (* "throw" *)
      in
      let v2 = expression env v2 in
      let throw = Throw (v1, v2, sc) |> G.s in
      G.stmt_to_expr throw
  | `Typeof_exp x -> type_of_expression env x
  | `With_exp (v1, v2, body) ->
      (* with_expression collapsed: brace tokens and initializer list now
         live inside the `with_body` 4-tuple. *)
      let v1 = expression env v1 in
      let v2 = token env v2 (* "with" *) in
      let lbrace, _items, _trailing, rbrace = body in
      let v3 = token env lbrace in
      let v4 = with_initializer_expression env body in
      let v5 = token env rbrace in
      let with_fields = G.Record (v3, v4, v5) |> G.e in
      (* THINK:
       * - with-expressions may deserve first-class support in Generic AST
       * - record patterns perhaps should match with-expressions
       *)
      G.OtherExpr (("RecordWith", v2), [ G.E v1; G.E with_fields ]) |> G.e
  | `Lit x ->
      let x = literal env x in
      G.L x |> G.e
  | `Exp_stmt_exp x -> expression_statement_expression env x
  (* New non_lvalue_expression variants — stub for now; downstream
     semantics can be wired up per-feature in follow-up PRs. *)
  | `Coll_exp (lbr, items, rbr) ->
      let lbr = token env lbr in
      let rbr = token env rbr in
      let elements : G.expr list =
        match items with
        | None -> []
        | Some (first, rest, _trailing_comma) ->
            let element (e : CST.collection_element) : G.expr =
              match e with
              | `Exp_elem ee -> expression env ee
              | `Spread_elem (dots, ee) ->
                  let dots = token env dots in
                  let inner = expression env ee in
                  G.OtherExpr (("Spread", dots), [ G.E inner ]) |> G.e
            in
            element first :: List.map (fun (_comma, e) -> element e) rest
      in
      G.Container (G.Array, (lbr, elements, rbr)) |> G.e
  | `Addr_of_exp (amp, e) ->
      let t = token env amp in
      Ref (t, lvalue_expression env e) |> G.e
  | `Prep_if_in_exp _ ->
      G.OtherExpr (("PreprocIfExpr", fake "#if"), []) |> G.e

and simple_name (env : env) (x : CST.simple_name) :
    G.ident * G.type_arguments option =
  match x with
  | `Gene_name (v1, v2) ->
      let v1 =
        identifier env v1
        (* identifier *)
      in
      let v2 = type_argument_list env v2 in
      (v1, Some v2)
  | `Id x -> (identifier env x, None)

and switch_body (env : env) ((v1, v2, v3) : CST.switch_body) :
    case_and_body list =
  let _v1 =
    token env v1
    (* "{" *)
  in
  let raw = List.map (switch_section env) v2 in
  (* The new grammar emits one `switch_section` per label. Multi-label
     fall-through (`case 1: case 2: …`) shows up as a sequence where all
     but the last section has an empty body — merge those into a single
     `CasesAndBody` so downstream generic-AST consumers see the labels
     grouped, matching the old single-section shape. *)
  let is_empty_stmt (s : G.stmt) =
    match s.G.s with G.Block (_, [], _) -> true | _ -> false
  in
  let rec merge acc = function
    | [] -> List.rev acc
    | CasesAndBody (labels, body) :: rest when is_empty_stmt body ->
        let rec gather labels rest =
          match rest with
          | CasesAndBody (l2, b2) :: rest' when is_empty_stmt b2 ->
              gather (labels @ l2) rest'
          | CasesAndBody (l2, b2) :: rest' ->
              merge (CasesAndBody (labels @ l2, b2) :: acc) rest'
          | [] ->
              merge (CasesAndBody (labels, G.Block (fb []) |> G.s) :: acc) []
          | _ -> merge (CasesAndBody (labels, G.Block (fb []) |> G.s) :: acc) rest
        in
        gather labels rest
    | x :: rest -> merge (x :: acc) rest
  in
  let v2 = merge [] raw in
  let _v3 =
    token env v3
    (* "}" *)
  in
  v2

and anon_choice_param_ce11a32 (env : env) (x : CST.anon_choice_param_ce11a32) =
  match x with
  | `Param x -> parameter env x
  | `Param_array (v1, v2, v3, v4) ->
      let v1 = List.concat_map (attribute_list_ env) v1 in
      let v2 = token env v2 (* "params" *) in
      (* params accepts any `type_pattern` (= `type_`) now, not a narrow
         array/nullable choice. *)
      let v3 = type_ env v3 in
      let v4 =
        identifier env v4
        (* identifier *)
      in
      ParamRest
        ( v2,
          {
            pname = Some v4;
            ptype = Some v3;
            pdefault = None;
            pattrs = v1;
            pinfo = empty_id_info ();
          } )

and map_anon_choice_pat_29be9ad (env : env) (x : CST.anon_choice_pat_29be9ad) =
  match x with
  | `Pat x -> pattern env x
  | `Slice_pat (tok, opt_pat) ->
      (* C# 11 slice pattern `..` optionally followed by a sub-pattern
         (e.g. `[1, .. var rest]`). We model it as `OtherPat("Slice", …)`
         carrying the dots token and, when present, the bound subpattern. *)
      let dots = token env tok in
      let extras =
        match opt_pat with
        | Some p -> [ G.P (pattern env p) ]
        | None -> []
      in
      OtherPat (("Slice", dots), extras)

and anon_opt_exp_elem_rep_interp_alig_clause_48c2617 (env : env)
    (opt : CST.anon_opt_exp_elem_rep_interp_alig_clause_48c2617) : expr list =
  match opt with
  | Some (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = List.map (interpolation_alignment_clause env) v2 in
      v1 :: v2
  | None -> []

and type_parameter_list (env : env) ((v1, v2, v3, v4) : CST.type_parameter_list)
    : G.type_parameters =
  let lt =
    token env v1
    (* "<" *)
  in
  let v2 = type_parameter env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let _v1 =
          token env v1
          (* "," *)
        in
        let v2 = type_parameter env v2 in
        v2)
      v3
  in
  let gt =
    token env v4
    (* ">" *)
  in
  (lt, v2 :: v3, gt)

and type_parameter_constraint (env : env) (x : CST.type_parameter_constraint) :
    (G.todo_kind, type_) Either.t =
  match x with
  | `Class_opt_QMARK (tok, _)
  (* "class" *)
  (* TODO handle question mark *)
  | `Struct tok (* "struct" *)
  | `Notn tok (* "notnull" *)
  | `Unma tok ->
      (* "unmanaged" *)
      let t = G.ty_builtin (str env tok) in
      Right t
  | `Cons_cons (v1, v2, v3) ->
      let v1 =
        token env v1
        (* "new" *)
      in
      let v2 =
        token env v2
        (* "(" *)
      in
      let v3 =
        token env v3
        (* ")" *)
      in
      let tok = Tok.combine_toks v1 [ v2; v3 ] in
      Left ("HasConstructor", tok)
  | `Type x -> Right (type_constraint env x)

(* The old `type_constraint` named alias was inlined; the constraint's
   "type" arm now embeds `type_pattern` (= `type_`) directly. We keep the
   helpers as thin wrappers so call sites need no further change. *)
and type_constraint (env : env) (x : CST.type_pattern) : type_ =
  (* can't be `var` *)
  type_ env x

and local_variable_type (env : env) (x : CST.type_) : type_ option =
  match x with
  | `Impl_type _tok -> None (* "var" *)
  | x -> Some (type_ env x)

and expr_statement (env : env) (x : CST.expression_statement) : stmt =
  match x with
  | `Exp_stmt_exp_SEMI (v1, v2) ->
      let v1 = expression_statement_expression env v1 in
      let v2 =
        token env v2
        (* ";" *)
      in
      G.ExprStmt (v1, v2) |> G.s
  | `Ellips_SEMI (v1, v2) ->
      let v1 = token env v1 in
      let v2 = token env v2 in
      G.ExprStmt (G.Ellipsis v1 |> G.e, v2) |> G.s
  | `Ellips v1 ->
      let v1 = token env v1 in
      let v2 = G.sc in
      G.ExprStmt (G.Ellipsis v1 |> G.e, v2) |> G.s
  | `Deep_ellips_SEMI (v1, v2) ->
      let v1 = deep_ellipsis env v1 in
      let v2 = (* ";" *) token env v2 in
      G.ExprStmt (v1, v2) |> G.s
  | `Member_access_ellips_exp_SEMI (v1, v2) ->
      let v1 = member_access_ellipsis env v1 in
      let v2 = (* ";" *) token env v2 in
      G.ExprStmt (v1, v2) |> G.s
  | `Semg_meta_SEMI (v1, v2) ->
      let v1 = (* semgrep_metavariable *) str env v1 in
      let v2 = (* ";" *) token env v2 in
      G.ExprStmt (G.N (AST_generic_helpers.name_of_id v1) |> G.e, v2) |> G.s
  | `Typed_meta_SEMI (v1, v2) ->
      let v1 = typed_metavariable env v1 in
      let v2 = (* ";" *) token env v2 in
      G.ExprStmt (v1, v2) |> G.s

and statement (env : env) (x : CST.statement) =
  match x with
  | `Blk x -> block env x
  | `Brk_stmt (v1, v2) ->
      let v1 =
        token env v1
        (* "break" *)
      in
      let v2 =
        token env v2
        (* ";" *)
      in
      G.Break (v1, G.LNone, v2) |> G.s
  | `Chec_stmt (v1, v2) ->
      let v1 =
        match v1 with
        | `Chec tok (* "checked" *) ->
            let tchecked = token env tok in
            OSWS_Block ("Checked", tchecked)
        | `Unch tok (* "unchecked" *) ->
            let tunchecked = token env tok in
            OSWS_Block ("Unchecked", tunchecked)
      in
      let v2 = block env v2 in
      OtherStmtWithStmt (v1, [], v2) |> G.s
  | `Cont_stmt (v1, v2) ->
      let v1 =
        token env v1
        (* "continue" *)
      in
      let v2 =
        token env v2
        (* ";" *)
      in
      Continue (v1, LNone, v2) |> G.s
  | `Do_stmt (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 =
        token env v1
        (* "do" *)
      in
      let v2 = statement env v2 in
      let _v3 =
        token env v3
        (* "while" *)
      in
      let _v4 =
        token env v4
        (* "(" *)
      in
      let v5 = expression env v5 in
      let _v6 =
        token env v6
        (* ")" *)
      in
      let _v7 =
        token env v7
        (* ";" *)
      in
      DoWhile (v1, v2, v5) |> G.s
  | `Empty_stmt tok ->
      let v1 =
        token env tok
        (* ";" *)
      in
      Block (v1, [], v1) |> G.s
  (* Can we have the same token as start and end of block? *)
  | `Exp_stmt v1 -> expr_statement env v1
  | `Fixed_stmt (v1, v2, v3, v4, v5) ->
      let v1 =
        token env v1
        (* "fixed" *)
      in
      let _v2 =
        token env v2
        (* "(" *)
      in
      let _v3 = variable_declaration env v3 in
      let _v4 =
        token env v4
        (* ")" *)
      in
      let _v5 = statement env v5 in
      todo_stmt env v1
  | `Fore_stmt (init, body) ->
      (* foreach_statement is now `(initializer, body)` where the
         initializer is a 7-tuple bundling await/foreach/(/choice/in/expr/). *)
      let v1, v2, _lp, v4, v5, v6, _rp = init in
      let v1 = Option.map (token env) v1 (* "await" *) in
      let v2 = token env v2 (* "foreach" *) in
      let v4 =
        match v4 with
        | `Type_choice_id (ty, x) -> (
            let pat =
              match x with
              | `Id x ->
                  let x = identifier env x in
                  PatId (x, empty_id_info ())
              | `Tuple_pat x -> tuple_pattern env x
            in
            match local_variable_type env ty with
            | Some t -> PatTyped (pat, t)
            | None -> pat)
        | `Exp x -> H2.expr_to_pattern (expression env x)
      in
      let v6 = expression env v6 in
      let v6 =
        match v1 with
        | Some tok -> Await (tok, v6) (* "await" *) |> G.e
        | None -> v6
      in
      let v5 = token env v5 (* "in" *) in
      let v8 = statement env body in
      For (v2, ForEach (v4, v5, v6), v8) |> G.s
  | `For_stmt (v1, conds, body) ->
      (* for_statement is now `(for, for_statement_conditions, body)`
         where conditions is `( ( init? ; cond? ; updates ) )`. *)
      let v1 = token env v1 (* "for" *) in
      let _lp, init_opt, semi1, cond_opt, semi2, updates, rp = conds in
      let v3 =
        match init_opt with
        | Some x -> (
            match x with
            | `Var_decl x ->
                List.map
                  (fun (e, v) -> ForInitVar (e, v))
                  (variable_declaration env x)
            | `Exp_rep_COMMA_exp (e1, rest) ->
                let e1 = expression env e1 in
                let rest = List.map (interpolation_alignment_clause env) rest in
                List.map (fun e -> ForInitExpr e) (e1 :: rest))
        | None -> []
      in
      let _ = semi1 in
      let v5 = Option.map (expression env) cond_opt in
      let v6 = token env semi2 (* ";" *) in
      let rp_tok = token env rp (* ")" *) in
      let v7 =
        anon_opt_exp_elem_rep_interp_alig_clause_48c2617 env updates
      in
      let v9 = statement env body in
      let next =
        match v7 with
        | [] -> None
        | [ e ] -> Some e
        | _exprs -> Some (Container (Tuple, (v6, v7, rp_tok)) |> G.e)
      in
      let for_header = ForClassic (v3, v5, next) in
      For (v1, for_header, v9) |> G.s
  | `Goto_stmt (v1, v2, v3, v4) -> (
      let v1 =
        token env v1
        (* "goto" *)
      in
      let v4 =
        token env v4
        (* ";" *)
      in
      match (v2, v3) with
      (* Only a few of these are allowable. See
         https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/grammar
      *)
      | None, Some exp -> (
          let exp = expression env exp in
          match exp.e with
          | N (Id (id, _)) -> Goto (v1, id, v4) |> G.s
          | _ ->
              (* This shouldn't be permitted by the grammar above. *)
              todo_stmt env v1)
      | Some (`Case tok), Some _exp ->
          let v1 =
            token env tok
            (* "case" *)
          in
          todo_stmt env v1
      | Some (`Defa tok), None ->
          let _tok =
            token env tok
            (* "default" *)
          in
          todo_stmt env v1
      | _ -> todo_stmt env v1)
  | `If_stmt (v1, v2, v3, v4, v5, v6) ->
      let v1 =
        token env v1
        (* "if" *)
      in
      let _v2 =
        token env v2
        (* "(" *)
      in
      let v3 = expression env v3 in
      let _v4 =
        token env v4
        (* ")" *)
      in
      let v5 = statement env v5 in
      let v6 =
        match v6 with
        | Some (v1, v2) ->
            let _v1 =
              token env v1
              (* "else" *)
            in
            let v2 = statement env v2 in
            Some v2
        | None -> None
      in
      G.If (v1, G.Cond v3, v5, v6) |> G.s
  | `Labe_stmt (v1, v2, v3) ->
      let v1 =
        identifier env v1
        (* identifier *)
      in
      let _v2 =
        token env v2
        (* ":" *)
      in
      let v3 = statement env v3 in
      Label (v1, v3) |> G.s
  | `Local_decl_stmt (v1, v2, v3, v4, v5) ->
      let _V1TODO =
        Option.map (token env) v1
        (* "await" *)
      in
      let _V2TODO =
        Option.map (token env) v2
        (* "using" *)
      in
      let attrs = List.map (modifier env) v3 in
      let vardefs = variable_declaration env v4 in
      let sc =
        token env v5
        (* ";" *)
      in
      var_def_stmt attrs vardefs sc
  | `Local_func_stmt (decl, v7, v8) ->
      (* local_function_declaration is now `(attrs, modifiers, type,
         id, type_params?, params)`; the constraints clauses and body
         hang off the statement directly. *)
      let v1_attrs, v2_mods, v3_ty, v4_id, v5_tparams, v6_params = decl in
      let v1 = List.concat_map (attribute_list_ env) v1_attrs in
      let v2 = List.map (modifier env) v2_mods in
      let v3 = type_pattern env v3_ty in
      let v4 = identifier env v4_id (* identifier *) in
      let _, tok = v4 in
      let tparams = Option.map (type_parameter_list env) v5_tparams in
      let v6 = parameter_list env v6_params in
      let v7 = List.map (type_parameter_constraints_clause env) v7 in
      let v8 = function_body env v8 in
      let tparams = type_parameters_with_constraints tparams v7 in
      let idinfo = empty_id_info () in
      let ent = { name = EN (Id (v4, idinfo)); attrs = v1 @ v2; tparams } in
      let def =
        G.FuncDef
          {
            fkind = (G.Method, tok);
            fparams = v6;
            frettype = Some v3;
            fbody = v8;
          }
      in
      G.DefStmt (ent, def) |> G.s
  | `Lock_stmt (v1, v2, v3, v4, v5) ->
      let tlock =
        token env v1
        (* "lock" *)
      in
      let _l =
        token env v2
        (* "(" *)
      in
      let e = expression env v3 in
      let _r =
        token env v4
        (* ")" *)
      in
      let st = statement env v5 in
      OtherStmtWithStmt (OSWS_Block ("Lock", tlock), [ E e ], st) |> G.s
  | `Ret_stmt (v1, v2, v3) ->
      let v1 =
        token env v1
        (* "return" *)
      in
      let v2 = Option.map (expression env) v2 in
      let v3 =
        token env v3
        (* ";" *)
      in
      Return (v1, v2, v3) |> G.s
  | `Switch_stmt (v1, v2, v3) ->
      let v1 =
        token env v1
        (* "switch" *)
      in
      let v2 =
        match v2 with
        | `LPAR_exp_RPAR (_v1, v2, _v3) -> expression env v2
        | `Tuple_exp v2 -> tuple_expression env v2
      in
      let v3 = switch_body env v3 in
      G.Switch (v1, Some (G.Cond v2), v3) |> G.s
  | `Throw_stmt (v1, v2, v3) ->
      let v1 =
        token env v1
        (* "throw" *)
      in
      let v2 = Option.map (expression env) v2 in
      let v3 =
        token env v3
        (* ";" *)
      in
      (match v2 with
        | Some expr -> Throw (v1, expr, v3)
        | None -> OtherStmt (OS_ThrowNothing, [ Tk v1; Tk v3 ]))
      |> G.s
  | `Try_stmt (v1, v2, v3, v4) ->
      let v1 =
        token env v1
        (* "try" *)
      in
      let v2 = block env v2 in
      let v3 = List.map (catch_clause env) v3 in
      let v4 = Option.map (finally_clause env) v4 in
      Try (v1, v2, v3, None, v4) |> G.s
  | `Unsafe_stmt (v1, v2) ->
      let tunsafe =
        token env v1
        (* "unsafe" *)
      in
      let v2 = block env v2 in
      OtherStmtWithStmt (OSWS_Block ("Unsafe", tunsafe), [], v2) |> G.s
  | `Using_stmt (v1, v2, v3, v4, v5, v6) ->
      let _v1TODO =
        Option.map (token env) v1
        (* "await" *)
      in
      let v2 =
        token env v2
        (* "using" *)
      in
      let _v3 =
        token env v3
        (* "(" *)
      in
      let v4 =
        match v4 with
        | `Using_var_decl (ty, first, rest) ->
            (* using_variable_declaration is type + first declarator +
               (",", declarator) list, where each declarator is
               (id, ("=", expression_element)?). Translate to a list
               of VarDefs sharing the same type so downstream consumers
               see the same DefStmt shape they used to. *)
            let vtype = local_variable_type env ty in
            let mk_def ((id, init_opt) : CST.using_variable_declarator) =
              let id = identifier env id in
              let vinit =
                Option.map (fun (_eq, e) -> expression env e) init_opt
              in
              let ent = basic_entity id in
              let vardef =
                { vinit; vtype; vtok = G.no_sc }
              in
              G.DefStmt (ent, VarDef vardef) |> G.s
            in
            let first = mk_def first in
            let rest = List.map (fun (_comma, d) -> mk_def d) rest in
            G.stmt1 (first :: rest)
        | `Exp x ->
            let expr = expression env x in
            ExprStmt (expr, sc) |> G.s
      in
      let _v5 =
        token env v5
        (* ")" *)
      in
      let v6 = statement env v6 in
      WithUsingResource (v2, [ v4 ], v6) |> G.s
  | `While_stmt (v1, v2, v3, v4, v5) ->
      let v1 =
        token env v1
        (* "while" *)
      in
      let _v2 =
        token env v2
        (* "(" *)
      in
      let v3 = expression env v3 in
      let _v4 =
        token env v4
        (* ")" *)
      in
      let v5 = statement env v5 in
      While (v1, G.Cond v3, v5) |> G.s
  | `Yield_stmt (v1, v2, v3) ->
      let v1 =
        token env v1
        (* "yield" *)
      in
      let v2 =
        match v2 with
        | `Ret_exp (v1, v2) ->
            let _v1 =
              token env v1
              (* "return" *)
            in
            let v2 = expression env v2 in
            Some v2
        | `Brk _tok -> None
        (* "break" *)
      in
      let v3 =
        token env v3
        (* ";" *)
      in
      ExprStmt (Yield (v1, v2, false) |> G.e, v3) |> G.s
  | `Prep_if_in_top_level (_if_tok, _cond, _nl, items, _else_opt, _endif) ->
      (* Preprocessor `#if`. The grammar only exposes the if-branch
         items at this slot; we walk them so guarded declarations remain
         visible to semgrep (most real .NET codebases gate code with
         `#if DEBUG`). The else-branch is dropped — semgrep doesn't pick
         a conditional. *)
      let stmts =
        List.concat_map
          (fun (x : CST.anon_choice_top_level_item_no_stmt_65e2afd) ->
            match x with
            | `Choice_extern_alias_dire t -> top_level_item_no_statement env t
            | `Stmt s -> [ global_statement env s ])
          items
      in
      G.Block (fb stmts) |> G.s

and interpolated_string_expression (env : env)
    (x : CST.interpolated_string_expression) =
  (* The new grammar models all three string flavors uniformly as
     `(prefix, start_quote, content list, end_quote)`. *)
  let x =
    match x with
    | `Interp_regu_start_interp_start_quote_rep_inte_str_content_interp_end_quote
        (v1, v2start, v3, v4) ->
        let v1 = token env v1 (* interp prefix *) in
        let _ = token env v2start in
        let v2 = List.map (interpolated_string_content env) v3 in
        let v3 = token env v4 in
        (v1, v2, v3)
    | `Interp_verb_start_interp_start_quote_rep_inte_verb_str_content_interp_end_quote
        (v1, v2start, v3, v4) ->
        let v1 = token env v1 in
        let _ = token env v2start in
        let v2 = List.map (interpolated_verbatim_string_content env) v3 in
        let v3 = token env v4 in
        (v1, v2, v3)
    | `Interp_raw_start_interp_start_quote_rep_inte_raw_str_content_interp_end_quote
        (v1, v2start, v3, v4) ->
        let v1 = token env v1 in
        let _ = token env v2start in
        let v2 = List.map (interpolated_raw_string_content env) v3 in
        let v3 = token env v4 in
        (v1, v2, v3)
  in
  let v1, v2, _v3 = x in
  let args = fb (List.map (fun e -> Arg e) v2) in
  (* TODO should we use FString here instead of InterpolatedConcat? *)
  Call (Special (ConcatString InterpolatedConcat, v1) |> G.e, args) |> G.e

and tuple_element (env : env) ((v1, v2) : CST.tuple_element) =
  let v1 = type_pattern env v1 in
  let _v2TODO =
    Option.map (identifier env) v2
    (* identifier *)
  in
  v1

and constant_pattern (env : env) (x : CST.constant_pattern) : G.pattern =
  let e = constant_pattern_aux env x in
  H2.expr_to_pattern e

(* TODO: generate directly pattern so no need to use H2.expr_to_pattern *)
and constant_pattern_aux env (x : CST.constant_pattern) : G.expr =
  match x with
  | `Bin_exp x -> binary_expression env x
  | `Defa_exp x -> default_expression env x
  | `Inte_str_exp x -> interpolated_string_expression env x
  | `Paren_exp x -> parenthesized_expression env x
  | `Post_un_exp x -> postfix_unary_expression env x
  | `Prefix_un_exp x -> prefix_unary_expression env x
  | `Sizeof_exp x -> size_of_expression env x
  | `Tuple_exp x -> tuple_expression env x
  | `Typeof_exp x -> type_of_expression env x
  | `Member_access_exp x -> member_access_expression env x
  | `Name_invo_pat (n, args) ->
      (* C# 12-ish: `Name(args)` as a constant pattern. Approximate as a
         call expression. *)
      let n = name env n in
      let args = argument_list env args in
      Call (N n |> G.e, args) |> G.e
  | `Comp_invo_exp (callee_choice, args) ->
      let callee =
        match callee_choice with
        | `Member_access_exp x -> member_access_expression env x
        | `Elem_access_exp _ ->
            G.OtherExpr (("ElemAccess", fake "["), []) |> G.e
        | `Invo_exp x -> invocation_expression env x
        | `Paren_exp x -> parenthesized_expression env x
        | `Cond_access_exp _ ->
            G.OtherExpr (("CondAccess", fake "?."), []) |> G.e
        | `Cast_exp x -> cast_expression env x
      in
      let args = argument_list env args in
      Call (callee, args) |> G.e
  | `Cast_exp x -> cast_expression env x
  | `Simple_name x -> simple_name_expression env x
  | `Lit x ->
      let lit = literal env x in
      G.L lit |> G.e

and catch_declaration (env : env) ((v1, v2, v3, v4) : CST.catch_declaration) :
    G.catch_exn =
  let _v1 =
    token env v1
    (* "(" *)
  in
  let v2 = type_pattern env v2 in
  let v3 =
    Option.map (identifier env) v3
    (* identifier *)
  in
  let _v4 =
    token env v4
    (* ")" *)
  in
  CatchParam (G.param_of_type v2 ?pname:v3)

(* Inlined into switch_section in the post-C#-14 grammar; the case/default/
   pattern label is now part of switch_section's tuple instead of a named
   subtree. The new grammar disambiguates `case <expr>:` and `case
   <pattern>:` differently from the old one — what used to reach the
   expression arm (a bare literal or identifier) now lands in the pattern
   arm. To keep the case-vs-pattern distinction the rest of the
   generic AST and downstream rules already rely on, we lift "trivial"
   constant patterns back to `CaseEqualExpr` when there's no `when`. *)
and case_pattern_to_case env (case_tok : Tree_sitter_run.Token.t)
    (pat : CST.pattern) (when_opt : CST.when_clause option) =
  let v1 = token env case_tok (* "case" *) in
  let mapped = pattern env pat in
  (* When the user wrote `case <constant-expr>:` (no `when`), the new
     grammar routes bare identifiers, dotted-names, and literals through
     the pattern arm. Lift those back to `CaseEqualExpr` so downstream
     analyses see an expression-equality switch label, not a pattern. *)
  match (when_opt, mapped) with
  | None, PatLiteral lit -> G.CaseEqualExpr (v1, G.L lit |> G.e)
  | None, PatId (id, _) ->
      G.CaseEqualExpr (v1, G.N (G.Id (id, empty_id_info ())) |> G.e)
  | None, PatConstructor (n, []) ->
      G.CaseEqualExpr (v1, G.N n |> G.e)
  | None, _ -> G.Case (v1, mapped)
  | Some x, _ -> G.Case (v1, PatWhen (mapped, when_clause env x))

and query_clause (env : env) (x : CST.query_clause) =
  match x with
  | `From_clause x -> from_clause env x
  | `Join_clause (v1, header, body, v10) ->
      (* join_clause restructured: header bundles
         `(type?, id, in, source)` and body bundles
         `(on, key, equals, key)`. *)
      let v1 = token env v1 (* "join" *) in
      let v2_ty, v3_id, _v4_in, v5_src = header in
      let v2 = Option.map (type_pattern env) v2_ty in
      let v3 = identifier env v3_id in
      let v5 = expression env v5_src in
      let _v6_on, v7_left, _v8_equals, v9_right = body in
      let v7 = expression env v7_left in
      let v9 = expression env v9_right in
      let v10 = Option.map (join_into_clause env) v10 in
      Join (v1, (v2, v3), v5, v7, v9, v10)
  | `Let_clause (v1, v2, v3, v4) ->
      let v1 =
        token env v1
        (* "let" *)
      in
      let v2 =
        identifier env v2
        (* identifier *)
      in
      let _v3 =
        token env v3
        (* "=" *)
      in
      let v4 = expression env v4 in
      Let (v1, v2, v4)
  | `Order_by_clause (v1, v2, v3) ->
      let v1 =
        token env v1
        (* "orderby" *)
      in
      let v2 = ordering env v2 in
      let v3 =
        List.map
          (fun (v1, v2) ->
            let _v1 =
              token env v1
              (* "," *)
            in
            let v2 = ordering env v2 in
            v2)
          v3
      in
      OrderBy (v1, v2 :: v3)
  | `Where_clause (v1, v2) ->
      let v1 =
        token env v1
        (* "where" *)
      in
      let v2 = expression env v2 in
      Where (v1, v2)

and arrow_expression_clause (env : env) ((v1, v2) : CST.arrow_expression_clause)
    =
  let v1 =
    token env v1
    (* "=>" *)
  in
  let v2 = expression env v2 in
  (v1, v2)

and attribute_argument (env : env) ((v1, v2) : CST.attribute_argument) =
  (* attribute_argument inlines `(identifier, ":" | "=")` directly now. *)
  let v1 =
    match v1 with
    | Some (id, sep) -> (
        match sep with
        | `EQ tok -> Some (name_equals env (id, tok))
        | `COLON tok -> Some (name_colon env (id, tok)))
    | None -> None
  in
  let v2 = expression env v2 in
  match v1 with
  | Some name -> ArgKwd (name, v2)
  | None -> Arg v2

and catch_filter_clause (env : env) ((v1, v2, v3, v4) : CST.catch_filter_clause)
    =
  let _v1 =
    token env v1
    (* "when" *)
  in
  let _v2 =
    token env v2
    (* "(" *)
  in
  let v3 = expression env v3 in
  let _v4 =
    token env v4
    (* ")" *)
  in
  v3

and formal_parameter_list (env : env)
    ((v1, v2) : CST.anon_choice_param_rep_COMMA_choice_param_6325c4d) =
  let v1 = anon_choice_param_ce11a32 env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _v1 =
          token env v1
          (* "," *)
        in
        let v2 = anon_choice_param_ce11a32 env v2 in
        v2)
      v2
  in
  v1 :: v2

(* The new grammar drops the named `equals_value_clause` rule and inlines
   `("=" * expression)` directly into variable_declarator etc. We keep
   the helper but type it on the inlined tuple so call sites stay short. *)
and equals_value_clause (env : env)
    ((v1, v2) : Tree_sitter_run.Token.t * CST.expression) : expr =
  let _v1 = token env v1 (* "=" *) in
  expression env v2

and switch_section (env : env) ((v1, _colon, v3) : CST.switch_section) :
    case_and_body =
  let label =
    match v1 with
    | `Case_choice_exp (case_tok, payload) -> (
        match payload with
        | `Exp e ->
            let v1 = token env case_tok (* "case" *) in
            G.CaseEqualExpr (v1, expression env e)
        | `Pat_opt_when_clause (pat, when_opt) ->
            case_pattern_to_case env case_tok pat when_opt)
    | `Defa tok -> G.Default (token env tok (* "default" *))
  in
  (* global_statement is just `statement`; inline rather than depend on
     a function defined in a later recursive group. *)
  let v3 = List.map (statement env) v3 in
  (* TODO: we convert list of statements to a block with fake brackets. Does this make sense? *)
  CasesAndBody ([ label ], G.stmt1 v3)

(* New `attribute_list_` wrapper distinguishes a real attribute list from
   a preprocessor `#if`-guarded one. Preprocessor branches don't survive
   into the AST, so we drop them and recurse only on real attribute lists. *)
and attribute_list_ (env : env) (x : CST.attribute_list_) : attribute list =
  match x with
  | `Attr_list al -> attribute_list env al
  | `Prep_if_in_attr_list _ -> []

and attribute_list (env : env) ((v1, v2, v3, v4, v5, v6) : CST.attribute_list) :
    attribute list =
  (* TODO: Handle unused tokens. *)
  let _v1 =
    token env v1
    (* "[" *)
  in
  let _v2 = Option.map (attribute_target_specifier env) v2 in
  let v3 = attribute env v3 in
  let v4 =
    List.map
      (fun (x, y) ->
        token env x (* "," *) |> ignore;
        attribute env y)
      v4
  in
  let _v5 =
    Option.map (token env) v5
    (* "," *)
  in
  let _v6 =
    token env v6
    (* "]" *)
  in
  v3 :: v4

and bracketed_argument_list (env : env)
    ((v1, v2, v3, _v4trailing, v5) : CST.bracketed_argument_list) =
  let v1 = token env v1 (* "[" *) in
  let v2 = argument env v2 in
  let v3 =
    List.map
      (fun (_, a) -> argument env a)
      v3
  in
  let v5 = token env v5 (* "]" *) in
  (v1, v2 :: v3, v5)

and pattern (env : env) (x : CST.pattern) : G.pattern =
  match x with
  | `Cst_pat x -> constant_pattern env x
  | `Decl_pat (v1, v2) ->
      (* The "type" of a declaration pattern may be implicit (`var`),
         in which case we don't have a real type to wrap with — return
         the variable designation directly. *)
      let v2 = variable_designation env v2 in
      (match local_variable_type env v1 with
       | Some ty -> PatTyped (v2, ty)
       | None -> v2)
  | `Disc tok -> PatWildcard (token env tok) (* "_" *)
  | `Recu_pat x -> recursive_pattern env x
  | `Var_pat (v1, v2) ->
      let _v1 =
        token env v1
        (* "var" *)
      in
      let v2 = variable_designation env v2 in
      v2
  | `Nega_pat (v1, v2) ->
      let v1 =
        token env v1
        (* "not" *)
      in
      let _v2 = pattern env v2 in
      todo_pat env v1
  | `Paren_pat (v1, v2, v3) ->
      let v1 =
        token env v1
        (* "(" *)
      in
      let _v2 = pattern env v2 in
      let _v3 =
        token env v3
        (* ")" *)
      in
      todo_pat env v1
  | `Rela_pat x -> relational_pattern env x
  | `And_pat (v1, v2, v3) ->
      let _v1 = pattern env v1 in
      let v2 = (* "and" *) token env v2 in
      let _v3 = pattern env v3 in
      todo_pat env v2
  | `Or_pat (v1, v2, v3) ->
      let _v1 = pattern env v1 in
      let v2 = (* "or" *) token env v2 in
      let _v3 = pattern env v3 in
      todo_pat env v2
  | `List_pat (v1, items_opt, v3, _vd_opt) ->
      (* C# 11 list pattern; the new grammar adds an optional trailing
         variable_designation slot. *)
      let v1 = (* "[" *) token env v1 in
      let v2 =
        match items_opt with
        | Some (first, rest, _trailing_comma) ->
            let first = map_anon_choice_pat_29be9ad env first in
            let rest =
              List.map
                (fun (_comma, p) -> map_anon_choice_pat_29be9ad env p)
                rest
            in
            first :: rest
        | None -> []
      in
      let v3 = (* "]" *) token env v3 in
      PatList (v1, v2, v3)
  | `Type_pat x ->
      let _xTODO = type_pattern env x in
      todo_pat env (fake "TODO_type_pattern")
  | `Paren_pat_with_desi (_lp, p, _rp, _vd) -> pattern env p

(* The new grammar enumerates each shape recursive_pattern can take as
   its own poly-variant arm instead of `(type?, body, var_desi?)`. We
   materialize the type prefix as `PatTyped`, fold the positional and
   property clauses into a single positional pattern (with the property
   subpatterns appended), and append the variable designation as
   `PatAs(...)`. *)
and property_pattern_clause_subpatterns env
    ((_lb, items_opt, _trail, _rb) : CST.property_pattern_clause) :
    G.pattern list =
  match items_opt with
  | None -> []
  | Some (first, rest) ->
      subpattern env first :: List.map (fun (_c, p) -> subpattern env p) rest

and merge_pos_and_prop env pos_clause prop_opt : G.pattern =
  let pos_pat = positional_pattern_clause env pos_clause in
  match prop_opt with
  | None -> pos_pat
  | Some prop ->
      let extra = property_pattern_clause_subpatterns env prop in
      (match pos_pat with
       | PatTuple (lp, sub, rp) -> PatTuple (lp, sub @ extra, rp)
       | other -> other)

and pat_with_desi (pat : G.pattern) (vd : G.pattern) : G.pattern =
  match vd with
  | PatId (id, _) -> PatAs (pat, (id, empty_id_info ()))
  | _ -> pat

and recursive_pattern env (x : CST.recursive_pattern) =
  match x with
  | `Name_posi_pat_clause_opt_prop_pat_clause_var_desi
      (n, pos, prop_opt, vd) ->
      let _n = name env n in
      let pat = merge_pos_and_prop env pos prop_opt in
      let vd = variable_designation env vd in
      pat_with_desi pat vd
  | `Name_posi_pat_clause_opt_prop_pat_clause (n, pos, prop_opt) ->
      let _n = name env n in
      merge_pos_and_prop env pos prop_opt
  | `Posi_pat_clause_var_desi (pos, vd) ->
      let pat = positional_pattern_clause env pos in
      let vd = variable_designation env vd in
      pat_with_desi pat vd
  | `Posi_pat_clause pos -> positional_pattern_clause env pos
  | `Type_choice_posi_pat_clause_opt_prop_pat_clause_opt_var_desi
      (ty, choice, vd_opt) ->
      let t = type_ env ty in
      let inner =
        match choice with
        | `Posi_pat_clause_opt_prop_pat_clause (pos, prop_opt) ->
            merge_pos_and_prop env pos prop_opt
        | `Prop_pat_clause prop ->
            let lb, _, _, rb = prop in
            PatTuple (token env lb,
                      property_pattern_clause_subpatterns env prop,
                      token env rb)
      in
      let typed = PatTyped (inner, t) in
      (match vd_opt with
       | Some vd ->
           let vd = variable_designation env vd in
           pat_with_desi typed vd
       | None -> typed)
  | `Choice_posi_pat_clause_prop_pat_clause_opt_var_desi (choice, vd_opt) ->
      let pat =
        match choice with
        | `Posi_pat_clause_prop_pat_clause (pos, prop) ->
            merge_pos_and_prop env pos (Some prop)
        | `Prop_pat_clause prop ->
            let lb, _, _, rb = prop in
            PatTuple (token env lb,
                      property_pattern_clause_subpatterns env prop,
                      token env rb)
      in
      (match vd_opt with
       | Some vd ->
           let vd = variable_designation env vd in
           pat_with_desi pat vd
       | None -> pat)

and positional_pattern_clause (env : env)
    ((v1, v2, v3) : CST.positional_pattern_clause) =
  let v1 =
    token env v1
    (* "(" *)
  in
  let v2 =
    match v2 with
    | Some (Some (first, rest)) ->
        first :: List.map snd rest |> List.map (subpattern env)
    | Some None | None -> []
  in
  let v3 =
    token env v3
    (* ")" *)
  in
  PatTuple (v1, v2, v3)

and subpattern (env : env) ((v1, v2) : CST.subpattern) =
  (* The new grammar splits the optional first half of a subpattern
     into either an expression-colon or an identifier-colon arm. Both
     are dropped today (the prefix is informational); we just walk past
     them for side effects. *)
  let _V1TODO =
    match v1 with
    | None -> ()
    | Some (`Exp_COLON (e, t)) ->
        let _ = expression_colon env (e, t) in
        ()
    | Some (`Id_COLON (id, t)) ->
        let _ = name_colon env (id, t) in
        ()
  in
  let v2 = pattern env v2 in
  v2

(* similar to name_colon — also inlined as `(expression * ":")` tuple
   in the new grammar; named CST type removed. *)
and expression_colon (env : env)
    ((v1, v2) : CST.expression * Tree_sitter_run.Token.t) =
  let t = expression env v1 in
  let tcolon = (* ":" *) token env v2 in
  (t, tcolon)

and _property_pattern_clause (env : env)
    ((v1, v2, _v3comma, v4) : CST.property_pattern_clause) =
  let v1 =
    token env v1
    (* "{" *)
  in
  let _v2 =
    match v2 with
    | Some _x -> [ todo_pat env v1 ]
    | None -> []
  in
  let _v4 =
    token env v4
    (* "}" *)
  in
  todo_pat env v1

and anonymous_object_member_declarator (env : env)
    (x : CST.anonymous_object_member_declarator) =
  match x with
  | `Id_EQ_exp init ->
      (* The new grammar reuses `with_initializer` here:
         `(id, "=", expression_element)`. *)
      with_initializer env init
  | `Exp x ->
      let expr = expression env x in
      F (exprstmt expr)

and function_body (env : env) (x : CST.function_body) : G.function_body =
  match x with
  | `Blk x -> G.FBStmt (block env x)
  | `Arrow_exp_clause_SEMI (v1, v2) ->
      let v1 = arrow_expression_clause env v1 in
      let v2 =
        token env v2
        (* ";" *)
      in
      let _arrow, expr = v1 in
      G.FBStmt (G.ExprStmt (expr, v2) |> G.s)
  | `SEMI tok ->
      let t =
        token env tok
        (* ";" *)
      in
      G.FBDecl t

and finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 =
    token env v1
    (* "finally" *)
  in
  let v2 = block env v2 in
  (v1, v2)

and parameter (env : env) (v1 : CST.parameter) : G.parameter =
  match v1 with
  | `Rep_attr_list__opt_rep_choice_this_type_id_opt_EQ_exp x ->
      explicit_parameter env x
  | `Ellips v1 -> ParamEllipsis (token env v1)

and parameter_type_with_modifiers (env : env)
    ((v1_mods, v2_ty) : CST.parameter_type_with_modifiers) =
  (* The new grammar collapses the four legacy modifier slots
     (this?, scoped?, ref-kind?, base-type) into
     `(modifier list, type_pattern)` where each modifier in the list
     is `This|Scoped|Ref|Out|In|Read`. We drop the modifier list for
     now and route the type through `type_`. *)
  let _v1TODO = List.map (fun _ -> ()) v1_mods in
  let _ = _v1TODO in
  type_ env v2_ty

and explicit_parameter (env : env) (v1, v2, v3, v4) =
  let v1 = List.concat_map (attribute_list_ env) v1 in
  let v2 = Option.map (parameter_type_with_modifiers env) v2 in
  let v3 =
    identifier env v3
    (* identifier *)
  in
  let v4 = Option.map (equals_value_clause env) v4 in
  Param
    {
      pname = Some v3;
      ptype = v2;
      pdefault = v4;
      pattrs = v1;
      pinfo = empty_id_info ();
    }

and from_clause (env : env) ((v1, v2, v3, v4, v5) : CST.from_clause) :
    linq_query_part =
  let v1 =
    token env v1
    (* "from" *)
  in
  let v2 = Option.map (type_pattern env) v2 in
  let v3 =
    identifier env v3
    (* identifier *)
  in
  let _v4 =
    token env v4
    (* "in" *)
  in
  let v5 = expression env v5 in
  From (v1, (v2, v3), v5)

and attribute (env : env) ((v1, v2) : CST.attribute) =
  let v1 = name env v1 in
  let v2 =
    match v2 with
    | Some x -> attribute_argument_list env x
    | None -> fb []
  in
  (* TODO get the first [ as token here? *)
  G.NamedAttr (fake "[", v1, v2)

and argument_list (env : env) ((v1, v2, v3) : CST.argument_list) : G.arguments =
  let v1 =
    token env v1
    (* "(" *)
  in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = argument env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let _v1 =
                token env v1
                (* "," *)
              in
              let v2 = argument env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 =
    token env v3
    (* ")" *)
  in
  (v1, v2, v3)

and pointer_type env (v1, v2) =
  let v1 = pointer_base_type env v1 in
  let v2 =
    token env v2
    (* "*" *)
  in
  TyPointer (v2, v1) |> G.t

and pointer_base_type (env : env) (x : CST.pointer_base_type) =
  match x with
  | `Name x -> type_name env x
  | `Null_type x -> nullable_type env x
  | `Poin_type x -> pointer_type env x
  | `Func_poin_type x -> function_pointer_type env x
  | `Pred_type tok -> predefined_type env tok (* predefined_type *)
  | `Tuple_type x -> tuple_type env x

and function_pointer_type env (v1, v2, _v3, v4, _v5, _v6, v7) =
  let v1 =
    token env v1
    (* "delegate" *)
  in
  let _v2 =
    token env v2
    (* "*" *)
  in
  let _v4 =
    token env v4
    (* "<" *)
  in
  let _v7 =
    token env v7
    (* ">" *)
  in
  todo_type env v1

(* tuple_type collapsed: the explicit comma+second element are now
   inside the trailing list. *)
and tuple_type env (v1, v2, rest, v6) =
  let v1 = token env v1 (* "(" *) in
  let v2 = tuple_element env v2 in
  let rest =
    List.map
      (fun (_comma, te) -> tuple_element env te)
      rest
  in
  let v6 = token env v6 (* ")" *) in
  TyTuple (v1, v2 :: rest, v6) |> G.t

and ref_type (env : env) ((v1, v2, v3) : CST.ref_type) =
  let v1 = (* "ref" *) token env v1 in
  let _v2TODO =
    match v2 with
    | Some tok -> Some ((* "readonly" *) token env tok)
    | None -> None
  in
  (* ref_type now uses `type_pattern` (= `type_`) directly instead of
     the narrower `ref_base_type` choice. *)
  let v3 = type_ env v3 in
  (* not exactly a real ref
     https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/ref#passing-an-argument-by-reference
  *)
  OtherType (("ref", v1), [ G.T v3 ]) |> G.t

(* The new grammar dropped the `type_name` alias; the `Type_name` arm
   in ref_base_type and friends became plain `Name of name`. We keep the
   helper to bridge that without rewriting every caller. *)
and type_name (env : env) (x : CST.name) =
  let n = name env x in
  TyN n |> G.t

and _ref_base_type (env : env) (x : CST.ref_base_type) =
  match x with
  | `Impl_type tok -> G.OtherType (("var", token env tok), []) |> G.t
  | `Array_type x -> array_type env x
  | `Name x -> type_name env x
  | `Null_type x -> nullable_type env x
  | `Poin_type x -> pointer_type env x
  | `Func_poin_type x -> function_pointer_type env x
  | `Pred_type tok -> predefined_type env tok (* predefined_type *)
  | `Tuple_type x -> tuple_type env x

and scoped_type (env : env) ((v1, v2) : CST.scoped_type) =
  (* I (Brandon) am not sure what this is. I couldn't find docs on it anywhere. *)
  G.OtherType (("scoped", token env v1), [ G.T (scoped_base_type env v2) ])
  |> G.t

and scoped_base_type (env : env) (x : CST.scoped_base_type) =
  match x with
  | `Name x -> type_name env x
  | `Ref_type x -> ref_type env x

and type_ (env : env) (x : CST.type_) : G.type_ =
  match x with
  | `Impl_type _tok ->
      (* When type_ is called, we expect an explicit type, not "var".
         The implicit type is handled in local_variable_type. *)
      raise
        (Parsing_error.Other_error
           ("Expected explicit type", Parse_tree_sitter_helpers.token env _tok))
  | `Array_type x -> array_type env x
  | `Name x -> type_name env x
  | `Null_type x -> nullable_type env x
  | `Poin_type x -> pointer_type env x
  | `Func_poin_type x -> function_pointer_type env x
  | `Pred_type tok -> predefined_type env tok (* predefined_type *)
  | `Tuple_type x -> tuple_type env x
  | `Ref_type x -> ref_type env x
  | `Scoped_type x -> scoped_type env x

and type_argument_list (env : env) ((v1, v2, v3) : CST.type_argument_list) =
  let v1 =
    token env v1
    (* "<" *)
  in
  let v2 =
    match v2 with
    | `Rep_COMMA _xs -> [] (* TODO What's this case? <,,,>? *)
    | `Type_rep_COMMA_type (v1, v2) ->
        let v1 = type_pattern env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let _v1 =
                token env v1
                (* "," *)
              in
              let v2 = type_pattern env v2 in
              v2)
            v2
        in
        v1 :: v2
  in
  let v3 =
    token env v3
    (* ">" *)
  in
  (v1, List.map (fun t -> TA t) v2, v3)

and type_parameter_constraints_clause (env : env)
    ((v1, v2, v3, v4, v5) : CST.type_parameter_constraints_clause) =
  let _v1 =
    token env v1
    (* "where" *)
  in
  let v2 = identifier_or_global env v2 in
  let _v3 =
    token env v3
    (* ":" *)
  in
  let v4 = type_parameter_constraint env v4 in
  let v5 =
    List.map
      (fun (v1, v2) ->
        let _v1 =
          token env v1
          (* "," *)
        in
        let v2 = type_parameter_constraint env v2 in
        v2)
      v5
  in
  (v2, v4 :: v5)

(* The new grammar replaces the old `implicit_parameter[_list]` with
   `implicit_parameter_list_with_modifiers`, which carries one-or-more
   modifiers (`scoped`, `ref`, `out`, `in`, `readonly`) per parameter to
   support C# 14 simple-lambda parameter modifiers. We treat each entry
   as a plain identifier param and drop the modifiers for now — they can
   be threaded through as attributes in a follow-up. *)
and implicit_parameter_list (env : env)
    (x : CST.implicit_parameter_list_with_modifiers) =
  let _lparen, first, rest, rparen = x in
  let lp = token env _lparen in
  let rp = token env rparen in
  let mk_param p =
    match p with
    | `Rep1_choice_scoped_id (_mods, id) ->
        Param (param_of_id (identifier env id))
    | `Id id -> Param (param_of_id (identifier env id))
  in
  let params =
    mk_param first
    :: List.map (fun (_comma, p) -> mk_param p) rest
  in
  (lp, params, rp)

and _implicit_parameter_TODO (env : env) (x : CST.identifier) =
  let id = identifier env x in
  let p = param_of_id id in
  fb [ Param p ]

and parameter_list (env : env) ((v1, v2, v3) : CST.parameter_list) : parameters
    =
  let lp =
    token env v1
    (* "(" *)
  in
  let xs =
    match v2 with
    | Some x -> formal_parameter_list env x
    | None -> []
  in
  let rp =
    token env v3
    (* ")" *)
  in
  (lp, xs, rp)

and attribute_argument_list (env : env)
    ((v1, v2, v3) : CST.attribute_argument_list) : arguments =
  let v1 =
    token env v1
    (* "(" *)
  in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = attribute_argument env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let _v1 =
                token env v1
                (* "," *)
              in
              let v2 = attribute_argument env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 =
    token env v3
    (* ")" *)
  in
  (v1, v2, v3)

and select_or_group_clause (env : env) (x : CST.select_or_group_clause) =
  match x with
  | `Group_clause (v1, v2, v3, v4) ->
      let v1 =
        token env v1
        (* "group" *)
      in
      let v2 = expression env v2 in
      let _v3 =
        token env v3
        (* "by" *)
      in
      let v4 = expression env v4 in
      Group (v1, v2, v4)
  | `Select_clause (v1, v2) ->
      let v1 =
        token env v1
        (* "select" *)
      in
      let v2 = expression env v2 in
      Select (v1, v2)

and declaration_expression (env : env) ((v1, v2) : CST.declaration_expression) =
  let v1 = local_variable_type env v1 in
  let v2 =
    identifier env v2
    (* identifier *)
  in
  match v1 with
  | Some t ->
      let ent = basic_entity v2 in
      let vardef = { vinit = None; vtype = Some t; vtok = G.no_sc } in
      let st = DefStmt (ent, VarDef vardef) |> G.s in
      G.stmt_to_expr st
  | None -> N (Id (v2, empty_id_info ())) |> G.e

and interpolation (env : env) ((v1, v2, v3, v4, v5) : CST.interpolation) :
    expr bracket =
  let v1 =
    token env v1
    (* "{" *)
  in
  let v2 = expression env v2 in
  let _v3_TODO = Option.map (interpolation_alignment_clause env) v3 in
  let _v4_TODO = Option.map (interpolation_format_clause env) v4 in
  let v5 =
    token env v5
    (* "}" *)
  in
  (v1, v2, v5)

and explicit_interface_specifier (env : env)
    ((v1, v2) : CST.explicit_interface_specifier) =
  let v1 = name env v1 in
  let _v2 =
    token env v2
    (* "." *)
  in
  v1

and accessor_declaration (env : env)
    ((v1, v2, v3, v4) : CST.accessor_declaration) =
  let v1 = List.concat_map (attribute_list_ env) v1 in
  let v2 = List.map (modifier env) v2 in
  let v3 =
    match v3 with
    | `Get tok -> (str env tok, KeywordAttr (Getter, token env tok)) (* "get" *)
    | `Set tok -> (str env tok, KeywordAttr (Setter, token env tok)) (* "set" *)
    | `Add tok -> (str env tok, unhandled_keywordattr (str env tok)) (* "add" *)
    | `Remove tok ->
        (str env tok, unhandled_keywordattr (str env tok)) (* "remove" *)
    | `Init tok ->
        (str env tok, unhandled_keywordattr (str env tok)) (* "init" *)
    | `Id tok ->
        let _, tok = identifier env tok in
        (("Todo", tok), todo_attr env tok)
  in
  let v4 = function_body env v4 in
  let id, attr = v3 in
  ((attr :: v1) @ v2, id, v4)

and bracketed_parameter_list (env : env)
    ((v1, v2, v3) : CST.bracketed_parameter_list) : G.parameters =
  let lbra =
    token env v1
    (* "[" *)
  in
  let params =
    match v2 with
    | Some x -> formal_parameter_list env x
    | None -> []
  in
  let rbra =
    token env v3
    (* "]" *)
  in
  (lbra, params, rbra)

and constructor_initializer (env : env)
    ((v1, v2, v3) : CST.constructor_initializer) =
  let _v1 =
    token env v1
    (* ":" *)
  in
  let v2 =
    match v2 with
    | `Base tok ->
        N (IdSpecial ((Super, token env tok), G.empty_id_info ()))
        (* "base" *) |> G.e
    | `This tok ->
        N (IdSpecial ((This, token env tok), G.empty_id_info ())) |> G.e
    (* "this" *)
  in
  let v3 = argument_list env v3 in
  ExprStmt (Call (v2, v3) |> G.e, sc) |> G.s

and enum_member_declaration (env : env) (x : CST.enum_member_declaration) =
  match x with
  | `Rep_attr_list__id_opt_EQ_exp (v1, v2, v3) ->
      let _v1TODO = List.concat_map (attribute_list_ env) v1 in
      let v2 =
        identifier env v2
        (* identifier *)
      in
      let v3 = Option.map (equals_value_clause env) v3 in
      OrEnum (v2, v3)
  | `Ellips tok -> OrEllipsis (token env tok)

(* base_list now carries an optional `argument_list` next to each
   parent type to express primary-constructor base calls
   (`: Base(args)`). We thread the args list as an argument list TODO
   for now; the existing AST shape doesn't carry it directly. *)
and base_list (env : env) ((v1, v2, _args1, rest) : CST.base_list) :
    G.class_parent list =
  let _v1 = token env v1 (* ":" *) in
  let v2 = type_pattern env v2 in
  let rest =
    List.map
      (fun (_comma, ty, _args) -> type_pattern env ty)
      rest
  in
  v2 :: rest |> List.map (fun t -> (t, None))

and accessor_list (env : env) ((v1, v2, v3) : CST.accessor_list) =
  let v1 =
    token env v1
    (* "{" *)
  in
  let v2 = List.map (accessor_declaration env) v2 in
  let v3 =
    token env v3
    (* "}" *)
  in
  (v1, v2, v3)

and enum_member_declaration_list (env : env)
    ((v1, v2, v3, v4) : CST.enum_member_declaration_list) =
  let _v1 =
    token env v1
    (* "{" *)
  in
  let unwrap_emd env x =
    match (x : CST.anon_choice_enum_member_decl_fe2c440) with
    | `Enum_member_decl x -> Some (enum_member_declaration env x)
    | `Prep_if_in_enum_member_decl _ -> None
  in
  let v2 =
    match v2 with
    | Some (first, rest) ->
        let first = unwrap_emd env first in
        let rest =
          List.map (fun (_comma, x) -> unwrap_emd env x) rest
        in
        first :: rest |> List.filter_map Fun.id
    | None -> []
  in
  let _v3 =
    Option.map (token env) v3
    (* "," *)
  in
  let _v4 =
    token env v4
    (* "}" *)
  in
  v2

and declaration_list (env : env)
    ((open_bracket, body, close_bracket) : CST.declaration_list) =
  let xs = List.map (declaration env) body in
  (token env open_bracket, xs, token env close_bracket)

and _extern_alias_directive (env : env)
    ((v1, v2, v3, v4) : CST.extern_alias_directive) =
  let v1 =
    token env v1
    (* "extern" *)
  in
  let _v2 =
    token env v2
    (* "alias" *)
  in
  let v3 =
    identifier env v3
    (* identifier *)
  in
  let v4 =
    token env v4
    (* ";" *)
  in
  let extern =
    G.OtherDirective (("ExternAlias", v1), [ G.I v3; G.Tk v4 ]) |> G.d
  in
  G.DirectiveStmt extern |> G.s

and using_directive (env : env) ((v0, v1, body, v4) : CST.using_directive) =
  (* using_directive collapsed: body is a choice between
     `unsafe? id = type` (the alias form) and
     `(static|unsafe)* name` (the plain form). *)
  let _globalTODO = Option.map (token env) v0 in
  let v1 = token env v1 (* "using" *) in
  let v4 = token env v4 (* ";" *) in
  match body with
  | `Opt_unsafe_id_EQ_type (_unsafe_opt, alias_id, _eq, ty) ->
      let alias = identifier env alias_id in
      (* The grammar lets the RHS be any `type_pattern`. The namespace-alias
         case (`using A = B.C;`) maps to `ImportAs` so Naming_AST can
         resolve cross-file references. C# 12 also allows arbitrary types
         on the RHS (`using Coord = (int, int);`, function-pointer types,
         nullable types, etc.); model those as a proper `TypeDef AliasType`
         so the alias participates in type resolution rather than being
         shelved into an opaque `OtherDirective`. *)
      (match ty with
       | `Name n ->
           let dotted = H2.dotted_ident_of_name (name env n) in
           let import =
             G.ImportAs (v1, G.DottedName dotted,
                         Some (alias, empty_id_info ()))
           in
           G.DirectiveStmt (import |> G.d) |> G.s
       | _ ->
           let t = type_ env ty in
           let entity = G.basic_entity alias in
           let def = G.TypeDef { tbody = G.AliasType t } in
           G.DefStmt (entity, def) |> G.s)
  | `Rep_choice_static_name (_kw_list, n) ->
      let v3 = name env n in
      let import =
        G.ImportAll (v1, G.DottedName (H2.dotted_ident_of_name v3), v4)
      in
      G.DirectiveStmt (import |> G.d) |> G.s

and global_attribute_list (env : env)
    ((v1, v2, v3, v4_first, v4_rest, _v_trailing_comma, v5) : CST.global_attribute) =
  (* `global_attribute_list` was renamed to `global_attribute` and the
     attribute body now spreads as `(first_attr, rest list, ",")` instead
     of being wrapped in an `Option`. *)
  let v1 = token env v1 (* "[" *) in
  let v2 =
    match v2 with
    | `Asse tok -> str env tok (* "assembly" *)
    | `Module tok -> str env tok (* "module" *)
  in
  let _v3 = token env v3 (* ":" *) in
  let v4 =
    let first = attribute env v4_first in
    let rest = List.map (fun (_comma, a) -> attribute env a) v4_rest in
    first :: rest
  in
  let _v5 = token env v5 (* "]" *) in
  let anys = List.map (fun a -> At a) v4 in
  (* TODO: better as OtherStmt *)
  ExprStmt (OtherExpr (v2, anys) |> G.e, v1) |> G.s

and global_statement (env : env) (x : CST.global_statement) = statement env x

and namespace_member_declaration (env : env)
    (x : CST.namespace_member_declaration) =
  match x with
  | `Name_decl x -> namespace_declaration env x
  | `Type_decl x -> type_declaration env x

and compilation_unit (env : env) (xs : CST.compilation_unit) : any =
  match xs with
  | `Opt_sheb_dire_rep_top_level_item (_sheb_opt, items) ->
      let stmts = List.concat_map (top_level_item env) items in
      G.Pr stmts
  | `Semg_exp (_v1, v2) ->
      let v2 = expression env v2 in
      G.E v2

and top_level_item (env : env) (x : CST.top_level_item) : stmt list =
  match x with
  | `Choice_extern_alias_dire x -> top_level_item_no_statement env x
  | `Global_stmt x -> [ global_statement env x ]

and top_level_item_no_statement (env : env)
    (x : CST.top_level_item_no_statement) : stmt list =
  match x with
  | `Extern_alias_dire (v1, _alias, v3, v4) ->
      (* extern alias ID; — preserve as an OtherDirective so downstream
         consumers can still see the alias name. *)
      let v1 = token env v1 (* "extern" *) in
      let id = identifier env v3 in
      let v4 = token env v4 (* ";" *) in
      let d =
        G.OtherDirective
          (("ExternAlias", v1), [ G.I id; G.Tk v4 ])
      in
      [ G.DirectiveStmt (d |> G.d) |> G.s ]
  | `Using_dire u -> [ using_directive env u ]
  | `Global_attr ga -> [ global_attribute_list env ga ]
  | `Prep_if_in_top_level
      (_if_tok, _cond, _nl, items, _else_opt, _endif) ->
      (* Preprocessor `#if`. Walk the if-branch items so guarded
         declarations survive — many real .NET codebases gate their
         entire file content with `#if !(NET20 || NET35 || …)`, and
         returning an empty block here drops the whole namespace. *)
      List.concat_map
        (fun (x : CST.anon_choice_top_level_item_no_stmt_65e2afd) ->
          match x with
          | `Choice_extern_alias_dire t -> top_level_item_no_statement env t
          | `Stmt s -> [ global_statement env s ])
        items
  | `Choice_name_decl nmd -> [ namespace_member_declaration env nmd ]
  | `File_scoped_name_decl fs ->
      [ file_scoped_namespace_declaration env fs ]

and file_scoped_namespace_declaration (env : env)
    ((tnamespace, name_node, _tsemi) :
      CST.file_scoped_namespace_declaration) =
  (* The new grammar shrinks file_scoped_namespace_declaration to
     `(namespace, name, ";")`; everything that used to live inside
     became sibling top_level_items. *)
  let tnamespace = (* "namespace" *) token env tnamespace in
  let n = name env name_node in
  let dotted_ident = H2.dotted_ident_of_name n in
  let package = G.Package (tnamespace, dotted_ident) |> G.d in
  G.DirectiveStmt package |> G.s

and namespace_declaration (env : env)
    ((v1, v2, v3, _semi) : CST.namespace_declaration) =
  (*
        namespace MySpace { ... } ;
            v1       v2      v3  v4
      *)
  let _v1 =
    token env v1
    (* "namespace" *)
  in
  let v2 = name env v2 in
  let open_brace, decls, close_brace = declaration_list env v3 in
  let body = G.Block (open_brace, decls, close_brace) |> G.s in
  let ent = { name = EN v2; attrs = []; tparams = None } in
  let mkind = G.ModuleStruct (None, [ body ]) in
  let def = { G.mbody = mkind } in
  G.DefStmt (ent, G.ModuleDef def) |> G.s

and type_declaration (env : env) (x : CST.type_declaration) : stmt =
  match x with
  | `Class_decl (init, body) ->
      let attrs, mods, kw, id, tparams_choice, constraints = init in
      let tparams_opt, base_opt, primary_params_opt =
        class_tparam_choice_split env tparams_choice
      in
      class_interface_struct env Class
        (attrs, mods, kw, id, tparams_opt, base_opt, primary_params_opt,
         constraints, body)
  | `Inte_decl (init, body) ->
      let attrs, mods, kw, id, tparams_opt, base_opt, constraints = init in
      let tparams_opt = Option.map (type_parameter_list env) tparams_opt in
      class_interface_struct env Interface
        (attrs, mods, kw, id, tparams_opt, base_opt, None, constraints, body)
  | `Enum_decl x -> enum_declaration env x
  | `Record_decl x -> record_declaration env x
  | `Dele_decl x -> delegate_declaration env x
  | `Struct_decl x -> struct_declaration env x
  | `Ellips tok ->
      let tok = token env tok in
      ExprStmt (Ellipsis tok |> G.e, tok) |> G.s

(* The C# 14 class initializer packs the type-parameter list, the
   optional primary-constructor parameter list, and the base list into
   a single `anon_choice_type_param_list_94781cc` list. Walk it once and
   return all three slots so callers can thread them into the legacy
   shape. The choice list never carries duplicates in practice (one
   each at most), so the first match per arm wins. *)
and class_tparam_choice_split env
    (xs : CST.anon_choice_type_param_list_94781cc list) :
    G.type_parameters option * CST.base_list option * CST.parameter_list option
    =
  List.fold_left
    (fun (tp, bl, pl) x ->
      match x with
      | `Type_param_list y when Option.is_none tp ->
          (Some (type_parameter_list env y), bl, pl)
      | `Base_list y when Option.is_none bl -> (tp, Some y, pl)
      | `Param_list y when Option.is_none pl -> (tp, bl, Some y)
      | _ -> (tp, bl, pl))
    (None, None, None) xs

and class_interface_struct (env : env) class_kind
    (v1, v2, v3, v4, v5_tparams_opt, v6_base_opt, v6_primary_params_opt,
     v7, v8_body) =
  (*
   [Attr] public class MyClass<MyType> : IClass where MyType : SomeType { ... };
      v1     v2    v3    v4     v5         v6           v7                v8
   *)
  let v1 = List.concat_map (attribute_list_ env) v1 in
  let v2 = List.map (modifier env) v2 in
  let v3 = token env v3 (* "class"/"interface" *) in
  let v4 = identifier env v4 in
  let v5 = v5_tparams_opt in
  let v6 =
    match v6_base_opt with
    | Some x -> base_list env x
    | None -> []
  in
  let cparams =
    match v6_primary_params_opt with
    | Some pl -> parameter_list env pl
    | None -> fb []
  in
  let primary_ctor_fields =
    cparams |> Tok.unbracket |> List.filter_map parameter_to_field
  in
  let v7 = List.map (type_parameter_constraints_clause env) v7 in
  let open_bra, stmts, close_bra =
    match v8_body with
    | `Decl_list_opt_semi (dl, _semi) -> declaration_list env dl
    | `SEMI tok ->
        let t = token env tok in
        (t, [], t)
  in
  let fields =
    List.map (fun x -> G.F x) (primary_ctor_fields @ stmts)
  in
  let tparams = type_parameters_with_constraints v5 v7 in
  let idinfo = empty_id_info () in
  let ent = { name = EN (Id (v4, idinfo)); attrs = v1 @ v2; tparams } in
  G.DefStmt
    ( ent,
      G.ClassDef
        {
          ckind = (class_kind, v3);
          cextends = v6;
          cimplements = [];
          cmixins = [];
          cparams;
          cbody = (open_bra, fields, close_bra);
        } )
  |> G.s

and struct_declaration (env : env) ((init, body) : CST.struct_declaration) =
  (* struct_declaration_initializer collapsed to 7-tuple:
     `(attrs, modifiers, "ref"?, "struct", id, tparam-choice list, constraints)`. *)
  let attrs, mods, ref_opt, kw, id, tparams_choice, constraints = init in
  let v1 = List.concat_map (attribute_list_ env) attrs in
  let v2 = List.map (modifier env) mods in
  let v3 =
    match ref_opt with
    | Some tok -> [ unhandled_keywordattr ("ref", token env tok) ]
    | None -> []
  in
  let v4 = token env kw (* "struct" *) in
  let v5 = identifier env id in
  let tparams_opt, base_opt, primary_params_opt =
    class_tparam_choice_split env tparams_choice
  in
  let v7 =
    match base_opt with
    | Some x -> base_list env x
    | None -> []
  in
  let v8 = List.map (type_parameter_constraints_clause env) constraints in
  let tparams = type_parameters_with_constraints tparams_opt v8 in
  let cparams =
    match primary_params_opt with
    | Some pl -> parameter_list env pl
    | None -> fb []
  in
  let primary_ctor_fields =
    cparams |> Tok.unbracket |> List.filter_map parameter_to_field
  in
  let lb, fields_stmts, rb =
    match body with
    | `Decl_list_opt_semi (dl, _semi) -> declaration_list env dl
    | `SEMI tok ->
        let t = token env tok in
        (t, [], t)
  in
  let fields =
    List.map (fun x -> G.F x) (primary_ctor_fields @ fields_stmts)
  in
  let idinfo = empty_id_info () in
  let ent = { name = EN (Id (v5, idinfo)); attrs = v1 @ v2 @ v3; tparams } in
  G.DefStmt
    ( ent,
      G.ClassDef
        {
          ckind = (Class, v4);
          cextends = v7;
          cimplements = [];
          cmixins = [];
          cparams;
          cbody = (lb, fields, rb);
        } )
  |> G.s

and enum_declaration env ((init, body) : CST.enum_declaration) =
  (* `(initializer, body)` where initializer is
     `(attrs, modifiers, "enum", id, base_list?)` and body is either
     a member list with optional `;` or a bare `;`. *)
  let attrs, mods, _enum, id, base_opt = init in
  let v1 = List.concat_map (attribute_list_ env) attrs in
  let v2 = List.map (modifier env) mods in
  let v4 = identifier env id in
  let _v5TODO =
    match base_opt with
    | Some x -> base_list env x
    | None -> []
  in
  let v6 =
    match body with
    | `Enum_member_decl_list_opt_semi (decls, _semi) ->
        enum_member_declaration_list env decls
    | `SEMI _ -> []
  in
  let idinfo = empty_id_info () in
  let ent = { name = EN (Id (v4, idinfo)); attrs = v1 @ v2; tparams = None } in
  G.DefStmt (ent, G.TypeDef { tbody = OrType v6 }) |> G.s

and delegate_declaration env ((init, constraints, _semi) : CST.delegate_declaration) =
  let attrs, mods, _delegate, ret_ty, id, tparams_opt, params_list = init in
  let v1 = List.concat_map (attribute_list_ env) attrs in
  let v2 = List.map (modifier env) mods in
  let v4 = type_pattern env ret_ty in
  let v5 = identifier env id in
  let v6 = Option.map (type_parameter_list env) tparams_opt in
  let _, params, _ = parameter_list env params_list in
  let v8 = List.map (type_parameter_constraints_clause env) constraints in
  let tparams = type_parameters_with_constraints v6 v8 in
  let func = TyFun (params, v4) |> G.t in
  let idinfo = empty_id_info () in
  let ent = { name = EN (Id (v5, idinfo)); attrs = v1 @ v2; tparams } in
  DefStmt (ent, TypeDef { tbody = NewType func }) |> G.s

and record_declaration env ((init, body) : CST.record_declaration) =
  (* C# 9+ record (record class or record struct). Lower to a ClassDef
     with the RecordClass attribute, matching how Java/Scala/Kotlin/C++
     model records. The primary-constructor parameter list (when present)
     populates cparams; the record_base populates cextends. *)
  let attrs, mods, rec_tok, kind_opt, id, tparam_choice, base_opt, constraints =
    init
  in
  let v1 = List.concat_map (attribute_list_ env) attrs in
  let v2 = List.map (modifier env) mods in
  let rec_tok = token env rec_tok in
  let record_attr = G.attr G.RecordClass rec_tok in
  let kind_attrs =
    match kind_opt with
    | Some (`Class tok) ->
        [ G.unhandled_keywordattr ("record_class", token env tok) ]
    | Some (`Struct tok) ->
        [ G.unhandled_keywordattr ("record_struct", token env tok) ]
    | None -> []
  in
  let id = identifier env id in
  let tparams_opt =
    List.find_map
      (fun x ->
        match x with
        | `Type_param_list tp -> Some (type_parameter_list env tp)
        | _ -> None)
      tparam_choice
  in
  let primary_params =
    List.find_map
      (fun x ->
        match x with
        | `Param_list pl -> Some (parameter_list env pl)
        | _ -> None)
      tparam_choice
    |> Option.value ~default:(fb [])
  in
  let primary_ctor_fields =
    primary_params |> Tok.unbracket |> List.filter_map parameter_to_field
  in
  let v7_extends =
    match base_opt with
    | None -> []
    | Some (`COLON_name_rep_COMMA_name (_colon, first, rest)) ->
        let first = TyN (name env first) |> G.t in
        let rest =
          List.map (fun (_comma, n) -> TyN (name env n) |> G.t) rest
        in
        List.map (fun t -> (t, None)) (first :: rest)
    | Some
        (`COLON_prim_cons_base_type_opt_COMMA_name_rep_COMMA_name
          (_colon, (base_name, base_args), rest_opt)) ->
        (* Primary-constructor base call `: Base(args)`. Preserve the
           argument list so downstream rules can pattern-match against it
           (e.g. checking which fields are forwarded). *)
        let base_ty = TyN (name env base_name) |> G.t in
        let base_args = argument_list env base_args in
        let rest =
          match rest_opt with
          | None -> []
          | Some (_comma, first, more) ->
              let first = TyN (name env first) |> G.t in
              let more =
                List.map (fun (_comma, n) -> TyN (name env n) |> G.t) more
              in
              first :: more
        in
        (base_ty, Some base_args) :: List.map (fun t -> (t, None)) rest
  in
  let v8 = List.map (type_parameter_constraints_clause env) constraints in
  let tparams = type_parameters_with_constraints tparams_opt v8 in
  let lb, fields_stmts, rb =
    match body with
    | `Decl_list_opt_semi (dl, _semi) -> declaration_list env dl
    | `SEMI tok ->
        let t = token env tok in
        (t, [], t)
  in
  let fields =
    List.map (fun x -> G.F x) (primary_ctor_fields @ fields_stmts)
  in
  let idinfo = empty_id_info () in
  let ent =
    {
      name = EN (Id (id, idinfo));
      attrs = (record_attr :: kind_attrs) @ v1 @ v2;
      tparams;
    }
  in
  G.DefStmt
    ( ent,
      G.ClassDef
        {
          ckind = (Class, rec_tok);
          cextends = v7_extends;
          cimplements = [];
          cmixins = [];
          cparams = primary_params;
          cbody = (lb, fields, rb);
        } )
  |> G.s

and declaration (env : env) (x : CST.declaration) : stmt =
  match x with
  | `Ellips v1 ->
      let v1 = token env v1 in
      G.ExprStmt (G.Ellipsis v1 |> G.e, sc) |> G.s
  | `Class_decl (init, body) ->
      let attrs, mods, kw, id, tparams_choice, constraints = init in
      let tparams_opt, base_opt, primary_params_opt =
        class_tparam_choice_split env tparams_choice
      in
      class_interface_struct env Class
        (attrs, mods, kw, id, tparams_opt, base_opt, primary_params_opt,
         constraints, body)
  | `Dele_decl x -> delegate_declaration env x
  | `Enum_decl x -> enum_declaration env x
  | `Inte_decl (init, body) ->
      let attrs, mods, kw, id, tparams_opt, base_opt, constraints = init in
      let tparams_opt = Option.map (type_parameter_list env) tparams_opt in
      class_interface_struct env Interface
        (attrs, mods, kw, id, tparams_opt, base_opt, None, constraints, body)
  | `Record_decl x -> record_declaration env x
  | `Struct_decl x -> struct_declaration env x
  | `Cons_decl (init, v6) ->
      let attrs, mods, name_id, params_list, ctor_init_opt = init in
      let v1 = List.concat_map (attribute_list_ env) attrs in
      let v2 = List.map (modifier env) mods in
      let v3 = identifier env name_id in
      let _, tok = v3 in
      let v4 = parameter_list env params_list in
      let v5 = Option.map (constructor_initializer env) ctor_init_opt in
      let v6 = function_body env v6 in
      (* TODO? separate ctor initializer from body in G.function_definition?*)
      let fbody =
        match v5 with
        | Some init ->
            G.FBStmt (Block (fb [ init; H2.funcbody_to_stmt v6 ]) |> G.s)
        | None -> v6
      in
      let def =
        G.FuncDef
          { fkind = (G.Method, tok); fparams = v4; frettype = None; fbody }
      in
      let ctor = KeywordAttr (Ctor, tok) in
      let attrs = (ctor :: v1) @ v2 in
      let ent = basic_entity v3 ~attrs in
      G.DefStmt (ent, def) |> G.s
  | `Conv_op_decl (v1, v2, v3, v4_list, v7, v8, v9) ->
      (* Conversion operator collapsed to
         `(attrs, mods, implicit|explicit, [`Chec|`Expl_inte_spec|`Op] list,
          type, parameter_list, body)` — the operator keyword/checked-marker/
          interface-spec slots merged into one list. *)
      let v1 = List.concat_map (attribute_list_ env) v1 in
      let v2 = List.map (modifier env) v2 in
      let v3 =
        match v3 with
        | `Impl tok -> ("op_Implicit", token env tok)
        | `Expl tok -> ("op_Explicit", token env tok)
      in
      let op_tok =
        List.find_map
          (fun item ->
            match item with
            | `Op tok -> Some (token env tok)
            | _ -> None)
          v4_list
        |> Option.value ~default:(snd v3)
      in
      (* C# 11 explicit-interface conversion operators have an interface
         specifier between `operator` and the target type, e.g.
         `static implicit IConvertible<T>.operator T(...)`. Preserve the
         specifier as an `OtherAttribute` on the entity so the original
         qualifier isn't lost in lowering. *)
      let iface_attrs =
        List.filter_map
          (fun item ->
            match item with
            | `Expl_inte_spec spec ->
                let n = explicit_interface_specifier env spec in
                Some
                  (G.OtherAttribute
                     (("ExplicitInterfaceSpec", snd v3), [ G.E (G.N n |> G.e) ]))
            | _ -> None)
          v4_list
      in
      let v7 = type_pattern env v7 in
      let v8 = parameter_list env v8 in
      let v9 = function_body env v9 in
      let idinfo = empty_id_info () in
      let ent =
        { name = EN (Id (v3, idinfo));
          attrs = v1 @ v2 @ iface_attrs;
          tparams = None }
      in
      let def =
        G.FuncDef
          {
            fkind = (G.Method, op_tok);
            fparams = v8;
            frettype = Some v7;
            fbody = v9;
          }
      in
      G.DefStmt (ent, def) |> G.s
  | `Dest_decl (v1, v2, v3, v4, v5, v6) ->
      let v1 = List.concat_map (attribute_list_ env) v1 in
      let v2 =
        match v2 with
        | Some tok -> [ KeywordAttr (Extern, token env tok) ] (* "extern" *)
        | None -> []
      in
      let v3 =
        token env v3
        (* "~" *)
      in
      let _v4TODO =
        identifier env v4
        (* identifier *)
      in
      let v5 = parameter_list env v5 in
      let v6 = function_body env v6 in
      let name = ("Finalize", v3) in
      let def =
        G.FuncDef
          { fkind = (G.Method, v3); fparams = v5; frettype = None; fbody = v6 }
      in
      let dtor = KeywordAttr (Dtor, v3) in
      let ent = basic_entity name ~attrs:((dtor :: v1) @ v2) in
      G.DefStmt (ent, def) |> G.s
  | `Event_decl (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = List.concat_map (attribute_list_ env) v1 in
      let _v2TODO = List.map (modifier env) v2 in
      let v3 =
        unhandled_keywordattr (str env v3)
        (* "event" *)
      in
      let v4 = type_pattern env v4 in
      let _v5TODO = Option.map (explicit_interface_specifier env) v5 in
      let id =
        identifier env v6
        (* identifier *)
      in
      let fname, _ftok = id in
      let v7 =
        match v7 with
        | `Acce_list x ->
            let open_br, accs, close_br = accessor_list env x in
            let funcs =
              accs
              |> List.map (fun (attrs, id, fbody) ->
                  let iname, itok = id in
                  let ent = basic_entity (iname ^ "_" ^ fname, itok) ~attrs in
                  let valparam =
                    Param
                      {
                        pname = Some ("value", fake "value");
                        ptype = Some v4;
                        pdefault = None;
                        pattrs = [];
                        pinfo = empty_id_info ();
                      }
                  in
                  let funcdef =
                    FuncDef
                      {
                        fkind = (Method, itok);
                        fparams = fb [ valparam ];
                        frettype = None;
                        fbody;
                      }
                  in
                  DefStmt (ent, funcdef) |> G.s)
            in
            (open_br, funcs, close_br)
        | `SEMI tok ->
            (* ";" *)
            let tok =
              token env tok
              (* ";" *)
            in
            fb [ todo_stmt env tok ]
      in
      let ent = basic_entity id ~attrs:(v1 @ v1 @ [ v3 ]) in
      let vardef = { vinit = None; vtype = Some v4; vtok = G.no_sc } in
      let open_br, funcs, close_br = v7 in
      Block (open_br, (DefStmt (ent, VarDef vardef) |> G.s) :: funcs, close_br)
      |> G.s
  | `Event_field_decl (v1, v2, v3, v4, v5) ->
      let v1 = List.concat_map (attribute_list_ env) v1 in
      let v2 = List.map (modifier env) v2 in
      let v3 =
        unhandled_keywordattr (str env v3)
        (* "event" *)
      in
      let attrs = (v3 :: v1) @ v2 in
      let vardefs = variable_declaration env v4 in
      let sc =
        token env v5
        (* ";" *)
      in
      var_def_stmt attrs vardefs sc
  | `Field_decl (v1, v2, v3, v4) ->
      let v1 = List.concat_map (attribute_list_ env) v1 in
      let v2 = List.map (modifier env) v2 in
      let attrs = v1 @ v2 in
      let vardefs = variable_declaration env v3 in
      let sc =
        token env v4
        (* ";" *)
      in
      var_def_stmt attrs vardefs sc
  | `Inde_decl (v1, v2, v3, v4, v5, v6, v7) -> (
      let v1 = List.concat_map (attribute_list_ env) v1 in
      let v2 = List.map (modifier env) v2 in
      let v3 = type_pattern env v3 in
      let _v4TODO = Option.map (explicit_interface_specifier env) v4 in
      let _v5 =
        token env v5
        (* "this" *)
      in
      let lbra, params, rbra = bracketed_parameter_list env v6 in
      let indexer_attrs = v1 @ v2 in
      match v7 with
      | `Acce_list x ->
          let open_br, accs, close_br = accessor_list env x in
          let funcs =
            accs
            |> List.map (fun (attrs, id, fbody) ->
                let iname, itok = id in
                match iname with
                | "get" ->
                    let ent = basic_entity ("get_Item", itok) ~attrs in
                    let funcdef =
                      FuncDef
                        {
                          fkind = (Method, itok);
                          fparams = (lbra, params, rbra);
                          frettype = Some v3;
                          fbody;
                        }
                    in
                    DefStmt (ent, funcdef) |> G.s
                | "set" ->
                    let valparam =
                      Param
                        {
                          pname = Some ("value", fake "value");
                          ptype = Some v3;
                          pdefault = None;
                          pattrs = [];
                          pinfo = empty_id_info ();
                        }
                    in
                    let ent = basic_entity ("set_Item", itok) ~attrs in
                    let funcdef =
                      FuncDef
                        {
                          fkind = (Method, itok);
                          fparams = (lbra, params @ [ valparam ], rbra);
                          frettype = None;
                          fbody;
                        }
                    in
                    DefStmt (ent, funcdef) |> G.s
                | _ -> raise Impossible)
          in
          Block (open_br, funcs, close_br) |> G.s
      | `Arrow_exp_clause_SEMI (v1, v2) ->
          let v1 = arrow_expression_clause env v1 in
          let v2 =
            token env v2
            (* ";" *)
          in
          let arrow, expr = v1 in
          let fbody = G.FBStmt (ExprStmt (expr, v2) |> G.s) in
          let ent = basic_entity ("get_Item", arrow) ~attrs:indexer_attrs in
          let funcdef =
            FuncDef
              {
                fkind = (Arrow, arrow);
                fparams = (lbra, params, rbra);
                frettype = Some v3;
                fbody;
              }
          in
          DefStmt (ent, funcdef) |> G.s)
  | `Meth_decl (v1, v2, v3, _v4TODO, v5, v6, v7, v8, v9) ->
      (*
        [Attr] static int IList<T>.MyMethod<T>(int p1) where T : Iterator { ... }
          v1     v2   v3    v4        v5    v6  v7           v8              v9
      *)
      let v1 = List.concat_map (attribute_list_ env) v1 in
      let v2 = List.map (modifier env) v2 in
      let v3 = type_pattern env v3 in
      let v5 =
        identifier env v5
        (* identifier *)
      in
      let _, tok = v5 in
      let v6 = Option.map (type_parameter_list env) v6 in
      let v7 = parameter_list env v7 in
      let v8 = List.map (type_parameter_constraints_clause env) v8 in
      let v9 = function_body env v9 in
      let tparams = type_parameters_with_constraints v6 v8 in
      let idinfo = empty_id_info () in
      let ent = { name = EN (Id (v5, idinfo)); attrs = v1 @ v2; tparams } in
      let def =
        G.FuncDef
          {
            fkind = (G.Method, tok);
            fparams = v7;
            frettype = Some v3;
            fbody = v9;
          }
      in
      G.DefStmt (ent, def) |> G.s
  | `Name_decl x -> namespace_declaration env x
  | `Op_decl (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 = List.concat_map (attribute_list_ env) v1 in
      let v2 = List.map (modifier env) v2 in
      let v3 = type_pattern env v3 in
      let _v4TODO =
        match v4 with
        | Some x -> Some (explicit_interface_specifier env x)
        | None -> None
      in
      let v5 = (* "operator" *) token env v5 in
      let _v6TODO =
        match v6 with
        | Some tok -> Some ((* "checked" *) token env tok)
        | None -> None
      in
      let v7 = overloadable_operator env v7 in
      let v8 = parameter_list env v8 in
      let v9 = function_body env v9 in
      (* TODO make clear that this is an operator overload, by using IdSpecial as the name, or adding a keyword attribute *)
      let idinfo = empty_id_info () in
      let ent =
        { name = EN (Id (v7, idinfo)); attrs = v1 @ v2; tparams = None }
      in
      let def =
        G.FuncDef
          {
            fkind = (G.Method, v5);
            fparams = v8;
            frettype = Some v3;
            fbody = v9;
          }
      in
      G.DefStmt (ent, def) |> G.s
  | `Prop_decl (v1, v2, v3, v4, v5, v6) ->
      (* [Attr] public string IFace.Field { get; public set { ... } } = "hello";
         [Attr] public string IFace.Field => "hello";
           v1     v2     v3    v4    v5      v6
         Map `Prop` as field. Map getter and setter as methods. *)
      let v1 = List.concat_map (attribute_list_ env) v1 in
      let v2 = List.map (modifier env) v2 in
      let v3 = type_pattern env v3 in
      let _v4TODO = Option.map (explicit_interface_specifier env) v4 in
      let v5 =
        identifier env v5
        (* identifier *)
      in
      let fname, _ftok = v5 in
      let accessors, vinit =
        match v6 with
        | `Acce_list_opt_EQ_exp_SEMI (v1, v2) ->
            let v1 = accessor_list env v1 in
            let v2 =
              match v2 with
              | Some (v1, v2, v3) ->
                  let _v1 =
                    token env v1
                    (* "=" *)
                  in
                  let v2 = expression env v2 in
                  let _v3 =
                    token env v3
                    (* ";" *)
                  in
                  Some v2
              | None -> None
            in
            let open_br, v1, close_br = v1 in
            let funcs =
              List.map
                (fun (attrs, id, fbody) ->
                  let iname, itok = id in
                  let has_params = iname <> "get" in
                  let has_return = iname = "get" in
                  let ent = basic_entity (iname ^ "_" ^ fname, itok) ~attrs in
                  let funcdef =
                    FuncDef
                      {
                        fkind = (Method, itok);
                        fparams =
                          fb
                            (if has_params then
                               [
                                 Param
                                   {
                                     pname = Some ("value", fake "value");
                                     ptype = Some v3;
                                     pdefault = None;
                                     pattrs = [];
                                     pinfo = empty_id_info ();
                                   };
                               ]
                             else []);
                        frettype = (if has_return then Some v3 else None);
                        (* TODO Should this be "void"? *)
                        fbody;
                      }
                  in
                  DefStmt (ent, funcdef) |> G.s)
                v1
            in
            ((open_br, funcs, close_br), v2)
        | `Arrow_exp_clause_SEMI (v1, v2) ->
            (* public int SomeProp => 3;
             * Convert it to `get_SomeProp { return 3; }`
             *)
            let v1 = arrow_expression_clause env v1 in
            let v2 =
              token env v2
              (* ";" *)
            in
            let arrow, expr = v1 in
            let ent = basic_entity ("get_" ^ fname, arrow) in
            let funcdef =
              FuncDef
                {
                  fkind = (Arrow, arrow);
                  fparams = fb [];
                  frettype = Some v3;
                  fbody = G.FBStmt (ExprStmt (expr, v2) |> G.s);
                }
            in
            let func = DefStmt (ent, funcdef) |> G.s in
            ((arrow, [ func ], v2), None)
      in
      let ent = basic_entity v5 ~attrs:(v1 @ v2) in
      let vardef = { vinit; vtype = Some v3; vtok = G.no_sc } in
      let open_br, funcs, close_br = accessors in
      Block (open_br, (DefStmt (ent, VarDef vardef) |> G.s) :: funcs, close_br)
      |> G.s
  | `Using_dire x -> using_directive env x
  | `Prep_if (_if_tok, _cond, _nl, decls, _else_opt, _endif) ->
      (* `#if`/`#endif` guarded declaration block. Walk the items so
         guarded members (e.g. `#if DEBUG public void Log() {}`) reach
         the AST. *)
      let stmts = List.map (declaration env) decls in
      G.Block (fb stmts) |> G.s
  | `Exte_decl
      (attrs, ext_tok, tparams_opt, lp, recv, rp, constraints, body) ->
      (* C# 14 extension declaration. Lower to a ClassDef carrying:
         - attrs (record-class style + the user-supplied attributes)
         - tparams + constraints
         - the receiver parameter as cparams (so rules and Naming_AST
           see the receiver as a method parameter)
         - the extension_member_declarations (methods/properties/operators)
           as cbody. Each member is re-routed through the same
           `declaration` arm that handles top-level member decls. *)
      let v1 = List.concat_map (attribute_list_ env) attrs in
      let ext_tok = token env ext_tok in
      let ext_attr = G.unhandled_keywordattr ("extension", ext_tok) in
      let lp = token env lp in
      let rp = token env rp in
      let recv_attrs, _recv_modifiers, recv_ty, recv_id = recv in
      let recv_attrs = List.concat_map (attribute_list_ env) recv_attrs in
      let recv_ty = type_pattern env recv_ty in
      let recv_param =
        G.Param
          {
            pname = Option.map (identifier env) recv_id;
            pdefault = None;
            ptype = Some recv_ty;
            pattrs = recv_attrs;
            pinfo = empty_id_info ();
          }
      in
      let tparams_opt = Option.map (type_parameter_list env) tparams_opt in
      let constraints =
        List.map (type_parameter_constraints_clause env) constraints
      in
      let tparams = type_parameters_with_constraints tparams_opt constraints in
      let lbra, _members, rbra, _semi = body in
      let lbra = token env lbra in
      let rbra = token env rbra in
      let member_stmts =
        let inner_members =
          let _lb_unused, members, _rb_unused, _ = body in
          members
        in
        List.map
          (fun (x : CST.extension_member_declaration) ->
            match x with
            | `Meth_decl m -> declaration env (`Meth_decl m)
            | `Prop_decl p -> declaration env (`Prop_decl p)
            | `Op_decl o -> declaration env (`Op_decl o))
          inner_members
      in
      let fields = List.map (fun s -> G.F s) member_stmts in
      let name =
        match recv_id with
        | Some id -> identifier env id
        | None -> ("_extension", ext_tok)
      in
      let ent =
        { name = EN (Id (name, empty_id_info ())); attrs = ext_attr :: v1; tparams }
      in
      G.DefStmt
        ( ent,
          G.ClassDef
            {
              ckind = (Class, ext_tok);
              cextends = [];
              cimplements = [];
              cmixins = [];
              cparams = (lp, [ recv_param ], rp);
              cbody = (lbra, fields, rbra);
            } )
      |> G.s

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)
let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_c_sharp.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      match compilation_unit env cst with
      | G.Pr xs -> xs
      | _ -> failwith "not a program")

let parse_pattern_aux str =
  (* ugly: coupling: see grammar.js of csharp.
   * todo: will need to adjust position information in parsing errors! *)
  let expr_str = "__SEMGREP_EXPRESSION " ^ str in
  (* If possible, we always prefer to parse a pattern as an expression than
   * as a program, since an expression is also a statement, but a statement
   * is not an expression! E.g., `Foo()` as an statement will not match
   * `if (null == Foo()) ...` whereas as an expression it does. *)
  let res = Tree_sitter_c_sharp.Parse.string expr_str in
  match res.errors with
  | [] -> res
  | _ -> Tree_sitter_c_sharp.Parse.string str

let parse_pattern str =
  H.wrap_parser
    (fun () -> parse_pattern_aux str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env = { H.file; conv = H.line_col_to_pos_pattern str; extra = () } in
      compilation_unit env cst)
