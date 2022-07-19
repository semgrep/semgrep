(* Sjoerd Langkemper
 *
 * Copyright (c) 2021 R2C
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common
module CST = Tree_sitter_c_sharp.CST
module H = Parse_tree_sitter_helpers
open AST_generic
module G = AST_generic
module H2 = AST_generic_helpers
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Csharp parser using tree-sitter-lang/semgrep-charp and converting
 * directly to pfff/h_program-lang/AST_generic.ml
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

(* less: we should check we consume all constraints *)
let type_parameters_with_constraints tparams constraints : type_parameter list =
  tparams
  |> Common.map (function
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
                 xs |> Common.partition_either (fun x -> x)
               in
               TP { tparam with tp_bounds = more_bounds @ tparam.tp_bounds }
           | None -> TP tparam))

let var_def_stmt (decls : (entity * variable_definition) list)
    (attrs : attribute list) =
  let stmts =
    Common.map
      (fun (ent, def) ->
        let ent = { ent with attrs = ent.attrs @ attrs } in
        DefStmt (ent, VarDef def) |> G.s)
      decls
  in
  stmt1 stmts

type direction = Ascending | Descending

(* TODO? integrate in AST_generic at some point? *)
and linq_query_part =
  | From of (tok * (type_ option * ident) * expr)
  | Group of (tok * expr * expr)
  | Select of (tok * expr)
  | Into of (tok * ident * linq_query_part list)
  | Join of (tok * (type_ option * ident) * expr * expr * expr * ident option)
  | Let of (tok * ident * expr)
  | OrderBy of (tok * (expr * direction) list)
  | Where of (tok * expr)

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
      let ids = Common.map (fun id -> PatId (id, empty_id_info ())) ids in
      ParamPattern (PatTuple (fake_bracket ids))

(* create lambda lambda_params -> expr *)
let create_lambda lambda_params expr =
  let fparams = [ param_from_lambda_params lambda_params ] in
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
  let fparams = [ p1; p2 ] in
  let ids =
    lambda_params @ [ ident ]
    |> Common.map (fun id -> N (Id (id, empty_id_info ())) |> G.e)
  in
  let expr = G.Container (G.Tuple, fake_bracket ids) |> G.e in
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
  let args = funcs |> Common.map (fun func -> Arg func) in
  let idinfo = empty_id_info () in
  let method_ =
    DotAccess (base_expr, tok, FN (Id ((funcname, tok), idinfo))) |> G.e
  in
  Call (method_, fake_bracket args) |> G.e

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
            Common.map
              (fun id -> N (Id (id, empty_id_info ())) |> G.e)
              lambda_params
          in
          let expr = Container (Tuple, fake_bracket (ids @ [ expr ])) |> G.e in
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
      fake_bracket [ Arg expr; Arg (L (Bool (true, fake "true")) |> G.e) ] )
  |> G.e

module List = struct
  include List

  (* not available in 4.09 *)
  let concat_map f xs = map f xs |> List.flatten
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

let _TODOparameter_modifier (env : env) (x : CST.parameter_modifier) =
  match x with
  | `Ref tok -> token env tok (* "ref" *)
  | `Out tok -> token env tok (* "out" *)
  | `This tok -> token env tok (* "this" *)
  | `In tok -> token env tok

(* "in" *)

let escape_sequence (env : env) (tok : CST.escape_sequence) =
  let s = str env tok in
  (* escape_sequence *)
  String s

let assignment_operator (env : env) (x : CST.assignment_operator) :
    operator wrap =
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
  | `LTLTEQ tok -> (LSL, token env tok) (* "<<=" *)
  | `GTGTEQ tok -> (LSR, token env tok) (* ">>=" *)
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
  G.String (str env tok)

(* verbatim_string_literal *)

let default_switch_label (env : env) ((v1, v2) : CST.default_switch_label) =
  let v1 = token env v1 (* "default" *) in
  let _v2 = token env v2 (* ":" *) in
  G.Default v1

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
    | `Type tok -> token env tok
    (* "type" *)
  in
  let v2 = token env v2 (* ":" *) in
  (v1, v2)

(* note that there's no octal literal in C# so no need for
 * H.int_of_string_c_octal_opt
 *)
let integer_literal (env : env) (tok : CST.integer_literal) =
  let s, t = str env tok in
  (* integer_literal *)
  G.Int (int_of_string_opt s, t)

let overloadable_operator (env : env) (x : CST.overloadable_operator) =
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
  | `EQEQ tok -> str env tok (* "==" *)
  | `BANGEQ tok -> str env tok (* "!=" *)
  | `GT tok -> str env tok (* ">" *)
  | `LT tok -> str env tok (* "<" *)
  | `GTEQ tok -> str env tok (* ">=" *)
  | `LTEQ tok -> str env tok

(* "<=" *)

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
  | `Ref tok -> unhandled_keywordattr (str env tok)
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
  let _v1 = token env v1 (* ":" *) in
  let v2 = token env v2 (* pattern "[^}\"]+" *) in
  v2

let interpolated_verbatim_string_text (env : env)
    (x : CST.interpolated_verbatim_string_text) =
  let x =
    match x with
    | `LCURLLCURL tok -> str env tok (* "{{" *)
    | `Inte_verb_str_text_frag tok -> str env tok (* pattern "[^{\"]+" *)
    | `DQUOTDQUOT tok -> str env tok
    (* "\"\"" *)
  in
  String x

let real_literal (env : env) (tok : CST.real_literal) =
  let s, t = str env tok (* real_literal *) in
  G.Float (float_of_string_opt s, t)

let contextual_keywords env x =
  match x with
  | `Asce x (* "ascending" *)
  | `By x (* "by" *)
  | `Desc x (* "descending" *)
  | `Equals x (* "equals" *)
  | `From x (* "from" *)
  | `Group x (* "group" *)
  | `Into x (* "into" *)
  | `Join x (* "join" *)
  | `Let x (* "let" *)
  | `On x (* "on" *)
  | `Orde x (* "orderby" *)
  | `Select x (* "select" *)
  | `Where x (* "where" *)
  | `Add x (* "add" *)
  | `Get x (* "get" *)
  | `Remove x (* "remove" *)
  | `Set x (* "set" *)
  | `Global x (* "global" *)
  | `Alias x (* "alias" *)
  | `Dyna x (* "dynamic" *)
  | `Nameof x (* "nameof" *)
  | `Notn x (* "notnull" *)
  | `Unma x (* "unmanaged" *)
  | `When x (* "when" *)
  | `Yield x (* "yield" *) ->
      str env x

let identifier (env : env) (tok : CST.identifier) : ident =
  match tok with
  | `Choice_id_tok (`Id_tok tok) -> str env tok
  | `Choice_id_tok (`Cont_keywos kw) -> contextual_keywords env kw
  | `Semg_meta tok -> str env tok

(* TODO: not sure why preprocessor_call was not generated. Because
 * was in extras?
 *)
let _preproc_directive_end (env : env) (tok : CST.preproc_directive_end) =
  token env tok

(* preproc_directive_end *)

let interpolated_string_text (env : env) (x : CST.interpolated_string_text) =
  match x with
  | `LCURLLCURL tok -> String (str env tok) (* "{{" *)
  | `Inte_str_text_frag tok ->
      String (str env tok) (* pattern "[^{\"\\\\\\n]+" *)
  | `Esc_seq tok -> escape_sequence env tok

(* escape_sequence *)

let rec variable_designation (env : env) (x : CST.variable_designation) =
  match x with
  | `Disc tok -> PatUnderscore (token env tok) (* "_" *)
  | `Paren_var_desi (v1, v2, v3) ->
      let v1 = token env v1 (* "(" *) in
      let v2 =
        match v2 with
        | Some (v1, v2) ->
            let v1 = variable_designation env v1 in
            let v2 =
              Common.map
                (fun (v1, v2) ->
                  let _v1 = token env v1 (* "," *) in
                  let v2 = variable_designation env v2 in
                  v2)
                v2
            in
            v1 :: v2
        | None -> []
      in
      let v3 = token env v3 (* ")" *) in
      PatTuple (v1, v2, v3)
  | `Id tok -> PatId (identifier env tok, empty_id_info ())

(* identifier *)

let join_into_clause (env : env) ((v1, v2) : CST.join_into_clause) =
  let _v1 = token env v1 (* "into" *) in
  let v2 = identifier env v2 (* identifier *) in
  v2

let identifier_or_global (env : env) (x : CST.identifier_or_global) =
  match x with
  | `Global tok -> str env tok (* "global" *)
  | `Id tok -> identifier env tok

(* identifier *)

let identifier_or_global_qualifier (env : env) (x : CST.identifier_or_global) :
    G.ident =
  match x with
  (* old: was QTop, but simpler to just return an ident for H2.name_of_ids *)
  | `Global tok -> str env tok (* "global" *)
  | `Id tok -> identifier env tok

(* identifier *)

let rec tuple_pattern (env : env) ((v1, v2, v3, v4) : CST.tuple_pattern) =
  let v1 = token env v1 (* "(" *) in
  let v2 = anon_choice_id_c036834 env v2 in
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = anon_choice_id_c036834 env v2 in
        v2)
      v3
  in
  let v4 = token env v4 (* ")" *) in
  PatTuple (v1, v2 :: v3, v4)

and anon_choice_id_c036834 (env : env) (x : CST.anon_choice_id_c036834) =
  match x with
  | `Id tok ->
      let id = identifier env tok (* identifier *) in
      PatId (id, empty_id_info ())
  | `Disc tok ->
      let tok = token env tok (* "_" *) in
      PatUnderscore tok
  | `Tuple_pat x -> tuple_pattern env x

let name_colon (env : env) ((v1, v2) : CST.name_colon) =
  let v1 = identifier_or_global env v1 in
  let _v2 = token env v2 (* ":" *) in
  v1

let name_equals (env : env) ((v1, v2) : CST.name_equals) =
  let v1 = identifier_or_global env v1 in
  let _v2 = token env v2 (* "=" *) in
  v1

let literal (env : env) (x : CST.literal) : literal =
  match x with
  | `Null_lit tok -> G.Null (token env tok) (* "null" *)
  | `Bool_lit x -> boolean_literal env x
  | `Char_lit (v1, v2, v3) ->
      let v1 = token env v1 (* "'" *) in
      let s, t =
        match v2 with
        | `Imm_tok_pat_684220d tok -> str env tok (* pattern "[^'\\\\]" *)
        | `Esc_seq tok -> str env tok
        (* escape_sequence *)
      in
      let v3 = token env v3 (* "'" *) in
      Char (s, PI.combine_infos v1 [ t; v3 ])
  | `Real_lit tok -> real_literal env tok (* real_literal *)
  | `Int_lit tok -> integer_literal env tok (* integer_literal *)
  | `Str_lit (v1, v2, v3) ->
      let v1 = token env v1 (* "\"" *) in
      let v2 =
        Common.map
          (fun x ->
            match x with
            | `Str_lit_frag tok -> str env tok (* pattern "[^\"\\\\\\n]+" *)
            | `Esc_seq tok -> str env tok
            (* escape_sequence *))
          v2
      in
      let v3 = token env v3 (* "\"" *) in
      let str = v2 |> Common.map fst |> String.concat "" in
      let toks = v2 |> Common.map snd in
      let toks = PI.combine_infos v1 (toks @ [ v3 ]) in
      G.String (str, toks)
  | `Verb_str_lit tok -> verbatim_string_literal env tok

(* verbatim_string_literal *)

let rec return_type (env : env) (x : CST.return_type) : type_ =
  match x with
  | `Type x -> type_constraint env x
  | `Void_kw tok -> G.ty_builtin (str env tok)

(* "void" *)
and type_pattern (env : env) (x : CST.type_pattern) = type_ env x

and variable_declaration (env : env) ((v1, v2, v3) : CST.variable_declaration) :
    (entity * variable_definition) list =
  let v1 = local_variable_type env v1 in
  let v2 = variable_declarator env v2 in
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = variable_declarator env v2 in
        v2)
      v3
  in
  let decls = v2 :: v3 in
  Common.map
    (fun (ent, vardef) -> (ent, { vinit = vardef.vinit; vtype = v1 }))
    decls

and interpolation_alignment_clause (env : env)
    ((v1, v2) : CST.interpolation_alignment_clause) =
  let _v1 = token env v1 (* "," *) in
  let v2 = expression env v2 in
  v2

and parenthesized_expression (env : env)
    ((_v1, v2, _v3) : CST.parenthesized_expression) =
  expression env v2

and postfix_unary_expression (env : env) (x : CST.postfix_unary_expression) =
  match x with
  | `Exp_PLUSPLUS (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "++" *) in
      Call
        ( IdSpecial (IncrDecr (Incr, Postfix), v2) |> G.e,
          fake_bracket [ Arg v1 ] )
      |> G.e
  | `Exp_DASHDASH (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "--" *) in
      Call
        ( IdSpecial (IncrDecr (Decr, Postfix), v2) |> G.e,
          fake_bracket [ Arg v1 ] )
      |> G.e
  | `Exp_BANG (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "!" *) in
      Call (IdSpecial (Op NotNullPostfix, v2) |> G.e, fake_bracket [ Arg v1 ])
      |> G.e

and when_clause (env : env) ((v1, v2) : CST.when_clause) =
  let _v1 = token env v1 (* "when" *) in
  let v2 = expression env v2 in
  v2

and query_continuation (env : env) (x : CST.query_continuation) =
  match x with
  | `Rectype (v1, v2, v3) ->
      let v1 = token env v1 (* "into" *) in
      let v2 = identifier env v2 (* identifier *) in
      let v3 = query_body env v3 in
      Into (v1, v2, v3)

and relational_pattern (env : env) (x : CST.relational_pattern) =
  match x with
  | `LT_exp (v1, v2) ->
      let v1 = token env v1 (* "<" *) in
      let _v2 = expression env v2 in
      todo_pat env v1
  | `LTEQ_exp (v1, v2) ->
      let v1 = token env v1 (* "<=" *) in
      let _v2 = expression env v2 in
      todo_pat env v1
  | `GT_exp (v1, v2) ->
      let v1 = token env v1 (* ">" *) in
      let _v2 = expression env v2 in
      todo_pat env v1
  | `GTEQ_exp (v1, v2) ->
      let v1 = token env v1 (* ">=" *) in
      let _v2 = expression env v2 in
      todo_pat env v1

and binary_expression (env : env) (x : CST.binary_expression) : G.expr =
  match x with
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "&&" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op And, v2) |> G.e, fake_bracket [ Arg v1; Arg v3 ])
      |> G.e
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "||" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Or, v2) |> G.e, fake_bracket [ Arg v1; Arg v3 ])
      |> G.e
  | `Exp_GTGT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ">>" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op LSR, v2) |> G.e, fake_bracket [ Arg v1; Arg v3 ])
      |> G.e
      (* TODO Is LSR the correct shift type? *)
  | `Exp_LTLT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "<<" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op LSL, v2) |> G.e, fake_bracket [ Arg v1; Arg v3 ])
      |> G.e
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "&" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op BitAnd, v2) |> G.e, fake_bracket [ Arg v1; Arg v3 ])
      |> G.e
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "^" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op BitXor, v2) |> G.e, fake_bracket [ Arg v1; Arg v3 ])
      |> G.e
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "|" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op BitOr, v2) |> G.e, fake_bracket [ Arg v1; Arg v3 ])
      |> G.e
  | `Exp_PLUS_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "+" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Plus, v2) |> G.e, fake_bracket [ Arg v1; Arg v3 ])
      |> G.e
  | `Exp_DASH_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "-" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Minus, v2) |> G.e, fake_bracket [ Arg v1; Arg v3 ])
      |> G.e
  | `Exp_STAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "*" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Mult, v2) |> G.e, fake_bracket [ Arg v1; Arg v3 ])
      |> G.e
  | `Exp_SLASH_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "/" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Div, v2) |> G.e, fake_bracket [ Arg v1; Arg v3 ])
      |> G.e
  | `Exp_PERC_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "%" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Mod, v2) |> G.e, fake_bracket [ Arg v1; Arg v3 ])
      |> G.e
  | `Exp_LT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "<" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Lt, v2) |> G.e, fake_bracket [ Arg v1; Arg v3 ])
      |> G.e
  | `Exp_LTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "<=" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op LtE, v2) |> G.e, fake_bracket [ Arg v1; Arg v3 ])
      |> G.e
  | `Exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "==" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Eq, v2) |> G.e, fake_bracket [ Arg v1; Arg v3 ])
      |> G.e
  | `Exp_BANGEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "!=" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op NotEq, v2) |> G.e, fake_bracket [ Arg v1; Arg v3 ])
      |> G.e
  | `Exp_GTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ">=" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op GtE, v2) |> G.e, fake_bracket [ Arg v1; Arg v3 ])
      |> G.e
  | `Exp_GT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ">" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Gt, v2) |> G.e, fake_bracket [ Arg v1; Arg v3 ])
      |> G.e
  | `Exp_QMARKQMARK_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "??" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Nullish, v2) |> G.e, fake_bracket [ Arg v1; Arg v3 ])
      |> G.e

and binary_pattern (env : env) (x : CST.binary_pattern) =
  match x with
  | `Pat_and_pat (v1, v2, v3) ->
      let _v1 = pattern env v1 in
      let v2 = token env v2 (* "and" *) in
      let _v3 = pattern env v3 in
      todo_pat env v2
  | `Pat_or_pat (v1, v2, v3) ->
      let _v1 = pattern env v1 in
      let v2 = token env v2 (* "or" *) in
      let _v3 = pattern env v3 in
      todo_pat env v2

and block (env : env) ((v1, v2, v3) : CST.block) : stmt =
  let v1 = token env v1 (* "{" *) in
  let v2 = Common.map (statement env) v2 in
  let v3 = token env v3 (* "}" *) in
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
  let vardef = { vinit; vtype = None } in
  (ent, vardef)

and with_initializer_expression (env : env)
    ((v1, v2) : CST.with_initializer_expression) : G.field list =
  let v1 = simple_assignment_expression env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = simple_assignment_expression env v2 in
        v2)
      v2
  in
  v1 :: v2

and prefix_unary_expression (env : env) (x : CST.prefix_unary_expression) =
  match x with
  | `BANG_exp (v1, v2) ->
      let v1 = token env v1 (* "!" *) in
      let v2 = expression env v2 in
      Call (IdSpecial (Op Not, v1) |> G.e, fake_bracket [ Arg v2 ]) |> G.e
  | `AMP_exp (v1, v2) ->
      let v1 = token env v1 (* "&" *) in
      let v2 = expression env v2 in
      Ref (v1, v2) |> G.e
  | `STAR_exp (v1, v2) ->
      let v1 = token env v1 (* "*" *) in
      let v2 = expression env v2 in
      DeRef (v1, v2) |> G.e
  | `PLUS_exp (v1, v2) ->
      let v1 = token env v1 (* "+" *) in
      let v2 = expression env v2 in
      Call (IdSpecial (Op Plus, v1) |> G.e, fake_bracket [ Arg v2 ]) |> G.e
  | `PLUSPLUS_exp (v1, v2) ->
      let v1 = token env v1 (* "++" *) in
      let v2 = expression env v2 in
      Call
        (IdSpecial (IncrDecr (Incr, Prefix), v1) |> G.e, fake_bracket [ Arg v2 ])
      |> G.e
  | `DASH_exp (v1, v2) ->
      let v1 = token env v1 (* "-" *) in
      let v2 = expression env v2 in
      Call (IdSpecial (Op Minus, v1) |> G.e, fake_bracket [ Arg v2 ]) |> G.e
  | `DASHDASH_exp (v1, v2) ->
      let v1 = token env v1 (* "--" *) in
      let v2 = expression env v2 in
      Call
        (IdSpecial (IncrDecr (Decr, Prefix), v1) |> G.e, fake_bracket [ Arg v2 ])
      |> G.e
  | `HAT_exp (v1, v2) ->
      let v1 = token env v1 (* "^" *) in
      let v2 = expression env v2 in
      new_index_from_end v1 v2
  | `TILDE_exp (v1, v2) ->
      let v1 = token env v1 (* "~" *) in
      let v2 = expression env v2 in
      Call (IdSpecial (Op BitNot, v1) |> G.e, fake_bracket [ Arg v2 ]) |> G.e

and name (env : env) (x : CST.name) : G.name =
  match x with
  | `Alias_qual_name (v1, v2, v3) ->
      let v1 = identifier_or_global_qualifier env v1 in
      let _v2 = token env v2 (* "::" *) in
      let v3 = simple_name env v3 in
      H2.name_of_ids_with_opt_typeargs [ (v1, None); v3 ]
  | `Qual_name (v1, v2, v3) ->
      let v1 = name env v1 in
      let _v2 = token env v2 (* "." *) in
      let v3 = simple_name env v3 in
      H2.add_id_opt_type_args_to_name v1 v3
  | `Simple_name x -> H2.name_of_ids_with_opt_typeargs [ simple_name env x ]

and type_parameter (env : env) ((v1, v2, v3) : CST.type_parameter) :
    G.type_parameter =
  let v1 = List.concat_map (attribute_list env) v1 in
  let v2 =
    match v2 with
    | Some x -> (
        match x with
        | `In tok -> Some (Contravariant, token env tok) (* "in" *)
        | `Out tok -> Some (Covariant, token env tok) (* "out" *))
    | None -> None
  in
  let v3 = identifier env v3 (* identifier *) in
  G.tparam_of_id v3 ~tp_attrs:v1 ~tp_variance:v2

and element_binding_expression (env : env) (x : CST.element_binding_expression)
    =
  let open_br, args, close_br = bracketed_argument_list env x in
  let exprs = Common.map H2.argument_to_expr args in
  (open_br, exprs, close_br)

and nullable_type (env : env) ((v1, v2) : CST.nullable_type) =
  let t = nullable_base_type env v1 in
  let tquestion = (* "?" *) token env v2 in
  TyQuestion (t, tquestion) |> G.t

and nullable_base_type (env : env) (x : CST.nullable_base_type) : G.type_ =
  match x with
  | `Array_type x -> array_type env x
  | `Name x ->
      let n = name env x in
      TyN n |> G.t
  | `Poin_type x -> pointer_type env x
  | `Func_poin_type x -> function_pointer_type env x
  | `Pred_type tok -> predefined_type env tok
  | `Tuple_type x -> tuple_type env x

and array_type (env : env) ((v1, v2) : CST.array_type) =
  let v1 = type_constraint env v1 in
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

and interpolated_verbatim_string_content (env : env)
    (x : CST.interpolated_verbatim_string_content) =
  match x with
  | `Inte_verb_str_text x -> L (interpolated_verbatim_string_text env x) |> G.e
  | `Interp x -> unbracket (interpolation env x)

and array_rank_specifier (env : env) ((v1, v2, v3) : CST.array_rank_specifier) =
  let v1 = token env v1 (* "[" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = Option.map (expression env) v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = Option.map (expression env) v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 = token env v3 (* "]" *) in
  (* TODO we could give each expression brackets, instead of using the same brackets for all expressions *)
  (v1, v2, v3)

and argument (env : env) ((v1, v2, v3) : CST.argument) : G.argument =
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
  | Some id -> G.ArgKwd (id, v3)

and initializer_expression (env : env)
    ((v1, v2, v3, v4) : CST.initializer_expression) : expr list G.bracket =
  let v1 = token env v1 (* "{" *) in
  let v2 = anon_opt_exp_rep_interp_alig_clause_cd88eaa env v2 in
  let _v3 = Option.map (token env) v3 (* "," *) in
  let v4 = token env v4 (* "}" *) in
  (v1, v2, v4)

and switch_expression_arm (env : env)
    ((v1, v2, v3, v4) : CST.switch_expression_arm) =
  let v1 = pattern env v1 in
  let v2 =
    match v2 with
    | Some x -> PatWhen (v1, when_clause env x)
    | None -> v1
  in
  let _v3 = token env v3 (* "=>" *) in
  let v4 = expression env v4 in
  G.case_of_pat_and_expr (v2, v4)

and tuple_expression (env : env) ((v1, v2, v3, v4) : CST.tuple_expression) =
  let v1 = token env v1 (* "(" *) in
  let v2 = argument env v2 in
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = argument env v2 in
        v2)
      v3
  in
  let v4 = token env v4 (* ")" *) in
  let exprs = Common.map H2.argument_to_expr (v2 :: v3) in
  Container (Tuple, (v1, exprs, v4)) |> G.e

and query_body (env : env) (x : CST.query_body) =
  match x with
  | `Rectype (v1, v2, v3) ->
      let v1 = Common.map (query_clause env) v1 in
      let v2 = select_or_group_clause env v2 in
      let v3 =
        match v3 with
        | Some x -> [ query_continuation env x ]
        | None -> []
      in
      v1 @ [ v2 ] @ v3

and catch_clause (env : env) ((v1, v2, v3, v4) : CST.catch_clause) =
  let v1 = token env v1 (* "catch" *) in
  let v2 =
    match v2 with
    | Some x -> catch_declaration env x
    | None -> CatchPattern (PatUnderscore (fake "_"))
  in
  let exn =
    match v3 with
    | Some x ->
        let _filterTODO = catch_filter_clause env x in
        v2
    | None -> v2
  in
  let v4 = block env v4 in
  (v1, exn, v4)

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
  | `Inte_str_text x -> L (interpolated_string_text env x) |> G.e
  | `Interp x -> unbracket (interpolation env x)

and checked_expression (env : env) (x : CST.checked_expression) =
  match x with
  | `Chec_LPAR_exp_RPAR (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "checked" *) in
      let _v2 = token env v2 (* "(" *) in
      let v3 = expression env v3 in
      let _v4 = token env v4 (* ")" *) in
      OtherExpr (("Checked", v1), [ E v3 ]) |> G.e
  | `Unch_LPAR_exp_RPAR (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "unchecked" *) in
      let _v2 = token env v2 (* "(" *) in
      let v3 = expression env v3 in
      let _v4 = token env v4 (* ")" *) in
      OtherExpr (("Unchecked", v1), [ E v3 ]) |> G.e

and pred_type env x =
  (* e.g. `int` in `int.maxValue` *)
  let id = str env x in
  N (Id (id, empty_id_info ())) |> G.e

and default_expression env (v1, v2) =
  let v1 = token env v1 (* "default" *) in
  let v2 =
    match v2 with
    | Some (v1, v2, v3) ->
        let v1 = token env v1 (* "(" *) in
        let v2 = ArgType (type_constraint env v2) in
        let v3 = token env v3 (* ")" *) in
        (v1, [ v2 ], v3)
    | None -> fake_bracket []
  in
  (* old: was a New *)
  let e = G.OtherExpr (("Default", v1), []) |> G.e in
  Call (e, v2) |> G.e

and size_of_expression env (v1, v2, v3, v4) =
  let v1 = token env v1 (* "sizeof" *) in
  let v2 = token env v2 (* "(" *) in
  let v3 = type_constraint env v3 in
  let v4 = token env v4 (* ")" *) in
  Call (IdSpecial (Sizeof, v1) |> G.e, (v2, [ ArgType v3 ], v4)) |> G.e

and type_of_expression env (v1, v2, v3, v4) =
  let v1 = token env v1 (* "typeof" *) in
  let v2 = token env v2 (* "(" *) in
  let v3 = type_constraint env v3 in
  let v4 = token env v4 (* ")" *) in
  Call (IdSpecial (Typeof, v1) |> G.e, (v2, [ ArgType v3 ], v4)) |> G.e

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
  let v1 = token env v1 (* "(" *) in
  let v2 = type_constraint env v2 in
  let _v3 = token env v3 (* ")" *) in
  let v4 = expression env v4 in
  Cast (v2, v1, v4) |> G.e

and simple_name_expression env x =
  N (H2.name_of_ids_with_opt_typeargs [ simple_name env x ]) |> G.e

and expression (env : env) (x : CST.expression) : G.expr =
  match x with
  (* semgrep: *)
  | `Member_access_ellips_exp (v1, v2, v3) ->
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
  | `Ellips v1 -> Ellipsis (token env v1) |> G.e
  | `Deep_ellips (v1, v2, v3) ->
      let v1 = token env v1 in
      let v2 = expression env v2 in
      let v3 = token env v3 in
      DeepEllipsis (v1, v2, v3) |> G.e
  | `Typed_meta (v1, v2, v3, v4) ->
      let lp = (* "(" *) token env v1 in
      let ty = type_pattern env v2 in
      let id = (* semgrep_metavariable *) str env v3 in
      let _rp = (* ")" *) token env v4 in
      TypedMetavar (id, lp, ty) |> G.e
  | `Anon_meth_exp (v1, v2, v3, v4) ->
      let _v1TODO =
        match v1 with
        | Some tok -> [ KeywordAttr (Async, token env tok) ] (* "async" *)
        | None -> []
      in
      let v2 = token env v2 (* "delegate" *) in
      let v3 =
        match v3 with
        | Some x -> parameter_list env x
        | None -> []
      in
      let v4 = block env v4 in
      Lambda
        {
          fkind = (LambdaKind, v2);
          fparams = v3;
          frettype = None;
          fbody = G.FBStmt v4;
        }
      |> G.e
  | `Anon_obj_crea_exp (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "new" *) in
      let v2 = token env v2 (* "{" *) in
      let v3 =
        match v3 with
        | Some (v1, v2) ->
            let v1 = anonymous_object_member_declarator env v1 in
            let v2 =
              Common.map
                (fun (v1, v2) ->
                  let _v1 = token env v1 (* "," *) in
                  let v2 = anonymous_object_member_declarator env v2 in
                  v2)
                v2
            in
            v1 :: v2
        | None -> []
      in
      let _v4 = Option.map (token env) v4 (* "," *) in
      let v5 = token env v5 (* "}" *) in
      AnonClass
        {
          ckind = (Class, v1);
          cextends = [];
          cimplements = [];
          cmixins = [];
          cparams = [];
          cbody = (v2, v3, v5);
        }
      |> G.e
  | `Array_crea_exp (v1, v2, v3) ->
      let v1 = token env v1 (* "new" *) in
      let v2 = array_type env v2 in
      let v3 =
        match v3 with
        | Some x -> initializer_expression env x
        | None -> fake_bracket []
      in
      let lb, _, rb = v3 in
      let args = (lb, [ Arg (G.Container (G.Tuple, v3) |> G.e) ], rb) in
      New (v1, v2, args) |> G.e
  | `As_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "as" *) in
      let v3 = type_ env v3 in
      (* TODO `as` is really a conditional cast *)
      Cast (v3, v2, v1) |> G.e
  | `Assign_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = assignment_operator env v2 in
      let v3 = expression env v3 in
      AssignOp (v1, v2, v3) |> G.e
  | `Await_exp (v1, v2) ->
      let v1 = token env v1 (* "await" *) in
      let v2 = expression env v2 in
      Await (v1, v2) |> G.e
  | `Base_exp tok ->
      let x = token env tok (* "base" *) in
      IdSpecial (Super, x) |> G.e
  | `Bin_exp x -> binary_expression env x
  | `Cast_exp x -> cast_expression env x
  | `Chec_exp x -> checked_expression env x
  | `Cond_access_exp (v1, v2, v3) ->
      (* map `a?.b` to `null == a ? null : a.b` *)
      let v1 = expression env v1 in
      let _v2 = token env v2 (* "?" *) in
      let fake_null = L (Null (fake "null")) |> G.e in
      let is_null =
        Call
          ( IdSpecial (Op Eq, fake "=") |> G.e,
            fake_bracket [ Arg fake_null; Arg v1 ] )
        |> G.e
      in
      let access =
        match v3 with
        | `Elem_bind_exp x ->
            let x = element_binding_expression env x in
            let open_br, _, close_br = x in
            ArrayAccess (v1, (open_br, Container (Tuple, x) |> G.e, close_br))
            |> G.e
        | `Member_bind_exp (x1, x2) ->
            let x1 = token env x1 (* "." *) in
            let x2 = simple_name env x2 in
            let n = H2.name_of_ids_with_opt_typeargs [ x2 ] in
            DotAccess (v1, x1, FN n) |> G.e
      in
      Conditional (is_null, fake_null, access) |> G.e
  | `Cond_exp (v1, v2, v3, v4, v5) ->
      let v1 = expression env v1 in
      let _v2 = token env v2 (* "?" *) in
      let v3 = expression env v3 in
      let _v4 = token env v4 (* ":" *) in
      let v5 = expression env v5 in
      Conditional (v1, v3, v5) |> G.e
  | `Defa_exp x -> default_expression env x
  | `Elem_access_exp (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = element_binding_expression env v2 in
      let open_br, _exprsTODO, close_br = v2 in
      (* TODO we map multidim arrays as jagged arrays when creating arrays, with as tuples here. Does that work? Should we map this as multiple nested ArrayAccess? *)
      ArrayAccess (v1, (open_br, Container (Tuple, v2) |> G.e, close_br)) |> G.e
  | `Elem_bind_exp x ->
      Container (Tuple, element_binding_expression env x) |> G.e
  | `Impl_array_crea_exp (v1, v2, v3, v4, v5) ->
      let _v1TODO = token env v1 (* "new" *) in
      let _v2 = token env v2 (* "[" *) in
      let _v3 = Common.map (token env) (* "," *) v3 in
      let _v4 = token env v4 (* "]" *) in
      let v5 = initializer_expression env v5 in
      Container (Array, v5) |> G.e
  | `Impl_obj_crea_exp (v1, v2, v3) ->
      let v1 = token env v1 (* "new" *) in
      let v2 = argument_list env v2 in
      let v3 =
        match v3 with
        | Some x -> initializer_expression env x
        | None -> fake_bracket []
      in
      let lp, v2', rp = v2 in
      let args = (lp, v2' @ [ Arg (Container (Tuple, v3) |> G.e) ], rp) in
      (* old: was New *)
      let e = G.OtherExpr (("NewNoType", v1), []) |> G.e in
      Call (e, args) |> G.e
  | `Impl_stack_alloc_array_crea_exp (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "stackalloc" *) in
      let _v2 = token env v2 (* "[" *) in
      let _v3 = token env v3 (* "]" *) in
      let _v4 = initializer_expression env v4 in
      todo_expr env v1
  | `Init_exp x -> Container (Tuple, initializer_expression env x) |> G.e
  | `Inte_str_exp x -> interpolated_string_expression env x
  | `Invo_exp x -> invocation_expression env x
  | `Is_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "is" *) in
      let v3 = type_ env v3 in
      Call
        (IdSpecial (Instanceof, v2) |> G.e, fake_bracket [ Arg v1; ArgType v3 ])
      |> G.e
  | `Is_pat_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let _v2 = token env v2 (* "is" *) in
      let v3 = pattern env v3 in
      LetPattern (v3, v1) |> G.e
  | `Lambda_exp (v1, _vTODO, v2, v3, v4) ->
      let _v1TODO =
        match v1 with
        | Some tok -> [ KeywordAttr (Async, token env tok) ] (* "async" *)
        | None -> []
      in
      let v2 =
        match v2 with
        | `Param_list x -> parameter_list env x
        | `Id tok ->
            let id = identifier env tok in
            let p = param_of_id id in
            [ Param p ]
        (* identifier *)
      in
      let v3 = token env v3 (* "=>" *) in
      let v4 =
        match v4 with
        | `Blk x -> G.FBStmt (block env x)
        | `Exp x -> G.FBExpr (expression env x)
      in
      Lambda { fkind = (Arrow, v3); fparams = v2; frettype = None; fbody = v4 }
      |> G.e
  | `Make_ref_exp (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "__makeref" *) in
      let _v2 = token env v2 (* "(" *) in
      let v3 = expression env v3 in
      let _v4 = token env v4 (* ")" *) in
      Ref (v1, v3) |> G.e
  | `Member_access_exp x -> member_access_expression env x
  | `Obj_crea_exp (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "new" *) in
      let v2 = type_constraint env v2 in
      let v3 =
        match v3 with
        | Some x -> argument_list env x
        | None -> fake_bracket []
      in
      let v4 =
        match v4 with
        | Some x -> initializer_expression env x
        | None -> fake_bracket []
      in
      let lp, v3', rp = v3 in
      let args = (lp, v3' @ [ Arg (Container (Tuple, v4) |> G.e) ], rp) in
      New (v1, v2, args) |> G.e
  | `Paren_exp x -> parenthesized_expression env x
  | `Post_un_exp x -> postfix_unary_expression env x
  | `Prefix_un_exp x -> prefix_unary_expression env x
  | `Query_exp (v1, v2) ->
      let v1 = from_clause env v1 in
      let v2 = query_body env v2 in
      linq_to_expr v1 v2
  | `Range_exp (v1, v2, v3) ->
      let fake_zero = L (Int (Some 0, fake "0")) |> G.e in
      let v1 =
        match v1 with
        | Some x -> expression env x
        | None -> fake_zero
      in
      let v2 = token env v2 (* ".." *) in
      let v3 =
        match v3 with
        | Some x -> expression env x
        | None -> new_index_from_end v2 fake_zero
      in
      Call (IdSpecial (Op Range, v2) |> G.e, fake_bracket [ Arg v1; Arg v3 ])
      |> G.e
  | `Ref_exp (v1, v2) ->
      let v1 = token env v1 (* "ref" *) in
      let v2 = expression env v2 in
      G.Ref (v1, v2) |> G.e
  | `Ref_type_exp (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "__reftype" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 = expression env v3 in
      let v4 = token env v4 (* ")" *) in
      Call
        ( IdSpecial (Typeof, v1) |> G.e,
          (v2, [ Arg (DeRef (v1, v3) |> G.e) ], v4) )
      |> G.e
  | `Ref_value_exp (v1, v2, v3, v4, v5, v6) ->
      let v1 = token env v1 (* "__refvalue" *) in
      let _v2 = token env v2 (* "(" *) in
      let v3 = expression env v3 in
      let _v4 = token env v4 (* "," *) in
      let _v5 = type_constraint env v5 in
      let _v6 = token env v6 (* ")" *) in
      DeRef (v1, v3) |> G.e
  | `Size_of_exp x -> size_of_expression env x
  | `Stack_alloc_array_crea_exp (v1, v2, v3) ->
      let v1 = token env v1 (* "stackalloc" *) in
      let _v2 = array_type env v2 in
      let _v3 =
        match v3 with
        | Some x -> initializer_expression env x
        | None -> fake_bracket [ todo_expr env v1 ]
      in
      todo_expr env v1
  | `Switch_exp (v1, v2, v3, v4, _vTODO, v5) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "switch" *) in
      let _v3 = token env v3 (* "{" *) in
      let v4 =
        match v4 with
        | Some (v1, v2) ->
            let v1 = switch_expression_arm env v1 in
            let v2 =
              Common.map
                (fun (v1, v2) ->
                  let _v1 = token env v1 (* "," *) in
                  let v2 = switch_expression_arm env v2 in
                  v2)
                v2
            in
            v1 :: v2
        | None -> []
      in
      let _v5 = token env v5 (* "}" *) in
      (* TODO: use Switch instead? *)
      let st = G.Switch (v2, Some (G.Cond v1), v4) |> G.s in
      G.stmt_to_expr st
  | `This_exp tok ->
      let t = token env tok (* "this" *) in
      IdSpecial (This, t) |> G.e
  | `Throw_exp (v1, v2) ->
      let v1 = token env v1 (* "throw" *) in
      let v2 = expression env v2 in
      let throw = Throw (v1, v2, sc) |> G.s in
      G.stmt_to_expr throw
  | `Tuple_exp x -> tuple_expression env x
  | `Type_of_exp x -> type_of_expression env x
  | `With_exp (v1, v2, v3, v4, v5) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "with" *) in
      let v3 = token env v3 (* "{" *) in
      let v4 =
        match v4 with
        | Some x -> with_initializer_expression env x
        | None -> []
      in
      let v5 = token env v5 (* "}" *) in
      let with_fields = G.Record (v3, v4, v5) |> G.e in
      (* THINK:
       * - with-expressions may deserve first-class support in Generic AST
       * - record patterns perhaps should match with-expressions
       *)
      G.OtherExpr (("RecordWith", v2), [ G.E v1; G.E with_fields ]) |> G.e
  | `Simple_name x -> simple_name_expression env x
  | `Lit x ->
      let x = literal env x in
      G.L x |> G.e

and simple_assignment_expression (env : env)
    ((v1, v2, v3) : CST.simple_assignment_expression) : G.field =
  let v1 = identifier env v1 in
  let _v2 = token env v2 (* "=" *) in
  let v3 = expression env v3 in
  G.basic_field v1 (Some v3) None

and simple_name (env : env) (x : CST.simple_name) :
    G.ident * G.type_arguments option =
  match x with
  | `Gene_name (v1, v2) ->
      let v1 = identifier env v1 (* identifier *) in
      let v2 = type_argument_list env v2 in
      (v1, Some v2)
  | `Choice_global x -> (identifier_or_global env x, None)

and switch_body (env : env) ((v1, v2, v3) : CST.switch_body) :
    case_and_body list =
  let _v1 = token env v1 (* "{" *) in
  let v2 = Common.map (switch_section env) v2 in
  let _v3 = token env v3 (* "}" *) in
  v2

and anon_choice_param_ce11a32 (env : env) (x : CST.anon_choice_param_ce11a32) =
  match x with
  | `Param x -> parameter env x
  | `Param_array (v1, v2, v3, v4) ->
      let v1 = List.concat_map (attribute_list env) v1 in
      let v2 = token env v2 (* "params" *) in
      let v3 =
        match v3 with
        | `Array_type v3 -> array_type env v3
        | `Null_type v3 -> nullable_type env v3
      in
      let v4 = identifier env v4 (* identifier *) in
      ParamRest
        ( v2,
          {
            pname = Some v4;
            ptype = Some v3;
            pdefault = None;
            pattrs = v1;
            pinfo = empty_id_info ();
          } )

and anon_opt_exp_rep_interp_alig_clause_cd88eaa (env : env)
    (opt : CST.anon_opt_exp_rep_interp_alig_clause_cd88eaa) : expr list =
  match opt with
  | Some (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = Common.map (interpolation_alignment_clause env) v2 in
      v1 :: v2
  | None -> []

and type_parameter_list (env : env) ((v1, v2, v3, v4) : CST.type_parameter_list)
    : G.type_parameter list =
  let _v1 = token env v1 (* "<" *) in
  let v2 = type_parameter env v2 in
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = type_parameter env v2 in
        v2)
      v3
  in
  let _v4 = token env v4 (* ">" *) in
  v2 :: v3

and type_parameter_constraint (env : env) (x : CST.type_parameter_constraint) :
    (G.todo_kind, type_) Common.either =
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
      let v1 = token env v1 (* "new" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 = token env v3 (* ")" *) in
      let tok = PI.combine_infos v1 [ v2; v3 ] in
      Left ("HasConstructor", tok)
  | `Type_cons x -> Right (type_constraint env x)

and type_constraint (env : env) (x : CST.type_constraint) : type_ =
  (* can't be `var` *)
  type_ env x

and local_variable_type (env : env) (x : CST.type_constraint) : type_ option =
  match x with
  | `Impl_type _tok -> None (* "var" *)
  | x -> Some (type_ env x)

and expr_statement (env : env) (x : CST.expression_statement) : stmt =
  match x with
  | `Exp_SEMI (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ";" *) in
      G.ExprStmt (v1, v2) |> G.s
  | `Ellips_SEMI (v1, v2) ->
      let v1 = token env v1 in
      let v2 = token env v2 in
      G.ExprStmt (G.Ellipsis v1 |> G.e, v2) |> G.s
  | `Ellips v1 ->
      let v1 = token env v1 in
      let v2 = G.sc in
      G.ExprStmt (G.Ellipsis v1 |> G.e, v2) |> G.s

and statement (env : env) (x : CST.statement) =
  match x with
  | `Blk x -> block env x
  | `Brk_stmt (v1, v2) ->
      let v1 = token env v1 (* "break" *) in
      let v2 = token env v2 (* ";" *) in
      G.Break (v1, G.LNone, v2) |> G.s
  | `Chec_stmt (v1, v2) ->
      let v1 =
        match v1 with
        | `Chec _tok -> OSWS_CheckedBlock (* "checked" *)
        | `Unch _tok -> OSWS_UncheckedBlock
        (* "unchecked" *)
      in
      let v2 = block env v2 in
      OtherStmtWithStmt (v1, [], v2) |> G.s
  | `Cont_stmt (v1, v2) ->
      let v1 = token env v1 (* "continue" *) in
      let v2 = token env v2 (* ";" *) in
      Continue (v1, LNone, v2) |> G.s
  | `Do_stmt (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = token env v1 (* "do" *) in
      let v2 = statement env v2 in
      let _v3 = token env v3 (* "while" *) in
      let _v4 = token env v4 (* "(" *) in
      let v5 = expression env v5 in
      let _v6 = token env v6 (* ")" *) in
      let _v7 = token env v7 (* ";" *) in
      DoWhile (v1, v2, v5) |> G.s
  | `Empty_stmt tok ->
      let v1 = token env tok (* ";" *) in
      Block (v1, [], v1) |> G.s
  (* Can we have the same token as start and end of block? *)
  | `Exp_stmt v1 -> expr_statement env v1
  | `Fixed_stmt (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "fixed" *) in
      let _v2 = token env v2 (* "(" *) in
      let _v3 = variable_declaration env v3 in
      let _v4 = token env v4 (* ")" *) in
      let _v5 = statement env v5 in
      todo_stmt env v1
  | `For_each_stmt (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 = Option.map (token env) v1 (* "await" *) in
      let v2 = token env v2 (* "foreach" *) in
      let _v3 = token env v3 (* "(" *) in
      let v4 =
        match v4 with
        | `Type_choice_id (v1, v2) -> (
            let v2 =
              match v2 with
              | `Id x ->
                  let x = identifier env x in
                  PatId (x, empty_id_info ())
              | `Tuple_pat x -> tuple_pattern env x
            in
            let v1 = local_variable_type env v1 in
            match v1 with
            | Some t -> PatTyped (v2, t)
            | None -> v2)
        | `Exp x -> H2.expr_to_pattern (expression env x)
      in
      let v5 = token env v5 (* "in" *) in
      let v6 = expression env v6 in
      let v6 =
        match v1 with
        | Some tok -> Await (tok, v6) (* "await" *) |> G.e
        | None -> v6
      in
      let _v7 = token env v7 (* ")" *) in
      let v8 = statement env v8 in
      For (v2, ForEach (v4, v5, v6), v8) |> G.s
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 = token env v1 (* "for" *) in
      let _v2 = token env v2 (* "(" *) in
      let v3 =
        match v3 with
        | Some x -> (
            match x with
            | `Var_decl x ->
                Common.map
                  (fun (e, v) -> ForInitVar (e, v))
                  (variable_declaration env x)
            | `Exp_rep_COMMA_exp (v1, v2) ->
                let v1 = expression env v1 in
                let v2 = Common.map (interpolation_alignment_clause env) v2 in
                let exprs = v1 :: v2 in
                Common.map (fun e -> ForInitExpr e) exprs)
        | None -> []
      in
      let _v4 = token env v4 (* ";" *) in
      let v5 = Option.map (expression env) v5 in
      let v6 = token env v6 (* ";" *) in
      let v7 = anon_opt_exp_rep_interp_alig_clause_cd88eaa env v7 in
      let v8 = token env v8 (* ")" *) in
      let v9 = statement env v9 in
      let next =
        match v7 with
        | [] -> None
        | [ e ] -> Some e
        | _exprs -> Some (Container (Tuple, (v6, v7, v8)) |> G.e)
      in
      let for_header = ForClassic (v3, v5, next) in
      For (v1, for_header, v9) |> G.s
  | `Goto_stmt (v1, v2, v3) -> (
      let v1 = token env v1 (* "goto" *) in
      let v3 = token env v3 (* ";" *) in
      match v2 with
      | `Id tok ->
          let label = identifier env tok (* identifier *) in
          Goto (v1, label, v3) |> G.s
      | `Case_exp (v1, v2) ->
          let v1 = token env v1 (* "case" *) in
          let _v2 = expression env v2 in
          todo_stmt env v1
      | `Defa tok ->
          let tok = token env tok (* "default" *) in
          todo_stmt env tok)
  | `If_stmt (v1, v2, v3, v4, v5, v6) ->
      let v1 = token env v1 (* "if" *) in
      let _v2 = token env v2 (* "(" *) in
      let v3 = expression env v3 in
      let _v4 = token env v4 (* ")" *) in
      let v5 = statement env v5 in
      let v6 =
        match v6 with
        | Some (v1, v2) ->
            let _v1 = token env v1 (* "else" *) in
            let v2 = statement env v2 in
            Some v2
        | None -> None
      in
      G.If (v1, G.Cond v3, v5, v6) |> G.s
  | `Labe_stmt (v1, v2, v3) ->
      let v1 = identifier env v1 (* identifier *) in
      let _v2 = token env v2 (* ":" *) in
      let v3 = statement env v3 in
      Label (v1, v3) |> G.s
  | `Local_decl_stmt (v1, v2, v3, v4, v5) ->
      let _V1TODO = Option.map (token env) v1 (* "await" *) in
      let _V2TODO = Option.map (token env) v2 (* "using" *) in
      let v3 = Common.map (modifier env) v3 in
      let v4 = variable_declaration env v4 in
      let _v5 = token env v5 (* ";" *) in
      var_def_stmt v4 v3
  | `Local_func_stmt (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 = List.concat_map (attribute_list env) v1 in
      let v2 = Common.map (modifier env) v2 in
      let v3 = return_type env v3 in
      let v4 = identifier env v4 (* identifier *) in
      let _, tok = v4 in
      let v5 =
        match v5 with
        | Some x -> type_parameter_list env x
        | None -> []
      in
      let v6 = parameter_list env v6 in
      let v7 = Common.map (type_parameter_constraints_clause env) v7 in
      let v8 = function_body env v8 in
      let tparams = type_parameters_with_constraints v5 v7 in
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
      let _v1 = token env v1 (* "lock" *) in
      let _v2 = token env v2 (* "(" *) in
      let v3 = expression env v3 in
      let _v4 = token env v4 (* ")" *) in
      let v5 = statement env v5 in
      OtherStmtWithStmt (OSWS_Sync, [ E v3 ], v5) |> G.s
  | `Ret_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "return" *) in
      let v2 = Option.map (expression env) v2 in
      let v3 = token env v3 (* ";" *) in
      Return (v1, v2, v3) |> G.s
  | `Switch_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "switch" *) in
      let v2 =
        match v2 with
        | `LPAR_exp_RPAR v2 -> parenthesized_expression env v2
        | `Tuple_exp v2 -> tuple_expression env v2
      in
      let v3 = switch_body env v3 in
      G.Switch (v1, Some (G.Cond v2), v3) |> G.s
  | `Throw_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "throw" *) in
      let v2 = Option.map (expression env) v2 in
      let v3 = token env v3 (* ";" *) in
      (match v2 with
      | Some expr -> Throw (v1, expr, v3)
      | None -> OtherStmt (OS_ThrowNothing, [ Tk v1; Tk v3 ]))
      |> G.s
  | `Try_stmt (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "try" *) in
      let v2 = block env v2 in
      let v3 = Common.map (catch_clause env) v3 in
      let v4 = Option.map (finally_clause env) v4 in
      Try (v1, v2, v3, v4) |> G.s
  | `Unsafe_stmt (v1, v2) ->
      let _v1 = token env v1 (* "unsafe" *) in
      let v2 = block env v2 in
      OtherStmtWithStmt (OSWS_UnsafeBlock, [], v2) |> G.s
  | `Using_stmt (v1, v2, v3, v4, v5, v6) ->
      let _v1TODO = Option.map (token env) v1 (* "await" *) in
      let v2 = token env v2 (* "using" *) in
      let _v3 = token env v3 (* "(" *) in
      let v4 =
        match v4 with
        | `Var_decl x ->
            let v4 = variable_declaration env x in
            var_def_stmt v4 []
        | `Exp x ->
            let expr = expression env x in
            ExprStmt (expr, sc) |> G.s
      in
      let _v5 = token env v5 (* ")" *) in
      let v6 = statement env v6 in
      WithUsingResource (v2, v4, v6) |> G.s
  | `While_stmt (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "while" *) in
      let _v2 = token env v2 (* "(" *) in
      let v3 = expression env v3 in
      let _v4 = token env v4 (* ")" *) in
      let v5 = statement env v5 in
      While (v1, G.Cond v3, v5) |> G.s
  | `Yield_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "yield" *) in
      let v2 =
        match v2 with
        | `Ret_exp (v1, v2) ->
            let _v1 = token env v1 (* "return" *) in
            let v2 = expression env v2 in
            Some v2
        | `Brk _tok -> None
        (* "break" *)
      in
      let v3 = token env v3 (* ";" *) in
      ExprStmt (Yield (v1, v2, false) |> G.e, v3) |> G.s

and interpolated_string_expression (env : env)
    (x : CST.interpolated_string_expression) =
  let x =
    match x with
    | `DOLLARDQUOT_rep_inte_str_content_DQUOT (v1, v2, v3) ->
        let v1 = token env v1 (* "$\"" *) in
        let v2 = Common.map (interpolated_string_content env) v2 in
        let v3 = token env v3 (* "\"" *) in
        (v1, v2, v3)
    | `ATDOLLARDQUOT_rep_inte_verb_str_content_DQUOT (v1, v2, v3)
    | `DOLLARATDQUOT_rep_inte_verb_str_content_DQUOT (v1, v2, v3) ->
        let v1 = token env v1 (* "$@\"" or "@$\"" *) in
        let v2 = Common.map (interpolated_verbatim_string_content env) v2 in
        let v3 = token env v3 (* "\"" *) in
        (v1, v2, v3)
  in
  let v1, v2, _v3 = x in
  let args = fake_bracket (Common.map (fun e -> Arg e) v2) in
  (* TODO should we use FString here instead of InterpolatedConcat? *)
  Call (IdSpecial (ConcatString InterpolatedConcat, v1) |> G.e, args) |> G.e

and tuple_element (env : env) ((v1, v2) : CST.tuple_element) =
  let v1 = type_constraint env v1 in
  let _v2TODO = Option.map (identifier env) v2 (* identifier *) in
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
  | `Size_of_exp x -> size_of_expression env x
  | `Tuple_exp x -> tuple_expression env x
  | `Type_of_exp x -> type_of_expression env x
  | `Member_access_exp x -> member_access_expression env x
  | `Invo_exp x -> invocation_expression env x
  | `Cast_exp x -> cast_expression env x
  | `Simple_name x -> simple_name_expression env x
  | `Lit x ->
      let lit = literal env x in
      G.L lit |> G.e

and catch_declaration (env : env) ((v1, v2, v3, v4) : CST.catch_declaration) :
    G.catch_exn =
  let _v1 = token env v1 (* "(" *) in
  let v2 = type_constraint env v2 in
  let v3 = Option.map (identifier env) v3 (* identifier *) in
  let _v4 = token env v4 (* ")" *) in
  CatchParam (G.param_of_type v2 ~pname:v3)

and case_pattern_switch_label (env : env)
    ((v1, v2, v3, v4) : CST.case_pattern_switch_label) =
  let v1 = token env v1 (* "case" *) in
  let v2 = pattern env v2 in
  let v3 =
    match v3 with
    | Some x -> PatWhen (v2, when_clause env x)
    | None -> v2
  in
  let _v4 = token env v4 (* ":" *) in
  G.Case (v1, v3)

and query_clause (env : env) (x : CST.query_clause) =
  match x with
  | `From_clause x -> from_clause env x
  | `Join_clause (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) ->
      let v1 = token env v1 (* "join" *) in
      let v2 = Option.map (type_constraint env) v2 in
      let v3 = identifier env v3 (* identifier *) in
      let _v4 = token env v4 (* "in" *) in
      let v5 = expression env v5 in
      let _v6 = token env v6 (* "on" *) in
      let v7 = expression env v7 in
      let _v8 = token env v8 (* "equals" *) in
      let v9 = expression env v9 in
      let v10 = Option.map (join_into_clause env) v10 in
      Join (v1, (v2, v3), v5, v7, v9, v10)
  | `Let_clause (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "let" *) in
      let v2 = identifier env v2 (* identifier *) in
      let _v3 = token env v3 (* "=" *) in
      let v4 = expression env v4 in
      Let (v1, v2, v4)
  | `Order_by_clause (v1, v2, v3) ->
      let v1 = token env v1 (* "orderby" *) in
      let v2 = ordering env v2 in
      let v3 =
        Common.map
          (fun (v1, v2) ->
            let _v1 = token env v1 (* "," *) in
            let v2 = ordering env v2 in
            v2)
          v3
      in
      OrderBy (v1, v2 :: v3)
  | `Where_clause (v1, v2) ->
      let v1 = token env v1 (* "where" *) in
      let v2 = expression env v2 in
      Where (v1, v2)

and arrow_expression_clause (env : env) ((v1, v2) : CST.arrow_expression_clause)
    =
  let v1 = token env v1 (* "=>" *) in
  let v2 = expression env v2 in
  (v1, v2)

and attribute_argument (env : env) ((v1, v2) : CST.attribute_argument) =
  let v1 =
    match v1 with
    | Some x -> (
        match x with
        | `Name_equals x -> Some (name_equals env x)
        | `Name_colon x -> Some (name_colon env x))
    | None -> None
  in
  let v2 = expression env v2 in
  match v1 with
  | Some name -> ArgKwd (name, v2)
  | None -> Arg v2

and catch_filter_clause (env : env) ((v1, v2, v3, v4) : CST.catch_filter_clause)
    =
  let _v1 = token env v1 (* "when" *) in
  let _v2 = token env v2 (* "(" *) in
  let v3 = expression env v3 in
  let _v4 = token env v4 (* ")" *) in
  v3

and formal_parameter_list (env : env) ((v1, v2) : CST.formal_parameter_list) =
  let v1 = anon_choice_param_ce11a32 env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = anon_choice_param_ce11a32 env v2 in
        v2)
      v2
  in
  v1 :: v2

and equals_value_clause (env : env) ((v1, v2) : CST.equals_value_clause) : expr
    =
  let _v1 = token env v1 (* "=" *) in
  let v2 = expression env v2 in
  v2

and case_switch_label (env : env) ((v1, v2, v3) : CST.case_switch_label) =
  let v1 = token env v1 (* "case" *) in
  let v2 = expression env v2 in
  let _v3 = token env v3 (* ":" *) in
  G.CaseEqualExpr (v1, v2)

and switch_section (env : env) ((v1, v2) : CST.switch_section) : case_and_body =
  let v1 =
    Common.map
      (fun x ->
        match x with
        | `Case_switch_label x -> case_switch_label env x
        | `Case_pat_switch_label x -> case_pattern_switch_label env x
        | `Defa_switch_label x -> default_switch_label env x)
      v1
  in
  let v2 = Common.map (statement env) v2 in
  (* TODO: we convert list of statements to a block with fake brackets. Does this make sense? *)
  CasesAndBody (v1, stmt1 v2)

and attribute_list (env : env) ((v1, v2, v3, v4, v5, v6) : CST.attribute_list) :
    attribute list =
  (* TODO: Handle unused tokens. *)
  let _v1 = token env v1 (* "[" *) in
  let _v2 = Option.map (attribute_target_specifier env) v2 in
  let v3 = attribute env v3 in
  let v4 =
    Common.map
      (fun (x, y) ->
        token env x (* "," *) |> ignore;
        attribute env y)
      v4
  in
  let _v5 = Option.map (token env) v5 (* "," *) in
  let _v6 = token env v6 (* "]" *) in
  v3 :: v4

and bracketed_argument_list (env : env)
    ((v1, v2, v3, v4) : CST.bracketed_argument_list) =
  let v1 = token env v1 (* "[" *) in
  let v2 = argument env v2 in
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = argument env v2 in
        v2)
      v3
  in
  let v4 = token env v4 (* "]" *) in
  (v1, v2 :: v3, v4)

and pattern (env : env) (x : CST.pattern) : G.pattern =
  match x with
  | `Cst_pat x -> constant_pattern env x
  | `Decl_pat (v1, v2) ->
      let v1 = type_constraint env v1 in
      let v2 = variable_designation env v2 in
      PatTyped (v2, v1)
  | `Disc tok -> PatUnderscore (token env tok) (* "_" *)
  | `Recu_pat (v1, v2, v3) -> recursive_pattern env (v1, v2, v3)
  | `Var_pat (v1, v2) ->
      let _v1 = token env v1 (* "var" *) in
      let v2 = variable_designation env v2 in
      v2
  | `Nega_pat (v1, v2) ->
      let v1 = token env v1 (* "not" *) in
      let _v2 = pattern env v2 in
      todo_pat env v1
  | `Paren_pat (v1, v2, v3) ->
      let v1 = token env v1 (* "(" *) in
      let _v2 = pattern env v2 in
      let _v3 = token env v3 (* ")" *) in
      todo_pat env v1
  | `Rela_pat x -> relational_pattern env x
  | `Bin_pat x -> binary_pattern env x
  | `Type_pat x ->
      let _xTODO = type_pattern env x in
      todo_pat env (fake "TODO_type_pattern")

and recursive_pattern env (v1, v2, v3) =
  let pat =
    match v2 with
    | `Posi_pat_clause_opt_prop_pat_clause (v1, v2) ->
        let v1 = positional_pattern_clause env v1 in
        let _v2TODO =
          match v2 with
          | None -> None
          | Some _x -> Some (todo_pat env (fake "TODO_recursive_pattern"))
        in
        v1
    | `Prop_pat_clause x -> property_pattern_clause env x
  in
  let pat =
    match v1 with
    | None -> pat
    | Some x ->
        let t = type_ env x in
        PatTyped (pat, t)
  in
  match v3 with
  | None -> pat
  | Some x ->
      let _v = variable_designation env x in
      todo_pat env (fake "TODO_recursive_pattern")

and positional_pattern_clause (env : env)
    ((v1, v2, v3) : CST.positional_pattern_clause) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, _comma, v3, v4) ->
        let v1 = subpattern env v1 in
        let v3 = subpattern env v3 in
        let v4 = Common.map (fun (_comma, v2) -> subpattern env v2) v4 in
        v1 :: v3 :: v4
    | None -> []
  in
  let v3 = token env v3 (* ")" *) in
  PatTuple (v1, v2, v3)

and subpattern (env : env) ((v1, v2) : CST.subpattern) =
  let _V1TODO = Option.map (expression_colon env) v1 in
  let v2 = pattern env v2 in
  v2

(* similar to name_colon *)
and expression_colon (env : env) ((v1, v2) : CST.expression_colon) =
  let t = expression env v1 in
  let tcolon = (* ":" *) token env v2 in
  (t, tcolon)

and property_pattern_clause (env : env)
    ((v1, v2, _v3comma, v4) : CST.property_pattern_clause) =
  let v1 = token env v1 (* "{" *) in
  let _v2 =
    match v2 with
    | Some _x -> [ todo_pat env v1 ]
    | None -> []
  in
  let _v4 = token env v4 (* "}" *) in
  todo_pat env v1

and anonymous_object_member_declarator (env : env)
    (x : CST.anonymous_object_member_declarator) =
  match x with
  | `Name_equals_exp (v1, v2) ->
      let v1 = name_equals env v1 in
      let v2 = expression env v2 in
      basic_field v1 (Some v2) None
  | `Exp x ->
      let expr = expression env x in
      F (exprstmt expr)

and function_body (env : env) (x : CST.function_body) : G.function_body =
  match x with
  | `Blk x -> G.FBStmt (block env x)
  | `Arrow_exp_clause_SEMI (v1, v2) ->
      let v1 = arrow_expression_clause env v1 in
      let v2 = token env v2 (* ";" *) in
      let _arrow, expr = v1 in
      G.FBStmt (G.ExprStmt (expr, v2) |> G.s)
  | `SEMI tok ->
      let t = token env tok (* ";" *) in
      G.FBDecl t

and finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 = token env v1 (* "finally" *) in
  let v2 = block env v2 in
  (v1, v2)

and parameter (env : env) (v1 : CST.parameter) : G.parameter =
  match v1 with
  | `Rep_attr_list_opt_choice_ref_opt_type_id_opt_equals_value_clause v1 ->
      explicit_parameter env v1
  | `Ellips v1 -> ParamEllipsis (token env v1)

and explicit_parameter (env : env) (v1, _v2param_modifier_TODO, v3, v4, v5) =
  (*
    [FromBody] ref string param1 = "default"
        v1     v2   v3     v4      v5
    TODO: add v2 as a keyword attribute? Pass v2 to parameter_modifier.
  *)
  let v1 = List.concat_map (attribute_list env) v1 in
  let v3 = Option.map (type_constraint env) v3 in
  let v4 = identifier env v4 (* identifier *) in
  let v5 = Option.map (equals_value_clause env) v5 in
  Param
    {
      pname = Some v4;
      ptype = v3;
      pdefault = v5;
      pattrs = v1;
      pinfo = empty_id_info ();
    }

and from_clause (env : env) ((v1, v2, v3, v4, v5) : CST.from_clause) :
    linq_query_part =
  let v1 = token env v1 (* "from" *) in
  let v2 = Option.map (type_constraint env) v2 in
  let v3 = identifier env v3 (* identifier *) in
  let _v4 = token env v4 (* "in" *) in
  let v5 = expression env v5 in
  From (v1, (v2, v3), v5)

and attribute (env : env) ((v1, v2) : CST.attribute) =
  let v1 = name env v1 in
  let v2 =
    match v2 with
    | Some x -> attribute_argument_list env x
    | None -> fake_bracket []
  in
  (* TODO get the first [ as token here? *)
  G.NamedAttr (fake "[", v1, v2)

and argument_list (env : env) ((v1, v2, v3) : CST.argument_list) : G.arguments =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = argument env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = argument env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 = token env v3 (* ")" *) in
  (v1, v2, v3)

and pointer_type env (v1, v2) =
  let v1 = type_constraint env v1 in
  let v2 = token env v2 (* "*" *) in
  TyPointer (v2, v1) |> G.t

and function_pointer_type env (v1, v2, _v3, v4, _v5, _v6, v7) =
  let v1 = token env v1 (* "delegate" *) in
  let _v2 = token env v2 (* "*" *) in
  let _v4 = token env v4 (* "<" *) in
  let _v7 = token env v7 (* ">" *) in
  todo_type env v1

and tuple_type env (v1, v2, v3, v4, v5, v6) =
  let v1 = token env v1 (* "(" *) in
  let v2 = tuple_element env v2 in
  let _v3 = token env v3 (* "," *) in
  let v4 = tuple_element env v4 in
  let v5 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = tuple_element env v2 in
        v2)
      v5
  in
  let v6 = token env v6 (* ")" *) in
  TyTuple (v1, v2 :: v4 :: v5, v6) |> G.t

and type_ (env : env) (x : CST.type_) : G.type_ =
  match x with
  | `Impl_type _tok ->
      (* When type_ is called, we expect an explicit type, not "var".
         The implicit type is handled in local_variable_type. *)
      raise
        (Parse_info.Other_error
           ("Expected explicit type", Parse_tree_sitter_helpers.token env _tok))
  | `Array_type x -> array_type env x
  | `Name x ->
      let n = name env x in
      TyN n |> G.t
  | `Null_type x -> nullable_type env x
  | `Poin_type x -> pointer_type env x
  | `Func_poin_type x -> function_pointer_type env x
  | `Pred_type tok -> predefined_type env tok (* predefined_type *)
  | `Tuple_type x -> tuple_type env x

and type_argument_list (env : env) ((v1, v2, v3) : CST.type_argument_list) =
  let v1 = token env v1 (* "<" *) in
  let v2 =
    match v2 with
    | `Rep_COMMA _xs -> [] (* TODO What's this case? <,,,>? *)
    | `Type_rep_COMMA_type (v1, v2) ->
        let v1 = type_constraint env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = type_constraint env v2 in
              v2)
            v2
        in
        v1 :: v2
  in
  let v3 = token env v3 (* ">" *) in
  (v1, Common.map (fun t -> TA t) v2, v3)

and type_parameter_constraints_clause (env : env)
    ((v1, v2, v3, v4, v5) : CST.type_parameter_constraints_clause) =
  let _v1 = token env v1 (* "where" *) in
  let v2 = identifier_or_global env v2 in
  let _v3 = token env v3 (* ":" *) in
  let v4 = type_parameter_constraint env v4 in
  let v5 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = type_parameter_constraint env v2 in
        v2)
      v5
  in
  (v2, v4 :: v5)

and parameter_list (env : env) ((v1, v2, v3) : CST.parameter_list) :
    parameter list =
  let _v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some x -> formal_parameter_list env x
    | None -> []
  in
  let _v3 = token env v3 (* ")" *) in
  v2

and attribute_argument_list (env : env)
    ((v1, v2, v3) : CST.attribute_argument_list) : arguments =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = attribute_argument env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = attribute_argument env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 = token env v3 (* ")" *) in
  (v1, v2, v3)

and select_or_group_clause (env : env) (x : CST.select_or_group_clause) =
  match x with
  | `Group_clause (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "group" *) in
      let v2 = expression env v2 in
      let _v3 = token env v3 (* "by" *) in
      let v4 = expression env v4 in
      Group (v1, v2, v4)
  | `Select_clause (v1, v2) ->
      let v1 = token env v1 (* "select" *) in
      let v2 = expression env v2 in
      Select (v1, v2)

and declaration_expression (env : env) ((v1, v2) : CST.declaration_expression) =
  let v1 = local_variable_type env v1 in
  let v2 = identifier env v2 (* identifier *) in
  match v1 with
  | Some t ->
      let ent = basic_entity v2 in
      let vardef = { vinit = None; vtype = Some t } in
      let st = DefStmt (ent, VarDef vardef) |> G.s in
      G.stmt_to_expr st
  | None -> N (Id (v2, empty_id_info ())) |> G.e

and interpolation (env : env) ((v1, v2, v3, v4, v5) : CST.interpolation) :
    expr bracket =
  let v1 = token env v1 (* "{" *) in
  let v2 = expression env v2 in
  let _v3_TODO = Option.map (interpolation_alignment_clause env) v3 in
  let _v4_TODO = Option.map (interpolation_format_clause env) v4 in
  let v5 = token env v5 (* "}" *) in
  (v1, v2, v5)

let explicit_interface_specifier (env : env)
    ((v1, v2) : CST.explicit_interface_specifier) =
  let v1 = name env v1 in
  let _v2 = token env v2 (* "." *) in
  v1

let accessor_declaration (env : env)
    ((v1, v2, v3, v4) : CST.accessor_declaration) =
  let v1 = List.concat_map (attribute_list env) v1 in
  let v2 = Common.map (modifier env) v2 in
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

let bracketed_parameter_list (env : env)
    ((v1, v2, v3) : CST.bracketed_parameter_list) =
  let _lbra = token env v1 (* "[" *) in
  let params = formal_parameter_list env v2 in
  let _rbra = token env v3 (* "]" *) in
  params

let constructor_initializer (env : env)
    ((v1, v2, v3) : CST.constructor_initializer) =
  let _v1 = token env v1 (* ":" *) in
  let v2 =
    match v2 with
    | `Base tok -> IdSpecial (Super, token env tok) (* "base" *) |> G.e
    | `This tok -> IdSpecial (This, token env tok) |> G.e
    (* "this" *)
  in
  let v3 = argument_list env v3 in
  ExprStmt (Call (v2, v3) |> G.e, sc) |> G.s

let enum_member_declaration (env : env)
    ((v1, v2, v3) : CST.enum_member_declaration) =
  let _v1TODO = List.concat_map (attribute_list env) v1 in
  let v2 = identifier env v2 (* identifier *) in
  let v3 = Option.map (equals_value_clause env) v3 in
  OrEnum (v2, v3)

let base_list (env : env) ((v1, v2, v3) : CST.base_list) : G.class_parent list =
  let _v1 = token env v1 (* ":" *) in
  let v2 = type_constraint env v2 in
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = type_constraint env v2 in
        v2)
      v3
  in
  v2 :: v3 |> Common.map (fun t -> (t, None))

let accessor_list (env : env) ((v1, v2, v3) : CST.accessor_list) =
  let v1 = token env v1 (* "{" *) in
  let v2 = Common.map (accessor_declaration env) v2 in
  let v3 = token env v3 (* "}" *) in
  (v1, v2, v3)

let enum_member_declaration_list (env : env)
    ((v1, v2, v3, v4) : CST.enum_member_declaration_list) =
  let _v1 = token env v1 (* "{" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = enum_member_declaration env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = enum_member_declaration env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let _v3 = Option.map (token env) v3 (* "," *) in
  let _v4 = token env v4 (* "}" *) in
  v2

let rec declaration_list (env : env)
    ((open_bracket, body, close_bracket) : CST.declaration_list) =
  let xs = Common.map (declaration env) body in
  (token env open_bracket, xs, token env close_bracket)

and extern_alias_directive (env : env)
    ((v1, v2, v3, v4) : CST.extern_alias_directive) =
  let v1 = token env v1 (* "extern" *) in
  let _v2 = token env v2 (* "alias" *) in
  let v3 = identifier env v3 (* identifier *) in
  let v4 = token env v4 (* ";" *) in
  let extern =
    G.OtherDirective (("ExternAlias", v1), [ G.I v3; G.Tk v4 ]) |> G.d
  in
  G.DirectiveStmt extern |> G.s

and using_directive (env : env) ((v0, v1, v2, v3, v4) : CST.using_directive) =
  let _globalTODO = Option.map (token env) v0 in
  let v1 = token env v1 (* "using" *) in
  let v3 = name env v3 in
  let v4 = token env v4 (* ";" *) in
  let import =
    match v2 with
    | Some x -> (
        match x with
        | `Static _tok ->
            (* "static" *)
            (* using static System.Math; *)
            (* THINK: The generic AST is undistinguishable from that of `using Foo`. *)
            G.ImportAll (v1, G.DottedName (H2.dotted_ident_of_name v3), v4)
        | `Name_equals x ->
            (* using Foo = System.Text; *)
            let alias = name_equals env x in
            G.ImportAs
              ( v1,
                G.DottedName (H2.dotted_ident_of_name v3),
                Some (alias, empty_id_info ()) ))
    | None ->
        (* using System.IO; *)
        G.ImportAll (v1, G.DottedName (H2.dotted_ident_of_name v3), v4)
  in
  G.DirectiveStmt (import |> G.d) |> G.s

and global_attribute_list (env : env)
    ((v1, v2, v3, v4, v5) : CST.global_attribute_list) =
  let v1 = token env v1 (* "[" *) in
  let v2 =
    match v2 with
    | `Asse tok -> str env tok (* "assembly" *)
    | `Module tok -> str env tok
    (* "module" *)
  in
  let _v3 = token env v3 (* ":" *) in
  let v4 =
    match v4 with
    | Some (v1, v2) ->
        let v1 = attribute env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = attribute env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let _v5 = token env v5 (* "]" *) in
  let anys = Common.map (fun a -> At a) v4 in
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
  | `Rep_extern_alias_dire_rep_using_dire_rep_global_attr_list_choice_rep_global_stmt_rep_name_member_decl
      (v1, v2, v3, v4) ->
      let v1 = v1 |> Common.map (extern_alias_directive env) in
      let v2 = v2 |> Common.map (using_directive env) in
      let v3 = v3 |> Common.map (global_attribute_list env) in
      let v4 =
        match v4 with
        | `Rep_global_stmt_rep_name_member_decl (v1, v2) ->
            let v1 = v1 |> Common.map (global_statement env) in
            let v2 = v2 |> Common.map (namespace_member_declaration env) in
            v1 @ v2
        | `File_scoped_name_decl x -> file_scoped_namespace_declaration env x
      in
      G.Pr (List.concat [ v1; v2; v3; v4 ])
  | `Semg_exp (_v1, v2) ->
      let v2 = expression env v2 in
      G.E v2

and file_scoped_namespace_declaration (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.file_scoped_namespace_declaration) =
  let tnamespace = (* "namespace" *) token env v1 in
  let n = name env v2 in
  let _tsemi = (* ";" *) token env v3 in
  let v4 = Common.map (extern_alias_directive env) v4 in
  let v5 = Common.map (using_directive env) v5 in
  let v6 = Common.map (type_declaration env) v6 in
  let dotted_ident = H2.dotted_ident_of_name n in
  let namespace = G.Package (tnamespace, dotted_ident) |> G.d in
  List.concat [ [ G.DirectiveStmt namespace |> G.s ]; v4; v5; v6 ]

and namespace_declaration (env : env)
    ((v1, v2, v3, _semi) : CST.namespace_declaration) =
  (*
        namespace MySpace { ... } ;
            v1       v2      v3  v4
      *)
  let _v1 = token env v1 (* "namespace" *) in
  let v2 = name env v2 in
  let open_brace, decls, close_brace = declaration_list env v3 in
  let body = G.Block (open_brace, decls, close_brace) |> G.s in
  let ent = { name = EN v2; attrs = []; tparams = [] } in
  let mkind = G.ModuleStruct (None, [ body ]) in
  let def = { G.mbody = mkind } in
  G.DefStmt (ent, G.ModuleDef def) |> G.s

and type_declaration (env : env) (x : CST.type_declaration) : stmt =
  match x with
  | `Class_decl x -> class_interface_struct env Class x
  | `Inte_decl x -> class_interface_struct env Interface x
  | `Enum_decl x -> enum_declaration env x
  | `Record_decl x -> record_declaration env x
  | `Record_struct_decl x -> record_struct_declaration env x
  | `Dele_decl x -> delegate_declaration env x
  | `Struct_decl x -> class_interface_struct env Class x

and class_interface_struct (env : env) class_kind
    (v1, v2, v3, v4, v5, v6, v7, v8, _v9) =
  (*
   [Attr] public class MyClass<MyType> : IClass where MyType : SomeType { ... };
      v1     v2    v3    v4     v5         v6           v7                v8   v9
   *)
  let v1 = List.concat_map (attribute_list env) v1 in
  let v2 = Common.map (modifier env) v2 in
  let v3 = token env v3 (* "class" *) in
  let v4 = identifier env v4 (* identifier *) in
  let v5 =
    match v5 with
    | Some x -> type_parameter_list env x
    | None -> []
  in
  let v6 =
    match v6 with
    | Some x -> base_list env x
    | None -> []
  in
  let v7 = Common.map (type_parameter_constraints_clause env) v7 in
  let open_bra, stmts, close_bra = declaration_list env v8 in
  let fields = Common.map (fun x -> G.F x) stmts in
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
          cparams = [];
          cbody = (open_bra, fields, close_bra);
        } )
  |> G.s

and enum_declaration env (v1, v2, v3, v4, v5, v6, v7) =
  let v1 = List.concat_map (attribute_list env) v1 in
  let v2 = Common.map (modifier env) v2 in
  let _v3TODO = token env v3 (* "enum" *) in
  let v4 = identifier env v4 (* identifier *) in
  let _v5TODO =
    match v5 with
    | Some x -> base_list env x
    | None -> []
  in
  let v6 = enum_member_declaration_list env v6 in
  let _v7 = Option.map (token env) v7 (* ";" *) in
  let idinfo = empty_id_info () in
  let ent = { name = EN (Id (v4, idinfo)); attrs = v1 @ v2; tparams = [] } in
  G.DefStmt (ent, G.TypeDef { tbody = OrType v6 }) |> G.s

and delegate_declaration env (v1, v2, v3, v4, v5, v6, v7, v8, v9) =
  let v1 = List.concat_map (attribute_list env) v1 in
  let v2 = Common.map (modifier env) v2 in
  let _v3 = token env v3 (* "delegate" *) in
  let v4 = return_type env v4 in
  let v5 = identifier env v5 (* identifier *) in
  let v6 =
    match v6 with
    | Some x -> type_parameter_list env x
    | None -> []
  in
  let v7 = parameter_list env v7 in
  let v8 = Common.map (type_parameter_constraints_clause env) v8 in
  let _v9 = token env v9 (* ";" *) in
  let tparams = type_parameters_with_constraints v6 v8 in
  let func = TyFun (v7, v4) |> G.t in
  let idinfo = empty_id_info () in
  let ent = { name = EN (Id (v5, idinfo)); attrs = v1 @ v2; tparams } in
  DefStmt (ent, TypeDef { tbody = NewType func }) |> G.s

and record_declaration env (_, _, v3, _, _, _, _, _, _, _) =
  let v3 = token env v3 (* "record" *) in
  todo_stmt env v3

and record_struct_declaration env (_, _, v3, _, _, _, _, _, _, _) =
  let v3 = token env v3 (* "record" *) in
  todo_stmt env v3

and declaration (env : env) (x : CST.declaration) : stmt =
  match x with
  | `Ellips v1 ->
      let v1 = token env v1 in
      G.ExprStmt (G.Ellipsis v1 |> G.e, sc) |> G.s
  | `Class_decl x -> class_interface_struct env Class x
  | `Dele_decl x -> delegate_declaration env x
  | `Enum_decl x -> enum_declaration env x
  | `Inte_decl x -> class_interface_struct env Interface x
  | `Record_decl x -> record_declaration env x
  | `Record_struct_decl x -> record_struct_declaration env x
  | `Struct_decl x -> class_interface_struct env Class x
  | `Cons_decl (v1, v2, v3, v4, v5, v6) ->
      let v1 = List.concat_map (attribute_list env) v1 in
      let v2 = Common.map (modifier env) v2 in
      let v3 = identifier env v3 (* identifier *) in
      let _, tok = v3 in
      let v4 = parameter_list env v4 in
      let v5 = Option.map (constructor_initializer env) v5 in
      let v6 = function_body env v6 in
      (* TODO? separate ctor initializer from body in G.function_definition?*)
      let fbody =
        match v5 with
        | Some init ->
            G.FBStmt
              (Block (fake_bracket [ init; H2.funcbody_to_stmt v6 ]) |> G.s)
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
  | `Conv_op_decl (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = List.concat_map (attribute_list env) v1 in
      let v2 = Common.map (modifier env) v2 in
      let v3 =
        match v3 with
        | `Impl tok -> ("op_Implicit", token env tok) (* "implicit" *)
        | `Expl tok -> ("op_Explicit", token env tok)
        (* "explicit" *)
      in
      let v4 = token env v4 (* "operator" *) in
      let v5 = type_constraint env v5 in
      let v6 = parameter_list env v6 in
      let v7 = function_body env v7 in
      let idinfo = empty_id_info () in
      let ent =
        { name = EN (Id (v3, idinfo)); attrs = v1 @ v2; tparams = [] }
      in
      let def =
        G.FuncDef
          {
            fkind = (G.Method, v4);
            fparams = v6;
            frettype = Some v5;
            fbody = v7;
          }
      in
      G.DefStmt (ent, def) |> G.s
  | `Dest_decl (v1, v2, v3, v4, v5, v6) ->
      let v1 = List.concat_map (attribute_list env) v1 in
      let v2 =
        match v2 with
        | Some tok -> [ KeywordAttr (Extern, token env tok) ] (* "extern" *)
        | None -> []
      in
      let v3 = token env v3 (* "~" *) in
      let _v4TODO = identifier env v4 (* identifier *) in
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
      let v1 = List.concat_map (attribute_list env) v1 in
      let _v2TODO = Common.map (modifier env) v2 in
      let v3 = unhandled_keywordattr (str env v3) (* "event" *) in
      let v4 = type_constraint env v4 in
      let _v5TODO = Option.map (explicit_interface_specifier env) v5 in
      let v6 = identifier env v6 (* identifier *) in
      let fname, _ftok = v6 in
      let v7 =
        match v7 with
        | `Acce_list x ->
            let open_br, accs, close_br = accessor_list env x in
            let funcs =
              accs
              |> Common.map (fun (attrs, id, fbody) ->
                     let iname, itok = id in
                     let ent =
                       basic_entity (iname ^ "_" ^ fname, itok) ~attrs
                     in
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
                           fparams = [ valparam ];
                           frettype = None;
                           fbody;
                         }
                     in
                     DefStmt (ent, funcdef) |> G.s)
            in
            (open_br, funcs, close_br)
        | `SEMI tok ->
            (* ";" *)
            let tok = token env tok (* ";" *) in
            fake_bracket [ todo_stmt env tok ]
      in
      let ent = basic_entity v6 ~attrs:(v1 @ v1 @ [ v3 ]) in
      let vardef = { vinit = None; vtype = Some v4 } in
      let open_br, funcs, close_br = v7 in
      Block (open_br, (DefStmt (ent, VarDef vardef) |> G.s) :: funcs, close_br)
      |> G.s
  | `Event_field_decl (v1, v2, v3, v4, v5) ->
      let v1 = List.concat_map (attribute_list env) v1 in
      let v2 = Common.map (modifier env) v2 in
      let v3 = unhandled_keywordattr (str env v3) (* "event" *) in
      let v4 = variable_declaration env v4 in
      let _v5 = token env v5 (* ";" *) in
      var_def_stmt v4 ((v3 :: v1) @ v2)
  | `Field_decl (v1, v2, v3, v4) ->
      let v1 = List.concat_map (attribute_list env) v1 in
      let v2 = Common.map (modifier env) v2 in
      let v3 = variable_declaration env v3 in
      let _v4 = token env v4 (* ";" *) in
      var_def_stmt v3 (v1 @ v2)
  | `Inde_decl (v1, v2, v3, v4, v5, v6, v7) -> (
      let v1 = List.concat_map (attribute_list env) v1 in
      let v2 = Common.map (modifier env) v2 in
      let v3 = type_constraint env v3 in
      let _v4TODO = Option.map (explicit_interface_specifier env) v4 in
      let _v5 = token env v5 (* "this" *) in
      let v6 = bracketed_parameter_list env v6 in
      let indexer_attrs = v1 @ v2 in
      match v7 with
      | `Acce_list x ->
          let open_br, accs, close_br = accessor_list env x in
          let funcs =
            accs
            |> Common.map (fun (attrs, id, fbody) ->
                   let iname, itok = id in
                   match iname with
                   | "get" ->
                       let ent = basic_entity ("get_Item", itok) ~attrs in
                       let funcdef =
                         FuncDef
                           {
                             fkind = (Method, itok);
                             fparams = v6;
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
                             fparams = v6 @ [ valparam ];
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
          let v2 = token env v2 (* ";" *) in
          let arrow, expr = v1 in
          let fbody = G.FBStmt (ExprStmt (expr, v2) |> G.s) in
          let ent = basic_entity ("get_Item", arrow) ~attrs:indexer_attrs in
          let funcdef =
            FuncDef
              {
                fkind = (Arrow, arrow);
                fparams = v6;
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
      let v1 = List.concat_map (attribute_list env) v1 in
      let v2 = Common.map (modifier env) v2 in
      let v3 = return_type env v3 in
      let v5 = identifier env v5 (* identifier *) in
      let _, tok = v5 in
      let v6 =
        match v6 with
        | Some x -> type_parameter_list env x
        | None -> []
      in
      let v7 = parameter_list env v7 in
      let v8 = Common.map (type_parameter_constraints_clause env) v8 in
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
  | `Op_decl (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = List.concat_map (attribute_list env) v1 in
      let v2 = Common.map (modifier env) v2 in
      let v3 = type_constraint env v3 in
      let v4 = token env v4 (* "operator" *) in
      let v5 = overloadable_operator env v5 in
      let v6 = parameter_list env v6 in
      let v7 = function_body env v7 in
      (* TODO make clear that this is an operator overload, by using IdSpecial as the name, or adding a keyword attribute *)
      let idinfo = empty_id_info () in
      let ent =
        { name = EN (Id (v5, idinfo)); attrs = v1 @ v2; tparams = [] }
      in
      let def =
        G.FuncDef
          {
            fkind = (G.Method, v4);
            fparams = v6;
            frettype = Some v3;
            fbody = v7;
          }
      in
      G.DefStmt (ent, def) |> G.s
  | `Prop_decl (v1, v2, v3, v4, v5, v6) ->
      (* [Attr] public string IFace.Field { get; public set { ... } } = "hello";
         [Attr] public string IFace.Field => "hello";
           v1     v2     v3    v4    v5      v6
         Map `Prop` as field. Map getter and setter as methods. *)
      let v1 = List.concat_map (attribute_list env) v1 in
      let v2 = Common.map (modifier env) v2 in
      let v3 = type_constraint env v3 in
      let _v4TODO = Option.map (explicit_interface_specifier env) v4 in
      let v5 = identifier env v5 (* identifier *) in
      let fname, _ftok = v5 in
      let accessors, vinit =
        match v6 with
        | `Acce_list_opt_EQ_exp_SEMI (v1, v2) ->
            let v1 = accessor_list env v1 in
            let v2 =
              match v2 with
              | Some (v1, v2, v3) ->
                  let _v1 = token env v1 (* "=" *) in
                  let v2 = expression env v2 in
                  let _v3 = token env v3 (* ";" *) in
                  Some v2
              | None -> None
            in
            let open_br, v1, close_br = v1 in
            let funcs =
              Common.map
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
            let v2 = token env v2 (* ";" *) in
            let arrow, expr = v1 in
            let ent = basic_entity ("get_" ^ fname, arrow) in
            let funcdef =
              FuncDef
                {
                  fkind = (Arrow, arrow);
                  fparams = [];
                  frettype = Some v3;
                  fbody = G.FBStmt (ExprStmt (expr, v2) |> G.s);
                }
            in
            let func = DefStmt (ent, funcdef) |> G.s in
            ((arrow, [ func ], v2), None)
      in
      let ent = basic_entity v5 ~attrs:(v1 @ v2) in
      let vardef = { vinit; vtype = Some v3 } in
      let open_br, funcs, close_br = accessors in
      Block (open_br, (DefStmt (ent, VarDef vardef) |> G.s) :: funcs, close_br)
      |> G.s
  | `Using_dire x -> using_directive env x

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)
let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_c_sharp.Parse.file file)
    (fun cst ->
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
    (fun cst ->
      let file = "<pattern>" in
      let env = { H.file; conv = Hashtbl.create 0; extra = () } in
      match compilation_unit env cst with
      | G.Pr [ x ] -> G.S x
      | G.Pr xs -> G.Ss xs
      | x -> x)
