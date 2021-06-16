(* Sjoerd Langkemper
 *
 * Copyright (c) 2021 R2C
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common
module CST = Tree_sitter_c_sharp.CST
module AST = AST_generic
module H = Parse_tree_sitter_helpers
open AST_generic
module H2 = AST_generic_helpers
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Csharp parser using tree-sitter-lang/semgrep-charp and converting
 * directly to pfff/h_program-lang/AST_generic.ml
 *
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = unit H.env

let _fake = AST_generic.fake

let token = H.token

let str = H.str

let unhandled_keywordattr_to_namedattr env tok =
  AST.unhandled_keywordattr (str env tok)

let map_opt func env = function None -> None | Some x -> Some (func env x)

let ids_of_name (name : name) : dotted_ident =
  match name with
  | Id (ident, _) -> [ ident ]
  | IdQualified ((ident, name_info), _) -> (
      match name_info.name_qualifier with
      | Some q -> (
          match q with
          | QDots ds -> ds @ [ ident ]
          | _ -> failwith "unexpected qualifier type" )
      | None -> [ ident ] )

let prepend_qualifier_to_name (qualifier : qualifier) (name : name) : name =
  match name with
  | Id (ident, id_info) ->
      let name_info =
        { name_qualifier = Some qualifier; name_typeargs = None }
      in
      IdQualified ((ident, name_info), id_info)
  | IdQualified ((ident, name_info), id_info) ->
      let new_qualifier =
        match (name_info.name_qualifier, qualifier) with
        | None, q -> q
        | Some (QTop _), QTop t2 -> QTop t2
        | Some (QDots t1), QTop _ -> QDots t1
        | Some (QTop _), QDots t2 -> QDots t2
        | Some (QDots t1), QDots t2 -> QDots (t2 @ t1)
        | _ -> failwith "qualifier not supported"
      in
      let name_info = { name_info with name_qualifier = Some new_qualifier } in
      IdQualified ((ident, name_info), id_info)

let type_parameters_with_constraints params constraints : type_parameter list =
  List.map
    (fun param ->
      let with_constraints =
        List.find_opt
          (fun p ->
            let id, _ = p in
            let id, _ = id in
            let param, _ = param in
            id = param)
          constraints
      in
      match with_constraints with Some x -> x | None -> (param, []))
    params

let arg_to_expr (a : argument) =
  match a with Arg e -> e | ArgKwd (_, e) -> e | _ -> raise Impossible

let var_def_stmt (decls : (entity * variable_definition) list)
    (attrs : attribute list) =
  let stmts =
    List.map
      (fun (ent, def) ->
        let ent = { ent with attrs = ent.attrs @ attrs } in
        DefStmt (ent, VarDef def) |> AST.s)
      decls
  in
  stmt1 stmts

type direction = Ascending | Descending

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
      ParamClassic
        {
          pname = Some id;
          ptype = None;
          pdefault = None;
          pattrs = [];
          pinfo = empty_id_info ();
        }
  | ids ->
      let ids = List.map (fun id -> PatId (id, empty_id_info ())) ids in
      ParamPattern (PatTuple (fake_bracket ids))

(* create lambda lambda_params -> expr *)
let create_lambda lambda_params expr =
  let fparams = [ param_from_lambda_params lambda_params ] in
  Lambda
    {
      fkind = (Arrow, fake "=>");
      fparams;
      frettype = None;
      fbody = exprstmt expr;
    }

(* create lambda (lambda_params, ident) -> (lambda_params..., ident) *)
let create_join_result_lambda lambda_params ident =
  let p1 = param_from_lambda_params lambda_params in
  let p2 = ParamClassic (param_of_id ident) in
  let fparams = [ p1; p2 ] in
  let ids =
    lambda_params @ [ ident ]
    |> List.map (fun id -> N (Id (id, empty_id_info ())))
  in
  let expr = Tuple (fake_bracket ids) in
  Lambda
    {
      fkind = (Arrow, fake "=>");
      fparams;
      frettype = None;
      fbody = exprstmt expr;
    }

(* create a new lambda in the form
 * base_expr.funcname(lambda_params => expr)
 *)
let call_lambda base_expr funcname tok funcs =
  (* let funcs = exprs |> List.map (fun expr -> create_lambda lambda_params expr) in *)
  let args = funcs |> List.map (fun func -> Arg func) in
  let idinfo = empty_id_info () in
  let method_ = DotAccess (base_expr, tok, EN (Id ((funcname, tok), idinfo))) in
  Call (method_, fake_bracket args)

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
            List.map (fun id -> N (Id (id, empty_id_info ()))) lambda_params
          in
          let expr = Tuple (fake_bracket (ids @ [ expr ])) in
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
          linq_remainder_to_expr tl base_expr lambda_params )

let linq_to_expr (from : linq_query_part) (body : linq_query_part list) =
  match from with
  | From (_, (_type, id), collection) ->
      linq_remainder_to_expr body collection [ id ]
  | _ -> raise Impossible

let new_index_from_end tok expr =
  let index =
    TyN
      (IdQualified
         ( ( ("Index", fake "Index"),
             {
               name_qualifier = Some (QDots [ ("System", fake "System") ]);
               name_typeargs = None;
             } ),
           empty_id_info () ))
  in
  Call
    ( IdSpecial (New, tok),
      fake_bracket
        [ ArgType index; Arg expr; Arg (L (Bool (true, fake "true"))) ] )

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

(* Disable warnings against unused variables *)
[@@@warning "-26-27-32"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

let todo_expr _env tok = AST.OtherExpr (AST.OE_Todo, [ AST.Tk tok ])

let todo_stmt _env tok = AST.s (AST.OtherStmt (AST.OS_Todo, [ AST.Tk tok ]))

let todo_pat _env tok = AST.OtherPat (AST.OP_Todo, [ AST.Tk tok ])

let todo_attr _env tok = AST.OtherAttribute (AST.OA_Expr, [ AST.Tk tok ])

let todo_type _env tok = AST.OtherType (AST.OT_Todo, [ AST.Tk tok ])

let parameter_modifier (env : env) (x : CST.parameter_modifier) =
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
  AST.TyBuiltin (str env tok)

let verbatim_string_literal (env : env) (tok : CST.verbatim_string_literal) =
  AST.String (str env tok)

(* verbatim_string_literal *)

let _preprocessor_directive (env : env) (tok : CST.preprocessor_directive) =
  token env tok

(* pattern #[a-z]\w* *)

let default_switch_label (env : env) ((v1, v2) : CST.default_switch_label) =
  let v1 = token env v1 (* "default" *) in
  let v2 = token env v2 (* ":" *) in
  AST.Default v1

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
  AST.Int (int_of_string_opt s, t)

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
  | `Fixed tok -> unhandled_keywordattr_to_namedattr env tok
  | `Inte tok -> unhandled_keywordattr_to_namedattr env tok
  | `New tok -> unhandled_keywordattr_to_namedattr env tok
  | `Over tok -> unhandled_keywordattr_to_namedattr env tok
  | `Part tok -> unhandled_keywordattr_to_namedattr env tok
  | `Priv tok -> KeywordAttr (Private, token env tok) (* "private" *)
  | `Prot tok -> KeywordAttr (Protected, token env tok) (* "protected" *)
  | `Public tok -> KeywordAttr (Public, token env tok) (* "public" *)
  | `Read tok -> KeywordAttr (Const, token env tok) (* "readonly" *)
  | `Ref tok -> unhandled_keywordattr_to_namedattr env tok
  | `Sealed tok ->
      (* TODO we map Sealed to Final here, is that OK? *)
      KeywordAttr (Final, token env tok)
      (* "sealed" *)
  | `Static tok -> KeywordAttr (Static, token env tok) (* "static" *)
  | `Unsafe tok -> unhandled_keywordattr_to_namedattr env tok
  | `Virt tok -> unhandled_keywordattr_to_namedattr env tok
  | `Vola tok -> KeywordAttr (Volatile, token env tok)

(* "volatile" *)

let interpolation_format_clause (env : env)
    ((v1, v2) : CST.interpolation_format_clause) =
  let v1 = token env v1 (* ":" *) in
  let v2 = token env v2 (* pattern "[^}\"]+" *) in
  v2

let interpolated_verbatim_string_text (env : env)
    (x : CST.interpolated_verbatim_string_text) =
  let x =
    match x with
    | `LCURLLCURL tok -> str env tok (* "{{" *)
    | `Pat_6d9db72 tok -> str env tok (* pattern "[^{\"]+" *)
    | `DQUOTDQUOT tok -> str env tok
    (* "\"\"" *)
  in
  String x

let real_literal (env : env) (tok : CST.real_literal) =
  let s, t = str env tok (* real_literal *) in
  AST.Float (float_of_string_opt s, t)

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
  | `Tok_pat_8cc7dbf tok -> str env tok

(* TODO: not sure why preprocessor_call was not generated. Because
 * was in extras?
 *)
let _preproc_directive_end (env : env) (tok : CST.preproc_directive_end) =
  token env tok

(* preproc_directive_end *)

let interpolated_string_text (env : env) (x : CST.interpolated_string_text) =
  match x with
  | `LCURLLCURL tok -> String (str env tok) (* "{{" *)
  | `Imm_tok_pat_2755817 tok ->
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
              List.map
                (fun (v1, v2) ->
                  let v1 = token env v1 (* "," *) in
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
  let v1 = token env v1 (* "into" *) in
  let v2 = identifier env v2 (* identifier *) in
  v2

let identifier_or_global (env : env) (x : CST.identifier_or_global) =
  match x with
  | `Global tok -> str env tok (* "global" *)
  | `Id tok -> identifier env tok

(* identifier *)

let identifier_or_global_qualifier (env : env) (x : CST.identifier_or_global) =
  match x with
  | `Global tok -> QTop (token env tok) (* "global" *)
  | `Id tok -> QDots [ identifier env tok ]

(* identifier *)

let rec tuple_pattern (env : env) ((v1, v2, v3, v4) : CST.tuple_pattern) =
  let v1 = token env v1 (* "(" *) in
  let v2 = anon_choice_id_c036834 env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let v1 = token env v1 (* "," *) in
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
  let v2 = token env v2 (* ":" *) in
  v1

let name_equals (env : env) ((v1, v2) : CST.name_equals) =
  let v1 = identifier_or_global env v1 in
  let v2 = token env v2 (* "=" *) in
  v1

let literal (env : env) (x : CST.literal) : literal =
  match x with
  | `Null_lit tok -> AST.Null (token env tok) (* "null" *)
  | `Bool_lit x -> boolean_literal env x
  | `Char_lit (v1, v2, v3) ->
      let v1 = token env v1 (* "'" *) in
      let v2 =
        match v2 with
        | `Imm_tok_pat_684220d tok -> str env tok (* pattern "[^'\\\\]" *)
        | `Esc_seq tok -> str env tok
        (* escape_sequence *)
      in
      let v3 = token env v3 (* "'" *) in
      Char v2
  | `Real_lit tok -> real_literal env tok (* real_literal *)
  | `Int_lit tok -> integer_literal env tok (* integer_literal *)
  | `Str_lit (v1, v2, v3) ->
      let v1 = token env v1 (* "\"" *) in
      let v2 =
        List.map
          (fun x ->
            match x with
            | `Imm_tok_pat_5a6fa79 tok ->
                str env tok (* pattern "[^\"\\\\\\n]+" *)
            | `Esc_seq tok -> str env tok
            (* escape_sequence *))
          v2
      in
      let v3 = token env v3 (* "\"" *) in
      let str = v2 |> List.map fst |> String.concat "" in
      let toks = v2 |> List.map snd in
      let toks = PI.combine_infos v1 (toks @ [ v3 ]) in
      AST.String (str, toks)
  | `Verb_str_lit tok -> verbatim_string_literal env tok

(* verbatim_string_literal *)

let rec return_type (env : env) (x : CST.return_type) : type_ =
  match x with
  | `Type x -> type_constraint env x
  | `Void_kw tok -> TyBuiltin (str env tok)

(* "void" *)
and type_pattern (env : env) (x : CST.type_pattern) = type_ env x

and variable_declaration (env : env) ((v1, v2, v3) : CST.variable_declaration) :
    (entity * variable_definition) list =
  let v1 = local_variable_type env v1 in
  let v2 = variable_declarator env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let v1 = token env v1 (* "," *) in
        let v2 = variable_declarator env v2 in
        v2)
      v3
  in
  let decls = v2 :: v3 in
  List.map
    (fun (ent, vardef) -> (ent, { vinit = vardef.vinit; vtype = v1 }))
    decls

and interpolation_alignment_clause (env : env)
    ((v1, v2) : CST.interpolation_alignment_clause) =
  let v1 = token env v1 (* "," *) in
  let v2 = expression env v2 in
  v2

and parenthesized_expression (env : env)
    ((v1, v2, v3) : CST.parenthesized_expression) =
  expression env v2

and postfix_unary_expression (env : env) (x : CST.postfix_unary_expression) =
  match x with
  | `Exp_PLUSPLUS (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "++" *) in
      Call (IdSpecial (IncrDecr (Incr, Postfix), v2), fake_bracket [ Arg v1 ])
  | `Exp_DASHDASH (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "--" *) in
      Call (IdSpecial (IncrDecr (Decr, Postfix), v2), fake_bracket [ Arg v1 ])
  | `Exp_BANG (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "!" *) in
      Call (IdSpecial (Op NotNullPostfix, v2), fake_bracket [ Arg v1 ])

and when_clause (env : env) ((v1, v2) : CST.when_clause) =
  let v1 = token env v1 (* "when" *) in
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
      let _v2 = constant_pattern env v2 in
      todo_pat env v1
  | `LTEQ_exp (v1, v2) ->
      let v1 = token env v1 (* "<=" *) in
      let _v2 = constant_pattern env v2 in
      todo_pat env v1
  | `GT_exp (v1, v2) ->
      let v1 = token env v1 (* ">" *) in
      let _v2 = constant_pattern env v2 in
      todo_pat env v1
  | `GTEQ_exp (v1, v2) ->
      let v1 = token env v1 (* ">=" *) in
      let _v2 = constant_pattern env v2 in
      todo_pat env v1

and binary_expression (env : env) (x : CST.binary_expression) =
  match x with
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "&&" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op And, v2), fake_bracket [ Arg v1; Arg v3 ])
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "||" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Or, v2), fake_bracket [ Arg v1; Arg v3 ])
  | `Exp_GTGT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ">>" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op LSR, v2), fake_bracket [ Arg v1; Arg v3 ])
      (* TODO Is LSR the correct shift type? *)
  | `Exp_LTLT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "<<" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op LSL, v2), fake_bracket [ Arg v1; Arg v3 ])
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "&" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op BitAnd, v2), fake_bracket [ Arg v1; Arg v3 ])
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "^" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op BitXor, v2), fake_bracket [ Arg v1; Arg v3 ])
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "|" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op BitOr, v2), fake_bracket [ Arg v1; Arg v3 ])
  | `Exp_PLUS_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "+" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Plus, v2), fake_bracket [ Arg v1; Arg v3 ])
  | `Exp_DASH_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "-" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Minus, v2), fake_bracket [ Arg v1; Arg v3 ])
  | `Exp_STAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "*" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Mult, v2), fake_bracket [ Arg v1; Arg v3 ])
  | `Exp_SLASH_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "/" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Div, v2), fake_bracket [ Arg v1; Arg v3 ])
  | `Exp_PERC_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "%" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Mod, v2), fake_bracket [ Arg v1; Arg v3 ])
  | `Exp_LT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "<" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Lt, v2), fake_bracket [ Arg v1; Arg v3 ])
  | `Exp_LTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "<=" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op LtE, v2), fake_bracket [ Arg v1; Arg v3 ])
  | `Exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "==" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Eq, v2), fake_bracket [ Arg v1; Arg v3 ])
  | `Exp_BANGEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "!=" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op NotEq, v2), fake_bracket [ Arg v1; Arg v3 ])
  | `Exp_GTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ">=" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op GtE, v2), fake_bracket [ Arg v1; Arg v3 ])
  | `Exp_GT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ">" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Gt, v2), fake_bracket [ Arg v1; Arg v3 ])
  | `Exp_QMARKQMARK_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "??" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Nullish, v2), fake_bracket [ Arg v1; Arg v3 ])

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
  let v2 = List.map (statement env) v2 in
  let v3 = token env v3 (* "}" *) in
  AST.Block (v1, v2, v3) |> AST.s

and variable_declarator (env : env) ((v1, v2, v3) : CST.variable_declarator) =
  let v1, pattern =
    match v1 with
    | `Id tok -> (identifier env tok, None) (* identifier *)
    | `Tuple_pat x ->
        let tok, _, _, _ = x in
        let id = (AST.special_multivardef_pattern, token env tok) in
        let pat = Some (tuple_pattern env x) in
        (id, pat)
  in
  let v2 = map_opt element_binding_expression env v2 in
  let v3 = map_opt equals_value_clause env v3 in
  let vinit =
    match (pattern, v3) with
    | Some pat, Some init -> Some (LetPattern (pat, init))
    | _ -> v3
  in
  let ent = basic_entity v1 [] in
  let vardef = { vinit; vtype = None } in
  (ent, vardef)

and with_initializer_expression (env : env)
    ((v1, v2) : CST.with_initializer_expression) : AST.field list =
  let v1 = simple_assignment_expression env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = simple_assignment_expression env v2 in
        v2)
      v2
  in
  v2

and prefix_unary_expression (env : env) (x : CST.prefix_unary_expression) =
  match x with
  | `BANG_exp (v1, v2) ->
      let v1 = token env v1 (* "!" *) in
      let v2 = expression env v2 in
      Call (IdSpecial (Op Not, v1), fake_bracket [ Arg v2 ])
  | `AMP_exp (v1, v2) ->
      let v1 = token env v1 (* "&" *) in
      let v2 = expression env v2 in
      Ref (v1, v2)
  | `STAR_exp (v1, v2) ->
      let v1 = token env v1 (* "*" *) in
      let v2 = expression env v2 in
      DeRef (v1, v2)
  | `PLUS_exp (v1, v2) ->
      let v1 = token env v1 (* "+" *) in
      let v2 = expression env v2 in
      Call (IdSpecial (Op Plus, v1), fake_bracket [ Arg v2 ])
  | `PLUSPLUS_exp (v1, v2) ->
      let v1 = token env v1 (* "++" *) in
      let v2 = expression env v2 in
      Call (IdSpecial (IncrDecr (Incr, Prefix), v1), fake_bracket [ Arg v2 ])
  | `DASH_exp (v1, v2) ->
      let v1 = token env v1 (* "-" *) in
      let v2 = expression env v2 in
      Call (IdSpecial (Op Minus, v1), fake_bracket [ Arg v2 ])
  | `DASHDASH_exp (v1, v2) ->
      let v1 = token env v1 (* "--" *) in
      let v2 = expression env v2 in
      Call (IdSpecial (IncrDecr (Decr, Prefix), v1), fake_bracket [ Arg v2 ])
  | `HAT_exp (v1, v2) ->
      let v1 = token env v1 (* "^" *) in
      let v2 = expression env v2 in
      new_index_from_end v1 v2
  | `TILDE_exp (v1, v2) ->
      let v1 = token env v1 (* "~" *) in
      let v2 = expression env v2 in
      Call (IdSpecial (Op BitNot, v1), fake_bracket [ Arg v2 ])

and name (env : env) (x : CST.name) : AST.name =
  match x with
  | `Alias_qual_name (v1, v2, v3) ->
      let v1 = identifier_or_global_qualifier env v1 in
      let v2 = token env v2 (* "::" *) in
      let v3 = simple_name env v3 in
      prepend_qualifier_to_name v1 v3
  | `Qual_name (v1, v2, v3) ->
      let v1 = name env v1 in
      let v2 = token env v2 (* "." *) in
      let v3 = simple_name env v3 in
      let qualifier = QDots (ids_of_name v1) in
      prepend_qualifier_to_name qualifier v3
  | `Simple_name x -> simple_name env x

and type_parameter (env : env) ((v1, v2, v3) : CST.type_parameter) =
  let v1 = match v1 with Some x -> attribute_list env x | None -> [] in
  let v2 =
    match v2 with
    | Some x -> (
        match x with
        | `In tok -> Some (token env tok) (* "in" *)
        | `Out tok -> Some (token env tok) (* "out" *) )
    | None -> None
  in
  let v3 = identifier env v3 (* identifier *) in
  (* TODO can we throw away v1 and v2? *)
  v3

and element_binding_expression (env : env) (x : CST.element_binding_expression)
    =
  let open_br, args, close_br = bracketed_argument_list env x in
  let exprs = List.map arg_to_expr args in
  (open_br, exprs, close_br)

and nullable_type (env : env) (x : CST.nullable_type) =
  match x with
  | `Type_QMARK (v1, v2) ->
      let v1 = type_constraint env v1 in
      let v2 = token env v2 (* "?" *) in
      TyQuestion (v1, v2)

and array_type (env : env) ((v1, v2) : CST.array_type) =
  let v1 = type_constraint env v1 in
  let v2 = array_rank_specifier env v2 in
  let open_br, exps, close_br = v2 in
  let rec jag exps t =
    match exps with
    | [] -> TyArray ((open_br, None, close_br), t)
    | [ e ] -> TyArray ((open_br, e, close_br), t)
    | e :: tl -> jag tl (TyArray ((open_br, e, close_br), t))
  in
  (* TODO correct order? *)
  jag exps v1

and interpolated_verbatim_string_content (env : env)
    (x : CST.interpolated_verbatim_string_content) =
  match x with
  | `Inte_verb_str_text x -> L (interpolated_verbatim_string_text env x)
  | `Interp x -> unbracket (interpolation env x)

and array_rank_specifier (env : env) ((v1, v2, v3) : CST.array_rank_specifier) =
  let v1 = token env v1 (* "[" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_opt expression env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let v1 = token env v1 (* "," *) in
              let v2 = map_opt expression env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 = token env v3 (* "]" *) in
  (* TODO we could give each expression brackets, instead of using the same brackets for all expressions *)
  (v1, v2, v3)

and argument (env : env) ((v1, v2, v3) : CST.argument) : AST.argument =
  let v1 = map_opt name_colon env v1 in
  let v2 =
    match v2 with
    | Some x -> (
        match x with
        | `Ref tok -> Some (token env tok) (* "ref" *)
        | `Out tok -> Some (token env tok) (* "out" *)
        | `In tok -> Some (token env tok) (* "in" *) )
    | None -> None
  in
  let v3 =
    match v3 with
    | `Exp x -> expression env x
    | `Decl_exp x -> declaration_expression env x
  in
  match v1 with None -> AST.Arg v3 | Some id -> AST.ArgKwd (id, v3)

and initializer_expression (env : env)
    ((v1, v2, v3, v4) : CST.initializer_expression) : expr list AST.bracket =
  let v1 = token env v1 (* "{" *) in
  let v2 = anon_opt_cst_pat_rep_interp_alig_clause_080fdff env v2 in
  let v3 = map_opt token env v3 (* "," *) in
  let v4 = token env v4 (* "}" *) in
  (v1, v2, v4)

and switch_expression_arm (env : env)
    ((v1, v2, v3, v4) : CST.switch_expression_arm) : action =
  let v1 = pattern env v1 in
  let v2 =
    match v2 with Some x -> PatWhen (v1, when_clause env x) | None -> v1
  in
  let v3 = token env v3 (* "=>" *) in
  let v4 = expression env v4 in
  (v2, v4)

and tuple_expression (env : env) ((v1, v2, v3, v4) : CST.tuple_expression) =
  let v1 = token env v1 (* "(" *) in
  let v2 = argument env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let v1 = token env v1 (* "," *) in
        let v2 = argument env v2 in
        v2)
      v3
  in
  let v4 = token env v4 (* ")" *) in
  let exprs = List.map arg_to_expr (v2 :: v3) in
  Tuple (v1, exprs, v4)

and query_body (env : env) (x : CST.query_body) =
  match x with
  | `Rectype (v1, v2, v3) ->
      let v1 = List.map (query_clause env) v1 in
      let v2 = select_or_group_clause env v2 in
      let v3 =
        match v3 with Some x -> [ query_continuation env x ] | None -> []
      in
      v1 @ [ v2 ] @ v3

and catch_clause (env : env) ((v1, v2, v3, v4) : CST.catch_clause) =
  let v1 = token env v1 (* "catch" *) in
  let v2 =
    match v2 with
    | Some x -> catch_declaration env x
    | None -> PatUnderscore (fake "_")
  in
  let pat =
    match v3 with
    | Some x ->
        let filter = catch_filter_clause env x in
        PatWhen (v2, filter)
    | None -> v2
  in
  let v4 = block env v4 in
  (v1, pat, v4)

and ordering (env : env) ((v1, v2) : CST.ordering) =
  let v1 = expression env v1 in
  let v2 =
    match v2 with
    | Some x -> (
        match x with
        | `Asce tok -> Ascending (* "ascending" *)
        | `Desc tok -> Descending (* "descending" *) )
    | None -> Ascending
  in
  (v1, v2)

and interpolated_string_content (env : env)
    (x : CST.interpolated_string_content) =
  match x with
  | `Inte_str_text x -> L (interpolated_string_text env x)
  | `Interp x -> unbracket (interpolation env x)

and checked_expression (env : env) (x : CST.checked_expression) =
  match x with
  | `Chec_LPAR_exp_RPAR (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "checked" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 = expression env v3 in
      let v4 = token env v4 (* ")" *) in
      OtherExpr (OE_Checked, [ E v3 ])
  | `Unch_LPAR_exp_RPAR (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "unchecked" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 = expression env v3 in
      let v4 = token env v4 (* ")" *) in
      OtherExpr (OE_Unchecked, [ E v3 ])

and expression (env : env) (x : CST.expression) : AST.expr =
  match x with
  (* semgrep: *)
  | `Ellips v1 -> Ellipsis (token env v1)
  | `Deep_ellips (v1, v2, v3) ->
      let v1 = token env v1 in
      let v2 = expression env v2 in
      let v3 = token env v3 in
      DeepEllipsis (v1, v2, v3)
  | `Anon_meth_exp (v1, v2, v3, v4) ->
      let v1 =
        match v1 with
        | Some tok -> [ KeywordAttr (Async, token env tok) ] (* "async" *)
        | None -> []
      in
      let v2 = token env v2 (* "delegate" *) in
      let v3 = match v3 with Some x -> parameter_list env x | None -> [] in
      let v4 = block env v4 in
      Lambda
        { fkind = (LambdaKind, v2); fparams = v3; frettype = None; fbody = v4 }
  | `Anon_obj_crea_exp (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "new" *) in
      let v2 = token env v2 (* "{" *) in
      let v3 =
        match v3 with
        | Some (v1, v2) ->
            let v1 = anonymous_object_member_declarator env v1 in
            let v2 =
              List.map
                (fun (v1, v2) ->
                  let v1 = token env v1 (* "," *) in
                  let v2 = anonymous_object_member_declarator env v2 in
                  v2)
                v2
            in
            v1 :: v2
        | None -> []
      in
      let v4 = map_opt token env v4 (* "," *) in
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
  | `Array_crea_exp (v1, v2, v3) ->
      let v1 = token env v1 (* "new" *) in
      let v2 = array_type env v2 in
      let v3 =
        match v3 with
        | Some x -> initializer_expression env x
        | None -> fake_bracket []
      in
      let lb, _, rb = v3 in
      let args = (lb, [ ArgType v2; Arg (AST.Tuple v3) ], rb) in
      Call (IdSpecial (New, v1), args)
  | `As_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "as" *) in
      let v3 = type_ env v3 in
      (* TODO `as` is really a conditional cast *)
      Cast (v3, v1)
  | `Assign_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = assignment_operator env v2 in
      let v3 = expression env v3 in
      AssignOp (v1, v2, v3)
  | `Await_exp (v1, v2) ->
      let v1 = token env v1 (* "await" *) in
      let v2 = expression env v2 in
      Await (v1, v2)
  | `Base_exp tok ->
      let x = token env tok (* "base" *) in
      IdSpecial (Super, x)
  | `Bin_exp x -> binary_expression env x
  | `Cast_exp (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = type_constraint env v2 in
      let v3 = token env v3 (* ")" *) in
      let v4 = expression env v4 in
      Cast (v2, v4)
  | `Chec_exp x -> checked_expression env x
  | `Cond_access_exp (v1, v2, v3) ->
      (* map `a?.b` to `null == a ? null : a.b` *)
      let v1 = expression env v1 in
      let v2 = token env v2 (* "?" *) in
      let fake_null = L (Null (fake "null")) in
      let is_null =
        Call
          (IdSpecial (Op Eq, fake "="), fake_bracket [ Arg fake_null; Arg v1 ])
      in
      let access =
        match v3 with
        | `Elem_bind_exp x ->
            let x = element_binding_expression env x in
            let open_br, _, close_br = x in
            ArrayAccess (v1, (open_br, Tuple x, close_br))
        | `Member_bind_exp (x1, x2) ->
            let x1 = token env x1 (* "." *) in
            let x2 = simple_name env x2 in
            DotAccess (v1, x1, EN x2)
      in
      Conditional (is_null, fake_null, access)
  | `Cond_exp (v1, v2, v3, v4, v5) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "?" *) in
      let v3 = expression env v3 in
      let v4 = token env v4 (* ":" *) in
      let v5 = expression env v5 in
      Conditional (v1, v3, v5)
  | `Defa_exp (v1, v2) ->
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
      Call (IdSpecial (New, v1), v2)
  | `Elem_access_exp (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = element_binding_expression env v2 in
      let open_br, exprs, close_br = v2 in
      (* TODO we map multidim arrays as jagged arrays when creating arrays, with as tuples here. Does that work? Should we map this as multiple nested ArrayAccess? *)
      ArrayAccess (v1, (open_br, Tuple v2, close_br))
  | `Elem_bind_exp x -> Tuple (element_binding_expression env x)
  | `Impl_array_crea_exp (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "new" *) in
      let v2 = token env v2 (* "[" *) in
      let v3 = List.map (token env) (* "," *) v3 in
      let v4 = token env v4 (* "]" *) in
      let v5 = initializer_expression env v5 in
      Container (Array, v5)
  | `Impl_obj_crea_exp (v1, v2, v3) ->
      let v1 = token env v1 (* "new" *) in
      let v2 = argument_list env v2 in
      let v3 =
        match v3 with
        | Some x -> initializer_expression env x
        | None -> fake_bracket []
      in
      let lp, v2', rp = v2 in
      let args = (lp, v2' @ [ Arg (Tuple v3) ], rp) in
      Call (IdSpecial (New, v1), args)
  | `Impl_stack_alloc_array_crea_exp (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "stackalloc" *) in
      let _v2 = token env v2 (* "[" *) in
      let _v3 = token env v3 (* "]" *) in
      let _v4 = initializer_expression env v4 in
      todo_expr env v1
  | `Init_exp x -> Tuple (initializer_expression env x)
  | `Inte_str_exp x -> interpolated_string_expression env x
  | `Invo_exp (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = argument_list env v2 in
      AST.Call (v1, v2)
  | `Is_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "is" *) in
      let v3 = type_ env v3 in
      Call (IdSpecial (Instanceof, v2), fake_bracket [ Arg v1; ArgType v3 ])
  | `Is_pat_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "is" *) in
      let v3 = pattern env v3 in
      LetPattern (v3, v1)
  | `Lambda_exp (v1, vtodo, v2, v3, v4) ->
      let v1 =
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
            [ ParamClassic p ]
        (* identifier *)
      in
      let v3 = token env v3 (* "=>" *) in
      let v4 =
        match v4 with
        | `Blk x -> block env x
        | `Exp x -> AST.ExprStmt (expression env x, v3) |> AST.s
      in
      Lambda { fkind = (Arrow, v3); fparams = v2; frettype = None; fbody = v4 }
  | `Make_ref_exp (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "__makeref" *) in
      let _v2 = token env v2 (* "(" *) in
      let v3 = expression env v3 in
      let _v4 = token env v4 (* ")" *) in
      Ref (v1, v3)
  | `Member_access_exp (v1, v2, v3) ->
      let v1 =
        match v1 with
        | `Exp x -> expression env x
        | `Pred_type x ->
            (* e.g. `int` in `int.maxValue` *)
            let id = str env x in
            N (Id (id, empty_id_info ()))
        | `Name x -> N (name env x)
      in
      let v2 =
        match v2 with
        | `DOT tok -> token env tok (* "." *)
        | `DASHGT tok -> token env tok
        (* "->" *)
      in
      let v3 = simple_name env v3 in
      AST.DotAccess (v1, v2, AST.EN v3)
  | `Obj_crea_exp (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "new" *) in
      let v2 = type_constraint env v2 in
      let v3 =
        match v3 with Some x -> argument_list env x | None -> fake_bracket []
      in
      let v4 =
        match v4 with
        | Some x -> initializer_expression env x
        | None -> fake_bracket []
      in
      let lp, v3', rp = v3 in
      let args = (lp, (ArgType v2 :: v3') @ [ Arg (Tuple v4) ], rp) in
      Call (IdSpecial (New, v1), args)
  | `Paren_exp x -> parenthesized_expression env x
  | `Post_un_exp x -> postfix_unary_expression env x
  | `Prefix_un_exp x -> prefix_unary_expression env x
  | `Query_exp (v1, v2) ->
      let v1 = from_clause env v1 in
      let v2 = query_body env v2 in
      linq_to_expr v1 v2
  | `Range_exp (v1, v2, v3) ->
      let fake_zero = L (Int (Some 0, fake "0")) in
      let v1 = match v1 with Some x -> expression env x | None -> fake_zero in
      let v2 = token env v2 (* ".." *) in
      let v3 =
        match v3 with
        | Some x -> expression env x
        | None -> new_index_from_end v2 fake_zero
      in
      Call (IdSpecial (Op Range, v2), fake_bracket [ Arg v1; Arg v3 ])
  | `Ref_exp (v1, v2) ->
      let v1 = token env v1 (* "ref" *) in
      let v2 = expression env v2 in
      AST.Ref (v1, v2)
  | `Ref_type_exp (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "__reftype" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 = expression env v3 in
      let v4 = token env v4 (* ")" *) in
      Call (IdSpecial (Typeof, v1), (v2, [ Arg (DeRef (v1, v3)) ], v4))
  | `Ref_value_exp (v1, v2, v3, v4, v5, v6) ->
      let v1 = token env v1 (* "__refvalue" *) in
      let _v2 = token env v2 (* "(" *) in
      let v3 = expression env v3 in
      let _v4 = token env v4 (* "," *) in
      let _v5 = type_constraint env v5 in
      let _v6 = token env v6 (* ")" *) in
      DeRef (v1, v3)
  | `Size_of_exp (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "sizeof" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 = type_constraint env v3 in
      let v4 = token env v4 (* ")" *) in
      Call (IdSpecial (Sizeof, v1), (v2, [ ArgType v3 ], v4))
  | `Stack_alloc_array_crea_exp (v1, v2, v3) ->
      let v1 = token env v1 (* "stackalloc" *) in
      let _v2 = array_type env v2 in
      let _v3 =
        match v3 with
        | Some x -> initializer_expression env x
        | None -> fake_bracket [ todo_expr env v1 ]
      in
      todo_expr env v1
  | `Switch_exp (v1, v2, v3, v4, vtodo, v5) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "switch" *) in
      let v3 = token env v3 (* "{" *) in
      let v4 =
        match v4 with
        | Some (v1, v2) ->
            let v1 = switch_expression_arm env v1 in
            let v2 =
              List.map
                (fun (v1, v2) ->
                  let v1 = token env v1 (* "," *) in
                  let v2 = switch_expression_arm env v2 in
                  v2)
                v2
            in
            v1 :: v2
        | None -> []
      in
      let v5 = token env v5 (* "}" *) in
      MatchPattern (v1, v4)
  | `This_exp tok ->
      let t = token env tok (* "this" *) in
      IdSpecial (This, t)
  | `Throw_exp (v1, v2) ->
      let v1 = token env v1 (* "throw" *) in
      let v2 = expression env v2 in
      let throw = Throw (v1, v2, sc) in
      OtherExpr (OE_StmtExpr, [ AST.S (AST.s throw) ])
  | `Tuple_exp x -> tuple_expression env x
  | `Type_of_exp (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "typeof" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 = type_constraint env v3 in
      let v4 = token env v4 (* ")" *) in
      Call (IdSpecial (Typeof, v1), (v2, [ ArgType v3 ], v4))
  | `With_exp (v1, v2, v3, v4, v5) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "with" *) in
      let v3 = token env v3 (* "{" *) in
      let v4 =
        match v4 with Some x -> with_initializer_expression env x | None -> []
      in
      let v5 = token env v5 (* "}" *) in
      let with_fields = AST.Record (v3, v4, v5) in
      (* THINK:
       * - with-expressions may deserve first-class support in Generic AST
       * - record patterns perhaps should match with-expressions
       *)
      AST.OtherExpr (AST.OE_RecordWith, [ AST.E v1; AST.E with_fields ])
  | `Simple_name x -> N (simple_name env x)
  | `Lit x ->
      let x = literal env x in
      AST.L x

and simple_assignment_expression (env : env)
    ((v1, v2, v3) : CST.simple_assignment_expression) : AST.field =
  let v1 = identifier env v1 in
  let _v2 = token env v2 (* "=" *) in
  let v3 = expression env v3 in
  AST.basic_field v1 (Some v3) None

and simple_name (env : env) (x : CST.simple_name) : AST.name =
  match x with
  | `Gene_name (v1, v2) ->
      let v1 = identifier env v1 (* identifier *) in
      let v2 = type_argument_list env v2 in
      IdQualified
        ( (v1, { name_qualifier = None; name_typeargs = Some v2 }),
          empty_id_info () )
  | `Choice_global x -> Id (identifier_or_global env x, empty_id_info ())

and switch_body (env : env) ((v1, v2, v3) : CST.switch_body) :
    case_and_body list =
  let v1 = token env v1 (* "{" *) in
  let v2 = List.map (switch_section env) v2 in
  let v3 = token env v3 (* "}" *) in
  v2

and anon_choice_param_ce11a32 (env : env) (x : CST.anon_choice_param_ce11a32) =
  match x with
  | `Param x -> ParamClassic (parameter env x)
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

and anon_opt_cst_pat_rep_interp_alig_clause_080fdff (env : env)
    (opt : CST.anon_opt_cst_pat_rep_interp_alig_clause_080fdff) : expr list =
  match opt with
  | Some (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = List.map (interpolation_alignment_clause env) v2 in
      v1 :: v2
  | None -> []

and type_parameter_list (env : env) ((v1, v2, v3, v4) : CST.type_parameter_list)
    =
  let v1 = token env v1 (* "<" *) in
  let v2 = type_parameter env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let v1 = token env v1 (* "," *) in
        let v2 = type_parameter env v2 in
        v2)
      v3
  in
  let v4 = token env v4 (* ">" *) in
  v2 :: v3

and type_parameter_constraint (env : env) (x : CST.type_parameter_constraint) =
  match x with
  | `Class_opt_QMARK (tok, _)
  (* "class" *)
  (* TODO handle question mark *)
  | `Struct tok (* "struct" *)
  | `Notn tok (* "notnull" *)
  | `Unma tok ->
      (* "unmanaged" *)
      let t = TyBuiltin (str env tok) in
      Extends t
  | `Cons_cons (v1, v2, v3) ->
      let v1 = token env v1 (* "new" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 = token env v3 (* ")" *) in
      let tok = PI.combine_infos v1 [ v2; v3 ] in
      HasConstructor tok
  | `Type_cons x -> Extends (type_constraint env x)

and type_constraint (env : env) (x : CST.type_constraint) : type_ =
  (* can't be `var` *)
  type_ env x

and local_variable_type (env : env) (x : CST.type_constraint) : type_ option =
  match x with `Impl_type tok -> None (* "var" *) | x -> Some (type_ env x)

and expr_statement (env : env) (x : CST.expression_statement) : stmt =
  match x with
  | `Exp_SEMI (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ";" *) in
      AST.ExprStmt (v1, v2) |> AST.s
  | `Ellips_SEMI (v1, v2) ->
      let v1 = token env v1 in
      let v2 = token env v2 in
      AST.ExprStmt (AST.Ellipsis v1, v2) |> AST.s
  | `Ellips v1 ->
      let v1 = token env v1 in
      let v2 = AST.sc in
      AST.ExprStmt (AST.Ellipsis v1, v2) |> AST.s

and statement (env : env) (x : CST.statement) =
  match x with
  | `Blk x -> block env x
  | `Brk_stmt (v1, v2) ->
      let v1 = token env v1 (* "break" *) in
      let v2 = token env v2 (* ";" *) in
      AST.Break (v1, AST.LNone, v2) |> AST.s
  | `Chec_stmt (v1, v2) ->
      let v1 =
        match v1 with
        | `Chec tok -> OSWS_CheckedBlock (* "checked" *)
        | `Unch tok -> OSWS_UncheckedBlock
        (* "unchecked" *)
      in
      let v2 = block env v2 in
      OtherStmtWithStmt (v1, None, v2) |> AST.s
  | `Cont_stmt (v1, v2) ->
      let v1 = token env v1 (* "continue" *) in
      let v2 = token env v2 (* ";" *) in
      Continue (v1, LNone, v2) |> AST.s
  | `Do_stmt (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = token env v1 (* "do" *) in
      let v2 = statement env v2 in
      let v3 = token env v3 (* "while" *) in
      let v4 = token env v4 (* "(" *) in
      let v5 = expression env v5 in
      let v6 = token env v6 (* ")" *) in
      let v7 = token env v7 (* ";" *) in
      DoWhile (v1, v2, v5) |> AST.s
  | `Empty_stmt tok ->
      let v1 = token env tok (* ";" *) in
      Block (v1, [], v1) |> AST.s
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
      let v1 = map_opt token env v1 (* "await" *) in
      let v2 = token env v2 (* "foreach" *) in
      let v3 = token env v3 (* "(" *) in
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
            match v1 with Some t -> PatTyped (v2, t) | None -> v2 )
        | `Exp x -> H2.expr_to_pattern (expression env x)
      in
      let v5 = token env v5 (* "in" *) in
      let v6 = expression env v6 in
      let v6 =
        match v1 with Some tok -> Await (tok, v6) (* "await" *) | None -> v6
      in
      let v7 = token env v7 (* ")" *) in
      let v8 = statement env v8 in
      For (v2, ForEach (v4, v5, v6), v8) |> AST.s
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 = token env v1 (* "for" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 =
        match v3 with
        | Some x -> (
            match x with
            | `Var_decl x ->
                List.map
                  (fun (e, v) -> ForInitVar (e, v))
                  (variable_declaration env x)
            | `Exp_rep_COMMA_exp (v1, v2) ->
                let v1 = expression env v1 in
                let v2 = List.map (interpolation_alignment_clause env) v2 in
                let exprs = v1 :: v2 in
                List.map (fun e -> ForInitExpr e) exprs )
        | None -> []
      in
      let v4 = token env v4 (* ";" *) in
      let v5 = map_opt expression env v5 in
      let v6 = token env v6 (* ";" *) in
      let v7 = anon_opt_cst_pat_rep_interp_alig_clause_080fdff env v7 in
      let v8 = token env v8 (* ")" *) in
      let v9 = statement env v9 in
      let next =
        match v7 with
        | [] -> None
        | [ e ] -> Some e
        | exprs -> Some (Tuple (v6, v7, v8))
      in
      let for_header = ForClassic (v3, v5, next) in
      For (v1, for_header, v9) |> AST.s
  | `Goto_stmt (v1, v2, v3) -> (
      let v1 = token env v1 (* "goto" *) in
      let v3 = token env v3 (* ";" *) in
      match v2 with
      | `Id tok ->
          let label = identifier env tok (* identifier *) in
          Goto (v1, label) |> AST.s
      | `Case_exp (v1, v2) ->
          let v1 = token env v1 (* "case" *) in
          let _v2 = expression env v2 in
          todo_stmt env v1
      | `Defa tok ->
          let tok = token env tok (* "default" *) in
          todo_stmt env tok )
  | `If_stmt (v1, v2, v3, v4, v5, v6) ->
      let v1 = token env v1 (* "if" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 = expression env v3 in
      let v4 = token env v4 (* ")" *) in
      let v5 = statement env v5 in
      let v6 =
        match v6 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* "else" *) in
            let v2 = statement env v2 in
            Some v2
        | None -> None
      in
      AST.If (v1, v3, v5, v6) |> AST.s
  | `Labe_stmt (v1, v2, v3) ->
      let v1 = identifier env v1 (* identifier *) in
      let v2 = token env v2 (* ":" *) in
      let v3 = statement env v3 in
      Label (v1, v3) |> AST.s
  | `Local_decl_stmt (v1, v2, v3, v4, v5) ->
      let v1_todo = map_opt token env v1 (* "await" *) in
      let v2_todo = map_opt token env v2 (* "using" *) in
      let v3 = List.map (modifier env) v3 in
      let v4 = variable_declaration env v4 in
      let v5 = token env v5 (* ";" *) in
      var_def_stmt v4 v3
  | `Local_func_stmt (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 = List.concat_map (attribute_list env) v1 in
      let v2 = List.map (modifier env) v2 in
      let v3 = return_type env v3 in
      let v4 = identifier env v4 (* identifier *) in
      let _, tok = v4 in
      let v5 =
        match v5 with Some x -> type_parameter_list env x | None -> []
      in
      let v6 = parameter_list env v6 in
      let v7 = List.map (type_parameter_constraints_clause env) v7 in
      let v8 = function_body env v8 in
      let tparams = type_parameters_with_constraints v5 v7 in
      let idinfo = empty_id_info () in
      let ent = { name = EN (Id (v4, idinfo)); attrs = v1 @ v2; tparams } in
      let def =
        AST.FuncDef
          {
            fkind = (AST.Method, tok);
            fparams = v6;
            frettype = Some v3;
            fbody = v8;
          }
      in
      AST.DefStmt (ent, def) |> AST.s
  | `Lock_stmt (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "lock" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 = expression env v3 in
      let v4 = token env v4 (* ")" *) in
      let v5 = statement env v5 in
      OtherStmt (OS_Sync, [ E v3; S v5 ]) |> AST.s
  | `Ret_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "return" *) in
      let v2 = map_opt expression env v2 in
      let v3 = token env v3 (* ";" *) in
      Return (v1, v2, v3) |> AST.s
  | `Switch_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "switch" *) in
      let v2 =
        match v2 with
        | `LPAR_exp_RPAR v2 -> parenthesized_expression env v2
        | `Tuple_exp v2 -> tuple_expression env v2
      in
      let v3 = switch_body env v3 in
      AST.Switch (v1, Some v2, v3) |> AST.s
  | `Throw_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "throw" *) in
      let v2 = map_opt expression env v2 in
      let v3 = token env v3 (* ";" *) in
      ( match v2 with
      | Some expr -> Throw (v1, expr, v3)
      | None -> OtherStmt (OS_ThrowNothing, [ Tk v1; Tk v3 ]) )
      |> AST.s
  | `Try_stmt (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "try" *) in
      let v2 = block env v2 in
      let v3 = List.map (catch_clause env) v3 in
      let v4 = map_opt finally_clause env v4 in
      Try (v1, v2, v3, v4) |> AST.s
  | `Unsafe_stmt (v1, v2) ->
      let v1 = token env v1 (* "unsafe" *) in
      let v2 = block env v2 in
      OtherStmtWithStmt (OSWS_UnsafeBlock, None, v2) |> AST.s
  | `Using_stmt (v1, v2, v3, v4, v5, v6) ->
      let v1 = map_opt token env v1 (* "await" *) in
      let v2 = token env v2 (* "using" *) in
      let v3 = token env v3 (* "(" *) in
      let v4 =
        match v4 with
        | `Var_decl x ->
            let v4 = variable_declaration env x in
            var_def_stmt v4 []
        | `Exp x ->
            let expr = expression env x in
            ExprStmt (expr, sc) |> AST.s
      in
      let v5 = token env v5 (* ")" *) in
      let v6 = statement env v6 in
      WithUsingResource (v2, v4, v6) |> AST.s
  | `While_stmt (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "while" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 = expression env v3 in
      let v4 = token env v4 (* ")" *) in
      let v5 = statement env v5 in
      While (v1, v3, v5) |> AST.s
  | `Yield_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "yield" *) in
      let v2 =
        match v2 with
        | `Ret_exp (v1, v2) ->
            let v1 = token env v1 (* "return" *) in
            let v2 = expression env v2 in
            Some v2
        | `Brk tok -> None
        (* "break" *)
      in
      let v3 = token env v3 (* ";" *) in
      ExprStmt (Yield (v1, v2, false), v3) |> AST.s

and interpolated_string_expression (env : env)
    (x : CST.interpolated_string_expression) =
  let x =
    match x with
    | `DOLLARDQUOT_rep_inte_str_content_DQUOT (v1, v2, v3) ->
        let v1 = token env v1 (* "$\"" *) in
        let v2 = List.map (interpolated_string_content env) v2 in
        let v3 = token env v3 (* "\"" *) in
        (v1, v2, v3)
    | `ATDOLLARDQUOT_rep_inte_verb_str_content_DQUOT (v1, v2, v3)
    | `DOLLARATDQUOT_rep_inte_verb_str_content_DQUOT (v1, v2, v3) ->
        let v1 = token env v1 (* "$@\"" or "@$\"" *) in
        let v2 = List.map (interpolated_verbatim_string_content env) v2 in
        let v3 = token env v3 (* "\"" *) in
        (v1, v2, v3)
  in
  let v1, v2, v3 = x in
  let args = fake_bracket (List.map (fun e -> Arg e) v2) in
  (* TODO should we use FString here instead of InterpolatedConcat? *)
  Call (IdSpecial (ConcatString InterpolatedConcat, v1), args)

and tuple_element (env : env) ((v1, v2) : CST.tuple_element) =
  let v1 = type_constraint env v1 in
  let v2_todo = map_opt identifier env v2 (* identifier *) in
  v1

and constant_pattern (env : env) (x : CST.constant_pattern) =
  H2.expr_to_pattern (expression env x)

and catch_declaration (env : env) ((v1, v2, v3, v4) : CST.catch_declaration) =
  let v1 = token env v1 (* "(" *) in
  let v2 = type_constraint env v2 in
  let v3 = map_opt identifier env v3 (* identifier *) in
  let v4 = token env v4 (* ")" *) in
  let var =
    match v3 with Some ident -> Some (ident, empty_id_info ()) | None -> None
  in
  PatVar (v2, var)

and case_pattern_switch_label (env : env)
    ((v1, v2, v3, v4) : CST.case_pattern_switch_label) =
  let v1 = token env v1 (* "case" *) in
  let v2 = pattern env v2 in
  let v3 =
    match v3 with Some x -> PatWhen (v2, when_clause env x) | None -> v2
  in
  let v4 = token env v4 (* ":" *) in
  AST.Case (v1, v3)

and query_clause (env : env) (x : CST.query_clause) =
  match x with
  | `From_clause x -> from_clause env x
  | `Join_clause (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) ->
      let v1 = token env v1 (* "join" *) in
      let v2 = map_opt type_constraint env v2 in
      let v3 = identifier env v3 (* identifier *) in
      let v4 = token env v4 (* "in" *) in
      let v5 = expression env v5 in
      let v6 = token env v6 (* "on" *) in
      let v7 = expression env v7 in
      let v8 = token env v8 (* "equals" *) in
      let v9 = expression env v9 in
      let v10 = map_opt join_into_clause env v10 in
      Join (v1, (v2, v3), v5, v7, v9, v10)
  | `Let_clause (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "let" *) in
      let v2 = identifier env v2 (* identifier *) in
      let v3 = token env v3 (* "=" *) in
      let v4 = expression env v4 in
      Let (v1, v2, v4)
  | `Order_by_clause (v1, v2, v3) ->
      let v1 = token env v1 (* "orderby" *) in
      let v2 = ordering env v2 in
      let v3 =
        List.map
          (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
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
        | `Name_colon x -> Some (name_colon env x) )
    | None -> None
  in
  let v2 = expression env v2 in
  match v1 with Some name -> ArgKwd (name, v2) | None -> Arg v2

and catch_filter_clause (env : env) ((v1, v2, v3, v4) : CST.catch_filter_clause)
    =
  let v1 = token env v1 (* "when" *) in
  let v2 = token env v2 (* "(" *) in
  let v3 = expression env v3 in
  let v4 = token env v4 (* ")" *) in
  v3

and formal_parameter_list (env : env) ((v1, v2) : CST.formal_parameter_list) =
  let v1 = anon_choice_param_ce11a32 env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let v1 = token env v1 (* "," *) in
        let v2 = anon_choice_param_ce11a32 env v2 in
        v2)
      v2
  in
  v1 :: v2

and equals_value_clause (env : env) ((v1, v2) : CST.equals_value_clause) : expr
    =
  let v1 = token env v1 (* "=" *) in
  let v2 = expression env v2 in
  v2

and case_switch_label (env : env) ((v1, v2, v3) : CST.case_switch_label) =
  let v1 = token env v1 (* "case" *) in
  let v2 = expression env v2 in
  let v3 = token env v3 (* ":" *) in
  AST.CaseEqualExpr (v1, v2)

and switch_section (env : env) ((v1, v2) : CST.switch_section) : case_and_body =
  let v1 =
    List.map
      (fun x ->
        match x with
        | `Case_switch_label x -> case_switch_label env x
        | `Case_pat_switch_label x -> case_pattern_switch_label env x
        | `Defa_switch_label x -> default_switch_label env x)
      v1
  in
  let v2 = List.map (statement env) v2 in
  (* TODO: we convert list of statements to a block with fake brackets. Does this make sense? *)
  CasesAndBody (v1, stmt1 v2)

and attribute_list (env : env) ((v1, v2, v3, v4, v5, v6) : CST.attribute_list) :
    attribute list =
  (* TODO: Handle unused tokens. *)
  let _v1 = token env v1 (* "[" *) in
  let _v2 = map_opt attribute_target_specifier env v2 in
  let v3 = attribute env v3 in
  let v4 =
    List.map
      (fun (x, y) ->
        token env x (* "," *) |> ignore;
        attribute env y)
      v4
  in
  let _v5 = map_opt token env v5 (* "," *) in
  let _v6 = token env v6 (* "]" *) in
  v3 :: v4

and bracketed_argument_list (env : env)
    ((v1, v2, v3, v4) : CST.bracketed_argument_list) =
  let v1 = token env v1 (* "[" *) in
  let v2 = argument env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let v1 = token env v1 (* "," *) in
        let v2 = argument env v2 in
        v2)
      v3
  in
  let v4 = token env v4 (* "]" *) in
  (v1, v2 :: v3, v4)

and pattern (env : env) (x : CST.pattern) : AST.pattern =
  match x with
  | `Cst_pat x -> constant_pattern env x
  | `Decl_pat (v1, v2) ->
      let v1 = type_constraint env v1 in
      let v2 = variable_designation env v2 in
      PatTyped (v2, v1)
  | `Disc tok -> PatUnderscore (token env tok) (* "_" *)
  | `Recu_pat (v1, v2, v3) -> recursive_pattern env (v1, v2, v3)
  | `Var_pat (v1, v2) ->
      let v1 = token env v1 (* "var" *) in
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
      let x = type_pattern env x in
      todo_pat env (_fake "TODO_type_pattern")

and recursive_pattern env (v1, v2, v3) =
  let pat =
    match v2 with
    | `Posi_pat_clause_opt_prop_pat_clause (v1, v2) ->
        let v1 = positional_pattern_clause env v1 in
        let v2 =
          match v2 with
          | None -> None
          | Some x -> Some (todo_pat env (_fake "TODO_recursive_pattern"))
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
      todo_pat env (_fake "TODO_recursive_pattern")

and positional_pattern_clause (env : env)
    ((v1, v2, v3) : CST.positional_pattern_clause) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, v2, v3, v4) ->
        let v1 = subpattern env v1 in
        let v3 = subpattern env v3 in
        let v4 = List.map (fun (v1, v2) -> subpattern env v2) v4 in
        v1 :: v3 :: v4
    | None -> []
  in
  let v3 = token env v3 (* ")" *) in
  PatTuple (v1, v2, v3)

and subpattern (env : env) ((v1, v2) : CST.subpattern) =
  let v1_todo = map_opt name_colon env v1 in
  let v2 = pattern env v2 in
  v2

and property_pattern_clause (env : env)
    ((v1, v2, v3, v4) : CST.property_pattern_clause) =
  let v1 = token env v1 (* "{" *) in
  let _v2 = match v2 with Some x -> [ todo_pat env v1 ] | None -> [] in
  let v4 = token env v4 (* "}" *) in
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
      FieldStmt (exprstmt expr)

and function_body (env : env) (x : CST.function_body) =
  match x with
  | `Blk x -> block env x
  | `Arrow_exp_clause_SEMI (v1, v2) ->
      let v1 = arrow_expression_clause env v1 in
      let v2 = token env v2 (* ";" *) in
      let arrow, expr = v1 in
      ExprStmt (expr, arrow) (* TODO Or return Block? *) |> AST.s
  | `SEMI tok ->
      let _ = token env tok (* ";" *) in
      empty_fbody

and finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 = token env v1 (* "finally" *) in
  let v2 = block env v2 in
  (v1, v2)

and parameter (env : env) ((v1, v2, v3, v4, v5) : CST.parameter) =
  (*
    [FromBody] ref string param1 = "default"
        v1     v2   v3     v4      v5
    TODO: add v2 as a keyword attribute? Pass v2 to parameter_modifier.
  *)
  let v1 = List.concat_map (attribute_list env) v1 in
  let v3 = map_opt type_constraint env v3 in
  let v4 = identifier env v4 (* identifier *) in
  let v5 = map_opt equals_value_clause env v5 in
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
  let v2 = map_opt type_constraint env v2 in
  let v3 = identifier env v3 (* identifier *) in
  let v4 = token env v4 (* "in" *) in
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
  AST.NamedAttr (fake "[", v1, v2)

and argument_list (env : env) ((v1, v2, v3) : CST.argument_list) :
    AST.arguments bracket =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = argument env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let v1 = token env v1 (* "," *) in
              let v2 = argument env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 = token env v3 (* ")" *) in
  (v1, v2, v3)

and type_ (env : env) (x : CST.type_) : AST.type_ =
  match x with
  | `Impl_type tok ->
      (* When type_ is called, we expect an explicit type, not "var".
         The implicit type is handled in local_variable_type. *)
      raise Impossible
  | `Array_type x -> array_type env x
  | `Name x ->
      let n = name env x in
      let ids = ids_of_name n in
      TyN (H2.name_of_ids ids)
  | `Null_type x -> nullable_type env x
  | `Poin_type (v1, v2) ->
      let v1 = type_constraint env v1 in
      let v2 = token env v2 (* "*" *) in
      TyPointer (v2, v1)
  | `Func_poin_type (v1, v2, _v3, v4, _v5, _v6, v7) ->
      let v1 = token env v1 (* "delegate" *) in
      let _v2 = token env v2 (* "*" *) in
      let _v4 = token env v4 (* "<" *) in
      let _v7 = token env v7 (* ">" *) in
      todo_type env v1
  | `Pred_type tok -> predefined_type env tok (* predefined_type *)
  | `Tuple_type (v1, v2, v3, v4, v5, v6) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = tuple_element env v2 in
      let v3 = token env v3 (* "," *) in
      let v4 = tuple_element env v4 in
      let v5 =
        List.map
          (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = tuple_element env v2 in
            v2)
          v5
      in
      let v6 = token env v6 (* ")" *) in
      TyTuple (v1, v2 :: v4 :: v5, v6)

and type_argument_list (env : env) ((v1, v2, v3) : CST.type_argument_list) =
  let v1 = token env v1 (* "<" *) in
  let v2 =
    match v2 with
    | `Rep_COMMA xs -> [] (* TODO What's this case? <,,,>? *)
    | `Type_rep_COMMA_type (v1, v2) ->
        let v1 = type_constraint env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let v1 = token env v1 (* "," *) in
              let v2 = type_constraint env v2 in
              v2)
            v2
        in
        v1 :: v2
  in
  let v3 = token env v3 (* ">" *) in
  List.map (fun t -> TypeArg t) v2

and type_parameter_constraints_clause (env : env)
    ((v1, v2, v3, v4, v5) : CST.type_parameter_constraints_clause) =
  let v1 = token env v1 (* "where" *) in
  let v2 = identifier_or_global env v2 in
  let v3 = token env v3 (* ":" *) in
  let v4 = type_parameter_constraint env v4 in
  let v5 =
    List.map
      (fun (v1, v2) ->
        let v1 = token env v1 (* "," *) in
        let v2 = type_parameter_constraint env v2 in
        v2)
      v5
  in
  (v2, v4 :: v5)

and parameter_list (env : env) ((v1, v2, v3) : CST.parameter_list) :
    parameter list =
  let v1 = token env v1 (* "(" *) in
  let v2 = match v2 with Some x -> formal_parameter_list env x | None -> [] in
  let v3 = token env v3 (* ")" *) in
  v2

and attribute_argument_list (env : env)
    ((v1, v2, v3) : CST.attribute_argument_list) : arguments bracket =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = attribute_argument env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let v1 = token env v1 (* "," *) in
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
      let v3 = token env v3 (* "by" *) in
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
      let ent = basic_entity v2 [] in
      let vardef = { vinit = None; vtype = Some t } in
      OtherExpr (OE_StmtExpr, [ S (s (DefStmt (ent, VarDef vardef))) ])
  | None -> N (Id (v2, empty_id_info ()))

and interpolation (env : env) ((v1, v2, v3, v4, v5) : CST.interpolation) :
    expr bracket =
  let v1 = token env v1 (* "{" *) in
  let v2 = expression env v2 in
  let v3_todo = map_opt interpolation_alignment_clause env v3 in
  let v4_todo = map_opt interpolation_format_clause env v4 in
  let v5 = token env v5 (* "}" *) in
  (v1, v2, v5)

let explicit_interface_specifier (env : env)
    ((v1, v2) : CST.explicit_interface_specifier) =
  let v1 = name env v1 in
  let v2 = token env v2 (* "." *) in
  v1

let accessor_declaration (env : env)
    ((v1, v2, v3, v4) : CST.accessor_declaration) =
  let v1 = List.concat_map (attribute_list env) v1 in
  let v2 = List.map (modifier env) v2 in
  let v3 =
    match v3 with
    | `Get tok -> (str env tok, KeywordAttr (Getter, token env tok)) (* "get" *)
    | `Set tok -> (str env tok, KeywordAttr (Setter, token env tok)) (* "set" *)
    | `Add tok ->
        (str env tok, unhandled_keywordattr_to_namedattr env tok) (* "add" *)
    | `Remove tok ->
        (str env tok, unhandled_keywordattr_to_namedattr env tok) (* "remove" *)
    | `Init tok ->
        (str env tok, unhandled_keywordattr_to_namedattr env tok) (* "init" *)
    | `Id tok ->
        let _, tok = identifier env tok in
        (("Todo", tok), todo_attr env tok)
  in
  let v4 = function_body env v4 in
  let id, attr = v3 in
  ((attr :: v1) @ v2, id, v4)

let bracketed_parameter_list (env : env)
    ((v1, v2, v3, v4) : CST.bracketed_parameter_list) =
  let v1 = token env v1 (* "[" *) in
  let v2 = parameter env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let v1 = token env v1 (* "," *) in
        let v2 = parameter env v2 in
        v2)
      v3
  in
  let v4 = token env v4 (* "]" *) in
  List.map (fun p -> ParamClassic p) (v2 :: v3)

let constructor_initializer (env : env)
    ((v1, v2, v3) : CST.constructor_initializer) =
  let v1 = token env v1 (* ":" *) in
  let v2 =
    match v2 with
    | `Base tok -> IdSpecial (Super, token env tok) (* "base" *)
    | `This tok -> IdSpecial (This, token env tok)
    (* "this" *)
  in
  let v3 = argument_list env v3 in
  ExprStmt (Call (v2, v3), sc) |> AST.s

let enum_member_declaration (env : env)
    ((v1, v2, v3) : CST.enum_member_declaration) =
  let v1 = List.concat_map (attribute_list env) v1 in
  let v2 = identifier env v2 (* identifier *) in
  let v3 = map_opt equals_value_clause env v3 in
  OrEnum (v2, v3)

let base_list (env : env) ((v1, v2, v3) : CST.base_list) =
  let v1 = token env v1 (* ":" *) in
  let v2 = type_constraint env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let v1 = token env v1 (* "," *) in
        let v2 = type_constraint env v2 in
        v2)
      v3
  in
  v2 :: v3

let accessor_list (env : env) ((v1, v2, v3) : CST.accessor_list) =
  let v1 = token env v1 (* "{" *) in
  let v2 = List.map (accessor_declaration env) v2 in
  let v3 = token env v3 (* "}" *) in
  (v1, v2, v3)

let enum_member_declaration_list (env : env)
    ((v1, v2, v3, v4) : CST.enum_member_declaration_list) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = enum_member_declaration env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let v1 = token env v1 (* "," *) in
              let v2 = enum_member_declaration env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 = map_opt token env v3 (* "," *) in
  let v4 = token env v4 (* "}" *) in
  v2

let rec declaration_list (env : env)
    ((open_bracket, body, close_bracket) : CST.declaration_list) =
  let xs = List.map (declaration env) body in
  (token env open_bracket, xs, token env close_bracket)

and extern_alias_directive (env : env)
    ((v1, v2, v3, v4) : CST.extern_alias_directive) =
  let v1 = token env v1 (* "extern" *) in
  let v2 = token env v2 (* "alias" *) in
  let v3 = identifier env v3 (* identifier *) in
  let v4 = token env v4 (* ";" *) in
  let extern =
    AST.OtherDirective
      (AST.OI_Extern, [ AST.Tk v1; AST.Tk v2; AST.I v3; AST.Tk v4 ])
  in
  AST.DirectiveStmt extern |> AST.s

and using_directive (env : env) ((v1, v2, v3, v4) : CST.using_directive) =
  let v1 = token env v1 (* "using" *) in
  let v3 = name env v3 in
  let v4 = token env v4 (* ";" *) in
  let import =
    match v2 with
    | Some x -> (
        match x with
        | `Static tok ->
            (* "static" *)
            (* using static System.Math; *)
            (* THINK: The generic AST is undistinguishable from that of `using Foo`. *)
            AST.ImportAll (v1, AST.DottedName (ids_of_name v3), v4)
        | `Name_equals x ->
            (* using Foo = System.Text; *)
            let alias = name_equals env x in
            AST.ImportAs
              ( v1,
                AST.DottedName (ids_of_name v3),
                Some (alias, empty_id_info ()) ) )
    | None ->
        (* using System.IO; *)
        AST.ImportAll (v1, AST.DottedName (ids_of_name v3), v4)
  in
  AST.DirectiveStmt import |> AST.s

and global_attribute_list (env : env)
    ((v1, v2, v3, v4, v5) : CST.global_attribute_list) =
  let v1 = token env v1 (* "[" *) in
  let v2 =
    match v2 with
    | `Asse tok -> token env tok (* "assembly" *)
    | `Module tok -> token env tok
    (* "module" *)
  in
  let v3 = token env v3 (* ":" *) in
  let v4 =
    match v4 with
    | Some (v1, v2) ->
        let v1 = attribute env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let v1 = token env v1 (* "," *) in
              let v2 = attribute env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v5 = token env v5 (* "]" *) in
  let anys = List.map (fun a -> At a) v4 in
  ExprStmt (OtherExpr (OE_Annot, anys), v1) |> AST.s

and global_statement (env : env) (x : CST.global_statement) = statement env x

and namespace_member_declaration (env : env)
    (x : CST.namespace_member_declaration) =
  match x with
  | `Name_decl x -> namespace_declaration env x
  | `Type_decl x -> type_declaration env x

and compilation_unit (env : env) (xs : CST.compilation_unit) : any =
  match xs with
  | `Rep_extern_alias_dire_rep_using_dire_rep_global_attr_list_rep_global_stmt_rep_name_member_decl
      (v1, v2, v3, v4, v5) ->
      let v1 = v1 |> List.map (extern_alias_directive env) in
      let v2 = v2 |> List.map (using_directive env) in
      let v3 = v3 |> List.map (global_attribute_list env) in
      let v4 = v4 |> List.map (global_statement env) in
      let v5 = v5 |> List.map (namespace_member_declaration env) in
      AST.Pr (List.concat [ v1; v2; v3; v4; v5 ])
  | `Semg_exp (_v1, v2) ->
      let v2 = expression env v2 in
      AST.E v2

and namespace_declaration (env : env)
    ((v1, v2, v3, v4) : CST.namespace_declaration) =
  (*
        namespace MySpace { ... } ;
            v1       v2      v3  v4
      *)
  let v1 = token env v1 (* "namespace" *) in
  let v2 = name env v2 in
  let open_brace, decls, close_brace = declaration_list env v3 in
  let body = AST.Block (open_brace, decls, close_brace) |> AST.s in
  let ent = { name = EN v2; attrs = []; tparams = [] } in
  let mkind = AST.ModuleStruct (None, [ body ]) in
  let def = { AST.mbody = mkind } in
  AST.DefStmt (ent, AST.ModuleDef def) |> AST.s

and type_declaration (env : env) (x : CST.type_declaration) : stmt =
  match x with
  | `Class_decl x -> class_interface_struct env Class x
  | `Inte_decl x -> class_interface_struct env Interface x
  | `Enum_decl x -> enum_declaration env x
  | `Record_decl x -> record_declaration env x
  | `Dele_decl x -> delegate_declaration env x
  | `Struct_decl x -> class_interface_struct env Class x

and class_interface_struct (env : env) class_kind
    (v1, v2, v3, v4, v5, v6, v7, v8, v9) =
  (*
   [Attr] public class MyClass<MyType> : IClass where MyType : SomeType { ... };
      v1     v2    v3    v4     v5         v6           v7                v8   v9
   *)
  let v1 = List.concat_map (attribute_list env) v1 in
  let v2 = List.map (modifier env) v2 in
  let v3 = token env v3 (* "class" *) in
  let v4 = identifier env v4 (* identifier *) in
  let v5 = match v5 with Some x -> type_parameter_list env x | None -> [] in
  let v6 = match v6 with Some x -> base_list env x | None -> [] in
  let v7 = List.map (type_parameter_constraints_clause env) v7 in
  let open_bra, stmts, close_bra = declaration_list env v8 in
  let fields = List.map (fun x -> AST.FieldStmt x) stmts in
  let tparams = type_parameters_with_constraints v5 v7 in
  let idinfo = empty_id_info () in
  let ent = { name = EN (Id (v4, idinfo)); attrs = v1 @ v2; tparams } in
  AST.DefStmt
    ( ent,
      AST.ClassDef
        {
          ckind = (class_kind, v3);
          cextends = v6;
          cimplements = [];
          cmixins = [];
          cparams = [];
          cbody = (open_bra, fields, close_bra);
        } )
  |> AST.s

and enum_declaration env (v1, v2, v3, v4, v5, v6, v7) =
  let v1 = List.concat_map (attribute_list env) v1 in
  let v2 = List.map (modifier env) v2 in
  let v3 = token env v3 (* "enum" *) in
  let v4 = identifier env v4 (* identifier *) in
  let v5 = match v5 with Some x -> base_list env x | None -> [] in
  let v6 = enum_member_declaration_list env v6 in
  let v7 = map_opt token env v7 (* ";" *) in
  let idinfo = empty_id_info () in
  let ent = { name = EN (Id (v4, idinfo)); attrs = v1 @ v2; tparams = [] } in
  AST.DefStmt (ent, AST.TypeDef { tbody = OrType v6 }) |> AST.s

and delegate_declaration env (v1, v2, v3, v4, v5, v6, v7, v8, v9) =
  let v1 = List.concat_map (attribute_list env) v1 in
  let v2 = List.map (modifier env) v2 in
  let v3 = token env v3 (* "delegate" *) in
  let v4 = return_type env v4 in
  let v5 = identifier env v5 (* identifier *) in
  let v6 = match v6 with Some x -> type_parameter_list env x | None -> [] in
  let v7 = parameter_list env v7 in
  let v8 = List.map (type_parameter_constraints_clause env) v8 in
  let v9 = token env v9 (* ";" *) in
  let tparams = type_parameters_with_constraints v6 v8 in
  let func = TyFun (v7, v4) in
  let idinfo = empty_id_info () in
  let ent = { name = EN (Id (v5, idinfo)); attrs = v1 @ v2; tparams } in
  DefStmt (ent, TypeDef { tbody = NewType func }) |> AST.s

and record_declaration env (_, _, v3, _, _, _, _, _, _) =
  let v3 = token env v3 (* "record" *) in
  todo_stmt env v3

and declaration (env : env) (x : CST.declaration) : stmt =
  match x with
  | `Class_decl x -> class_interface_struct env Class x
  | `Dele_decl x -> delegate_declaration env x
  | `Enum_decl x -> enum_declaration env x
  | `Inte_decl x -> class_interface_struct env Interface x
  | `Record_decl x -> record_declaration env x
  | `Struct_decl x -> class_interface_struct env Class x
  | `Cons_decl (v1, v2, v3, v4, v5, v6) ->
      let v1 = List.concat_map (attribute_list env) v1 in
      let v2 = List.map (modifier env) v2 in
      let v3 = identifier env v3 (* identifier *) in
      let _, tok = v3 in
      let v4 = parameter_list env v4 in
      let v5 = map_opt constructor_initializer env v5 in
      let v6 = function_body env v6 in
      let fbody =
        match v5 with
        | Some init -> Block (fake_bracket [ init; v6 ]) |> AST.s
        | None -> v6
      in
      let def =
        AST.FuncDef
          { fkind = (AST.Method, tok); fparams = v4; frettype = None; fbody }
      in
      let ctor = KeywordAttr (Ctor, tok) in
      let attrs = (ctor :: v1) @ v2 in
      let ent = basic_entity v3 attrs in
      AST.DefStmt (ent, def) |> AST.s
  | `Conv_op_decl (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = List.concat_map (attribute_list env) v1 in
      let v2 = List.map (modifier env) v2 in
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
        AST.FuncDef
          {
            fkind = (AST.Method, v4);
            fparams = v6;
            frettype = Some v5;
            fbody = v7;
          }
      in
      AST.DefStmt (ent, def) |> AST.s
  | `Dest_decl (v1, v2, v3, v4, v5, v6) ->
      let v1 = List.concat_map (attribute_list env) v1 in
      let v2 =
        match v2 with
        | Some tok -> [ KeywordAttr (Extern, token env tok) ] (* "extern" *)
        | None -> []
      in
      let v3 = token env v3 (* "~" *) in
      let v4 = identifier env v4 (* identifier *) in
      let v5 = parameter_list env v5 in
      let v6 = function_body env v6 in
      let name = ("Finalize", v3) in
      let def =
        AST.FuncDef
          {
            fkind = (AST.Method, v3);
            fparams = v5;
            frettype = None;
            fbody = v6;
          }
      in
      let dtor = KeywordAttr (Dtor, v3) in
      let ent = basic_entity name ((dtor :: v1) @ v2) in
      AST.DefStmt (ent, def) |> AST.s
  | `Event_decl (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = List.concat_map (attribute_list env) v1 in
      let v2 = List.map (modifier env) v2 in
      let v3 = unhandled_keywordattr_to_namedattr env v3 (* "event" *) in
      let v4 = type_constraint env v4 in
      let v5 = map_opt explicit_interface_specifier env v5 in
      let v6 = identifier env v6 (* identifier *) in
      let fname, ftok = v6 in
      let v7 =
        match v7 with
        | `Acce_list x ->
            let open_br, accs, close_br = accessor_list env x in
            let funcs =
              accs
              |> List.map (fun (attrs, id, fbody) ->
                     let iname, itok = id in
                     let ent = basic_entity (iname ^ "_" ^ fname, itok) attrs in
                     let valparam =
                       ParamClassic
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
                     DefStmt (ent, funcdef) |> AST.s)
            in
            (open_br, funcs, close_br)
        | `SEMI tok ->
            (* ";" *)
            let tok = token env tok (* ";" *) in
            fake_bracket [ todo_stmt env tok ]
      in
      let ent = basic_entity v6 (v1 @ v1 @ [ v3 ]) in
      let vardef = { vinit = None; vtype = Some v4 } in
      let open_br, funcs, close_br = v7 in
      Block (open_br, (DefStmt (ent, VarDef vardef) |> AST.s) :: funcs, close_br)
      |> AST.s
  | `Event_field_decl (v1, v2, v3, v4, v5) ->
      let v1 = List.concat_map (attribute_list env) v1 in
      let v2 = List.map (modifier env) v2 in
      let v3 = unhandled_keywordattr_to_namedattr env v3 (* "event" *) in
      let v4 = variable_declaration env v4 in
      let v5 = token env v5 (* ";" *) in
      var_def_stmt v4 ((v3 :: v1) @ v2)
  | `Field_decl (v1, v2, v3, v4) ->
      let v1 = List.concat_map (attribute_list env) v1 in
      let v2 = List.map (modifier env) v2 in
      let v3 = variable_declaration env v3 in
      let v4 = token env v4 (* ";" *) in
      var_def_stmt v3 (v1 @ v2)
  | `Inde_decl (v1, v2, v3, v4, v5, v6, v7) -> (
      let v1 = List.concat_map (attribute_list env) v1 in
      let v2 = List.map (modifier env) v2 in
      let v3 = type_constraint env v3 in
      let v4 = map_opt explicit_interface_specifier env v4 in
      let v5 = token env v5 (* "this" *) in
      let v6 = bracketed_parameter_list env v6 in
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
                       let ent = basic_entity ("get_Item", itok) attrs in
                       let funcdef =
                         FuncDef
                           {
                             fkind = (Method, itok);
                             fparams = v6;
                             frettype = Some v3;
                             fbody;
                           }
                       in
                       DefStmt (ent, funcdef) |> AST.s
                   | "set" ->
                       let valparam =
                         ParamClassic
                           {
                             pname = Some ("value", fake "value");
                             ptype = Some v3;
                             pdefault = None;
                             pattrs = [];
                             pinfo = empty_id_info ();
                           }
                       in
                       let ent = basic_entity ("set_Item", itok) attrs in
                       let funcdef =
                         FuncDef
                           {
                             fkind = (Method, itok);
                             fparams = v6 @ [ valparam ];
                             frettype = None;
                             fbody;
                           }
                       in
                       DefStmt (ent, funcdef) |> AST.s
                   | _ -> raise Impossible)
          in
          Block (open_br, funcs, close_br) |> AST.s
      | `Arrow_exp_clause_SEMI (v1, v2) ->
          let v1 = arrow_expression_clause env v1 in
          let v2 = token env v2 (* ";" *) in
          let arrow, expr = v1 in
          let fbody = ExprStmt (expr, v2) |> AST.s in
          let ent = basic_entity ("get_Item", arrow) indexer_attrs in
          let funcdef =
            FuncDef
              {
                fkind = (Arrow, arrow);
                fparams = v6;
                frettype = Some v3;
                fbody;
              }
          in
          DefStmt (ent, funcdef) |> AST.s )
  | `Meth_decl (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      (*
        [Attr] static int IList<T>.MyMethod<T>(int p1) where T : Iterator { ... }
          v1     v2   v3    v4        v5    v6  v7           v8              v9
      *)
      let v1 = List.concat_map (attribute_list env) v1 in
      let v2 = List.map (modifier env) v2 in
      let v3 = return_type env v3 in
      let v5 = identifier env v5 (* identifier *) in
      let _, tok = v5 in
      let v6 =
        match v6 with Some x -> type_parameter_list env x | None -> []
      in
      let v7 = parameter_list env v7 in
      let v8 = List.map (type_parameter_constraints_clause env) v8 in
      let v9 = function_body env v9 in
      let tparams = type_parameters_with_constraints v6 v8 in
      let idinfo = empty_id_info () in
      let ent = { name = EN (Id (v5, idinfo)); attrs = v1 @ v2; tparams } in
      let def =
        AST.FuncDef
          {
            fkind = (AST.Method, tok);
            fparams = v7;
            frettype = Some v3;
            fbody = v9;
          }
      in
      AST.DefStmt (ent, def) |> AST.s
  | `Name_decl x -> namespace_declaration env x
  | `Op_decl (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = List.concat_map (attribute_list env) v1 in
      let v2 = List.map (modifier env) v2 in
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
        AST.FuncDef
          {
            fkind = (AST.Method, v4);
            fparams = v6;
            frettype = Some v3;
            fbody = v7;
          }
      in
      AST.DefStmt (ent, def) |> AST.s
  | `Prop_decl (v1, v2, v3, v4, v5, v6) ->
      (* [Attr] public string IFace.Field { get; public set { ... } } = "hello";
         [Attr] public string IFace.Field => "hello";
           v1     v2     v3    v4    v5      v6
         Map `Prop` as field. Map getter and setter as methods. *)
      let v1 = List.concat_map (attribute_list env) v1 in
      let v2 = List.map (modifier env) v2 in
      let v3 = type_constraint env v3 in
      let v4 = map_opt explicit_interface_specifier env v4 in
      let v5 = identifier env v5 (* identifier *) in
      let fname, ftok = v5 in
      let accessors, vinit =
        match v6 with
        | `Acce_list_opt_EQ_exp_SEMI (v1, v2) ->
            let v1 = accessor_list env v1 in
            let v2 =
              match v2 with
              | Some (v1, v2, v3) ->
                  let v1 = token env v1 (* "=" *) in
                  let v2 = expression env v2 in
                  let v3 = token env v3 (* ";" *) in
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
                  let ent = basic_entity (iname ^ "_" ^ fname, itok) attrs in
                  let funcdef =
                    FuncDef
                      {
                        fkind = (Method, itok);
                        fparams =
                          ( if has_params then
                            [
                              ParamClassic
                                {
                                  pname = Some ("value", fake "value");
                                  ptype = Some v3;
                                  pdefault = None;
                                  pattrs = [];
                                  pinfo = empty_id_info ();
                                };
                            ]
                          else [] );
                        frettype = (if has_return then Some v3 else None);
                        (* TODO Should this be "void"? *)
                        fbody;
                      }
                  in
                  DefStmt (ent, funcdef) |> AST.s)
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
            let ent = basic_entity ("get_" ^ fname, arrow) [] in
            let funcdef =
              FuncDef
                {
                  fkind = (Arrow, arrow);
                  fparams = [];
                  frettype = Some v3;
                  fbody = ExprStmt (expr, v2) |> AST.s;
                }
            in
            let func = DefStmt (ent, funcdef) |> AST.s in
            ((arrow, [ func ], v2), None)
      in
      let ent = basic_entity v5 (v1 @ v2) in
      let vardef = { vinit; vtype = Some v3 } in
      let open_br, funcs, close_br = accessors in
      Block (open_br, (DefStmt (ent, VarDef vardef) |> AST.s) :: funcs, close_br)
      |> AST.s
  | `Using_dire x -> using_directive env x

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)
let parse file =
  H.wrap_parser
    (fun () ->
      Parallel.backtrace_when_exn := false;
      Parallel.invoke Tree_sitter_c_sharp.Parse.file file ())
    (fun cst ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in

      try
        match compilation_unit env cst with
        | AST.Pr xs -> xs
        | _ -> failwith "not a program"
      with Failure "not implemented" as exn ->
        let s = Printexc.get_backtrace () in
        pr2 "Some constructs are not handled yet";
        pr2 "CST was:";
        CST.dump_tree cst;
        pr2 "Original backtrace:";
        pr2 s;
        raise exn)

let parse_pattern_aux str =
  (* ugly: coupling: see grammar.js of csharp.
   * todo: will need to adjust position information in parsing errors! *)
  let expr_str = "__SEMGREP_EXPRESSION " ^ str in
  (* If possible, we always prefer to parse a pattern as an expression than
   * as a program, since an expression is also a statement, but a statement
   * is not an expression! E.g., `Foo()` as an statement will not match
   * `if (null == Foo()) ...` whereas as an expression it does. *)
  let res = Tree_sitter_c_sharp.Parse.string expr_str in
  match res.errors with [] -> res | _ -> Tree_sitter_c_sharp.Parse.string str

let parse_pattern str =
  H.wrap_parser
    (fun () ->
      Parallel.backtrace_when_exn := false;
      Parallel.invoke parse_pattern_aux str ())
    (fun cst ->
      let file = "<pattern>" in
      let env = { H.file; conv = Hashtbl.create 0; extra = () } in
      match compilation_unit env cst with
      | AST.Pr [ x ] -> AST.S x
      | AST.Pr xs -> AST.Ss xs
      | x -> x)
