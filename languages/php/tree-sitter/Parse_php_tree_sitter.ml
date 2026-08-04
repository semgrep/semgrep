(* Yoann Padioleau
 *
 * Copyright (C) 2021 r2c
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
open Fpath_.Operators
module CST = Tree_sitter_php.CST
module H = Parse_tree_sitter_helpers
module A = Ast_php
module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* PHP parser using tree-sitter-lang/semgrep-php and converting
 * to ../ast/ast_php.ml
 *
 * The resulting AST can then be converted to the generic AST by using
 * php_to_generic.ml
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type env = unit H.env

let token = H.token
let str = H.str

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)

(* This was started by copying tree-sitter-lang/semgrep-php/.../Boilerplate.ml *)

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type classmember =
  | ConstantDef of A.constant_def
  | ClassVar of A.class_var
  | MethodDef of A.method_def
  | UseTrait of A.class_name
  | EnumCase of A.class_var (* TODO add enum case to AST *)

let todo (env : env) _ = failwith "not implemented"
let map_name (env : env) tok : A.name = [ str env tok ]

let rec _split_classmembers env members constants variables methods uses =
  match members with
  | [] -> (constants, variables, methods, uses)
  | hd :: rest -> (
      let constants, variables, methods, uses =
        _split_classmembers env rest constants variables methods uses
      in
      match hd with
      | ConstantDef c -> (c :: constants, variables, methods, uses)
      | ClassVar c -> (constants, c :: variables, methods, uses)
      | MethodDef m -> (constants, variables, m :: methods, uses)
      | UseTrait u -> (constants, variables, methods, u :: uses)
      | EnumCase c -> (constants, c :: variables, methods, uses))

let split_classmembers env members = _split_classmembers env members [] [] [] []
let map_empty_block (env : env) semi = A.Block (Tok.fake_bracket semi [])

let stmt1 xs =
  match xs with
  | [] -> A.Block (Tok.fake_bracket Tok.unsafe_sc [])
  | [ st ] -> st
  | xs -> A.Block (Tok.fake_bracket Tok.unsafe_sc xs)

let fake_call_to_builtin (env : env) tok args =
  let str, tok = tok in
  A.Call
    ( A.Id [ (A.builtin str, tok) ],
      Tok.fake_bracket tok (args |> List.map (fun x -> A.Arg x)) )

let rec chain_else_if (env : env) ifelses (else_ : A.stmt option) :
    A.stmt option =
  match ifelses with
  | [] -> else_
  | (tok, expr, stmt) :: tail ->
      let else_chain = chain_else_if env tail else_ in
      Some (A.If (tok, expr, stmt, else_chain))

let map_primitive_type (env : env) (x : CST.primitive_type) : A.hint_type =
  match x with
  | `Array tok -> (* "array" *) HintArray (token env tok)
  | `Bool tok -> (* "bool" *) Hint (map_name env tok)
  | `Pat_call tok -> (* "callable" *) Hint (map_name env tok)
  | `Pat_false tok -> (* "false" *) Hint (map_name env tok)
  | `Float tok -> (* "float" *) Hint (map_name env tok)
  | `Int tok -> (* "int" *) Hint (map_name env tok)
  | `Pat_iter tok -> (* "iterable" *) Hint (map_name env tok)
  | `Pat_mixed tok -> (* "mixed" *) Hint (map_name env tok)
  | `Null tok -> (* "null" *) Hint (map_name env tok)
  | `Obj tok -> (* "object" *) Hint (map_name env tok)
  | `Str tok -> (* "string" *) Hint (map_name env tok)
  | `Pat_true tok -> (* "true" *) Hint (map_name env tok)
  | `Pat_void tok -> (* "void" *) Hint (map_name env tok)

let map_cast_type (env : env) (x : CST.cast_type) =
  match x with
  | `Pat_array tok -> (* "array" *) (A.ArrayTy, token env tok)
  | `Pat_bin tok -> (* "binary" *) (A.StringTy, token env tok)
  | `Pat_bool tok -> (* "bool" *) (A.BoolTy, token env tok)
  | `Pat_bool_ tok -> (* "boolean" *) (A.BoolTy, token env tok)
  | `Pat_double tok -> (* "double" *) (A.DoubleTy, token env tok)
  | `Pat_float tok -> (* "float" *) (A.DoubleTy, token env tok)
  | `Pat_int tok -> (* "int" *) (A.IntTy, token env tok)
  | `Pat_int_ tok -> (* "integer" *) (A.IntTy, token env tok)
  | `Pat_obj tok -> (* "object" *) (A.ObjectTy, token env tok)
  | `Pat_real tok -> (* "real" *) (A.DoubleTy, token env tok)
  | `Pat_str tok -> (* "string" *) (A.StringTy, token env tok)
  | `Pat_unset tok -> (* "unset" *) (A.ObjectTy, token env tok)

let map_anon_choice_COLON_5102e09 (env : env)
    (x : CST.anon_choice_COLON_5102e09) =
  match x with
  | `COLON tok -> (* ":" *) token env tok
  | `SEMI tok -> (* ";" *) token env tok

let map_text (env : env) (xs : CST.text) =
  List.map
    (fun x ->
      match x with
      | `Tok_prec_n1_pat_524a507 tok -> (* < *) token env tok
      | `Tok_prec_p1_pat_b91d208 tok -> (* pattern [^\s<][^<]* *) token env tok)
    xs

let map_namespace_name (env : env) ((v1, v2) : CST.namespace_name) =
  let v1 =
    (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) str env v1
  in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let v1 = (* "\\" *) token env v1 in
        let v2 =
          (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
          str env v2
        in
        v2)
      v2
  in
  v1 :: v2

let map_named_label_statement (env : env) ((v1, v2) : CST.named_label_statement)
    =
  let v1 =
    (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) str env v1
  in
  let v2 = (* ":" *) token env v2 in
  A.Label (v1, v2, map_empty_block env v2)

let map_variable_name (env : env) ((v1, v2) : CST.variable_name) : A.var =
  let v1 = (* "$" *) token env v1 in
  let v2str, v2tok = str env v2 in
  let combined = Tok.combine_toks v1 [ v2tok ] in
  ("$" ^ v2str, combined)

let map_namespace_aliasing_clause (env : env) ((v1, v2) : CST.pat_as * CST.name)
    : A.ident =
  let v1 = (* pattern as *) token env v1 in
  let v2 = str env v2 in
  v2

let map_visibility_modifier (env : env) ((v1, _v2) : CST.visibility_modifier) =
  (* v2 is an optional set/get scoped visibility marker which we don't
   * currently model; e.g. `private(set)`. *)
  match v1 with
  | `Pat_public tok -> (* pattern public *) (A.Public, token env tok)
  | `Pat_prot tok -> (* pattern protected *) (A.Protected, token env tok)
  | `Pat_priv tok -> (* pattern private *) (A.Private, token env tok)

let map_string__ (env : env) ((v1, v2, v3) : CST.string__) =
  (* Single-quoted string: prefix opener, content list, close quote. *)
  let openstr, opentok =
    match v1 with
    | `Pat_e816325 tok -> (* pattern [bB]' *) str env tok
    | `SQUOT tok -> (* "'" *) str env tok
  in
  let content_strs_rev, content_toks_rev =
    List.fold_left
      (fun (strs, toks) x ->
        match x with
        | `Tok_choice_bsla tok ->
            let s, t = str env tok in
            (s :: strs, t :: toks)
        | `Str_content xs ->
            List.fold_left
              (fun (strs, toks) tok ->
                let s, t = str env tok in
                (s :: strs, t :: toks))
              (strs, toks) xs)
      ([], []) v2
  in
  let content_str = String.concat "" (List.rev content_strs_rev) in
  let close_str, close_tok = str env v3 in
  let content_toks = List.rev_append content_toks_rev [ close_tok ] in
  let combined = Tok.combine_toks opentok content_toks in
  A.String (content_str, combined)

let map_func_or_const (env : env) (x : CST.namespace_use_type) =
  match x with
  | `Pat_func tok ->
      (* pattern [fF][uU][nN][cC][tT][iI][oO][nN] *) token env tok
  | `Pat_const tok -> (* pattern [cC][oO][nN][sS][tT] *) token env tok

let map_semicolon (env : env) (x : CST.semicolon) =
  match x with
  | `Auto_semi tok -> (* automatic_semicolon *) token env tok
  | `SEMI tok -> (* ";" *) token env tok

let map_namespace_root env tok = [ (A.special "ROOT", token env tok) ]

let map_relative_name (env : env) ((v1, v2, v3, v4) : CST.relative_name) :
    A.name =
  let _v1 = (* pattern [nN][aA][mM][eE][sS][pP][aA][cC][eE] *) token env v1 in
  let v2 =
    match v2 with
    | Some (b, n) ->
        let _b = (* "\\" *) token env b in
        map_namespace_name env n
    | None -> []
  in
  let _v3 = (* "\\" *) token env v3 in
  let v4 = str env v4 in
  v2 @ [ v4 ]

(* In `use (&$x)` / promoted `__construct(&$x)`, the new grammar admits any
 * [variable] after the [&] but in practice almost every real call site is a
 * plain [$x]. Try to recover the simple name; fall back to a synthetic
 * "$_use" only for non-simple variable forms (which existing rules don't
 * match against anyway). Returns the [&] token if present and the variable
 * name. *)
let map_use_clause_var (env : env) (x : CST.anon_choice_by_ref_06f912a) :
    Tok.t option * A.var =
  let simple_var_name (v : CST.variable) : A.var option =
    let from_simple = function
      | `Var_name vn -> Some (map_variable_name env vn)
      | `Dyna_var_name _ -> None
    in
    match v with
    | `Call_var (`Simple_var sv)
    | `New_var (`Simple_var sv) ->
        from_simple sv
    | _ -> None
  in
  match x with
  | `Var_name vn -> (None, map_variable_name env vn)
  | `By_ref (amp, v) ->
      let amp_tok = token env amp in
      let name =
        match simple_var_name v with
        | Some n -> n
        | None -> ("$_use", amp_tok)
      in
      (Some amp_tok, name)

let map_anonymous_function_use_clause (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.anonymous_function_use_clause) :
    (bool (* is_ref *) * A.var) list =
  let _v1 = (* pattern [uU][sS][eE] *) token env v1 in
  let _v2 = (* "(" *) token env v2 in
  let to_use_pair (amp_opt, name) = (Option.is_some amp_opt, name) in
  let v3 = to_use_pair (map_use_clause_var env v3) in
  let v4 =
    List.map
      (fun (c, x) ->
        let _c = (* "," *) token env c in
        to_use_pair (map_use_clause_var env x))
      v4
  in
  let _v5 =
    match v5 with
    | Some tok -> (* "," *) Some (token env tok)
    | None -> None
  in
  let _v6 = (* ")" *) token env v6 in
  v3 :: v4

let map_integer env tok =
  let value, tok = str env tok in
  let pi = Parsed_int.parse (value, tok) in
  A.Int pi

let map_boolean env tok =
  let bool_of_string value =
    let canonicalized_value = String.lowercase_ascii value in
    if canonicalized_value = "true" then true
    else if canonicalized_value = "false" then false
    else failwith ("Not a valid PHP boolean: " ^ value)
  in
  let value, tok = str env tok in
  let value = bool_of_string value in
  A.Bool (value, tok)

let map_qualified_name (env : env) ((v1, v2, v3, v4) : CST.qualified_name) =
  (* In the new grammar, qualified_name is (\\ option, namespace_name option, \\, name).
   * v3 is always present: it's the leading "\\" when v1=None and v2=None
   * (e.g. "\Foo"), and the separator before v4 otherwise.
   * A name is fully-qualified (rooted in the global namespace) when v1=Some,
   * or when there is no namespace prefix (v2=None), since a bare relative
   * name would parse as `Name` rather than `Qual_name`. *)
  let prefix =
    match (v1, v2) with
    | Some tok, None -> map_namespace_root env tok
    | Some tok, Some n -> map_namespace_root env tok @ map_namespace_name env n
    | None, None -> map_namespace_root env v3
    | None, Some n -> map_namespace_name env n
  in
  let _v3 = (* "\\" *) token env v3 in
  let last = str env v4 in
  prefix @ [ last ]

let map_namespace_use_group_clause (env : env)
    ((v1, v2, v3) : CST.namespace_use_clause) =
  let _v1 =
    match v1 with
    | Some x -> Some (map_func_or_const env x)
    | None -> None
  in
  let v2 =
    match v2 with
    | `Name tok -> map_name env tok
    | `Qual_name x -> map_qualified_name env x
  in
  let v3 =
    match v3 with
    | Some x -> Some (map_namespace_aliasing_clause env x)
    | None -> None
  in
  (v2, v3)

let map_modifier (env : env) (x : CST.modifier) : A.modifier =
  match x with
  | `Var_modi tok -> (* pattern [vV][aA][rR] *) failwith "not a modifier"
  | `Visi_modi x -> map_visibility_modifier env x
  | `Static_modi tok ->
      (* pattern [sS][tT][aA][tT][iI][cC] *) (A.Static, token env tok)
  | `Final_modi tok ->
      (* pattern [fF][iI][nN][aA][lL] *) (A.Final, token env tok)
  | `Abst_modi tok ->
      (* pattern [aA][bB][sS][tT][rR][aA][cC][tT] *) (A.Abstract, token env tok)
  | `Read_modi tok ->
      (* pattern [rR][eE][aA][dD][oO][nN][lL][yY] *)
      (* readonly is not a first-class modifier in Ast_php; approximate *)
      (A.Final, token env tok)

let map_modifiers (env : env) (x : CST.modifier list) : A.modifier list =
  List.concat_map
    (fun m ->
      match m with
      | `Var_modi tok ->
          []
          (* pattern [vV][aA][rR] *)
          (* `var` isn't a modifier *)
      | _ -> [ map_modifier env m ])
    x

let map_relative_scope (env : env) (x : CST.relative_scope) =
  match x with
  | `Pat_self tok -> (* "self" *) A.IdSpecial (A.Self, token env tok)
  | `Pat_parent tok -> (* "parent" *) A.IdSpecial (A.Parent, token env tok)
  | `Pat_static tok ->
      (* pattern [sS][tT][aA][tT][iI][cC] *) A.Id (map_name env tok)

let map_namespace_use_group (env : env)
    ((v1, v2, v3, v4) : CST.namespace_use_group) =
  let _v1 =
    match v1 with
    | Some x -> Some (map_func_or_const env x)
    | None -> None
  in
  let prefix = map_namespace_name env v2 in
  let _v3 = (* "\\" *) token env v3 in
  let _lbrace, c1, rest, _rbrace = v4 in
  let _lbrace = (* "{" *) token env _lbrace in
  let c1 = map_namespace_use_group_clause env c1 in
  let rest =
    List.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        map_namespace_use_group_clause env v2)
      rest
  in
  let _rbrace = (* "}" *) token env _rbrace in
  (* Prefix each clause name with the group's namespace prefix. *)
  List.map (fun (n, alias) -> (prefix @ n, alias)) (c1 :: rest)

let map_named_type (env : env) (x : CST.named_type) : A.hint_type =
  match x with
  | `Name tok ->
      (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
      Hint (map_name env tok)
  | `Qual_name x -> Hint (map_qualified_name env x)
  | `Rela_name x -> Hint (map_relative_name env x)

let map_anon_choice_name_062e4f2 (env : env) (x : CST.name_) : A.name =
  match x with
  | `Name tok ->
      (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
      map_name env tok
  | `Pat_static tok -> [ str env tok ]
  | `Qual_name x -> map_qualified_name env x
  | `Rela_name x -> map_relative_name env x

let map_type_list (env : env) ((v1, v2) : CST.type_list) : A.hint_type list =
  let v1 = map_named_type env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let v1 = (* "|" *) token env v1 in
        let v2 = map_named_type env v2 in
        v2)
      v2
  in
  v1 :: v2

let map_base_clause (env : env) ((v1, v2, v3) : CST.base_clause) =
  let v1 = (* pattern [eE][xX][tT][eE][nN][dD][sS] *) token env v1 in
  let v2 = map_anon_choice_name_062e4f2 env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_anon_choice_name_062e4f2 env v2 in
        v2)
      v3
  in
  List.map (fun c -> A.Hint c) (v2 :: v3)

let map_class_interface_clause (env : env)
    ((v1, v2, v3) : CST.class_interface_clause) : A.class_name list =
  let v1 =
    (* pattern [iI][mM][pP][lL][eE][mM][eE][nN][tT][sS] *) token env v1
  in
  let v2 = A.Hint (map_anon_choice_name_062e4f2 env v2) in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = A.Hint (map_anon_choice_name_062e4f2 env v2) in
        v2)
      v3
  in
  v2 :: v3

let map_namespace_use_clause (env : env)
    ((v1, v2, v3) : CST.namespace_use_clause) =
  let _v1 =
    match v1 with
    | Some x -> Some (map_func_or_const env x)
    | None -> None
  in
  let v2 =
    match v2 with
    | `Name tok -> map_name env tok
    | `Qual_name x -> map_qualified_name env x
  in
  let v3 =
    match v3 with
    | Some x -> Some (map_namespace_aliasing_clause env x)
    | None -> None
  in
  (v2, v3)

let map_types (env : env) (x : CST.types) =
  match x with
  | `Opt_type (v1, v2) ->
      let v1 = (* "?" *) token env v1 in
      let v2 =
        match v2 with
        | `Named_type x -> map_named_type env x
        | `Prim_type x -> map_primitive_type env x
      in
      A.HintQuestion (v1, v2)
  | `Named_type x -> map_named_type env x
  | `Prim_type x -> map_primitive_type env x

let map_intersection_type (env : env) ((v1, v2) : CST.intersection_type) =
  let v1 = map_types env v1 in
  let v2 = List.map (fun (_amp, v2) -> map_types env v2) v2 in
  match v2 with
  | [] -> v1
  | _ -> A.HintTuple (Tok.fake_bracket Tok.unsafe_sc (v1 :: v2))

let map_type_ (env : env) (x : CST.type_) : A.hint_type =
  match x with
  | `Types t -> map_types env t
  | `Union_type (v1, v2) -> (
      let v1 = map_types env v1 in
      let v2 = List.map (fun (_pipe, t) -> map_types env t) v2 in
      match v2 with
      | [] -> v1
      | _ -> A.HintTuple (Tok.fake_bracket Tok.unsafe_sc (v1 :: v2)))
  | `Inte_type x -> map_intersection_type env x
  | `Disj_normal_form_type (v1, v2) -> (
      let map_alt = function
        | `LPAR_inte_type_RPAR (_, it, _) -> map_intersection_type env it
        | `Types t -> map_types env t
      in
      let v1 = map_alt v1 in
      let v2 = List.map (fun (_pipe, x) -> map_alt x) v2 in
      match v2 with
      | [] -> v1
      | _ -> A.HintTuple (Tok.fake_bracket Tok.unsafe_sc (v1 :: v2)))

let map_return_type (env : env) ((v1, v2) : CST.return_type) =
  let _v1 = (* ":" *) token env v1 in
  match v2 with
  | `Type t -> map_type_ env t
  | `Bottom_type tok ->
      (* `never` is the bottom type. Approximate as a hint named `never`. *)
      A.Hint (map_name env tok)

let rec map_anon_array_elem_init_rep_COMMA_array_elem_init_1dad3d4 (env : env)
    ((v1, v2) : CST.anon_array_elem_init_rep_COMMA_array_elem_init_1dad3d4) =
  let v1 = map_array_element_initializer env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_array_element_initializer env v2 in
        v2)
      v2
  in
  v1 :: v2

(* ------------------------------------------------------------------ *)
(* String literals: encapsed (double-quoted), single-quoted, heredoc, *)
(* nowdoc.                                                            *)
(* ------------------------------------------------------------------ *)

(* A part of an encapsed-string body: either a literal text segment or an
 * embedded variable / `{$expr}`. Returned as an [A.expr] for use in
 * [A.Guil]. Literal segments lower to [A.String]; embedded segments lower
 * to the corresponding variable / expression node. *)
and map_simple_string_part (env : env) (x : CST.simple_string_part) : A.expr =
  match x with
  | `Simple_str_member_access_exp (v1, v2, v3) ->
      let var = A.Var (map_variable_name env v1) in
      let arrow = (* "->" *) token env v2 in
      let prop = str env v3 in
      A.Obj_get (var, arrow, A.Id [ prop ])
  | `Simple_var x -> map_simple_variable env x
  | `Simple_str_subs_exp (v1, v2, v3, v4) ->
      let var = A.Var (map_variable_name env v1) in
      let lbrack = (* "[" *) token env v2 in
      let idx = map_simple_string_array_access_argument env v3 in
      let rbrack = (* "]" *) token env v4 in
      A.Array_get (var, (lbrack, Some idx, rbrack))

and map_simple_string_array_access_argument (env : env)
    (x : CST.simple_string_array_access_argument) : A.expr =
  match x with
  | `Int tok -> map_integer env tok
  | `Simple_str_subs_un_exp (v1, v2) ->
      let minus = (* "-" *) token env v1 in
      let n = map_integer env v2 in
      A.Unop ((G.Minus, minus), n)
  | `Name tok -> A.Id [ str env tok ]
  | `Var_name x -> A.Var (map_variable_name env x)

and map_complex_string_part (env : env) ((v1, v2, v3) : CST.complex_string_part)
    : A.expr =
  let _l = (* "{" *) token env v1 in
  let e = map_expression env v2 in
  let _r = (* "}" *) token env v3 in
  e

(* Lower one body item from an [interpolated_string_body] (regular or
 * heredoc variant) into 0..N expression pieces. Both variants share the
 * same shape modulo the encapsed-chars token type (which we just read as
 * a string). Adjacent literal pieces are emitted as separate [A.String]s;
 * the caller may merge them. *)
and map_var_followed_by_chars (env : env) (vn : CST.variable_name)
    (chars : Tree_sitter_run.Token.t) : A.expr list =
  let var = A.Var (map_variable_name env vn) in
  let s, t = str env chars in
  if String.length s = 0 then [ var ] else [ var; A.String (s, t) ]

and map_interpolated_string_body (env : env) (xs : CST.interpolated_string_body)
    : A.expr list =
  List.concat_map
    (fun x ->
      match x with
      | `Esc_seq tok
      | `Enca_str_chars tok
      | `BSLA tok ->
          let s, t = str env tok in
          [ A.String (s, t) ]
      | `Var_name_enca_str_chars_after_var (vn, chars) ->
          map_var_followed_by_chars env vn chars
      | `Simple_str_part x -> [ map_simple_string_part env x ]
      | `Comp_str_part x -> [ map_complex_string_part env x ])
    xs

and map_interpolated_string_body_heredoc (env : env)
    (xs : CST.interpolated_string_body_heredoc) : A.expr list =
  List.concat_map
    (fun x ->
      match x with
      | `Esc_seq tok
      | `Enca_str_chars_here tok
      | `BSLA tok ->
          let s, t = str env tok in
          [ A.String (s, t) ]
      | `Var_name_enca_str_chars_after_var_here (vn, chars) ->
          map_var_followed_by_chars env vn chars
      | `Simple_str_part x -> [ map_simple_string_part env x ]
      | `Comp_str_part x -> [ map_complex_string_part env x ])
    xs

and map_heredoc_body (env : env) ((_v1, segs) : CST.heredoc_body) : A.expr list
    =
  List.concat_map
    (fun (_opt_nl, parts) -> map_interpolated_string_body_heredoc env parts)
    segs

and map_nowdoc_body (env : env) ((_v1, toks) : CST.nowdoc_body) : A.expr list =
  List.map
    (fun tok ->
      let s, t = str env tok in
      A.String (s, t))
    toks

(* Merge runs of adjacent literal [A.String] segments into a single [A.String].
 * Encapsed strings like ["\r\n"] are tokenized into multiple literal parts;
 * concatenating them avoids spurious [A.Guil] wrapping when there's no actual
 * interpolation. *)
and merge_string_parts (parts : A.expr list) : A.expr list =
  let flush strs_rev acc =
    match List.rev strs_rev with
    | [] -> acc
    | (s, t) :: ss ->
        let strs, toks = List_.split ss in
        A.String (String.concat "" (s :: strs), Tok.combine_toks t toks) :: acc
  in
  let rec loop s acc = function
    | [] -> List.rev (flush s acc)
    | A.String (s1, t1) :: rest -> loop ((s1, t1) :: s) acc rest
    | x :: rest -> loop [] (x :: flush s acc) rest
  in
  loop [] [] parts

(* If a Guil-style body is just a single [A.String] (after merging adjacent
 * literal segments), return it directly so we don't pay for [A.Guil] wrapping
 * when there is no actual interpolation. *)
and string_or_guil (l : Tok.t) (parts : A.expr list) (r : Tok.t) : A.expr =
  match merge_string_parts parts with
  | [] -> A.String ("", Tok.combine_toks l [ r ])
  | [ A.String (s, _) ] -> A.String (s, Tok.combine_toks l [ r ])
  | merged -> A.Guil (l, merged, r)

and map_string_ (env : env) (x : CST.string_) : A.expr =
  match x with
  | `Str_ x -> map_string__ env x
  | `Enca_str (v1, v2, v3) ->
      let opentok =
        match v1 with
        | `Pat_8694eac tok -> token env tok
        | `DQUOT tok -> (* "\"" *) token env tok
      in
      let parts =
        match v2 with
        | None -> []
        | Some xs -> map_interpolated_string_body env xs
      in
      let closetok = (* "\"" *) token env v3 in
      string_or_guil opentok parts closetok
  | `Here (v1, _v2_opt_dquot, v3, _v4_opt_dquot, v5, v6) ->
      let opentok = token env v1 in
      let _start_tok = token env v3 in
      let parts =
        match v5 with
        | `Here_body_new_line (body, _nl) -> map_heredoc_body env body
        | `Opt_here_body None -> []
        | `Opt_here_body (Some body) -> map_heredoc_body env body
      in
      let endtok = token env v6 in
      string_or_guil opentok parts endtok
  | `Nowdoc (v1, _v2_squot, v3, _v4, v5, v6) ->
      let opentok = token env v1 in
      let _start_tok = token env v3 in
      let parts =
        match v5 with
        | `Nowdoc_body_new_line (body, _nl) -> map_nowdoc_body env body
        | `Opt_nowdoc_body None -> []
        | `Opt_nowdoc_body (Some body) -> map_nowdoc_body env body
      in
      let endtok = token env v6 in
      string_or_guil opentok parts endtok

and map_literal (env : env) (x : CST.literal) : A.expr =
  match x with
  | `Int tok -> map_integer env tok
  | `Float tok ->
      let value, tok = str env tok in
      let value = float_of_string value in
      A.Double (Some value, tok)
  | `Str x -> map_string_ env x
  | `Bool tok -> map_boolean env tok
  | `Null tok ->
      (* TODO Null should have its own AST node *)
      A.Id [ str env tok ]

and map_declare_directive (env : env) ((v1, v2, v3) : CST.declare_directive) =
  let v1 =
    match v1 with
    | `Ticks tok -> (* "ticks" *) A.Id (map_name env tok)
    | `Enco tok -> (* "encoding" *) A.Id (map_name env tok)
    | `Strict_types tok -> (* "strict_types" *) A.Id (map_name env tok)
  in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_literal env v3 in
  A.Assign (v1, v2, v3)

and map_anon_choice_array_dest_08f4c18 (env : env)
    (x : CST.anon_choice_array_dest_4a8a962) =
  match x with
  | `Array_dest x -> map_array_destructing env x
  | `Choice_cast_var x -> map_variable env x
  | `By_ref (_amp, v) -> map_variable env v

and map_anon_choice_case_stmt_f1b35bc (env : env)
    (x : CST.anon_choice_case_stmt_f1b35bc) =
  match x with
  | `Case_stmt (v1, v2, v3, v4) ->
      let v1 = (* pattern [cC][aA][sS][eE] *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = map_anon_choice_COLON_5102e09 env v3 in
      let v4 = List.map (map_statement env) v4 in
      A.Case (v1, v2, v4)
  | `Defa_stmt (v1, v2, v3) ->
      let v1 = (* pattern [dD][eE][fF][aA][uU][lL][tT] *) token env v1 in
      let v2 = map_anon_choice_COLON_5102e09 env v2 in
      let v3 = List.map (map_statement env) v3 in
      A.Default (v1, v3)

and map_anon_choice_choice_array_dest_abfb170 (env : env)
    (x : CST.array_destructing_element) =
  match x with
  | `Choice_array_dest x -> map_anon_choice_array_dest_08f4c18 env x
  | `Exp_EQGT_choice_array_dest (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 = map_anon_choice_array_dest_08f4c18 env v3 in
      A.Arrow (v1, v2, v3)

and map_anon_choice_choice_list_dest_c865322 (env : env)
    (x : CST.anon_choice_list_dest_284bbd6) =
  match x with
  | `List_dest x -> map_list_destructing env x
  | `Choice_cast_var x -> map_variable env x
  | `By_ref (_amp, v) -> map_variable env v
  | `Exp_EQGT_choice_list_dest (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 = map_anon_choice_list_dest_bb41c20 env v3 in
      A.Arrow (v1, v2, v3)

and map_anon_choice_class_cst_access_exp_18f5288 (env : env)
    (x :
      [ `Class_cst_access_exp of CST.class_constant_access_expression
      | `Name of CST.name ]) =
  match x with
  | `Class_cst_access_exp x -> map_class_constant_access_expression env x
  | `Name tok ->
      (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
      A.Id (map_name env tok)

and map_anon_choice_list_dest_bb41c20 (env : env)
    (x : CST.anon_choice_list_dest_8617a9f) =
  match x with
  | `List_dest x -> map_list_destructing env x
  | `Choice_cast_var x -> map_variable env x
  | `By_ref (_amp, v) -> map_variable env v

and map_anon_choice_match_cond_exp_d891119 (env : env)
    (x : CST.anon_choice_match_cond_exp_d891119) : A.match_ =
  match x with
  | `Match_cond_exp (v1, v2, v3) ->
      let v1 = map_match_condition_list env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 = map_expression env v3 in
      A.MCase (v1, v3)
  | `Match_defa_exp (v1, v2, v3) ->
      let v1 = (* pattern [dD][eE][fF][aA][uU][lL][tT] *) token env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 = map_expression env v3 in
      A.MDefault (v1, v3)

and map_anon_choice_simple_param_5af5eb3 (env : env)
    (x : CST.anon_choice_simple_param_5af5eb3) =
  match x with
  | `Simple_param (v1, v2, v3, v4, v5) ->
      let v1 =
        match v1 with
        | Some x -> map_attribute_list env x
        | None -> []
      in
      let v2 =
        match v2 with
        | Some x -> Some (map_type_ env x)
        | None -> None
      in
      let v3 =
        match v3 with
        | Some tok -> (* "&" *) Some (token env tok)
        | None -> None
      in
      let v4 = map_variable_name env v4 in
      let v5 =
        match v5 with
        | Some x -> Some (map_property_initializer env x)
        | None -> None
      in
      A.ParamClassic
        {
          p_type = v2;
          p_ref = v3;
          p_name = v4;
          p_default = v5;
          p_attrs = v1;
          p_variadic = None;
        }
  | `Vari_param (v1, v2, v3, v4, v5) ->
      let v1 =
        match v1 with
        | Some x -> map_attribute_list env x
        | None -> []
      in
      let v2 =
        match v2 with
        | Some x -> Some (map_type_ env x)
        | None -> None
      in
      let v3 =
        match v3 with
        | Some tok -> (* "&" *) Some (token env tok)
        | None -> None
      in
      let v4 = (* "..." *) token env v4 in
      let v5 = map_variable_name env v5 in
      A.ParamClassic
        {
          p_type = v2;
          p_ref = v3;
          p_name = v5;
          p_default = None;
          p_attrs = v1;
          p_variadic = Some v4;
        }
  | `Prop_prom_param (v1, v2, _v3_readonly, v4, v5, v6, _v7_hooks) ->
      let v1 =
        match v1 with
        | Some x -> map_attribute_list env x
        | None -> []
      in
      let _v2_visi = map_visibility_modifier env v2 in
      let v4 =
        match v4 with
        | Some x -> Some (map_type_ env x)
        | None -> None
      in
      let p_ref, p_name = map_use_clause_var env v5 in
      let v6 =
        match v6 with
        | Some x -> Some (map_property_initializer env x)
        | None -> None
      in
      A.ParamClassic
        {
          p_type = v4;
          p_ref;
          p_name;
          p_default = v6;
          p_attrs = v1;
          p_variadic = None;
        }

and map_argument (env : env) ((v1, v2, v3) : CST.argument) =
  let arg_name_str =
    match v1 with
    | Some (name_choice, colon) ->
        let str_tok =
          match name_choice with
          | `Name tok
          | `Pat_array tok
          | `Pat_fn tok
          | `Pat_func tok
          | `Pat_match tok
          | `Pat_name tok
          | `Pat_null tok
          | `Pat_static tok
          | `Pat_throw tok
          | `Pat_parent tok
          | `Pat_self tok
          | `Pat_215c2d4 tok ->
              str env tok
        in
        Some (str_tok, token env colon)
    | None -> None
  in
  let amp_tok = Option.map (token env) v2 in
  let wrap_ref e =
    match amp_tok with
    | Some t -> A.ArgRef (t, e)
    | None -> A.Arg e
  in
  match v3 with
  | `Vari_unpa (v1, v2) ->
      let v1 = (* "..." *) token env v1 in
      let v2 = map_expression env v2 in
      A.ArgUnpack (v1, v2)
  | `Exp x -> (
      let e = map_expression env x in
      match arg_name_str with
      | Some (s, colon) -> A.ArgLabel (s, colon, e)
      | None -> wrap_ref e)
  | `Rela_scope x -> wrap_ref (map_relative_scope env x)

and map_arguments (env : env) ((v1, v2, v3) : CST.arguments) =
  let v1 = (* "(" *) token env v1 in
  let args =
    match v2 with
    | Some (`Arg_rep_COMMA_arg_opt_COMMA (a1, rest, _trailing)) ->
        let a1 = map_argument env a1 in
        let rest = List.map (fun (_comma, a) -> map_argument env a) rest in
        a1 :: rest
    | Some (`Vari_plac _tok) ->
        (* PHP 8.1 first-class callable syntax: f(...). Ast_php has no
         * dedicated representation, so we lower to an empty argument list. *)
        []
    | None -> []
  in
  let v3 = (* ")" *) token env v3 in
  (v1, args, v3)

and map_array_creation_expression (env : env)
    (x : CST.array_creation_expression) =
  match x with
  | `Pat_array_LPAR_opt_array_elem_init_rep_COMMA_array_elem_init_opt_COMMA_RPAR
      (v1, v2, v3, v4, v5) ->
      let v1 = (* "array" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        match v3 with
        | Some x ->
            map_anon_array_elem_init_rep_COMMA_array_elem_init_1dad3d4 env x
        | None -> []
      in
      let v4 =
        match v4 with
        | Some tok -> (* "," *) Some (token env tok)
        | None -> None
      in
      let v5 = (* ")" *) token env v5 in
      A.ConsArray (v2, v3, v5)
  | `LBRACK_opt_array_elem_init_rep_COMMA_array_elem_init_opt_COMMA_RBRACK
      (v1, v2, v3, v4) ->
      let v1 = (* "[" *) token env v1 in
      let v2 =
        match v2 with
        | Some x ->
            map_anon_array_elem_init_rep_COMMA_array_elem_init_1dad3d4 env x
        | None -> []
      in
      let v3 =
        match v3 with
        | Some tok -> (* "," *) Some (token env tok)
        | None -> None
      in
      let v4 = (* "]" *) token env v4 in
      A.ConsArray (v1, v2, v4)

and map_array_destructing (env : env) ((v1, v2, v3, v4) : CST.array_destructing)
    =
  let v1 = (* "[" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_anon_choice_choice_array_dest_abfb170 env x
    | None -> A.Id [ ("", Tok.fake_tok v1 "") ]
  in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 =
          match v2 with
          | Some x -> map_anon_choice_choice_array_dest_abfb170 env x
          | None -> A.Id [ ("", Tok.fake_tok v1 "") ]
        in
        v2)
      v3
  in
  let v4 = (* "]" *) token env v4 in
  A.ConsArray (v1, v2 :: v3, v4)

and map_anon_choice_by_ref_2379e10 (env : env)
    (x : CST.anon_choice_by_ref_2379e10) : A.expr =
  match x with
  | `By_ref (amp, v) -> A.Ref (token env amp, map_variable env v)
  | `Exp e -> map_expression env e

and map_array_element_initializer (env : env)
    (x : CST.array_element_initializer) =
  match x with
  | `Choice_by_ref x -> map_anon_choice_by_ref_2379e10 env x
  | `Exp_EQGT_choice_by_ref (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 = map_anon_choice_by_ref_2379e10 env v3 in
      A.Arrow (v1, v2, v3)
  | `Vari_unpa x -> map_variadic_unpacking env x

and map_attribute (env : env) ((v1, v2) : CST.attribute) : A.attribute =
  let v1 = map_anon_choice_name_062e4f2 env v1 in
  match v2 with
  | Some x ->
      let args = map_arguments env x in
      A.Call (A.Id v1, args)
  | None -> A.Id v1

and map_attribute_list (env : env) (xs : CST.attribute_list) : A.attribute list
    =
  List.concat_map
    (fun (v1, v2, v3, _v4_trailing, v5) ->
      let _v1 = (* "#[" *) token env v1 in
      let v2 = map_attribute env v2 in
      let v3 = List.map (fun (_v1, v2) -> map_attribute env v2) v3 in
      let _v5 = (* "]" *) token env v5 in
      v2 :: v3)
    xs

and map_binary_expression (env : env) (x : CST.binary_expression) =
  match x with
  | `Un_exp_pat_inst__class_name_ref (v1, v2, v3) ->
      let v1 = map_unary_expression env v1 in
      let v2 =
        (* pattern [iI][nN][sS][tT][aA][nN][cC][eE][oO][fF] *)
        (A.ArithOp G.Is, token env v2)
      in
      let v3 = map_class_type_designator env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_STARSTAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (A.ArithOp G.Pow, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_BARGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (A.ArithOp G.Pipe, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_QMARKQMARK_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "??" *) (A.ArithOp G.Nullish, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_pat_and_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* pattern and|AND *) (A.ArithOp G.And, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_pat_or_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* pattern or|OR *) (A.ArithOp G.Or, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_pat_xor_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* pattern xor|XOR *) (A.ArithOp G.Xor, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "||" *) (A.ArithOp G.Or, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "&&" *) (A.ArithOp G.And, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "|" *) (A.ArithOp G.BitOr, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "^" *) (A.ArithOp G.BitXor, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "&" *) (A.ArithOp G.BitAnd, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "==" *) (A.ArithOp G.Eq, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_BANGEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "!=" *) (A.ArithOp G.NotEq, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_LTGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<>" *) (A.ArithOp G.NotEq, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_EQEQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "===" *) (A.ArithOp G.PhysEq, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_BANGEQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "!==" *) (A.ArithOp G.NotPhysEq, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_LT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<" *) (A.ArithOp G.Lt, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_GT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ">" *) (A.ArithOp G.Gt, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_LTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<=" *) (A.ArithOp G.LtE, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_GTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ">=" *) (A.ArithOp G.GtE, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_LTEQGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<=>" *) (A.ArithOp G.Cmp, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_LTLT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<<" *) (A.ArithOp G.LSL, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_GTGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ">>" *) (A.ArithOp G.ASR, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_PLUS_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "+" *) (A.ArithOp G.Plus, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_DASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "-" *) (A.ArithOp G.Minus, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_DOT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "." *) (A.ArithOp G.Concat, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_STAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "*" *) (A.ArithOp G.Mult, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_SLASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "/" *) (A.ArithOp G.Div, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)
  | `Exp_PERC_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "%" *) (A.ArithOp G.Mod, token env v2) in
      let v3 = map_expression env v3 in
      A.Binop (v1, v2, v3)

and map_callable_expression (env : env) (x : CST.callable_expression) : A.expr =
  match x with
  | `Call_var x -> map_callable_variable env x
  | `Paren_exp x -> map_parenthesized_expression env x
  | `Dere_scalar x -> map_dereferencable_scalar env x
  | `New_dere_exp x -> map_new_dereferencable_expression env x

and map_callable_variable (env : env) (x : CST.callable_variable) =
  match x with
  | `Simple_var x -> map_simple_variable env x
  | `Dere_subs_exp (v1, v2, v3, v4) ->
      let v1 = map_dereferencable_expression env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 =
        match v3 with
        | Some x -> Some (map_expression env x)
        | None -> None
      in
      let v4 = (* "]" *) token env v4 in
      A.Array_get (v1, (v2, v3, v4))
  | `Member_call_exp (v1, v2, v3, v4) ->
      let v1 = map_dereferencable_expression env v1 in
      let v2 = (* "->" *) token env v2 in
      let v3 = map_member_name env v3 in
      let v4 = map_arguments env v4 in
      A.Call (A.Obj_get (v1, v2, v3), v4)
  | `Null_member_call_exp (v1, v2, v3, v4) ->
      let v1 = map_dereferencable_expression env v1 in
      let v2 = (* "?->" *) token env v2 in
      (* TODO add nullsafe operator to AST *)
      let v3 = map_member_name env v3 in
      let v4 = map_arguments env v4 in
      A.Call (A.Obj_get (v1, v2, v3), v4)
  | `Scoped_call_exp (v1, v2, v3, v4) ->
      let v1 = map_scope_resolution_qualifier env v1 in
      let v2 = (* "::" *) token env v2 in
      let v3 = map_member_name env v3 in
      let v4 = map_arguments env v4 in
      A.Call (A.Class_get (v1, v2, v3), v4)
  | `Func_call_exp (v1, v2) ->
      let v1 =
        match v1 with
        | `Name_ (`Name tok) -> map_function_name env tok
        | `Name_ x -> A.Id (map_anon_choice_name_062e4f2 env x)
        | `Call_exp x -> map_callable_expression env x
      in
      let v2 = map_arguments env v2 in
      A.Call (v1, v2)

and map_function_name env tok =
  let id = str env tok in
  let str, tok = id in
  match String.lowercase_ascii str with
  | "die" -> A.IdSpecial (A.FuncLike A.Exit, tok)
  | "empty" -> A.IdSpecial (A.FuncLike A.Empty, tok)
  | "eval" -> A.IdSpecial (A.FuncLike A.Eval, tok)
  | "exit" -> A.IdSpecial (A.FuncLike A.Exit, tok)
  | "isset" -> A.IdSpecial (A.FuncLike A.Isset, tok)
  | "unset" -> A.IdSpecial (A.FuncLike A.Unset, tok)
  | _ -> A.Id [ id ]

and map_catch_clause (env : env) ((v1, v2, v3, v4, v5, v6) : CST.catch_clause) :
    A.catch =
  let v1 = (* pattern [cC][aA][tT][cC][hH] *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_type_list env v3 in
  let v4 =
    match v4 with
    | Some x -> map_variable_name env x
    | None -> ("", Tok.fake_tok v2 "")
  in
  let v5 = (* ")" *) token env v5 in
  let v6 = map_compound_statement env v6 in
  let ht = A.HintTuple (v2, v3, v5) in
  (v1, ht, v4, v6)

and map_class_constant_access_expression (env : env)
    ((v1, v2, v3) : CST.class_constant_access_expression) : A.expr =
  let v1 = map_scope_resolution_qualifier env v1 in
  let v2 = (* "::" *) token env v2 in
  let v3 =
    match v3 with
    | `Name tok -> A.Id [ str env tok ]
    | `LCURL_exp_RCURL (_lc, e, _rc) -> map_expression env e
  in
  A.Class_get (v1, v2, v3)

and map_new_variable (env : env) (x : CST.new_variable) : A.expr =
  match x with
  | `Simple_var x -> map_simple_variable env x
  | `Var_subs_exp (v1, v2, v3, v4) ->
      let v1 = map_new_variable env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 =
        match v3 with
        | Some x -> Some (map_expression env x)
        | None -> None
      in
      let v4 = (* "]" *) token env v4 in
      A.Array_get (v1, (v2, v3, v4))
  | `Var_member_access_exp (v1, v2, v3) ->
      let v1 = map_new_variable env v1 in
      let v2 = (* "->" *) token env v2 in
      let v3 = map_member_name env v3 in
      A.Obj_get (v1, v2, v3)
  | `Var_null_member_access_exp (v1, v2, v3) ->
      let v1 = map_new_variable env v1 in
      let v2 = (* "?->" *) token env v2 in
      let v3 = map_member_name env v3 in
      A.Obj_get (v1, v2, v3)
  | `Var_scoped_prop_access_exp (v1, v2, v3) ->
      let v1 =
        match v1 with
        | `Name_ x -> A.Id (map_anon_choice_name_062e4f2 env x)
        | `New_var x -> map_new_variable env x
      in
      let v2 = (* "::" *) token env v2 in
      let v3 = map_simple_variable env v3 in
      A.Class_get (v1, v2, v3)

and map_class_type_designator (env : env) (x : CST.class_name_reference) =
  match x with
  | `Name_ x -> A.Id (map_anon_choice_name_062e4f2 env x)
  | `New_var x -> map_new_variable env x
  | `Paren_exp x -> map_parenthesized_expression env x

and map_clone_expression (env : env) ((v1, v2) : CST.clone_expression) =
  let v1 = (* "clone" *) str env v1 in
  let v2 = map_primary_expression env v2 in
  fake_call_to_builtin env v1 [ v2 ]

and map_colon_block (env : env) ((v1, v2) : CST.colon_block) =
  let v1 = (* ":" *) token env v1 in
  let v2 = List.map (map_statement env) v2 in
  A.Block (v1, v2, v1)

and map_compound_statement_ (env : env) ((v1, v2, v3) : CST.compound_statement)
    =
  let v1 = (* "{" *) token env v1 in
  let v2 = List.map (map_statement env) v2 in
  let v3 = (* "}" *) token env v3 in
  (v1, v2, v3)

and map_compound_statement (env : env) ((v1, v2, v3) : CST.compound_statement) =
  A.Block (map_compound_statement_ env (v1, v2, v3))

and map_const_declaration_ (env : env)
    ((v1, v2, v3, _v4, v5, v6, v7) : CST.const_declaration) =
  (* Ast_php's constant_def has no fields for attribute lists, modifiers, or
   * the optional const type, so these are discarded after walking them. *)
  let _v1_attrs =
    match v1 with
    | Some x -> map_attribute_list env x
    | None -> []
  in
  let _v2_mods = List.map (map_modifier env) v2 in
  let v3 = (* pattern [cC][oO][nN][sS][tT] *) token env v3 in
  let v5 = map_const_element env v5 in
  let v6 =
    List.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        map_const_element env v2)
      v6
  in
  let _v7 = map_semicolon env v7 in
  List.map
    (fun (name, expr) ->
      { A.cst_tok = v3; A.cst_name = name; A.cst_body = expr })
    (v5 :: v6)

and map_const_element (env : env) ((v1, v2, v3) : CST.const_element) =
  let v1 = str env v1 in
  let _v2 = (* "=" *) token env v2 in
  let v3 = map_expression env v3 in
  (v1, v3)

and map_declaration_list (env : env) ((v1, v2, v3) : CST.declaration_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 = List.concat_map (map_member_declaration env) v2 in
  let v3 = (* "}" *) token env v3 in
  (v1, v2, v3)

and map_dereferencable_expression (env : env)
    (x : CST.dereferencable_expression) : A.expr =
  match x with
  | `Choice_cast_var x -> map_variable env x
  | `New_dere_exp x -> map_new_dereferencable_expression env x
  | `Class_cst_access_exp x -> map_class_constant_access_expression env x
  | `Paren_exp x -> map_parenthesized_expression env x
  | `Dere_scalar x -> map_dereferencable_scalar env x
  | `Name_ x -> A.Id (map_anon_choice_name_062e4f2 env x)

and map_dereferencable_scalar (env : env) (x : CST.dereferencable_scalar) :
    A.expr =
  match x with
  | `Array_crea_exp x -> map_array_creation_expression env x
  | `Str x -> map_string_ env x

and map_new_dereferencable_expression (env : env)
    ((v1, v2) : CST.new_dereferencable_expression) : A.expr =
  let new_tok = (* "new" *) token env v1 in
  match v2 with
  | `Class_name_ref_args (cnref, args) ->
      let cls = map_class_type_designator env cnref in
      let _, args, _ = map_arguments env args in
      A.New (new_tok, cls, args)
  | `Anon_class x -> map_anonymous_class env new_tok x

and map_dynamic_variable_name (env : env) (x : CST.dynamic_variable_name) =
  match x with
  | `DOLLAR_simple_var (v1, v2) ->
      let v1 = (* "$" *) token env v1 in
      let v2 = map_simple_variable env v2 in
      A.Call
        (A.Id [ (A.builtin "eval_var", v1) ], Tok.fake_bracket v1 [ A.Arg v2 ])
  | `DOLLAR_LCURL_exp_RCURL (v1, v2, v3, v4) ->
      let v1 = (* "$" *) token env v1 in
      let v2 = (* "{" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* "}" *) token env v4 in
      A.Call (A.Id [ (A.builtin "eval_var", v1) ], (v2, [ A.Arg v3 ], v4))

and map_else_clause (env : env) ((v1, v2) : CST.else_clause) =
  let v1 = (* pattern [eE][lL][sS][eE] *) token env v1 in
  let v2 = map_statement env v2 in
  v2

and map_else_clause_2 (env : env) ((v1, v2) : CST.else_clause_2) =
  let v1 = (* pattern [eE][lL][sS][eE] *) token env v1 in
  let v2 = map_colon_block env v2 in
  v2

and map_else_if_clause (env : env) ((v1, v2, v3) : CST.else_if_clause) =
  let v1 = (* pattern [eE][lL][sS][eE][iI][fF] *) token env v1 in
  let v2 = map_parenthesized_expression env v2 in
  let v3 = map_statement env v3 in
  (v1, v2, v3)

and map_else_if_clause_2 (env : env) ((v1, v2, v3) : CST.else_if_clause_2) =
  let v1 = (* pattern [eE][lL][sS][eE][iI][fF] *) token env v1 in
  let v2 = map_parenthesized_expression env v2 in
  let v3 = map_colon_block env v3 in
  (v1, v2, v3)

and map_enum_declaration_list (env : env)
    ((v1, v2, v3) : CST.enum_declaration_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 = List.concat_map (map_enum_member_declaration env) v2 in
  let v3 = (* "}" *) token env v3 in
  (v1, v2, v3)

and map_enum_member_declaration (env : env) (x : CST.enum_member_declaration) :
    classmember list =
  match x with
  | `Enum_case (v1, v2, v3, v4, v5) ->
      let v1 =
        match v1 with
        | Some x -> map_attribute_list env x
        | None -> []
      in
      let v2 = (* "case" *) token env v2 in
      let v3 =
        (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
        str env v3
      in
      let v4 =
        match v4 with
        | Some (v1, v2) ->
            let _v1 = (* "=" *) token env v1 in
            (None, Some (map_expression env v2))
        | None -> (None, None)
      in
      let v5 = map_semicolon env v5 in
      let type_, value = v4 in
      [
        EnumCase
          { cv_name = v3; cv_type = type_; cv_value = value; cv_modifiers = [] };
      ]
  | `Meth_decl x -> [ map_method_declaration env x ]
  | `Use_decl x -> map_use_declaration env x
  | `Class_const_decl x ->
      List.map (fun c -> ConstantDef c) (map_class_const_declaration env x)

and map_class_const_declaration (env : env)
    ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.class_const_declaration) =
  (* Ast_php's constant_def has no fields for attributes, final, modifiers, or
   * the optional const type, so these are discarded after walking them. *)
  let _v1_attrs =
    match v1 with
    | Some x -> map_attribute_list env x
    | None -> []
  in
  let _v2_final =
    match v2 with
    | Some x -> [ (A.Final, token env x) ]
    | None -> []
  in
  let _v3_mods = List.map (map_modifier env) v3 in
  let v4 = (* pattern [cC][oO][nN][sS][tT] *) token env v4 in
  let _v5_type =
    match v5 with
    | Some x -> Some (map_type_ env x)
    | None -> None
  in
  let mk (n, _eq, e) =
    let n = str env n in
    let _eq = (* "=" *) token env _eq in
    let e = map_expression env e in
    { A.cst_tok = v4; A.cst_name = n; A.cst_body = e }
  in
  let v6 = mk v6 in
  let v7 = List.map (fun (_c, e) -> mk e) v7 in
  let _v8 = map_semicolon env v8 in
  v6 :: v7

(* Exponentiation moved into binary_expression; handled there. *)

and map_expression (env : env) (x : CST.expression) : A.expr =
  match x with
  | `Cond_exp (v1, v2, v3, v4, v5) -> (
      let v1 = map_expression env v1 in
      let v2 = (* "?" *) token env v2 in
      let v3 =
        match v3 with
        | Some x -> Some (map_expression env x)
        | None -> None
      in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_expression env v5 in
      match v3 with
      | Some e -> A.CondExpr (v1, e, v5)
      | None ->
          let elvis = (A.ArithOp G.Elvis, Tok.combine_toks v2 [ v4 ]) in
          A.Binop (v1, elvis, v5))
  | `Match_exp (v1, v2, v3) ->
      let v1 = (* pattern [mM][aA][tT][cC][hH] *) token env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_match_block env v3 in
      A.Match (v1, v2, v3)
  | `Augm_assign_exp (v1, v2, v3) ->
      let v1 = map_variable env v1 in
      let v2 =
        match v2 with
        | `STARSTAREQ tok -> (* "**=" *) (A.ArithOp G.Pow, token env tok)
        | `STAREQ tok -> (* "*=" *) (A.ArithOp G.Mult, token env tok)
        | `SLASHEQ tok -> (* "/=" *) (A.ArithOp G.Div, token env tok)
        | `PERCEQ tok -> (* "%=" *) (A.ArithOp G.Mod, token env tok)
        | `PLUSEQ tok -> (* "+=" *) (A.ArithOp G.Plus, token env tok)
        | `DASHEQ tok -> (* "-=" *) (A.ArithOp G.Minus, token env tok)
        | `DOTEQ tok -> (* ".=" *) (A.ArithOp G.Concat, token env tok)
        | `LTLTEQ tok -> (* "<<=" *) (A.ArithOp G.LSL, token env tok)
        | `GTGTEQ tok -> (* ">>=" *) (A.ArithOp G.ASR, token env tok)
        | `AMPEQ tok -> (* "&=" *) (A.ArithOp G.BitAnd, token env tok)
        | `HATEQ tok -> (* "^=" *) (A.ArithOp G.BitXor, token env tok)
        | `BAREQ tok -> (* "|=" *) (A.ArithOp G.BitOr, token env tok)
        | `QMARKQMARKEQ tok -> (* "??=" *) (A.ArithOp G.Nullish, token env tok)
      in
      let v3 = map_expression env v3 in
      A.AssignOp (v1, v2, v3)
  | `Ref_assign_exp (v1, v2, v3, v4) ->
      let v1 =
        match v1 with
        | `Choice_cast_var x -> map_variable env x
        | `List_lit x -> map_list_literal env x
      in
      let v2 = (* "=" *) token env v2 in
      let v3 = (* "&" *) token env v3 in
      let v4 = map_expression env v4 in
      A.Assign (v1, v2, A.Ref (v3, v4))
  | `Error_supp_exp x -> map_error_suppression_expression env x
  | `Assign_exp (v1, v2, v3) ->
      let v1 =
        match v1 with
        | `Choice_cast_var x -> map_variable env x
        | `List_lit x -> map_list_literal env x
      in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      A.Assign (v1, v2, v3)
  | `Yield_exp x -> map_yield_expression env x
  | `Un_exp x -> map_unary_expression env x
  | `Bin_exp x -> map_binary_expression env x
  | `Incl_exp (v1, v2) ->
      let v1 = (* pattern [iI][nN][cC][lL][uU][dD][eE] *) str env v1 in
      let v2 = map_expression env v2 in
      fake_call_to_builtin env v1 [ v2 ]
  | `Incl_once_exp (v1, v2) ->
      let v1 =
        (* pattern [iI][nN][cC][lL][uU][dD][eE][__][oO][nN][cC][eE] *)
        str env v1
      in
      let v2 = map_expression env v2 in
      fake_call_to_builtin env v1 [ v2 ]
  | `Requ_exp (v1, v2) ->
      let v1 = (* pattern [rR][eE][qQ][uU][iI][rR][eE] *) str env v1 in
      let v2 = map_expression env v2 in
      fake_call_to_builtin env v1 [ v2 ]
  | `Requ_once_exp (v1, v2) ->
      let v1 =
        (* pattern [rR][eE][qQ][uU][iI][rR][eE][__][oO][nN][cC][eE] *)
        str env v1
      in
      let v2 = map_expression env v2 in
      fake_call_to_builtin env v1 [ v2 ]

and map_expressions (env : env) (x : CST.expressions) : A.expr list =
  match x with
  | `Exp x -> [ map_expression env x ]
  | `Seq_exp x -> map_sequence_expression env x

and map_finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 = (* pattern [fF][iI][nN][aA][lL][lL][yY] *) token env v1 in
  let v2 = map_compound_statement env v2 in
  (v1, v2)

and split_catch_finally (env : env) cfs catches finallies =
  match cfs with
  | [] -> (catches, finallies)
  | cf :: tail -> (
      let catches, finallies = split_catch_finally env tail catches finallies in
      match cf with
      | `Catch_clause x ->
          let c = map_catch_clause env x in
          (c :: catches, finallies)
      | `Fina_clause x ->
          let catches, finallies =
            split_catch_finally env tail catches finallies
          in
          let f = map_finally_clause env x in
          (catches, f :: finallies))

and map_foreach_pair (env : env) ((v1, v2, v3) : CST.foreach_pair) =
  let v1 = map_expression env v1 in
  let v2 = (* "=>" *) token env v2 in
  let v3 = map_foreach_value env v3 in
  A.Arrow (v1, v2, v3)

and map_foreach_value (env : env) (x : CST.foreach_value) =
  match x with
  | `By_ref (amp, v) -> A.Ref (token env amp, map_variable env v)
  | `Exp e -> map_expression env e
  | `List_lit x -> map_list_literal env x

and map_formal_parameters (env : env) ((v1, v2, v3, v4) : CST.formal_parameters)
    : A.parameter list =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_simple_param_5af5eb3 env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let v1 = (* "," *) token env v1 in
              let v2 = map_anon_choice_simple_param_5af5eb3 env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 =
    match v3 with
    | Some tok -> (* "," *) Some (token env tok)
    | None -> None
  in
  let v4 = (* ")" *) token env v4 in
  v2

(* Returns: (function_tok, is_ref, name, params, return_type) *)
and map_function_definition_header_5 (env : env)
    ((v1, v2, v3, v4, v5) :
      CST.pat_func
      * CST.variadic_placeholder option
      * CST.name
      * CST.formal_parameters
      * CST.return_type option) =
  let v1 = (* pattern [fF][uU][nN][cC][tT][iI][oO][nN] *) token env v1 in
  let v2 = Option.is_some v2 in
  let v3 = str env v3 in
  let v4 = map_formal_parameters env v4 in
  let v5 =
    match v5 with
    | Some x -> Some (map_return_type env x)
    | None -> None
  in
  (v1, v2, v3, v4, v5)

and map_list_destructing (env : env)
    ((v1, v2, v3, v4, v5) : CST.list_destructing) =
  let v1 = (* "list" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    match v3 with
    | Some x -> map_anon_choice_choice_list_dest_c865322 env x
    | None -> A.Id [ ("", Tok.fake_tok v2 "") ]
  in
  let v4 =
    List.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 =
          match v2 with
          | Some x -> map_anon_choice_choice_list_dest_c865322 env x
          | None -> A.Id [ ("", Tok.fake_tok v1 "") ]
        in
        v2)
      v4
  in
  let v5 = (* ")" *) token env v5 in
  A.List (v2, v3 :: v4, v5)

and map_list_literal (env : env) (x : CST.list_literal) =
  match x with
  | `List_dest x -> map_list_destructing env x
  | `Array_dest x -> map_array_destructing env x

and map_match_block (env : env) ((v1, v2, _v3_trail, v4) : CST.match_block) :
    A.match_ list =
  let _v1 = (* "{" *) token env v1 in
  let cases =
    match v2 with
    | Some (h, rest) ->
        let h = map_anon_choice_match_cond_exp_d891119 env h in
        let rest =
          List.map
            (fun (_c, x) -> map_anon_choice_match_cond_exp_d891119 env x)
            rest
        in
        h :: rest
    | None -> []
  in
  let _v4 = (* "}" *) token env v4 in
  cases

and map_match_condition_list (env : env)
    ((v1, v2, _v3_trail) : CST.match_condition_list) =
  let v1 = map_expression env v1 in
  let v2 = List.map (fun (_c, e) -> map_expression env e) v2 in
  v1 :: v2

and map_member_access_expression (env : env)
    ((v1, v2, v3) : CST.member_access_expression) =
  let v1 = map_dereferencable_expression env v1 in
  let v2 = (* "->" *) token env v2 in
  let v3 = map_member_name env v3 in
  A.Obj_get (v1, v2, v3)

and map_member_declaration (env : env) (x : CST.member_declaration) :
    classmember list =
  match x with
  | `Class_const_decl x ->
      List.map (fun c -> ConstantDef c) (map_class_const_declaration env x)
  | `Prop_decl (v1, v2, v3, v4, v5, v6) ->
      let v1 =
        match v1 with
        | Some x -> map_attribute_list env x
        | None -> []
      in
      let v2 = map_modifiers env v2 in
      let v3 =
        match v3 with
        | Some x -> Some (map_type_ env x)
        | None -> None
      in
      let v4 = map_property_element env v4 in
      let v5 =
        List.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_property_element env v2 in
            v2)
          v5
      in
      let _v6 =
        match v6 with
        | `Semi x -> map_semicolon env x
        | `Prop_hook_list _ -> Tok.unsafe_fake_tok ";"
      in
      List.map
        (fun (name, value) ->
          ClassVar
            {
              A.cv_name = name;
              A.cv_type = v3;
              A.cv_value = value;
              A.cv_modifiers = v2;
            })
        (v4 :: v5)
  | `Meth_decl x -> [ map_method_declaration env x ]
  | `Use_decl x -> map_use_declaration env x

and map_member_name (env : env) (x : CST.member_name) =
  match x with
  | `Choice_name x -> (
      match x with
      | `Name tok ->
          (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
          A.Id (map_name env tok)
      | `Simple_var x -> map_simple_variable env x)
  | `LCURL_exp_RCURL (v1, v2, v3) ->
      let _v1 = (* "{" *) token env v1 in
      let v2 = map_expression env v2 in
      let _v3 = (* "}" *) token env v3 in
      v2

and map_method_declaration (env : env)
    ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.method_declaration) : classmember =
  let v1 =
    match v1 with
    | Some x -> map_attribute_list env x
    | None -> []
  in
  let v2 = map_modifiers env v2 in
  let header = map_function_definition_header_5 env (v3, v4, v5, v6, v7) in
  let v4 =
    match v8 with
    | `Comp_stmt x -> map_compound_statement env x
    | `Semi x -> map_empty_block env (map_semicolon env x)
  in
  let tok, is_ref, name, params, return = header in
  MethodDef
    {
      A.f_name = name;
      A.f_kind = (Method, tok);
      A.f_params = params;
      A.f_return_type = return;
      A.f_ref = is_ref;
      A.m_modifiers = v2;
      A.f_attrs = v1;
      A.l_uses = [];
      A.f_body = v4;
    }

and map_nullsafe_member_access_expression (env : env)
    ((v1, v2, v3) : CST.nullsafe_member_access_expression) =
  let v1 = map_dereferencable_expression env v1 in
  let v2 = (* "?->" *) token env v2 in
  (* TODO add nullsafe operator to AST *)
  let v3 = map_member_name env v3 in
  A.Obj_get (v1, v2, v3)

and map_object_creation_expression (env : env)
    (x : CST.object_creation_expression) =
  match x with
  | `New_dere_exp x -> map_new_dereferencable_expression env x
  | `New_non_dere_exp (v1, v2) -> (
      let v1 = (* "new" *) token env v1 in
      match v2 with
      | `Name_ _
      | `New_var _
      | `Paren_exp _ ->
          A.New (v1, map_class_type_designator env v2, []))

and map_anonymous_class (env : env) (new_tok : G.tok)
    ((v1, v2, v3, v4, v5, v6, v7) : CST.anonymous_class) : A.expr =
  let v1 =
    match v1 with
    | Some x -> map_attribute_list env x
    | None -> []
  in
  let v2 = map_modifiers env v2 in
  let v3 = (* "class" *) token env v3 in
  let v4 =
    match v4 with
    | Some x ->
        let _, args, _ = map_arguments env x in
        args
    | None -> []
  in
  let v5 =
    match v5 with
    | Some x ->
        Some (List_.hd_exn "unexpected empty list" (map_base_clause env x))
    | None -> None
  in
  let v6 =
    match v6 with
    | Some x -> map_class_interface_clause env x
    | None -> []
  in
  let opn, decls, cls = map_declaration_list env v7 in
  let consts, vars, methods, uses = split_classmembers env decls in
  A.NewAnonClass
    ( v3,
      v4,
      {
        c_name = ("", new_tok);
        c_kind = (A.Class, new_tok);
        c_extends = v5;
        c_implements = v6;
        c_uses = uses;
        c_enum_type = None;
        c_modifiers = v2;
        c_attrs = v1;
        c_constants = consts;
        c_variables = vars;
        c_methods = methods;
        c_braces = (opn, (), cls);
      } )

and map_parenthesized_expression (env : env)
    ((v1, v2, v3) : CST.parenthesized_expression) : A.expr =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* ")" *) token env v3 in
  v2

and map_primary_expression (env : env) (x : CST.primary_expression) : A.expr =
  match x with
  | `Choice_cast_var x -> map_variable env x
  | `Lit x -> map_literal env x
  | `Class_cst_access_exp x -> map_class_constant_access_expression env x
  | `Qual_name x -> A.Id (map_qualified_name env x)
  | `Rela_name x -> A.Id (map_relative_name env x)
  | `Name tok ->
      (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
      A.Id (map_name env tok)
  | `Array_crea_exp x -> map_array_creation_expression env x
  | `Print_intr (v1, v2) ->
      let v1 = (* "print" *) str env v1 in
      let v2 = map_expression env v2 in
      fake_call_to_builtin env v1 [ v2 ]
  | `Anon_func ((v1a, v1b, v2, v3, v4, v5, v6), v7) ->
      let v1a =
        match v1a with
        | Some x -> map_attribute_list env x
        | None -> []
      in
      let v1 =
        match v1b with
        | Some tok ->
            (* pattern [sS][tT][aA][tT][iI][cC] *) [ (A.Static, token env tok) ]
        | None -> []
      in
      let v2 = (* pattern [fF][uU][nN][cC][tT][iI][oO][nN] *) token env v2 in
      let v3 = (* "&" *) Option.is_some v3 in
      let v4 = map_formal_parameters env v4 in
      let v5 =
        match v5 with
        | Some x -> map_anonymous_function_use_clause env x
        | None -> []
      in
      let v6 =
        match v6 with
        | Some x -> Some (map_return_type env x)
        | None -> None
      in
      let v7 = map_compound_statement env v7 in
      A.Lambda
        {
          A.f_name = ("", v2);
          A.f_kind = (AnonLambda, v2);
          A.f_params = v4;
          A.f_return_type = v6;
          A.f_ref = v3;
          A.m_modifiers = v1;
          A.f_attrs = v1a;
          A.l_uses = v5;
          A.f_body = v7;
        }
  | `Arrow_func ((v1a, v1, v2, v3, v4, v5), v6, v7) ->
      let v1a =
        match v1a with
        | Some x -> map_attribute_list env x
        | None -> []
      in
      let v1 =
        match v1 with
        | Some tok ->
            (* pattern [sS][tT][aA][tT][iI][cC] *) [ (A.Static, token env tok) ]
        | None -> []
      in
      let v2 = (* pattern [fF][nN] *) token env v2 in
      let v3 = (* "&" *) Option.is_some v3 in
      let v4 = map_formal_parameters env v4 in
      let v5 =
        match v5 with
        | Some x -> Some (map_return_type env x)
        | None -> None
      in
      let v6 = (* "=>" *) token env v6 in
      let v7 = map_expression env v7 in
      A.Lambda
        {
          A.f_name = ("", v2);
          A.f_kind = (ShortLambda, v2);
          A.f_params = v4;
          A.f_return_type = v5;
          A.f_ref = v3;
          A.m_modifiers = v1;
          A.f_attrs = v1a;
          A.l_uses = [];
          A.f_body = Expr (v7, Tok.unsafe_sc);
        }
  | `Obj_crea_exp x -> map_object_creation_expression env x
  | `Update_exp x -> map_update_expression env x
  | `Shell_cmd_exp (v1, _v2_body, v3) ->
      let tok = token env v1 in
      let _ = token env v3 in
      A.Call
        ( A.Id [ (A.builtin "exec", tok (* not really an exec token *)) ],
          Tok.fake_bracket tok [ (* TODO insert content of backquote expr *) ]
        )
  | `Paren_exp x -> map_parenthesized_expression env x
  | `Throw_exp (v1, v2) ->
      let v1 = (* pattern [tT][hH][rR][oO][wW] *) token env v1 in
      let v2 = map_expression env v2 in
      A.Throw (v1, v2)

and map_property_element (env : env) ((v1, v2) : CST.property_element) =
  let v1 = map_variable_name env v1 in
  let v2 =
    match v2 with
    | Some x -> Some (map_property_initializer env x)
    | None -> None
  in
  (v1, v2)

and map_property_initializer (env : env)
    ((v1, v2) : CST.variadic_placeholder * CST.expression) =
  let _v1 = (* "=" *) token env v1 in
  map_expression env v2

and map_scope_resolution_qualifier (env : env)
    (x : CST.scope_resolution_qualifier) =
  match x with
  | `Rela_scope x -> map_relative_scope env x
  | `Name_ x -> A.Id (map_anon_choice_name_062e4f2 env x)
  | `Dere_exp x -> map_dereferencable_expression env x

and map_sequence_expression (env : env) ((v1, v2, v3) : CST.sequence_expression)
    =
  let v1 = map_expression env v1 in
  let v2 = (* "," *) token env v2 in
  let v3 =
    match v3 with
    | `Seq_exp x -> map_sequence_expression env x
    | `Exp x -> [ map_expression env x ]
  in
  v1 :: v3

and map_statement (env : env) (x : CST.statement) =
  match x with
  | `Empty_stmt tok -> (* ";" *) map_empty_block env (token env tok)
  | `Comp_stmt x -> map_compound_statement env x
  | `Named_label_stmt x -> map_named_label_statement env x
  | `Exp_stmt (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = map_semicolon env v2 in
      Expr (v1, v2)
  | `If_stmt (v1, v2, v3) ->
      let v1 = (* pattern [iI][fF] *) token env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 =
        match v3 with
        | `Stmt_rep_else_if_clause_opt_else_clause (v1, v2, v3) ->
            let v1 = map_statement env v1 in
            let v2 = List.map (map_else_if_clause env) v2 in
            let v3 =
              match v3 with
              | Some x -> Some (map_else_clause env x)
              | None -> None
            in
            (v1, v2, v3)
        | `Colon_blk_rep_else_if_clause_2_opt_else_clause_2_pat_endif_semi
            (v1, v2, v3, v4, v5) ->
            let v1 = map_colon_block env v1 in
            let v2 = List.map (map_else_if_clause_2 env) v2 in
            let v3 =
              match v3 with
              | Some x -> Some (map_else_clause_2 env x)
              | None -> None
            in
            let v4 = (* pattern [eE][nN][dD][iI][fF] *) token env v4 in
            let v5 = map_semicolon env v5 in
            (v1, v2, v3)
      in
      let stmt, elseifs, else_opt = v3 in
      let else_chain = chain_else_if env elseifs else_opt in
      A.If (v1, v2, stmt, else_chain)
  | `Switch_stmt (v1, v2, v3) ->
      let v1 = (* pattern [sS][wW][iI][tT][cC][hH] *) token env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_switch_block env v3 in
      A.Switch (v1, v2, v3)
  | `While_stmt (v1, v2, v3) ->
      let v1 = (* pattern [wW][hH][iI][lL][eE] *) token env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 =
        match v3 with
        | `Stmt x -> map_statement env x
        | `Colon_blk_pat_endw_semi (v1, v2, v3) ->
            let v1 = map_colon_block env v1 in
            let v2 =
              (* pattern [eE][nN][dD][wW][hH][iI][lL][eE] *) token env v2
            in
            let v3 = map_semicolon env v3 in
            v1
      in
      A.While (v1, v2, v3)
  | `Do_stmt (v1, v2, v3, v4, v5) ->
      let v1 = (* pattern [dD][oO] *) token env v1 in
      let v2 = map_statement env v2 in
      let v3 = (* pattern [wW][hH][iI][lL][eE] *) token env v3 in
      let v4 = map_parenthesized_expression env v4 in
      let v5 = map_semicolon env v5 in
      A.Do (v1, v2, v4)
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 = (* pattern [fF][oO][rR] *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        match v3 with
        | Some x -> map_expressions env x
        | None -> []
      in
      let v4 = (* ";" *) token env v4 in
      let v5 =
        match v5 with
        | Some x -> map_expressions env x
        | None -> []
      in
      let v6 = (* ";" *) token env v6 in
      let v7 =
        match v7 with
        | Some x -> map_expressions env x
        | None -> []
      in
      let v8 = (* ")" *) token env v8 in
      let v9 =
        match v9 with
        | `Semi x -> map_empty_block env (map_semicolon env x)
        | `Stmt x -> map_statement env x
        | `COLON_rep_stmt_pat_endfor_semi (v1, v2, v3, v4) ->
            let v1 = (* ":" *) token env v1 in
            let v2 = List.map (map_statement env) v2 in
            let v3 = (* pattern [eE][nN][dD][fF][oO][rR] *) token env v3 in
            let v4 = map_semicolon env v4 in
            A.Block (v1, v2, v3)
      in
      A.For (v1, v3, v5, v7, v9)
  | `Fore_stmt (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = (* pattern [fF][oO][rR][eE][aA][cC][hH] *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* pattern [aA][sS] *) token env v4 in
      let v5 =
        match v5 with
        | `Fore_pair x -> map_foreach_pair env x
        | `Fore_value x -> map_foreach_value env x
      in
      let v6 = (* ")" *) token env v6 in
      let v7 =
        match v7 with
        | `Semi x -> map_empty_block env (map_semicolon env x)
        | `Stmt x -> map_statement env x
        | `Colon_blk_pat_endf_semi (v1, v2, v3) ->
            let v1 = map_colon_block env v1 in
            let v2 =
              (* pattern [eE][nN][dD][fF][oO][rR][eE][aA][cC][hH] *)
              token env v2
            in
            let v3 = map_semicolon env v3 in
            v1
      in
      A.Foreach (v1, v3, v4, v5, v7)
  | `Goto_stmt (v1, v2, v3) ->
      let v1 = (* pattern [gG][oO][tT][oO] *) token env v1 in
      let v2 =
        (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
        str env v2
      in
      let v3 = map_semicolon env v3 in
      A.Goto (v1, v2)
  | `Cont_stmt (v1, v2, v3) ->
      let v1 = (* pattern [cC][oO][nN][tT][iI][nN][uU][eE] *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> Some (map_expression env x)
        | None -> None
      in
      let v3 = map_semicolon env v3 in
      A.Continue (v1, v2)
  | `Brk_stmt (v1, v2, v3) ->
      let v1 = (* pattern [bB][rR][eE][aA][kK] *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> Some (map_expression env x)
        | None -> None
      in
      let v3 = map_semicolon env v3 in
      A.Break (v1, v2)
  | `Ret_stmt (v1, v2, v3) ->
      let v1 = (* pattern [rR][eE][tT][uU][rR][nN] *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> Some (map_expression env x)
        | None -> None
      in
      let v3 = map_semicolon env v3 in
      A.Return (v1, v2)
  | `Try_stmt (v1, v2, v3) ->
      let v1 = (* pattern [tT][rR][yY] *) token env v1 in
      let v2 = map_compound_statement env v2 in
      let catches, finallies = split_catch_finally env v3 [] [] in
      A.Try (v1, v2, catches, finallies)
  | `Decl_stmt (v1, v2, v3, v4, v5) ->
      let v1 = (* "declare" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = A.Arg (map_declare_directive env v3) in
      let v4 = (* ")" *) token env v4 in
      let v5 =
        match v5 with
        | `Stmt x -> (
            (* approximate: treat statement as carrying its own semicolon *)
            match x with
            | `Empty_stmt t -> token env t
            | _ -> Tok.unsafe_fake_tok ";")
        | `COLON_rep_stmt_pat_endd_semi (v1, v2, v3, v4) ->
            let _v1 = (* ":" *) token env v1 in
            let _v2 = List.map (map_statement env) v2 in
            let _v3 =
              (* pattern [eE][nN][dD][dD][eE][cC][lL][aA][rR][eE] *)
              token env v3
            in
            map_semicolon env v4
        | `Semi x -> map_semicolon env x
      in
      A.Expr (A.Call (A.Id [ (A.builtin "declare", v1) ], (v2, [ v3 ], v4)), v5)
  | `Echo_stmt (v1, v2, v3) ->
      let v1 = (* pattern [eE][cC][hH][oO] *) str env v1 in
      let v2 = map_expressions env v2 in
      let v3 = map_semicolon env v3 in
      A.Expr (fake_call_to_builtin env v1 v2, v3)
  | `Unset_stmt (v1, v2, v3, v4, _v5_trail, v6, v7) ->
      let v1 = (* "unset" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = A.Arg (map_variable env v3) in
      let v4 = List.map (fun (_c, v) -> A.Arg (map_variable env v)) v4 in
      let v6 = (* ")" *) token env v6 in
      let v7 = map_semicolon env v7 in
      A.Expr
        (A.Call (A.IdSpecial (A.FuncLike A.Unset, v1), (v2, v3 :: v4, v6)), v7)
  | `Const_decl x ->
      let consts = map_const_declaration_ env x in
      let consts = List.map (fun c -> A.ConstantDef c) consts in
      stmt1 consts
  | `Func_defi (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 =
        match v1 with
        | Some x -> map_attribute_list env x
        | None -> []
      in
      let header = map_function_definition_header_5 env (v2, v3, v4, v5, v6) in
      let v3 = map_compound_statement env v7 in
      let tok, is_ref, name, params, return = header in
      A.FuncDef
        {
          A.f_name = name;
          A.f_kind = (Function, tok);
          A.f_params = params;
          A.f_return_type = return;
          A.f_ref = is_ref;
          A.m_modifiers = [];
          A.f_attrs = v1;
          A.l_uses = [];
          A.f_body = v3;
        }
  | `Class_decl (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 =
        match v1 with
        | Some x -> map_attribute_list env x
        | None -> []
      in
      let v2 = map_modifiers env v2 in
      let v3 = (* pattern [cC][lL][aA][sS][sS] *) token env v3 in
      let v4 =
        (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
        str env v4
      in
      let v5 =
        match v5 with
        | Some x ->
            Some (List_.hd_exn "unexpected empty list" (map_base_clause env x))
        | None -> None
      in
      let v6 =
        match v6 with
        | Some x -> map_class_interface_clause env x
        | None -> []
      in
      let v7 = map_declaration_list env v7 in
      let opn, decls, cls = v7 in
      let consts, vars, methods, uses = split_classmembers env decls in
      ClassDef
        {
          c_name = v4;
          c_kind = (A.Class, v3);
          c_extends = v5;
          c_implements = v6;
          c_uses = uses;
          c_enum_type = None;
          c_modifiers = v2;
          c_attrs = v1;
          c_constants = consts;
          c_variables = vars;
          c_methods = methods;
          c_braces = (opn, (), cls);
        }
  | `Inte_decl (v0, v1, v2, v3, v4) ->
      let v0 =
        match v0 with
        | Some x -> map_attribute_list env x
        | None -> []
      in
      let v1 =
        (* pattern [iI][nN][tT][eE][rR][fF][aA][cC][eE] *) token env v1
      in
      let v2 =
        (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
        str env v2
      in
      let v3 =
        match v3 with
        | Some x -> map_base_clause env x
        | None -> []
      in
      let v4 = map_declaration_list env v4 in
      let opn, decls, cls = v4 in
      let consts, vars, methods, uses = split_classmembers env decls in
      ClassDef
        {
          c_name = v2;
          c_kind = (A.Interface, v1);
          c_extends = None;
          c_implements = v3;
          c_uses = uses;
          c_enum_type = None;
          c_modifiers = [];
          c_attrs = v0;
          c_constants = consts;
          c_variables = vars;
          c_methods = methods;
          c_braces = (opn, (), cls);
        }
  | `Trait_decl (v0, v1, v2, v3) ->
      let v0 =
        match v0 with
        | Some x -> map_attribute_list env x
        | None -> []
      in
      let v1 = (* pattern [tT][rR][aA][iI][tT] *) token env v1 in
      let v2 =
        (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
        str env v2
      in
      let v3 = map_declaration_list env v3 in
      let opn, decls, cls = v3 in
      let consts, vars, methods, uses = split_classmembers env decls in
      ClassDef
        {
          c_name = v2;
          c_kind = (A.Trait, v1);
          c_extends = None;
          c_implements = [];
          c_uses = uses;
          c_enum_type = None;
          c_modifiers = [];
          c_attrs = v0;
          c_constants = consts;
          c_variables = vars;
          c_methods = methods;
          c_braces = (opn, (), cls);
        }
  | `Enum_decl (v1, v2, v3, v4, v5, v6) ->
      let v1 =
        match v1 with
        | Some x -> map_attribute_list env x
        | None -> []
      in
      let v2 = (* pattern [eE][nN][uU][mM] *) token env v2 in
      let v3 =
        (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
        str env v3
      in
      let v4 =
        match v4 with
        | Some (colon, choice) ->
            let _colon = token env colon in
            let base =
              match choice with
              | `Int tok -> A.Hint [ ("int", token env tok) ]
              | `Str tok -> A.Hint [ ("string", token env tok) ]
            in
            Some { A.e_base = base; A.e_constraint = None }
        | None -> None
      in
      let v5 =
        match v5 with
        | Some x -> map_class_interface_clause env x
        | None -> []
      in
      let opn, decls, cls = map_enum_declaration_list env v6 in
      let consts, vars, methods, uses = split_classmembers env decls in
      ClassDef
        {
          c_name = v3;
          c_kind = (A.Enum, v2);
          c_extends = None;
          c_implements = v5;
          c_uses = uses;
          c_enum_type = v4;
          c_modifiers = [];
          c_attrs = v1;
          c_constants = consts;
          c_variables = vars;
          c_methods = methods;
          c_braces = (opn, (), cls);
        }
  | `Name_defi (v1, v2) ->
      let v1 =
        (* pattern [nN][aA][mM][eE][sS][pP][aA][cC][eE] *) token env v1
      in
      let v2 =
        match v2 with
        | `Name_name_semi (v1, v2) ->
            let v1 = map_namespace_name env v1 in
            let v2 = map_semicolon env v2 in
            (v1, Tok.fake_bracket v2 [])
        | `Opt_name_name_comp_stmt (v1, v2) ->
            let v1 =
              match v1 with
              | Some x -> map_namespace_name env x
              | None -> []
            in
            let v2 = map_compound_statement_ env v2 in
            (v1, v2)
      in
      let name, block = v2 in
      A.NamespaceDef (v1, name, block)
  | `Name_use_decl (v1, v2, v3) ->
      let use_tok = (* pattern [uU][sS][eE] *) token env v1 in
      let v2 =
        match v2 with
        | `Name_use_clause_rep_COMMA_name_use_clause (c1, rest) ->
            let c1 = map_namespace_use_clause env c1 in
            let rest =
              List.map (fun (_c, x) -> map_namespace_use_clause env x) rest
            in
            let names = c1 :: rest in
            List.map
              (fun (name, alias) -> A.NamespaceUse (use_tok, name, alias))
              names
        | `Name_use_group g ->
            let entries = map_namespace_use_group env g in
            List.map
              (fun (name, alias) -> A.NamespaceUse (use_tok, name, alias))
              entries
      in
      let _v3 = map_semicolon env v3 in
      stmt1 v2
  | `Global_decl (v1, v2, v3, v4) ->
      let v1 = (* pattern [gG][lL][oO][bB][aA][lL] *) token env v1 in
      let v2 = map_simple_variable env v2 in
      let v3 =
        List.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_simple_variable env v2 in
            v2)
          v3
      in
      let v4 = map_semicolon env v4 in
      Global (v1, v2 :: v3)
  | `Func_static_decl (v1, v2, v3, v4) ->
      let v1 = (* pattern [sS][tT][aA][tT][iI][cC] *) token env v1 in
      let v2 = map_static_variable_declaration env v2 in
      let v3 =
        List.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_static_variable_declaration env v2 in
            v2)
          v3
      in
      let v4 = map_semicolon env v4 in
      StaticVars (v1, v2 :: v3)
  | `Exit_stmt (v1, v2, v3) ->
      let v1 = (* "exit" *) str env v1 in
      let args =
        match v2 with
        | Some (_lp, e, _rp) -> (
            match e with
            | Some e -> [ map_expression env e ]
            | None -> [])
        | None -> []
      in
      let v3 = map_semicolon env v3 in
      A.Expr (fake_call_to_builtin env v1 args, v3)

and map_static_variable_declaration (env : env)
    ((v1, v2) : CST.static_variable_declaration) : A.var * A.expr option =
  let v1 = map_variable_name env v1 in
  let v2 =
    match v2 with
    | Some x -> Some (map_property_initializer env x)
    | None -> None
  in
  (v1, v2)

and map_switch_block (env : env) (x : CST.switch_block) =
  match x with
  | `LCURL_rep_choice_case_stmt_RCURL (v1, v2, v3) ->
      let v1 = (* "{" *) token env v1 in
      let v2 = List.map (map_anon_choice_case_stmt_f1b35bc env) v2 in
      let v3 = (* "}" *) token env v3 in
      v2
  | `COLON_rep_choice_case_stmt_pat_ends_semi (v1, v2, v3, v4) ->
      let v1 = (* ":" *) token env v1 in
      let v2 = List.map (map_anon_choice_case_stmt_f1b35bc env) v2 in
      let v3 =
        (* pattern [eE][nN][dD][sS][wW][iI][tT][cC][hH] *) token env v3
      in
      let v4 = map_semicolon env v4 in
      v2

and map_unary_expression (env : env) (x : CST.unary_expression) =
  match x with
  | `Clone_exp x -> map_clone_expression env x
  | `Prim_exp x -> map_primary_expression env x
  | `Un_op_exp (op, e) ->
      let e = map_expression env e in
      let op_tok =
        match op with
        | `PLUS tok -> (G.Plus, token env tok)
        | `DASH tok -> (G.Minus, token env tok)
        | `TILDE tok -> (G.BitNot, token env tok)
        | `BANG tok -> (G.Not, token env tok)
      in
      A.Unop (op_tok, e)
  | `Cast_exp (v1, v2, v3, v4) ->
      let _v1 = (* "(" *) token env v1 in
      let v2 = map_cast_type env v2 in
      let _v3 = (* ")" *) token env v3 in
      let v4 =
        match v4 with
        | `Un_exp x -> map_unary_expression env x
        | `Incl_exp x -> map_include_expression env x
        | `Incl_once_exp x -> map_include_once_expression env x
        | `Error_supp_exp x -> map_error_suppression_expression env x
      in
      A.Cast (v2, v4)

and map_update_expression (env : env) (x : CST.update_expression) =
  let map_op = function
    | `PLUSPLUS tok -> (G.Incr, token env tok)
    | `DASHDASH tok -> (G.Decr, token env tok)
  in
  match x with
  | `Choice_DASHDASH_choice_cast_var (op, v) ->
      let op = map_op op in
      let v = map_variable env v in
      A.Infix (op, v)
  | `Choice_cast_var_choice_DASHDASH (v, op) ->
      let v = map_variable env v in
      let op = map_op op in
      A.Postfix (op, v)

and map_use_as_clause (env : env) ((v1, v2, v3) : CST.use_as_clause) =
  let v1 = map_anon_choice_class_cst_access_exp_18f5288 env v1 in
  let v2 = (* pattern [aA][sS] *) token env v2 in
  let v3 =
    match v3 with
    | `Opt_visi_modi_name (v1, v2) ->
        let v1 =
          match v1 with
          | Some x -> map_visibility_modifier env x
          | None -> todo env ()
        in
        let v2 =
          (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
          token env v2
        in
        todo env (v1, v2)
    | `Visi_modi_opt_name (v1, v2) ->
        let v1 = map_visibility_modifier env v1 in
        let v2 =
          match v2 with
          | Some tok ->
              (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *)
              token env tok
          | None -> todo env ()
        in
        todo env (v1, v2)
  in
  todo env (v1, v2, v3)

and map_use_declaration (env : env) ((v1, v2, v3, v4) : CST.use_declaration) :
    classmember list =
  let v1 = (* pattern [uU][sS][eE] *) token env v1 in
  let v2 = A.Hint (map_anon_choice_name_062e4f2 env v2) in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = A.Hint (map_anon_choice_name_062e4f2 env v2) in
        v2)
      v3
  in
  (* The use list is ignored by the pfff parser. For now, for consistency
   * let's ignore it here too. But at some point it could be worthwhile to
   * find a way to represent this in the generic AST. *)
  let _v4 =
    match v4 with
    | `Use_list uses ->
        let _ = map_use_list in
        ()
    | `Semi x ->
        let _ = map_semicolon env x in
        ()
  in
  let uses = v2 :: v3 in
  List.map (fun u -> UseTrait u) uses

and map_use_instead_of_clause (env : env)
    ((v1, v2, v3) : CST.use_instead_of_clause) =
  let v1 = map_class_constant_access_expression env v1 in
  let v2 = (* pattern [iI][nN][sS][tT][eE][aA][dD][oO][fF] *) token env v2 in
  let v3 =
    (* pattern [_a-zA-Z\u00A1-\u00ff][_a-zA-Z\u00A1-\u00ff\d]* *) token env v3
  in
  todo env (v1, v2, v3)

and map_use_list (env : env) ((v1, v2, v3) : CST.use_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let v1 =
          match v1 with
          | `Use_inst_of_clause x -> map_use_instead_of_clause env x
          | `Use_as_clause x -> map_use_as_clause env x
        in
        let v2 = map_semicolon env v2 in
        todo env (v1, v2))
      v2
  in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and map_variable (env : env) (x : CST.variable) =
  match x with
  | `Cast_var (v1, v2, v3, v4) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = map_cast_type env v2 in
      let v3 = (* ")" *) token env v3 in
      let v4 = map_variable env v4 in
      A.Cast (v2, v4)
  | `New_var x -> map_new_variable env x
  | `Call_var x -> map_callable_variable env x
  | `Scoped_prop_access_exp (v1, v2, v3) ->
      let v1 = map_scope_resolution_qualifier env v1 in
      let v2 = (* "::" *) token env v2 in
      let v3 = map_simple_variable env v3 in
      A.Class_get (v1, v2, v3)
  | `Member_access_exp x -> map_member_access_expression env x
  | `Null_member_access_exp x -> map_nullsafe_member_access_expression env x

and map_simple_variable (env : env) (x : CST.simple_variable) : A.expr =
  match x with
  | `Dyna_var_name x -> map_dynamic_variable_name env x
  | `Var_name x -> (
      let str, tok = map_variable_name env x in
      match str with
      | "$this" -> A.IdSpecial (A.This, tok)
      | _ -> A.Id [ (str, tok) ])

and map_variadic_unpacking (env : env) ((v1, v2) : CST.variadic_unpacking) =
  let v1 = (* "..." *) token env v1 in
  let v2 = map_expression env v2 in
  A.Unpack v2

and map_error_suppression_expression (env : env)
    ((v1, v2) : CST.error_suppression_expression) : A.expr =
  let v1 = (* "@" *) token env v1 in
  let v2 = A.Arg (map_expression env v2) in
  A.Call (A.Id [ (A.builtin "at", v1) ], Tok.fake_bracket v1 [ v2 ])

and map_include_expression (env : env) ((v1, v2) : CST.include_expression) =
  let v1 = (* "include" *) str env v1 in
  let v2 = map_expression env v2 in
  fake_call_to_builtin env v1 [ v2 ]

and map_include_once_expression (env : env)
    ((v1, v2) : CST.include_once_expression) =
  let v1 = (* "include_once" *) str env v1 in
  let v2 = map_expression env v2 in
  fake_call_to_builtin env v1 [ v2 ]

and map_yield_expression (env : env) (x : CST.yield_expression) : A.expr =
  match x with
  | `Pat_yield_opt_array_elem_init (v1, v2) ->
      let v1 = (* "yield" *) str env v1 in
      let v2 =
        match v2 with
        | Some x -> [ map_array_element_initializer env x ]
        | None -> []
      in
      fake_call_to_builtin env v1 v2
  | `Pat_13043a2_exp (v1, v2) ->
      let v1 = (* "yield from" *) str env v1 in
      let v2 = map_expression env v2 in
      fake_call_to_builtin env v1 [ v2 ]

let map_program (env : env) ((v1, v2) : CST.program) : A.program =
  let v1 =
    match v1 with
    | Some x -> Some (map_text env x)
    | None -> None
  in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = (* pattern <\?([pP][hH][pP]|=)? *) token env v1 in
        let v2 = List.map (map_statement env) v2 in
        v2
    | None -> []
  in
  v2

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_php.Parse.file !!file)
    (fun cst _extras ->
      let extra = () in
      let env = { H.file; conv = H.line_col_to_pos file; extra } in
      map_program env cst)
