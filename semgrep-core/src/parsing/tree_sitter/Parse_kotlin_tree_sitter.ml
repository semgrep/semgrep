(* Colleen Dai
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
module CST = Tree_sitter_kotlin.CST
module AST = AST_generic
module H = Parse_tree_sitter_helpers
module PI = Parse_info
open AST_generic
module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* kotlin parser using tree-sitter-lang/semgrep-kotlin and converting
 * directly to pfff/h_program-lang/ast_generic.ml
 *
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = unit H.env

let _fake = AST_generic.fake

let token = H.token

let str = H.str

let fb = fake_bracket

let sc = PI.fake_info ";"

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-kotlin/Boilerplate.ml *)

(**
   Boilerplate to be used as a template when mapping the kotlin CST
   to another type of tree.
*)

let todo (_env : env) _ = failwith "not implemented"

(* less: or ExprStmt (Void)? *)
let empty_stmt env t =
  let t = token env t (* ";" *) in
  Block (t, [], t) |> G.s

let unhandled_keywordattr_to_namedattr (env : env) tok =
  NamedAttr (token env tok, Id (str env tok, empty_id_info ()), fake_bracket [])

let _escaped_identifier (env : env) (tok : CST.escaped_identifier) =
  token env tok

(* pattern "\\\\[tbrn'\dq\\\\$]" *)

let _pat_b294348 (env : env) (tok : CST.pat_b294348) = token env tok

(* pattern "[^\\n\\r'\\\\]" *)

let visibility_modifier (env : env) (x : CST.visibility_modifier) =
  match x with
  | `Public tok -> KeywordAttr (Public, token env tok) (* "public" *)
  | `Priv tok -> KeywordAttr (Private, token env tok) (* "private" *)
  | `Inte tok -> unhandled_keywordattr_to_namedattr env tok (* "internal" *)
  | `Prot tok -> KeywordAttr (Protected, token env tok)

(* "protected" *)

let equality_operator (env : env) (x : CST.equality_operator) =
  match x with
  | `BANGEQ tok -> (NotEq, token env tok) (* "!=" *)
  | `BANGEQEQ tok -> (NotPhysEq, token env tok) (* "!==" *)
  | `EQEQ tok -> (Eq, token env tok) (* "==" *)
  | `EQEQEQ tok -> (PhysEq, token env tok)

(* "===" *)

let _multi_line_str_text (env : env) (tok : CST.multi_line_str_text) =
  token env tok

(* pattern "[^\dq$]+" *)

let _pat_a2e2132 (env : env) (tok : CST.pat_a2e2132) = token env tok

(* pattern [0-9a-fA-F]{4} *)

let _pat_c793459 (env : env) (tok : CST.pat_c793459) = token env tok

(* pattern [uU] *)

let anon_choice_val_2833752 (env : env) (x : CST.anon_choice_val_2833752) =
  match x with
  | `Val tok -> token env tok (* "val" *)
  | `Var tok -> token env tok

(* "var" *)

let platform_modifier (env : env) (x : CST.platform_modifier) =
  match x with
  | `Expect tok -> unhandled_keywordattr_to_namedattr env tok (* "expect" *)
  | `Actual tok -> unhandled_keywordattr_to_namedattr env tok

(* "actual" *)

let _label (env : env) (tok : CST.label) = token env tok

(* label *)

let real_literal (env : env) (tok : CST.real_literal) =
  let s, t = str env tok in
  (* real_literal *)
  Float (float_of_string_opt s, t)

let comparison_operator (env : env) (x : CST.comparison_operator) =
  match x with
  | `LT tok -> (Lt, token env tok) (* "<" *)
  | `GT tok -> (Gt, token env tok) (* ">" *)
  | `LTEQ tok -> (LtE, token env tok) (* "<=" *)
  | `GTEQ tok -> (GtE, token env tok)

(* ">=" *)

let assignment_and_operator (env : env) (x : CST.assignment_and_operator) =
  match x with
  | `PLUSEQ tok -> (Plus, token env tok) (* "+=" *)
  | `DASHEQ tok -> (Minus, token env tok) (* "-=" *)
  | `STAREQ tok -> (Mult, token env tok) (* "*=" *)
  | `SLASHEQ tok -> (Div, token env tok) (* "/=" *)
  | `PERCEQ tok -> (Mod, token env tok)

(* "%=" *)

let inheritance_modifier (env : env) (x : CST.inheritance_modifier) =
  match x with
  | `Abst tok -> KeywordAttr (Abstract, token env tok) (* "abstract" *)
  | `Final tok -> KeywordAttr (Final, token env tok) (* "final" *)
  | `Open tok -> unhandled_keywordattr_to_namedattr env tok

(* "open" *)

let postfix_unary_operator (env : env) (x : CST.postfix_unary_operator) =
  match x with
  | `PLUSPLUS tok -> (Left Incr, token env tok) (* "++" *)
  | `DASHDASH tok -> (Left Decr, token env tok) (* "--" *)
  | `BANGBANG tok -> (Right NotNullPostfix, token env tok)

(* "!!" *)

let variance_modifier (env : env) (x : CST.variance_modifier) =
  match x with `In tok -> token env tok (* "in" *) | `Out tok -> token env tok

(* "out" *)

let member_modifier (env : env) (x : CST.member_modifier) =
  match x with
  | `Over tok -> KeywordAttr (Override, token env tok) (* "override" *)
  | `Late tok -> unhandled_keywordattr_to_namedattr env tok

(* "lateinit" *)

let class_modifier (env : env) (x : CST.class_modifier) =
  match x with
  | `Sealed tok -> unhandled_keywordattr_to_namedattr env tok (* "sealed" *)
  | `Anno tok -> unhandled_keywordattr_to_namedattr env tok (* "annotation" *)
  | `Data tok -> unhandled_keywordattr_to_namedattr env tok (* "data" *)
  | `Inner tok -> unhandled_keywordattr_to_namedattr env tok

(* "inner" *)

let boolean_literal (env : env) (x : CST.boolean_literal) =
  match x with
  | `True tok -> Bool (true, token env tok) (* "true" *)
  | `False tok -> Bool (false, token env tok)

(* "false" *)

let hex_literal (env : env) (tok : CST.hex_literal) =
  let s, t = str env tok (* hex_literal *) in
  (int_of_string_opt s, t)

let use_site_target (env : env) ((v1, v2) : CST.use_site_target) =
  let v1 =
    match v1 with
    | `Field tok -> token env tok (* "field" *)
    | `Prop tok -> token env tok (* "property" *)
    | `Get tok -> token env tok (* "get" *)
    | `Set tok -> token env tok (* "set" *)
    | `Rece tok -> token env tok (* "receiver" *)
    | `Param tok -> token env tok (* "param" *)
    | `Setp tok -> token env tok (* "setparam" *)
    | `Dele tok -> token env tok
    (* "delegate" *)
  in
  let _v2 = token env v2 (* ":" *) in
  v1

let additive_operator (env : env) (x : CST.additive_operator) =
  match x with
  | `PLUS tok -> (Plus, token env tok) (* "+" *)
  | `DASH tok -> (Minus, token env tok)

(* "-" *)

(* note that there's no octal literal in Kotlin so no need for
 * H.int_of_string_c_octal_opt
 *)
let integer_literal (env : env) (tok : CST.integer_literal) =
  let s, t = str env tok (* integer_literal *) in
  (int_of_string_opt s, t)

let as_operator (env : env) (x : CST.as_operator) =
  match x with
  | `As tok -> token env tok (* "as" *)
  | `AsQM tok -> token env tok

(* "as?" *)

let function_modifier (env : env) (x : CST.function_modifier) =
  match x with
  | `Tail tok -> unhandled_keywordattr_to_namedattr env tok (* "tailrec" *)
  | `Op tok -> unhandled_keywordattr_to_namedattr env tok (* "operator" *)
  | `Infix tok -> unhandled_keywordattr_to_namedattr env tok (* "infix" *)
  | `Inline tok -> KeywordAttr (Inline, token env tok) (* "inline" *)
  | `Exte tok -> KeywordAttr (Extern, token env tok) (* "external" *)
  | `Susp tok -> unhandled_keywordattr_to_namedattr env tok

(* "suspend" *)

let _line_str_text (env : env) (tok : CST.line_str_text) = token env tok

(* pattern "[^\\\\\double_quote$]+" *)

let prefix_unary_operator (env : env) (x : CST.prefix_unary_operator) =
  match x with
  | `PLUSPLUS tok -> (Left Incr, token env tok) (* "++" *)
  | `DASHDASH tok -> (Left Decr, token env tok) (* "--" *)
  | `DASH tok -> (Right Minus, token env tok) (* "-" *)
  | `PLUS tok -> (Right Plus, token env tok) (* "+" *)
  | `BANG tok -> (Right Not, token env tok)

(* "!" *)

let in_operator (env : env) (x : CST.in_operator) =
  match x with
  | `In tok -> (In, token env tok) (* "in" *)
  | `BANGin tok -> (NotIn, token env tok)

(* "!in" *)

let multiplicative_operator (env : env) (x : CST.multiplicative_operator) =
  match x with
  | `STAR tok -> (Mult, token env tok) (* "*" *)
  | `SLASH tok -> (Div, token env tok) (* "/" *)
  | `PERC tok -> (Mod, token env tok)

(* "%" *)

let parameter_modifier (env : env) (x : CST.parameter_modifier) =
  match x with
  | `Vararg tok -> unhandled_keywordattr_to_namedattr env tok (* "vararg" *)
  | `Noin tok -> unhandled_keywordattr_to_namedattr env tok (* "noinline" *)
  | `Cros tok -> unhandled_keywordattr_to_namedattr env tok

(* "crossinline" *)

let bin_literal (env : env) (tok : CST.bin_literal) =
  let s, t = str env tok in
  (* bin_literal *)
  (int_of_string_opt s, t)

let multi_line_string_content (env : env) (x : CST.multi_line_string_content) =
  match x with
  | `Multi_line_str_text tok -> token env tok (* pattern "[^\"$]+" *)
  | `DQUOT tok -> token env tok

(* "\"" *)

let uni_character_literal (env : env) ((v1, v2, v3) : CST.uni_character_literal)
    =
  let v1 = str env v1 (* "\\" *) in
  let _v2 = str env v2 (* "u" *) in
  let v3 = str env v3 (* pattern [0-9a-fA-F]{4} *) in
  (fst v3, PI.combine_infos (snd v1) [ snd v3 ])

let type_projection_modifier (env : env) (x : CST.type_projection_modifier) =
  let _ = variance_modifier env x in
  raise Todo

(* ignore, treat as a comment *)
let shebang_line (env : env) ((v1, v2) : CST.shebang_line) =
  let _v1 = token env v1 (* "#!" *) in
  let _v2 = token env v2 (* pattern [^\r\n]* *) in
  ()

let is_operator (env : env) (x : CST.is_operator) =
  match x with
  | `Is tok -> (Is, token env tok) (* "is" *)
  | `Not_is tok -> (NotIs, token env tok)

(* "!is" *)

let modifier (env : env) (x : CST.modifier) : attribute =
  match x with
  | `Class_modi x -> class_modifier env x
  | `Member_modi x -> member_modifier env x
  | `Visi_modi x -> visibility_modifier env x
  | `Func_modi x -> function_modifier env x
  | `Prop_modi tok -> KeywordAttr (Const, token env tok) (* "const" *)
  | `Inhe_modi x -> inheritance_modifier env x
  | `Param_modi x -> parameter_modifier env x
  | `Plat_modi x -> platform_modifier env x

let member_access_operator (env : env) (x : CST.member_access_operator) =
  match x with
  | `DOT tok -> token env tok (* "." *)
  | `Safe_nav tok -> token env tok (* "?." *)
  | `COLONCOLON tok -> token env tok

(* "::" *)

let anon_choice_int_lit_9015f32 (env : env)
    (x : CST.anon_choice_int_lit_9015f32) =
  match x with
  | `Int_lit tok -> integer_literal env tok (* integer_literal *)
  | `Hex_lit tok -> hex_literal env tok (* hex_literal *)
  | `Bin_lit tok -> bin_literal env tok

(* bin_literal *)

let lexical_identifier (env : env) (x : CST.lexical_identifier) : ident =
  match x with
  | `Pat_ddcb2a5 tok -> str env tok (* pattern [a-zA-Z_][a-zA-Z_0-9]* *)
  | `Pat_b9a3713 tok -> str env tok

(* pattern `[^\r\n`]+` *)

let escape_seq (env : env) (x : CST.escape_seq) =
  match x with
  | `Uni_char_lit x -> uni_character_literal env x
  | `Esca_id tok -> str env tok

(* pattern "\\\\[tbrn'\dq\\\\$]" *)

let line_str_escaped_char (env : env) (x : CST.line_str_escaped_char) =
  match x with
  | `Esca_id tok -> str env tok (* pattern "\\\\[tbrn'\dq\\\\$]" *)
  | `Uni_char_lit x -> uni_character_literal env x

let type_projection_modifiers (env : env) (xs : CST.type_projection_modifiers) =
  List.map (type_projection_modifier env) xs

let simple_identifier (env : env) (x : CST.simple_identifier) : ident =
  match x with
  | `Lexi_id x -> lexical_identifier env x
  | `Pat_831065d x -> str env x

(* pattern \$[a-zA-Z_][a-zA-Z_0-9]* *)

let line_string_content (env : env) (x : CST.line_string_content) =
  match x with
  | `Line_str_text tok -> str env tok (* pattern "[^\\\\\double_quote$]+" *)
  | `Line_str_esca_char x -> line_str_escaped_char env x

let return_at (env : env) ((v1, v2) : CST.return_at) =
  let v1 = token env v1 (* "return@" *) in
  let v2 = lexical_identifier env v2 in
  (v1, Some v2)

let identifier (env : env) ((v1, v2) : CST.identifier) : dotted_ident =
  let v1 = simple_identifier env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "." *) in
        let v2 = simple_identifier env v2 in
        v2)
      v2
  in
  v1 :: v2

let directly_assignable_expression (env : env)
    (x : CST.directly_assignable_expression) : expr =
  match x with
  | `Simple_id x ->
      let id = simple_identifier env x in
      N (Id (id, empty_id_info ()))

let import_alias (env : env) ((v1, v2) : CST.import_alias) =
  let v1 = token env v1 (* "as" *) in
  let v2 = simple_identifier env v2 in
  (v1, v2)

let literal_constant (env : env) (x : CST.literal_constant) =
  match x with
  | `Bool_lit x -> boolean_literal env x
  | `Int_lit tok -> Int (integer_literal env tok) (* integer_literal *)
  | `Hex_lit tok -> Int (hex_literal env tok) (* hex_literal *)
  | `Bin_lit tok -> Int (bin_literal env tok) (* bin_literal *)
  | `Char_lit (v1, v2, v3) ->
      let v1 = token env v1 (* "'" *) in
      let v2 =
        match v2 with
        | `Esc_seq x -> escape_seq env x
        | `Pat_b294348 tok -> str env tok
        (* pattern "[^\\n\\r'\\\\]" *)
      in
      let v3 = token env v3 (* "'" *) in
      let toks = [ snd v2 ] @ [ v3 ] in
      Char (fst v2, PI.combine_infos v1 toks)
  | `Real_lit tok -> real_literal env tok (* real_literal *)
  | `Null tok -> Null (token env tok) (* "null" *)
  | `Long_lit (v1, v2) ->
      let v1 = anon_choice_int_lit_9015f32 env v1 in
      let _v2 = token env v2 (* "L" *) in
      Int (fst v1, snd v1)
  | `Unsi_lit (v1, v2, v3) ->
      let iopt, v1 = anon_choice_int_lit_9015f32 env v1 in
      let v2 = str env v2 (* pattern [uU] *) in
      let _v3 =
        match v3 with Some tok -> Some (str env tok) (* "L" *) | None -> None
      in
      let _str = PI.str_of_info v1 ^ fst v2 in
      Int (iopt, PI.combine_infos v1 [ snd v2 ])

let package_header (env : env) ((v1, v2, v3) : CST.package_header) : directive =
  let v1 = token env v1 (* "package" *) in
  let v2 = identifier env v2 in
  let _v3 = token env v3 (* pattern [\r\n]+ *) in
  Package (v1, v2)

let import_header (env : env) ((v1, v2, v3, v4) : CST.import_header) : directive
    =
  let v1 = token env v1 (* "import" *) in
  let v2 = identifier env v2 in
  let v3 =
    match v3 with
    | Some x -> (
        match x with
        | `DOTSTAR x ->
            let t = token env x (* ".*" *) in
            ImportAll (v1, DottedName v2, t)
        | `Import_alias x ->
            let t, id = import_alias env x in
            ImportAs (t, DottedName v2, Some (id, empty_id_info ())) )
    | None -> ImportAs (v1, DottedName v2, None)
  in
  let _v4 = token env v4 (* pattern [\r\n]+ *) in
  v3

let rec _annotated_lambda (env : env) (v1 : CST.annotated_lambda) =
  lambda_literal env v1

and annotation (env : env) (x : CST.annotation) : attribute list =
  match x with
  | `Single_anno (origv1, v2, v3) ->
      let v1 = token env origv1 (* "@" *) in
      let v3 = unescaped_annotation env v3 in
      let v2 =
        match v2 with
        | Some x ->
            let v2 = use_site_target env x in
            NamedAttr
              ( v1,
                Id (("use site target", v2), empty_id_info ()),
                fake_bracket [ v3 ] )
        | None ->
            NamedAttr
              (v1, Id (str env origv1, empty_id_info ()), fake_bracket [ v3 ])
      in
      [ v2 ]
  | `Multi_anno (origv1, v2, v3, v4, v5) ->
      let v1 = token env origv1 (* "@" *) in
      let _v3 = token env v3 (* "[" *) in
      let v4 = List.map (unescaped_annotation env) v4 in
      let _v5 = token env v5 (* "]" *) in
      let v2 =
        match v2 with
        | Some x ->
            let v2 = use_site_target env x in
            NamedAttr
              ( v1,
                Id (("use site target", v2), empty_id_info ()),
                fake_bracket v4 )
        | None ->
            NamedAttr
              (v1, Id (str env origv1, empty_id_info ()), fake_bracket v4)
      in
      [ v2 ]

and anon_choice_param_b77c1d8 (env : env) (x : CST.anon_choice_param_b77c1d8) =
  match x with
  | `Param x ->
      let v1, v2 = parameter env x in
      let param = { (param_of_id v1) with ptype = Some v2 } in
      ParamClassic param
  | `Type x ->
      let v1 = type_ env x in
      ParamClassic (param_of_type v1)

and assignment (env : env) (x : CST.assignment) : expr =
  match x with
  | `Dire_assi_exp_assign_and_op_exp (v1, v2, v3) ->
      let v1 = directly_assignable_expression env v1 in
      let v2 = assignment_and_operator env v2 in
      let v3 = expression env v3 in
      AssignOp (v1, v2, v3)

and binary_expression (env : env) (x : CST.binary_expression) =
  match x with
  | `Mult_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, tok = multiplicative_operator env v2 in
      let v3 = expression env v3 in
      Call (IdSpecial (Op v2, tok), fb [ Arg v1; Arg v3 ])
  | `Addi_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, tok = additive_operator env v2 in
      let v3 = expression env v3 in
      Call (IdSpecial (Op v2, tok), fb [ Arg v1; Arg v3 ])
  | `Range_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, tok = (Range, token env v2) (* ".." *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op v2, tok), fb [ Arg v1; Arg v3 ])
  | `Infix_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = simple_identifier env v2 in
      let v2_id = N (Id (v2, empty_id_info ())) in
      let v3 = expression env v3 in
      Call (v2_id, fb [ Arg v1; Arg v3 ])
  | `Elvis_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, tok = (Elvis, token env v2) (* "?:" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op v2, tok), fb [ Arg v1; Arg v3 ])
  | `Check_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, tok =
        match v2 with
        | `In_op x -> in_operator env x
        | `Is_op x -> is_operator env x
      in
      let v3 = expression env v3 in
      Call (IdSpecial (Op v2, tok), fb [ Arg v1; Arg v3 ])
  | `Comp_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, tok = comparison_operator env v2 in
      let v3 = expression env v3 in
      Call (IdSpecial (Op v2, tok), fb [ Arg v1; Arg v3 ])
  | `Equa_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, tok = equality_operator env v2 in
      let v3 = expression env v3 in
      Call (IdSpecial (Op v2, tok), fb [ Arg v1; Arg v3 ])
  | `Conj_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, tok = (And, token env v2) (* "&&" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op v2, tok), fb [ Arg v1; Arg v3 ])
  | `Disj_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, tok = (Or, token env v2) (* "||" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op v2, tok), fb [ Arg v1; Arg v3 ])

and block (env : env) ((v1, v2, v3) : CST.block) =
  let v1 = token env v1 (* "{" *) in
  let v2 = match v2 with Some x -> statements env x | None -> [] in
  let v3 = token env v3 (* "}" *) in
  Block (v1, v2, v3) |> G.s

and call_suffix (env : env) (v1 : CST.call_suffix) =
  match v1 with
  | `Opt_value_args_anno_lambda (v1, _v2) ->
      let v1 =
        match v1 with
        | Some x -> value_arguments env x
        | None -> fake_bracket []
      in
      (*let v2 = annotated_lambda env v2 in*)
      v1
  | `Value_args x -> value_arguments env x

and catch_block (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.catch_block)
    =
  let v1 = token env v1 (* "catch" *) in
  let _v2 = token env v2 (* "(" *) in
  let v3 = List.map (annotation env) v3 in
  let v4 = simple_identifier env v4 in
  let _v5 = token env v5 (* ":" *) in
  let v6 = type_ env v6 in
  let _v7 = token env v7 (* ")" *) in
  let v8 = block env v8 in
  let _list = [ v3 ] in
  let id = Some (v4, empty_id_info ()) in
  let pattern = PatVar (v6, id) in
  (v1, pattern, v8)

and class_body (env : env) ((v1, v2, v3) : CST.class_body) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    match v2 with Some x -> class_member_declarations env x | None -> []
  in
  let v3 = token env v3 (* "}" *) in
  (v1, v2, v3)

and class_declaration (env : env) (x : CST.class_declaration) :
    entity * class_definition =
  match x with
  | `Opt_modifs_choice_class_simple_id_opt_type_params_opt_prim_cons_opt_COLON_dele_specis_opt_type_consts_opt_class_body
      (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 = match v1 with Some x -> modifiers env x | None -> [] in
      let v2 =
        match v2 with
        | `Class tok -> (Class, token env tok) (* "class" *)
        | `Inte tok -> (Interface, token env tok)
        (* "interface" *)
      in
      let v3 = simple_identifier env v3 in
      let _v4 = match v4 with Some x -> type_parameters env x | None -> [] in
      let _v5 =
        match v5 with
        | Some x -> Some (primary_constructor env x)
        | None -> None
      in
      let _v6 =
        match v6 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* ":" *) in
            let v2 = delegation_specifiers env v2 in
            Some (v1, v2)
        | None -> None
      in
      let _v7 = match v7 with Some x -> type_constraints env x | None -> [] in
      let v8 = match v8 with Some x -> class_body env x | None -> fb [] in
      let ent = G.basic_entity v3 v1 in
      let cdef =
        {
          ckind = v2;
          cextends = [];
          cimplements = [];
          cmixins = [];
          cparams = [];
          cbody = v8;
        }
      in
      (ent, cdef)
  | `Opt_modifs_enum_class_simple_id_opt_type_params_opt_prim_cons_opt_COLON_dele_specis_opt_type_consts_opt_enum_class_body
      (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 = match v1 with Some x -> modifiers env x | None -> [] in
      let _v2 = token env v2 (* "enum" *) in
      let v3 = (Class, token env v3) (* "class" *) in
      let v4 = simple_identifier env v4 in
      let _v5 = match v5 with Some x -> type_parameters env x | None -> [] in
      let _v6 =
        match v6 with
        | Some x -> Some (primary_constructor env x)
        | None -> None
      in
      let _v7 =
        match v7 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* ":" *) in
            let v2 = delegation_specifiers env v2 in
            Some (v1, v2)
        | None -> None
      in
      let _v8 = match v8 with Some x -> type_constraints env x | None -> [] in
      let v9 =
        match v9 with Some x -> enum_class_body env x | None -> fb []
      in
      let ent = G.basic_entity v4 v1 in
      let cdef =
        {
          ckind = v3;
          cextends = [];
          cimplements = [];
          cmixins = [];
          cparams = [];
          cbody = v9;
        }
      in
      (ent, cdef)

and class_member_declaration (env : env) (x : CST.class_member_declaration) :
    field =
  match x with
  | `Decl x ->
      let d = declaration env x in
      FieldStmt (s (DefStmt d))
  | `Comp_obj (v1, v2, v3, v4, v5, v6) ->
      let v1 = match v1 with Some x -> modifiers env x | None -> [] in
      let v2 = token env v2 (* "companion" *) in
      let v3 = token env v3 (* "object" *) in
      let v4 =
        match v4 with
        | Some x -> simple_identifier env x
        | None -> ("companion", v2)
      in
      let _v5 =
        match v5 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* ":" *) in
            let v2 = delegation_specifiers env v2 in
            Some (v1, v2)
        | None -> None
      in
      let v6 = match v6 with Some x -> class_body env x | None -> fb [] in
      let ent = G.basic_entity v4 v1 in
      let cdef =
        {
          ckind = (Object, v3);
          cextends = [];
          cimplements = [];
          cmixins = [];
          cparams = [];
          cbody = v6;
        }
      in
      FieldStmt (s (DefStmt (ent, ClassDef cdef)))
  | `Anon_init (v1, v2) ->
      let _v1 = token env v1 (* "init" *) in
      let v2 = block env v2 in
      FieldStmt v2
  | `Seco_cons (v1, v2, v3, v4, v5) ->
      let v1 = match v1 with Some x -> modifiers env x | None -> [] in
      let v2 = token env v2 (* "constructor" *) in
      let v3 = function_value_parameters env v3 in
      let v4 =
        match v4 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* ":" *) in
            let v2 = constructor_delegation_call env v2 in
            Some (v1, v2)
        | None -> None
      in
      let v5 = match v5 with Some x -> block env x | None -> G.empty_fbody in
      todo env (v1, v2, v3, v4, v5)

and class_member_declarations (env : env) (xs : CST.class_member_declarations) :
    field list =
  List.map
    (fun (v1, v2) ->
      let v1 = class_member_declaration env v1 in
      let _v2 = token env v2 (* pattern [\r\n]+ *) in
      v1)
    xs

and class_parameter (env : env) ((v1, v2, v3, v4, v5, v6) : CST.class_parameter)
    =
  let v1 = match v1 with Some x -> modifiers env x | None -> [] in
  let v2 =
    match v2 with
    | Some x -> Some (anon_choice_val_2833752 env x)
    | None -> None
  in
  let v3 = simple_identifier env v3 in
  let v4 = token env v4 (* ":" *) in
  let v5 = type_ env v5 in
  let v6 =
    match v6 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "=" *) in
        let v2 = expression env v2 in
        Some (v1, v2)
    | None -> None
  in
  todo env (v1, v2, v3, v4, v5, v6)

and class_parameters (env : env) ((v1, v2, v3) : CST.class_parameters) =
  let _v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = class_parameter env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = class_parameter env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let _v3 = token env v3 (* ")" *) in
  v2

and constructor_delegation_call (env : env)
    ((v1, v2) : CST.constructor_delegation_call) : expr =
  let v1 =
    match v1 with
    | `This tok -> (This, token env tok) (* "this" *)
    | `Super tok -> (Super, token env tok)
    (* "super" *)
  in
  let v2 = value_arguments env v2 in
  let e = IdSpecial v1 in
  Call (e, v2)

and constructor_invocation (env : env) ((v1, v2) : CST.constructor_invocation) =
  let v1 = user_type env v1 in
  let _v2 = value_arguments env v2 in
  (* TODO: add in v2 *)
  v1

and control_structure_body (env : env) (x : CST.control_structure_body) : stmt =
  match x with `Blk x -> block env x | `Stmt x -> statement env x

and declaration (env : env) (x : CST.declaration) : definition =
  match x with
  | `Class_decl x ->
      let ent, cdef = class_declaration env x in
      (ent, ClassDef cdef)
  | `Obj_decl (v1, v2, v3, v4, v5) ->
      let v1 = match v1 with Some x -> modifiers env x | None -> [] in
      let v2 = token env v2 (* "object" *) in
      let v3 = simple_identifier env v3 in
      let _v4 =
        match v4 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* ":" *) in
            let v2 = delegation_specifiers env v2 in
            Some (v1, v2)
        | None -> None
      in
      let v5 = match v5 with Some x -> class_body env x | None -> fb [] in
      let ent = G.basic_entity v3 v1 in
      let cdef =
        {
          ckind = (Object, v2);
          cextends = [];
          cimplements = [];
          cmixins = [];
          cparams = [];
          cbody = v5;
        }
      in
      (ent, ClassDef cdef)
  | `Func_decl (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let _v1 = match v1 with Some x -> modifiers env x | None -> [] in
      let _v2 = match v2 with Some x -> type_parameters env x | None -> [] in
      let v3 = token env v3 (* "fun" *) in
      let v4 = simple_identifier env v4 in
      let v5 = function_value_parameters env v5 in
      let v6 =
        match v6 with
        | Some (v1, v2) ->
            let _v1 = token env v1 (* ":" *) in
            let v2 = type_ env v2 in
            Some v2
        | None -> None
      in
      let _v7 = match v7 with Some x -> type_constraints env x | None -> [] in
      let v8 =
        match v8 with Some x -> function_body env x | None -> empty_fbody
      in
      let entity = basic_entity v4 [] in
      let func_def =
        { fkind = (Function, v3); fparams = v5; frettype = v6; fbody = v8 }
      in
      let def_kind = FuncDef func_def in
      (entity, def_kind)
  | `Prop_decl (v1, v2, v3, v4, v5, v6, v7) ->
      let _v1 = match v1 with Some x -> modifiers env x | None -> [] in
      let _v2 = anon_choice_val_2833752 env v2 in
      let _v3 = match v3 with Some x -> type_parameters env x | None -> [] in
      let v4 = variable_declaration env v4 in
      let tok, type_info = v4 in
      let _v5 = match v5 with Some x -> type_constraints env x | None -> [] in
      let v6 =
        match v6 with
        | Some x -> (
            match x with
            | `EQ_exp (v1, v2) ->
                let _v1 = token env v1 (* "=" *) in
                let v2 = expression env v2 in
                Some v2
            | `Prop_dele x -> property_delegate env x )
        | None -> None
      in
      let _v7 =
        match v7 with
        | `Opt_getter opt -> (
            match opt with
            | Some x ->
                let x = getter env x in
                todo env x
            | None -> None )
        | `Opt_setter opt -> (
            match opt with
            | Some x ->
                let x = setter env x in
                todo env x
            | None -> None )
      in
      let vdef = { vinit = v6; vtype = type_info } in
      let ent = basic_entity tok [] in
      (ent, VarDef vdef)
  | `Type_alias (v1, v2, v3, v4) ->
      let _v1 = token env v1 (* "typealias" *) in
      let v2 = simple_identifier env v2 in
      let _v3 = token env v3 (* "=" *) in
      let v4 = type_ env v4 in
      let ent = basic_entity v2 [] in
      let tdef = { tbody = AliasType v4 } in
      (ent, TypeDef tdef)

and delegation_specifier (env : env) (x : CST.delegation_specifier) =
  match x with
  | `Cons_invo x -> constructor_invocation env x
  | `Expl_dele (v1, v2, v3) ->
      let v1 =
        match v1 with
        | `User_type x -> user_type env x
        | `Func_type x -> function_type env x
      in
      let v2 = token env v2 (* "by" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `User_type x -> user_type env x
  | `Func_type x -> function_type env x

and delegation_specifiers (env : env) ((v1, v2) : CST.delegation_specifiers) =
  let v1 = delegation_specifier env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = delegation_specifier env v2 in
        v2)
      v2
  in
  v1 :: v2

and enum_class_body (env : env) ((v1, v2, v3, v4) : CST.enum_class_body) =
  let v1 = token env v1 (* "{" *) in
  let v2 = match v2 with Some x -> enum_entries env x | None -> [] in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        let _v1 = token env v1 (* ";" *) in
        let v2 =
          match v2 with Some x -> class_member_declarations env x | None -> []
        in
        v2
    | None -> []
  in
  let v4 = token env v4 (* "}" *) in
  todo env (v1, v2, v3, v4)

and enum_entries (env : env) ((v1, v2, v3) : CST.enum_entries) =
  let v1 = enum_entry env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = enum_entry env v2 in
        v2)
      v2
  in
  let _v3 =
    match v3 with Some tok -> Some (token env tok) (* "," *) | None -> None
  in
  v1 :: v2

and enum_entry (env : env) ((v1, v2, v3, v4) : CST.enum_entry) =
  let v1 = match v1 with Some x -> modifiers env x | None -> [] in
  let v2 = simple_identifier env v2 in
  let v3 =
    match v3 with Some x -> Some (value_arguments env x) | None -> None
  in
  let v4 = match v4 with Some x -> Some (class_body env x) | None -> None in
  todo env (v1, v2, v3, v4)

and expression (env : env) (x : CST.expression) : expr =
  match x with
  | `Choice_un_exp x -> (
      match x with
      | `Un_exp x -> unary_expression env x
      | `Bin_exp x -> binary_expression env x
      | `Prim_exp x -> primary_expression env x )
  | `Ellips x -> Ellipsis (token env x)
  | `Deep_ellips (x1, x2, x3) ->
      let x1 = token env x1 in
      let x2 = expression env x2 in
      let x3 = token env x3 in
      G.DeepEllipsis (x1, x2, x3)

and finally_block (env : env) ((v1, v2) : CST.finally_block) =
  let v1 = token env v1 (* "finally" *) in
  let v2 = block env v2 in
  (v1, v2)

and function_body (env : env) (x : CST.function_body) =
  match x with
  | `Blk x -> block env x
  | `EQ_exp (v1, v2) ->
      let _v1 = token env v1 (* "=" *) in
      let v2 = expression env v2 in
      ExprStmt (v2, sc) |> G.s

and function_literal (env : env) (x : CST.function_literal) =
  match x with
  | `Lambda_lit x -> lambda_literal env x
  | `Anon_func (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "fun" *) in
      let _v2 =
        match v2 with
        | Some (v1, v2, v3) ->
            let v1 = simple_user_type env v1 in
            let v2 =
              List.map
                (fun (v1, v2) ->
                  let _v1 = token env v1 (* "." *) in
                  let v2 = simple_user_type env v2 in
                  v2)
                v2
            in
            let _v3 = token env v3 (* "." *) in
            v1 :: v2
        | None -> []
      in
      let _v3 = token env v3 (* "(" *) in
      let _v4 = token env v4 (* ")" *) in
      let v5 =
        match v5 with Some x -> function_body env x | None -> empty_fbody
      in
      let kind = (Function, v1) in
      let func_def =
        { fkind = kind; fparams = []; frettype = None; fbody = v5 }
      in
      Lambda func_def

and function_type (env : env) ((v1, v2, v3, v4) : CST.function_type) =
  let _v1 =
    match v1 with
    | Some (v1, v2) ->
        let v1 = simple_user_type env v1 in
        let _v2 = token env v2 (* "." *) in
        Some v1
    | None -> None
  in
  let v2 = function_type_parameters env v2 in
  let _v3 = token env v3 (* "->" *) in
  let v4 = type_ env v4 in
  TyFun (v2, v4)

and function_type_parameters (env : env)
    ((v1, v2, v3) : CST.function_type_parameters) =
  let _v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = anon_choice_param_b77c1d8 env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = anon_choice_param_b77c1d8 env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let _v3 = token env v3 (* ")" *) in
  v2

and function_value_parameter (env : env)
    ((v1, v2, v3) : CST.function_value_parameter) =
  let v1 = match v1 with Some x -> parameter_modifiers env x | None -> [] in
  let param1, param2 = parameter env v2 in
  let v3 =
    match v3 with
    | Some (tok, expr) ->
        let _tok = token env tok (* "=" *) in
        let expr = expression env expr in
        {
          pname = Some param1;
          pdefault = Some expr;
          ptype = Some param2;
          pattrs = v1;
          pinfo = basic_id_info (Param, sid_TODO);
        }
    | None ->
        {
          pname = Some param1;
          pdefault = None;
          ptype = Some param2;
          pattrs = v1;
          pinfo = basic_id_info (Param, sid_TODO);
        }
  in
  ParamClassic v3

and function_value_parameters (env : env)
    ((v1, v2, v3) : CST.function_value_parameters) =
  let _v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = function_value_parameter env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = function_value_parameter env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let _v3 = token env v3 (* ")" *) in
  v2

and getter (env : env) ((v1, v2) : CST.getter) =
  let v1 = token env v1 (* "get" *) in
  let v2 =
    match v2 with
    | Some (v1, v2, v3, v4) ->
        let _v1 = token env v1 (* "(" *) in
        let _v2 = token env v2 (* ")" *) in
        let v3 =
          match v3 with
          | Some (v1, v2) ->
              let _v1 = token env v1 (* ":" *) in
              let v2 = type_ env v2 in
              Some v2
          | None -> None
        in
        let v4 = function_body env v4 in
        Some (v3, v4)
    | None -> None
  in
  Some (v1, v2)

(* TODO: unsugar as chain of [][][] fold *)
and indexing_suffix (env : env) ((v1, v2, v3, v4) : CST.indexing_suffix) =
  let v1 = token env v1 (* "[" *) in
  let v2 = Arg (expression env v2) in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = Arg (expression env v2) in
        v2)
      v3
  in
  let combine = v2 :: v3 in
  let v4 = token env v4 (* "]" *) in
  (v1, combine, v4)

and interpolation (env : env) (x : CST.interpolation) : expr =
  match x with
  | `DOLLARLCURL_exp_RCURL (v1, v2, v3) ->
      let _v1 = token env v1 (* "${" *) in
      let v2 = expression env v2 in
      let _v3 = token env v3 (* "}" *) in
      v2
  | `DOLLAR_simple_id (v1, v2) ->
      let _v1 = token env v1 (* "$" *) in
      let v2 = simple_identifier env v2 in
      N (Id (v2, empty_id_info ()))

and jump_expression (env : env) (x : CST.jump_expression) =
  match x with
  | `Throw_exp (v1, v2) ->
      let v1 = token env v1 (* "throw" *) in
      let v2 = expression env v2 in
      Throw (v1, v2, sc) |> G.s
  | `Choice_ret_opt_exp (v1, v2) ->
      let v1 =
        match v1 with
        | `Ret tok ->
            let v1 = token env tok (* "return" *) in
            (v1, None)
        | `Ret_at x -> return_at env x
      in
      let v2 =
        match v2 with
        | Some x ->
            let v1 = expression env x in
            Some v1
        | None -> None
      in
      let return_tok, id = v1 in
      ( match id with
      | None -> Return (return_tok, v2, sc)
      | Some simple_id -> (
          let id = N (Id (simple_id, empty_id_info ())) in
          match v2 with
          | None ->
              let list = [ TodoK ("return@", return_tok); E id ] in
              OtherStmt (OS_Todo, list)
          | Some v2_expr ->
              let list = [ TodoK ("return@", return_tok); E id; E v2_expr ] in
              OtherStmt (OS_Todo, list) ) )
      |> G.s
  | `Cont tok ->
      let v1 = token env tok (* "continue" *) in
      Continue (v1, LNone, sc) |> G.s
  | `Cont_at (v1, v2) ->
      let v1 = token env v1 (* "continue@" *) in
      let v2 = lexical_identifier env v2 in
      let ident = LId v2 in
      Continue (v1, ident, sc) |> G.s
  | `Brk tok ->
      let v1 = token env tok (* "break" *) in
      Break (v1, LNone, sc) |> G.s
  | `Brk_at (v1, v2) ->
      let v1 = token env v1 (* "break@" *) in
      let v2 = lexical_identifier env v2 in
      let ident = LId v2 in
      Break (v1, ident, sc) |> G.s

and lambda_literal (env : env) ((v1, v2, v3, v4) : CST.lambda_literal) =
  let v1 = token env v1 (* "{" *) in
  let _v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 =
          match v1 with Some x -> lambda_parameters env x | None -> []
        in
        let _v2 = token env v2 (* "->" *) in
        v1
    | None -> []
  in
  let v3 = match v3 with Some x -> statements env x | None -> [] in
  let block_v3 = Block (fake_bracket v3) |> G.s in
  let _v4 = token env v4 (* "}" *) in
  let kind = (LambdaKind, v1) in
  let func_def =
    { fkind = kind; fparams = []; frettype = None; fbody = block_v3 }
  in
  Lambda func_def

and lambda_parameter (env : env) (x : CST.lambda_parameter) =
  match x with `Var_decl x -> variable_declaration env x

and lambda_parameters (env : env) ((v1, v2) : CST.lambda_parameters) =
  let v1 = lambda_parameter env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = lambda_parameter env v2 in
        v2)
      v2
  in
  v1 :: v2

and loop_statement (env : env) (x : CST.loop_statement) =
  match x with
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 = token env v1 (* "for" *) in
      let _v2 = token env v2 (* "(" *) in
      let _v3 = List.map (annotation env) v3 in
      let v4 = lambda_parameter env v4 in
      let _id, _type_info = v4 in
      let v5 = token env v5 (* "in" *) in
      let v6 = expression env v6 in
      let _v7 = token env v7 (* ")" *) in
      let v8 =
        match v8 with
        | Some x -> control_structure_body env x
        | None -> empty_fbody
      in
      let params =
        match v4 with
        | v1, Some v2 ->
            let pattern = PatId (v1, empty_id_info ()) in
            PatTyped (pattern, v2)
        | v1, None -> PatId (v1, empty_id_info ())
      in
      let header = ForEach (params, v5, v6) in
      For (v1, header, v8) |> AST.s
  | `While_stmt (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "while" *) in
      let _v2 = token env v2 (* "(" *) in
      let v3 = expression env v3 in
      let _v4 = token env v4 (* ")" *) in
      let v5 =
        match v5 with
        | `SEMI _tok -> empty_fbody
        | `Cont_stru_body x -> control_structure_body env x
      in
      While (v1, v3, v5) |> AST.s
  | `Do_while_stmt (v1, v2, v3, v4, v5, v6) ->
      let v1 = token env v1 (* "do" *) in
      let v2 =
        match v2 with
        | Some x -> control_structure_body env x
        | None -> empty_fbody
      in
      let _v3 = token env v3 (* "while" *) in
      let _v4 = token env v4 (* "(" *) in
      let v5 = expression env v5 in
      let _v6 = token env v6 (* ")" *) in
      DoWhile (v1, v2, v5) |> AST.s

and modifiers (env : env) (x : CST.modifiers) : attribute list =
  match x with
  | `Anno x -> annotation env x
  | `Rep1_modi xs -> List.map (modifier env) xs

and navigation_suffix (env : env) ((v1, v2) : CST.navigation_suffix) =
  let _v1 = member_access_operator env v1 in
  let v2 =
    match v2 with
    | `Simple_id x ->
        let id = simple_identifier env x in
        G.N (Id (id, empty_id_info ()))
    | `Paren_exp x -> parenthesized_expression env x
    | `Class tok ->
        let id = str env tok in
        (* "class" *)
        G.N (Id (id, empty_id_info ()))
  in
  v2

and nullable_type (env : env) ((v1, v2) : CST.nullable_type) =
  let v1 =
    match v1 with
    | `Type_ref x -> type_reference env x
    | `Paren_type x -> parenthesized_type env x
  in
  let v2 = List.map (token env) (* "?" *) v2 in
  match v2 with hd :: _tl -> TyQuestion (v1, hd) | [] -> raise Impossible

(* see repeat1($._quest) in grammar.js *)
and parameter (env : env) ((v1, v2, v3) : CST.parameter) =
  let v1 = simple_identifier env v1 in
  let _v2 = token env v2 (* ":" *) in
  let v3 = type_ env v3 in
  (v1, v3)

and parameter_modifiers (env : env) (x : CST.parameter_modifiers) =
  match x with
  | `Anno x -> annotation env x
  | `Rep1_param_modi xs -> List.map (parameter_modifier env) xs

and parameter_with_optional_type (env : env)
    ((v1, v2, v3) : CST.parameter_with_optional_type) : ident * type_ option =
  let _v1 = match v1 with Some x -> parameter_modifiers env x | None -> [] in
  let v2 = simple_identifier env v2 in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        let _v1 = token env v1 (* ":" *) in
        let v2 = type_ env v2 in
        Some v2
    | None -> None
  in
  (v2, v3)

and parenthesized_expression (env : env)
    ((v1, v2, v3) : CST.parenthesized_expression) =
  let _v1 = token env v1 (* "(" *) in
  let v2 = expression env v2 in
  let _v3 = token env v3 (* ")" *) in
  v2

and parenthesized_type (env : env) ((v1, v2, v3) : CST.parenthesized_type) =
  let _v1 = token env v1 (* "(" *) in
  let v2 = type_ env v2 in
  let _v3 = token env v3 (* ")" *) in
  v2

and primary_constructor (env : env) ((v1, v2) : CST.primary_constructor) =
  let v1 =
    match v1 with
    | Some (v1, v2) ->
        let _v1 = match v1 with Some x -> modifiers env x | None -> [] in
        let v2 = token env v2 (* "constructor" *) in
        Some v2
    | None -> None
  in
  let v2 = class_parameters env v2 in
  todo env (v1, v2)

and primary_expression (env : env) (x : CST.primary_expression) : expr =
  match x with
  | `Paren_exp x -> parenthesized_expression env x
  | `Simple_id x ->
      let id = simple_identifier env x in
      G.N (Id (id, empty_id_info ()))
  | `Lit_cst x -> L (literal_constant env x)
  | `Str_lit x -> L (String (string_literal env x))
  | `Call_ref (v1, v2, v3) ->
      let v1 =
        match v1 with
        | Some x ->
            let id = simple_identifier env x in
            G.N (Id (id, empty_id_info ()))
        | None ->
            let fake_id = ("None", fake "None") in
            G.N (Id (fake_id, empty_id_info ()))
      in
      let v2 = token env v2 (* "::" *) in
      let v3 =
        match v3 with
        | `Simple_id x -> simple_identifier env x
        | `Class tok -> str env tok
        (* "class" *)
      in
      let ident_v3 = EN (Id (v3, empty_id_info ())) in
      DotAccess (v1, v2, ident_v3)
  | `Func_lit x -> function_literal env x
  | `Obj_lit (v1, v2, v3) ->
      let v1 = token env v1 (* "object" *) in
      let _v2 =
        match v2 with
        | Some (v1, v2) ->
            let _v1 = token env v1 (* ":" *) in
            let v2 = delegation_specifiers env v2 in
            v2
        | None -> []
      in
      let v3 = class_body env v3 in
      AnonClass
        {
          ckind = (Object, v1);
          cextends = [];
          cimplements = [];
          cmixins = [];
          cparams = [];
          cbody = v3;
        }
  | `Coll_lit (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "[" *) in
      let v2 = expression env v2 in
      let v3 =
        List.map
          (fun (v1, v2) ->
            let _v1 = token env v1 (* "," *) in
            let v2 = expression env v2 in
            v2)
          v3
      in
      let v4 = token env v4 (* "]" *) in
      let all_expr = v2 :: v3 in
      let container_list = (v1, all_expr, v4) in
      Container (List, container_list)
  | `This_exp tok ->
      let tok = token env tok in
      IdSpecial (This, tok)
      (* "this" *)
  | `Super_exp v1 ->
      let tok = token env v1 in
      IdSpecial (Super, tok)
  | `If_exp (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "if" *) in
      let _v2 = token env v2 (* "(" *) in
      let v3 = expression env v3 in
      let _v4 = token env v4 (* ")" *) in
      let v5 =
        match v5 with
        | `Cont_stru_body x ->
            let v1 = control_structure_body env x in
            (v1, None)
        | `SEMI t -> (empty_stmt env t, None)
        | `Opt_cont_stru_body_opt_SEMI_else_choice_cont_stru_body
            (v1, v2, v3, v4) ->
            let v1 =
              match v1 with
              | Some x -> control_structure_body env x
              | None -> empty_fbody
            in
            let _v2 =
              match v2 with
              | Some t -> Some (empty_stmt env t) (* ";" *)
              | None -> None
            in
            let _v3 = token env v3 (* "else" *) in
            let v4 =
              match v4 with
              | `Cont_stru_body x -> control_structure_body env x
              | `SEMI tok -> empty_stmt env tok
              (* ";" *)
            in
            (v1, Some v4)
      in
      let v6, v7 = v5 in
      let if_stmt = If (v1, v3, v6, v7) |> G.s in
      OtherExpr (OE_StmtExpr, [ S if_stmt ])
  | `When_exp (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "when" *) in
      let v2 = match v2 with Some x -> when_subject env x | None -> None in
      let _v3 = token env v3 (* "{" *) in
      let v4 = List.map (when_entry env) v4 in
      let _v5 = token env v5 (* "}" *) in
      let switch_stmt = Switch (v1, v2, v4) |> G.s in
      OtherExpr (OE_StmtExpr, [ S switch_stmt ])
  | `Try_exp (v1, v2, v3) ->
      let v1 = token env v1 (* "try" *) in
      let v2 = block env v2 in
      let v3 =
        match v3 with
        | `Rep1_catch_blk_opt_fina_blk (v1, v2) ->
            let v1 = List.map (catch_block env) v1 in
            let v2 =
              match v2 with
              | Some x -> Some (finally_block env x)
              | None -> None
            in
            (v1, v2)
        | `Fina_blk x ->
            let finally = finally_block env x in
            ([], Some finally)
      in
      let catch, finally = v3 in
      let try_stmt = Try (v1, v2, catch, finally) |> G.s in
      OtherExpr (OE_StmtExpr, [ S try_stmt ])
  | `Jump_exp x ->
      let v1 = jump_expression env x in
      OtherExpr (OE_StmtExpr, [ S v1 ])

and property_delegate (env : env) ((v1, v2) : CST.property_delegate) =
  let _v1 = token env v1 (* "by" *) in
  let v2 = expression env v2 in
  Some v2

and range_test (env : env) ((v1, v2) : CST.range_test) =
  let op, tok = in_operator env v1 in
  let v2 = expression env v2 in
  Call (IdSpecial (Op op, tok), fb [ Arg v2 ])

and setter (env : env) ((v1, v2) : CST.setter) =
  let v1 = token env v1 (* "set" *) in
  let v2 =
    match v2 with
    | Some (v1, v2, v3, v4, v5) ->
        let _v1 = token env v1 (* "(" *) in
        let v2 = parameter_with_optional_type env v2 in
        let _v3 = token env v3 (* ")" *) in
        let v4 =
          match v4 with
          | Some (v1, v2) ->
              let _v1 = token env v1 (* ":" *) in
              let v2 = type_ env v2 in
              Some v2
          | None -> None
        in
        let v5 = function_body env v5 in
        Some (v2, v4, v5)
    | None -> None
  in
  Some (v1, v2)

and simple_user_type (env : env) ((v1, v2) : CST.simple_user_type) =
  let v1 = simple_identifier env v1 in
  let v2 =
    match v2 with
    | Some x ->
        let args = type_arguments env x in
        let name = [ v1 ] in
        TyNameApply (name, args)
    | None -> TyN (Id (v1, empty_id_info ()))
  in
  v2

and statement (env : env) (x : CST.statement) : stmt =
  match x with
  | `Decl x ->
      let dec = declaration env x in
      DefStmt dec |> AST.s
  | `Rep_choice_label_choice_assign (_v1, v2) ->
      (*let v1 =
        List.map (fun x ->
          (match x with
          | `Label tok -> let t = token env tok in (* label *)
              raise Todo
          | `Anno x -> annotation env x
          )
        ) v1
        in*)
      let v2 =
        match v2 with
        | `Assign x ->
            let e = assignment env x in
            G.exprstmt e
        | `Loop_stmt x -> loop_statement env x
        | `Exp x ->
            let v1 = expression env x in
            G.exprstmt v1
      in
      v2

and statements (env : env) ((v1, v2, v3) : CST.statements) =
  let v1 = statement env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* pattern [\r\n]+ *) in
        let v2 = statement env v2 in
        v2)
      v2
  in
  let () =
    match v3 with
    | Some tok ->
        let _ = token env tok (* pattern [\r\n]+ *) in
        ()
    | None -> ()
  in
  v1 :: v2

and string_literal (env : env) (x : CST.string_literal) =
  match x with
  | `Line_str_lit (v1, v2, v3) ->
      let v1 = token env v1 (* "\dq" *) in
      let v2 =
        List.map
          (fun x ->
            match x with
            | `Line_str_content x -> line_string_content env x
            | `Interp x ->
                let e = interpolation env x in
                todo env e)
          v2
      in
      let v3 = token env v3 (* "\dq" *) in
      let str = v2 |> List.map fst |> String.concat "" in
      let toks = (v2 |> List.map snd) @ [ v3 ] in
      (str, PI.combine_infos v1 toks)
  | `Multi_line_str_lit (v1, v2, v3) ->
      let v1 = token env v1 (* "\"\"\dq" *) in
      let v2 =
        List.map
          (fun x ->
            match x with
            | `Multi_line_str_content x -> multi_line_string_content env x
            | `Interp x ->
                let _ = interpolation env x in
                raise Todo)
          v2
      in
      let v3 = token env v3 (* "\"\"\dq" *) in
      todo env (v1, v2, v3)

and type_ (env : env) ((v1, v2) : CST.type_) : type_ =
  let _v1 = match v1 with Some x -> type_modifiers env x | None -> [] in
  let v2 =
    match v2 with
    | `Paren_type x -> parenthesized_type env x
    | `Null_type x -> nullable_type env x
    | `Type_ref x -> type_reference env x
    | `Func_type x -> function_type env x
  in
  (* TODO: add type_modifier info *)
  v2

and type_arguments (env : env) ((v1, v2, v3, v4) : CST.type_arguments) =
  let _v1 = token env v1 (* "<" *) in
  let v2 = type_projection env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = type_projection env v2 in
        v2)
      v3
  in
  let _v4 = token env v4 (* ">" *) in
  v2 :: v3

and type_constraint (env : env) ((v1, v2, v3, v4) : CST.type_constraint) :
    ident * type_ =
  let _v1 = List.map (annotation env) v1 in
  let v2 = simple_identifier env v2 in
  let _v3 = token env v3 (* ":" *) in
  let v4 = type_ env v4 in
  (v2, v4)

and type_constraints (env : env) ((v1, v2, v3) : CST.type_constraints) =
  let _v1 = token env v1 (* "where" *) in
  let v2 = type_constraint env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = type_constraint env v2 in
        v2)
      v3
  in
  v2 :: v3

and type_modifier (env : env) (x : CST.type_modifier) : attribute list =
  match x with
  | `Anno x -> annotation env x
  | `Susp tok ->
      let x = str env tok (* "suspend" *) in
      [ G.unhandled_keywordattr x ]

and type_modifiers (env : env) (xs : CST.type_modifiers) : attribute list =
  List.map (type_modifier env) xs |> List.flatten

and type_parameter (env : env) ((v1, v2, v3) : CST.type_parameter) =
  let _v1 =
    match v1 with Some x -> type_parameter_modifiers env x | None -> []
  in
  let origv2 = simple_identifier env v2 in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* ":" *) in
        let v2 = type_ env v2 in
        let type_parameter_constraints = [ HasConstructor v1; Extends v2 ] in
        (origv2, type_parameter_constraints)
    | None -> (origv2, [])
  in
  v3

and type_parameter_modifier (env : env) (x : CST.type_parameter_modifier) =
  match x with
  | `Reif_modi tok ->
      let _t = token env tok in
      (* "reified" *)
      raise Todo
  | `Vari_modi x -> type_projection_modifier env x
  | `Anno x -> annotation env x

and type_parameter_modifiers (env : env) (xs : CST.type_parameter_modifiers) =
  List.map (type_parameter_modifier env) xs

and type_parameters (env : env) ((v1, v2, v3, v4) : CST.type_parameters) =
  let _v1 = token env v1 (* "<" *) in
  let v2 = type_parameter env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = type_parameter env v2 in
        v2)
      v3
  in
  let _v4 = token env v4 (* ">" *) in
  v2 :: v3

and type_projection (env : env) (x : CST.type_projection) =
  match x with
  | `Opt_type_proj_modifs_type (v1, v2) ->
      let _v1 =
        match v1 with Some x -> type_projection_modifiers env x | None -> []
      in
      let v2 = type_ env v2 in
      let fake_token = Parse_info.fake_info "type projection" in
      let list = [ TodoK ("type projection", fake_token); T v2 ] in
      let othertype = OtherType (OT_Todo, list) in
      TypeArg othertype
  | `STAR tok ->
      let star = str env tok in
      let othertype = OtherType (OT_Todo, [ TodoK star ]) (* "*" *) in
      TypeArg othertype

and type_reference (env : env) (x : CST.type_reference) =
  match x with
  | `User_type x -> user_type env x
  | `Dyna tok -> TyBuiltin (str env tok)

(* "dynamic" *)
and type_test (env : env) ((v1, v2) : CST.type_test) =
  let op, tok = is_operator env v1 in
  let v2 = expression env v2 in
  Call (IdSpecial (Op op, tok), fb [ Arg v2 ])

and unary_expression (env : env) (x : CST.unary_expression) =
  match x with
  | `Post_exp (v1, v2) -> (
      let v1 = expression env v1 in
      let v2, v3 = postfix_unary_operator env v2 in
      match v2 with
      | Left incr_decr ->
          Call (IdSpecial (IncrDecr (incr_decr, Postfix), v3), fb [ Arg v1 ])
      | Right operator -> Call (IdSpecial (Op operator, v3), fb [ Arg v1 ]) )
  | `Call_exp (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = call_suffix env v2 in
      Call (v1, v2)
  | `Inde_exp (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = indexing_suffix env v2 in
      Call (v1, v2)
  | `Navi_exp (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = navigation_suffix env v2 in
      Call (v1, fb [ Arg v2 ])
  | `Prefix_exp (v1, v2) -> (
      let v1 =
        match v1 with
        | `Anno x ->
            let _ = annotation env x in
            None
        | `Label tok ->
            let _lbl = str env tok (* label *) in
            None
        | `Prefix_un_op x -> Some (prefix_unary_operator env x)
      in
      let v2 = expression env v2 in
      match v1 with
      | None -> v2
      | Some (Left incr_decr, tok) ->
          Call (IdSpecial (IncrDecr (incr_decr, Postfix), tok), fb [ Arg v2 ])
      | Some (Right operator, tok) ->
          Call (IdSpecial (Op operator, tok), fb [ Arg v2 ]) )
  | `As_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let _v2 = as_operator env v2 in
      let v3 = type_ env v3 in
      Cast (v3, v1)
  | `Spread_exp (v1, v2) ->
      let v1 = token env v1 (* "*" *) in
      let v2 = expression env v2 in
      Call (IdSpecial (Spread, v1), fb [ Arg v2 ])

and unescaped_annotation (env : env) (x : CST.unescaped_annotation) =
  match x with
  | `Cons_invo x ->
      let v1 = constructor_invocation env x in
      ArgType v1
  | `User_type x ->
      let v1 = user_type env x in
      ArgType v1

and user_type (env : env) ((v1, v2) : CST.user_type) =
  let v1 = simple_user_type env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "." *) in
        let v2 = simple_user_type env v2 in
        v2)
      v2
  in
  let list = v1 :: v2 in
  TyTuple (fake_bracket list)

and value_argument (env : env) ((v1, v2, v3, v4) : CST.value_argument) :
    argument =
  let _v1 = match v1 with Some x -> annotation env x | None -> [] in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = simple_identifier env v1 in
        let v2 = token env v2 (* "=" *) in
        Some (v1, v2)
    | None -> None
  in
  let v3 =
    match v3 with Some tok -> Some (token env tok) (* "*" *) | None -> None
  in
  let v4 = expression env v4 in
  let e =
    match v3 with
    | None -> v4
    | Some t -> Call (IdSpecial (Spread, t), fb [ Arg v4 ])
  in
  match v2 with None -> Arg e | Some (id, _t) -> ArgKwd (id, e)

and value_arguments (env : env) ((v1, v2, v3) : CST.value_arguments) :
    arguments bracket =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = value_argument env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = value_argument env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 = token env v3 (* ")" *) in
  (v1, v2, v3)

and variable_declaration (env : env) ((v1, v2) : CST.variable_declaration) =
  let v1 = simple_identifier env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let _v1 = token env v1 (* ":" *) in
        let v2 = type_ env v2 in
        Some v2
    | None -> None
  in
  (v1, v2)

and when_condition (env : env) ((v1, v2, v3) : CST.when_condition) =
  let v1 = expression env v1 in
  let v2 = range_test env v2 in
  let v3 = type_test env v3 in
  Conditional (v1, v2, v3)

and when_entry (env : env) ((v1, v2, v3, v4) : CST.when_entry) =
  let v1 =
    match v1 with
    | `When_cond_rep_COMMA_when_cond (v1, v2) ->
        let v1 = when_condition env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = when_condition env v2 in
              v2)
            v2
        in
        Left (v1 :: v2)
    | `Else tok -> Right (token env tok)
    (* "else" *)
  in
  let v2 = token env v2 (* "->" *) in
  let v3 = control_structure_body env v3 in
  let () =
    match v4 with
    | Some tok ->
        let _ = token env tok (* pattern [\r\n]+ *) in
        ()
    | None -> ()
  in
  todo env (v1, v2, v3, v4)

and when_subject (env : env) ((v1, v2, v3, v4) : CST.when_subject) =
  let _v1 = token env v1 (* "(" *) in
  let _v2 =
    match v2 with
    | Some (v1, v2, v3, v4) ->
        let _v1 = List.map (annotation env) v1 in
        let v2 = token env v2 (* "val" *) in
        let v3 = variable_declaration env v3 in
        let v4 = token env v4 (* "=" *) in
        todo env (v1, v2, v3, v4)
    | None -> None
  in
  let v3 = expression env v3 in
  let _v4 = token env v4 (* ")" *) in
  Some v3

(*
let rec parenthesized_user_type (env : env) ((v1, v2, v3) : CST.parenthesized_user_type) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
     | `User_type x -> user_type env x
     | `Paren_user_type x -> parenthesized_user_type env x
    )
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)
*)

let file_annotation (env : env) ((v1, v2, v3, v4) : CST.file_annotation) =
  let _v1 = token env v1 (* "@" *) in
  let _v2 = token env v2 (* "file" *) in
  let _v3 = token env v3 (* ":" *) in
  let _v4 =
    match v4 with
    | `LBRACK_rep1_unes_anno_RBRACK (v1, v2, v3) ->
        let _v1 = token env v1 (* "[" *) in
        let v2 = List.map (unescaped_annotation env) v2 in
        let _v3 = token env v3 (* "]" *) in
        v2
    | `Unes_anno x ->
        let v1 = unescaped_annotation env x in
        [ v1 ]
  in
  ()

let source_file (env : env) (x : CST.source_file) : any =
  match x with
  | `Opt_sheb_line_opt_rep1_file_anno_semi_opt_pack_header_rep_import_header_rep_stmt_semi
      (v1, v2, v3, v4, v5) ->
      let _v1 = match v1 with Some x -> shebang_line env x | None -> () in
      let _v2 =
        match v2 with
        | Some (v1, v2) ->
            let _v1 = List.map (file_annotation env) v1 in
            let _v2 = token env v2 (* pattern [\r\n]+ *) in
            ()
        | None -> ()
      in
      let v3 =
        match v3 with Some x -> [ package_header env x ] | None -> []
      in
      let v4 = List.map (import_header env) v4 in
      let v5 =
        List.map
          (fun (v1, v2) ->
            let v1 = statement env v1 in
            let _v2 = token env v2 (* pattern [\r\n]+ *) in
            v1)
          v5
      in
      let dirs = v3 @ v4 |> List.map (fun d -> DirectiveStmt d |> G.s) in
      Pr (dirs @ v5)
  | `Semg_exp (_v1, v2) ->
      let v2 = expression env v2 in
      E v2

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse file =
  H.wrap_parser
    (fun () ->
      Parallel.backtrace_when_exn := false;
      Parallel.invoke Tree_sitter_kotlin.Parse.file file ())
    (fun cst ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in

      try
        match source_file env cst with
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

let parse_expression_or_source_file str =
  let res = Tree_sitter_kotlin.Parse.string str in
  match res.errors with
  | [] -> res
  | _ ->
      let expr_str = "__SEMGREP_EXPRESSION " ^ str in
      Tree_sitter_kotlin.Parse.string expr_str

(* todo: special mode to convert Ellipsis in the right construct! *)
let parse_pattern str =
  H.wrap_parser
    (fun () ->
      Parallel.backtrace_when_exn := false;
      Parallel.invoke parse_expression_or_source_file str ())
    (fun cst ->
      let file = "<pattern>" in
      let env = { H.file; conv = Hashtbl.create 0; extra = () } in
      match source_file env cst with
      | AST.Pr [ x ] -> AST.S x
      | AST.Pr xs -> AST.Ss xs
      | x -> x)
