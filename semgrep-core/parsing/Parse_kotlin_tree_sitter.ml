 (* PUT YOUR NAME HERE
  *
  * Copyright (c) PUT YOUR COPYRIGHT HERE
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

 (*****************************************************************************)
 (* Prelude *)
 (*****************************************************************************)
 (* Kotlin parser using ocaml-tree-sitter-lang/kotlin and converting
  * directly to pfff/h_program-lang/AST_generic.ml
  *
  *)

 (*****************************************************************************)
 (* Helpers *)
 (*****************************************************************************)
 type env = H.env
 let _fake = AST_generic.fake
 let token = H.token
 let _str = H.str

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying ocaml-tree-sitter-lang/kotlin/Boilerplate.ml *)

(**
   Boilerplate to be used as a template when mapping the kotlin CST
   to another type of tree.
*)

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

(* TODO: FIX!! Disable warning against unused value *)
[@@@warning "-32"]

let blank (env : env) () =
  failwith "not implemented"

let todo (env : env) _ =
   failwith "not implemented"

let token_todo (env : env) _ = 
    failwith "token Todo"

let escaped_identifier (env : env) (tok : CST.escaped_identifier) =
  token env tok (* pattern "\\\\[tbrn'\"\\\\$]" *)

let pat_b294348 (env : env) (tok : CST.pat_b294348) =
  token env tok (* pattern "[^\\n\\r'\\\\]" *)

let visibility_modifier (env : env) (x : CST.visibility_modifier) =
  (match x with
  | `Public tok -> token env tok (* "public" *)
  | `Priv tok -> token env tok (* "private" *)
  | `Inte tok -> token env tok (* "internal" *)
  | `Prot tok -> token env tok (* "protected" *)
  )

let equality_operator (env : env) (x : CST.equality_operator) =
  (match x with
  | `BANGEQ tok -> token env tok (* "!=" *)
  | `BANGEQEQ tok -> token env tok (* "!==" *)
  | `EQEQ tok -> token env tok (* "==" *)
  | `EQEQEQ tok -> token env tok (* "===" *)
  )

let multi_line_str_text (env : env) (tok : CST.multi_line_str_text) =
  token env tok (* pattern "[^\"$]+" *)

let pat_a2e2132 (env : env) (tok : CST.pat_a2e2132) =
  token env tok (* pattern [0-9a-fA-F]{4} *)

let pat_c793459 (env : env) (tok : CST.pat_c793459) =
  token env tok (* pattern [uU] *)

let anon_choice_val_2833752 (env : env) (x : CST.anon_choice_val_2833752) =
  (match x with
  | `Val tok -> token env tok (* "val" *)
  | `Var tok -> token env tok (* "var" *)
  )

let platform_modifier (env : env) (x : CST.platform_modifier) =
  (match x with
  | `Expect tok -> token env tok (* "expect" *)
  | `Actual tok -> token env tok (* "actual" *)
  )

let label (env : env) (tok : CST.label) =
  token env tok (* label *)

let real_literal (env : env) (tok : CST.real_literal) =
  token env tok (* real_literal *)

let comparison_operator (env : env) (x : CST.comparison_operator) =
  (match x with
  | `LT tok -> token env tok (* "<" *)
  | `GT tok -> token env tok (* ">" *)
  | `LTEQ tok -> token env tok (* "<=" *)
  | `GTEQ tok -> token env tok (* ">=" *)
  )

let assignment_and_operator (env : env) (x : CST.assignment_and_operator) =
  (match x with
  | `PLUSEQ tok -> token env tok (* "+=" *)
  | `DASHEQ tok -> token env tok (* "-=" *)
  | `STAREQ tok -> token env tok (* "*=" *)
  | `SLASHEQ tok -> token env tok (* "/=" *)
  | `PERCEQ tok -> token env tok (* "%=" *)
  )

let inheritance_modifier (env : env) (x : CST.inheritance_modifier) =
  (match x with
  | `Abst tok -> token env tok (* "abstract" *)
  | `Final tok -> token env tok (* "final" *)
  | `Open tok -> token env tok (* "open" *)
  )

let postfix_unary_operator (env : env) (x : CST.postfix_unary_operator) =
  (match x with
  | `PLUSPLUS tok -> token env tok (* "++" *)
  | `DASHDASH tok -> token env tok (* "--" *)
  | `BANGBANG tok -> token env tok (* "!!" *)
  )

let variance_modifier (env : env) (x : CST.variance_modifier) =
  (match x with
  | `In tok -> token env tok (* "in" *)
  | `Out tok -> token env tok (* "out" *)
  )

let member_modifier (env : env) (x : CST.member_modifier) =
  (match x with
  | `Over tok -> token env tok (* "override" *)
  | `Late tok -> token env tok (* "lateinit" *)
  )

let class_modifier (env : env) (x : CST.class_modifier) =
  (match x with
  | `Sealed tok -> token env tok (* "sealed" *)
  | `Anno tok -> token env tok (* "annotation" *)
  | `Data tok -> token env tok (* "data" *)
  | `Inner tok -> token env tok (* "inner" *)
  )

let boolean_literal (env : env) (x : CST.boolean_literal) =
  (match x with
  | `True tok -> token env tok (* "true" *)
  | `False tok -> token env tok (* "false" *)
  )

let hex_literal (env : env) (tok : CST.hex_literal) =
  token env tok (* hex_literal *)

let pat_f630af3 (env : env) (tok : CST.pat_f630af3) =
  token env tok (* pattern [^\r\n]* *)

let use_site_target (env : env) ((v1, v2) : CST.use_site_target) =
  let v1 =
    (match v1 with
    | `Field tok -> token env tok (* "field" *)
    | `Prop tok -> token env tok (* "property" *)
    | `Get tok -> token env tok (* "get" *)
    | `Set tok -> token env tok (* "set" *)
    | `Rece tok -> token env tok (* "receiver" *)
    | `Param tok -> token env tok (* "param" *)
    | `Setp tok -> token env tok (* "setparam" *)
    | `Dele tok -> token env tok (* "delegate" *)
    )
  in
  let v2 = token env v2 (* ":" *) in
  todo env (v1, v2)

let additive_operator (env : env) (x : CST.additive_operator) =
  (match x with
  | `PLUS tok -> token env tok (* "+" *)
  | `DASH tok -> token env tok (* "-" *)
  )

let integer_literal (env : env) (tok : CST.integer_literal) =
  token env tok (* integer_literal *)

let pat_ddcb2a5 (env : env) (tok : CST.pat_ddcb2a5) =
  token env tok (* pattern [a-zA-Z_][a-zA-Z_0-9]* *)

let semis (env : env) (tok : CST.semis) =
  token env tok (* pattern [\r\n]+ *)

let as_operator (env : env) (x : CST.as_operator) =
  (match x with
  | `As tok -> token env tok (* "as" *)
  | `AsQM tok -> token env tok (* "as?" *)
  )

let function_modifier (env : env) (x : CST.function_modifier) =
  (match x with
  | `Tail tok -> token env tok (* "tailrec" *)
  | `Op tok -> token env tok (* "operator" *)
  | `Infix tok -> token env tok (* "infix" *)
  | `Inline tok -> token env tok (* "inline" *)
  | `Exte tok -> token env tok (* "external" *)
  | `Susp tok -> token env tok (* "suspend" *)
  )

let line_str_text (env : env) (tok : CST.line_str_text) =
  token env tok (* pattern "[^\\\\\"$]+" *)

let semi (env : env) (tok : CST.semi) =
  token env tok (* pattern [\r\n]+ *)

let prefix_unary_operator (env : env) (x : CST.prefix_unary_operator) =
  let _ = 
  (match x with
  | `PLUSPLUS tok -> token env tok (* "++" *)
  | `DASHDASH tok -> token env tok (* "--" *)
  | `DASH tok -> token env tok (* "-" *)
  | `PLUS tok -> token env tok (* "+" *)
  | `BANG tok -> token env tok (* "!" *)
  ) in 
    raise Todo

let in_operator (env : env) (x : CST.in_operator) =
  (match x with
  | `In tok -> token env tok (* "in" *)
  | `BANGin tok -> token env tok (* "!in" *)
  )

let multiplicative_operator (env : env) (x : CST.multiplicative_operator) =
  (match x with
  | `STAR tok -> token env tok (* "*" *)
  | `SLASH tok -> token env tok (* "/" *)
  | `PERC tok -> token env tok (* "%" *)
  )

let parameter_modifier (env : env) (x : CST.parameter_modifier) =
  (match x with
  | `Vararg tok -> token env tok (* "vararg" *)
  | `Noin tok -> token env tok (* "noinline" *)
  | `Cros tok -> token env tok (* "crossinline" *)
  )

let bin_literal (env : env) (tok : CST.bin_literal) =
  token env tok (* bin_literal *)

let pat_b9a3713 (env : env) (tok : CST.pat_b9a3713) =
  token env tok (* pattern `[^\r\n`]+` *)

let multi_line_string_content (env : env) (x : CST.multi_line_string_content) =
  (match x with
  | `Multi_line_str_text tok ->
      token env tok (* pattern "[^\"$]+" *)
  | `DQUOT tok -> token env tok (* "\"" *)
  )

let uni_character_literal (env : env) ((v1, v2, v3) : CST.uni_character_literal) =
  let v1 = token env v1 (* "\\" *) in
  let v2 = token env v2 (* "u" *) in
  let v3 = token env v3 (* pattern [0-9a-fA-F]{4} *) in
  todo env (v1, v2, v3)

let type_projection_modifier (env : env) (x : CST.type_projection_modifier) =
  let _ = variance_modifier env x in
    raise Todo

let shebang_line (env : env) ((v1, v2) : CST.shebang_line) =
  let v1 = token env v1 (* "#!" *) in
  let v2 = token env v2 (* pattern [^\r\n]* *) in
  todo env (v1, v2)

let is_operator (env : env) (x : CST.is_operator) =
  (match x with
  | `Is tok -> token env tok (* "is" *)
  | `Not_is tok -> token env tok (* "!is" *)
  )

let modifier (env : env) (x : CST.modifier) =
  (match x with
  | `Class_modi x -> class_modifier env x
  | `Member_modi x -> member_modifier env x
  | `Visi_modi x -> visibility_modifier env x
  | `Func_modi x -> function_modifier env x
  | `Prop_modi tok -> token env tok (* "const" *)
  | `Inhe_modi x -> inheritance_modifier env x
  | `Param_modi x -> parameter_modifier env x
  | `Plat_modi x -> platform_modifier env x
  )

let member_access_operator (env : env) (x : CST.member_access_operator) =
  (match x with
  | `DOT tok -> token env tok (* "." *)
  | `Safe_nav tok -> token env tok (* "?." *)
  | `COLONCOLON tok -> token env tok (* "::" *)
  )

let anon_choice_int_lit_9015f32 (env : env) (x : CST.anon_choice_int_lit_9015f32) =
  (match x with
  | `Int_lit tok -> token env tok (* integer_literal *)
  | `Hex_lit tok -> token env tok (* hex_literal *)
  | `Bin_lit tok -> token env tok (* bin_literal *)
  )

let lexical_identifier (env : env) (x : CST.lexical_identifier) =
  (match x with
  | `Pat_ddcb2a5 tok ->
      token env tok (* pattern [a-zA-Z_][a-zA-Z_0-9]* *)
  | `Pat_b9a3713 tok ->
      token env tok (* pattern `[^\r\n`]+` *)
  )

let escape_seq (env : env) (x : CST.escape_seq) =
  (match x with
  | `Uni_char_lit x -> uni_character_literal env x
  | `Esca_id tok ->
      token env tok (* pattern "\\\\[tbrn'\"\\\\$]" *)
  )

let line_str_escaped_char (env : env) (x : CST.line_str_escaped_char) =
  (match x with
  | `Esca_id tok ->
      token env tok (* pattern "\\\\[tbrn'\"\\\\$]" *)
  | `Uni_char_lit x -> uni_character_literal env x
  )

let type_projection_modifiers (env : env) (xs : CST.type_projection_modifiers) =
  List.map (type_projection_modifier env) xs

let simple_identifier (env : env) (x : CST.simple_identifier) =
  let _ = lexical_identifier env x in
    raise Todo

let line_string_content (env : env) (x : CST.line_string_content) =
  (match x with
  | `Line_str_text tok ->
      token env tok (* pattern "[^\\\\\"$]+" *)
  | `Line_str_esca_char x -> line_str_escaped_char env x
  )

let return_at (env : env) ((v1, v2) : CST.return_at) =
  let v1 = token env v1 (* "return@" *) in
  let v2 = simple_identifier env v2 in
  todo env (v1, v2)

let identifier (env : env) ((v1, v2) : CST.identifier) =
  let v1 = simple_identifier env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "." *) in
      let v2 = simple_identifier env v2 in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)

let directly_assignable_expression (env : env) (x : CST.directly_assignable_expression) =
  (match x with
  | `Simple_id x -> simple_identifier env x
  )

let import_alias (env : env) ((v1, v2) : CST.import_alias) =
  let v1 = token env v1 (* "as" *) in
  let v2 = simple_identifier env v2 in
  todo env (v1, v2)

let literal_constant (env : env) (x : CST.literal_constant) =
  let _ = (match x with
  | `Bool_lit x -> boolean_literal env x
  | `Int_lit tok -> token env tok (* integer_literal *)
  | `Hex_lit tok -> token env tok (* hex_literal *)
  | `Bin_lit tok -> token env tok (* bin_literal *)
  | `Char_lit (v1, v2, v3) ->
      let v1 = token env v1 (* "'" *) in
      let v2 =
        (match v2 with
        | `Esc_seq x -> escape_seq env x
        | `Pat_b294348 tok ->
            token env tok (* pattern "[^\\n\\r'\\\\]" *)
        )
      in
      let v3 = token env v3 (* "'" *) in
      todo env (v1, v2, v3)
  | `Real_lit tok -> token env tok (* real_literal *)
  | `Null tok -> token env tok (* "null" *)
  | `Long_lit (v1, v2) ->
      let v1 = anon_choice_int_lit_9015f32 env v1 in
      let v2 = token env v2 (* "L" *) in
      todo env (v1, v2)
  | `Unsi_lit (v1, v2, v3) ->
      let v1 = anon_choice_int_lit_9015f32 env v1 in
      let v2 = token env v2 (* pattern [uU] *) in
      let v3 =
        (match v3 with
        | Some tok -> token env tok (* "L" *)
        | None -> todo env ())
      in
      todo env (v1, v2, v3)
  ) in
    raise Todo

let package_header (env : env) ((v1, v2, v3) : CST.package_header) =
  let v1 = token env v1 (* "package" *) in
  let v2 = identifier env v2 in
  let v3 = token env v3 (* pattern [\r\n]+ *) in
  todo env (v1, v2, v3)

let import_header (env : env) ((v1, v2, v3, v4) : CST.import_header) =
  let v1 = token env v1 (* "import" *) in
  let v2 = identifier env v2 in
  let v3 =
    (match v3 with
    | Some x ->
        (match x with
        | `DOTSTAR v1 -> token env v1 (* ".*" *)
        | `Import_alias x -> import_alias env x
        )
    | None -> todo env ())
  in
  let v4 = token env v4 (* pattern [\r\n]+ *) in
  todo env (v1, v2, v3, v4)

let rec annotated_lambda (env : env) (v1 : CST.annotated_lambda) =
  lambda_literal env v1

and annotation (env : env) (x : CST.annotation) =
  (match x with
  | `Single_anno (v1, v2, v3) ->
      let v1 = token env v1 (* "@" *) in
      let v2 =
        (match v2 with
        | Some x -> use_site_target env x
        | None -> todo env ())
      in
      let v3 = unescaped_annotation env v3 in
      todo env (v1, v2, v3)
  | `Multi_anno (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "@" *) in
      let v2 =
        (match v2 with
        | Some x -> use_site_target env x
        | None -> todo env ())
      in
      let v3 = token env v3 (* "[" *) in
      let v4 = List.map (unescaped_annotation env) v4 in
      let v5 = token env v5 (* "]" *) in
      todo env (v1, v2, v3, v4, v5)
  )

and anon_choice_param_b77c1d8 (env : env) (x : CST.anon_choice_param_b77c1d8) =
  (match x with
  | `Param x -> parameter env x
  | `Type x -> let _ =  type_ env x in 
        raise Todo
  )

and assignment (env : env) (x : CST.assignment) =
  (match x with
  | `Dire_assi_exp_assign_and_op_exp (v1, v2, v3) ->
      let v1 = directly_assignable_expression env v1 in
      let v2 = assignment_and_operator env v2 in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  )

and binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Mult_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = multiplicative_operator env v2 in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Addi_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = additive_operator env v2 in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Range_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ".." *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Infix_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = simple_identifier env v2 in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Elvis_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "?:" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Check_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        (match v2 with
        | `In_op x -> in_operator env x
        | `Is_op x -> is_operator env x
        )
      in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Comp_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = comparison_operator env v2 in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Equa_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = equality_operator env v2 in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Conj_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "&&" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Disj_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "||" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  )

and block (env : env) ((v1, v2, v3) : CST.block) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some x -> statements env x
    | None -> todo env ())
  in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)

and call_suffix (env : env) (v1 : CST.call_suffix) =
  (match v1 with
  | `Opt_value_args_anno_lambda (v1, v2) ->
      let v1 =
        (match v1 with
        | Some x -> value_arguments env x
        | None -> todo env ())
      in
      let v2 = annotated_lambda env v2 in
      todo env (v1, v2)
  | `Value_args x -> value_arguments env x
  )

and catch_block (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.catch_block) =
  let v1 = token env v1 (* "catch" *) in
  let v2 = token env v2 (* "(" *) in
  let v3 = List.map (annotation env) v3 in
  let v4 = simple_identifier env v4 in
  let v5 = token env v5 (* ":" *) in
  let v6 = type_ env v6 in
  let v7 = token env v7 (* ")" *) in
  let v8 = block env v8 in
  todo env (v1, v2, v3, v4, v5, v6, v7, v8)

and class_body (env : env) ((v1, v2, v3) : CST.class_body) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some x -> class_member_declarations env x
    | None -> todo env ())
  in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)

and class_declaration (env : env) (x : CST.class_declaration) =
  (match x with
  | `Opt_modifs_choice_class_simple_id_opt_type_params_opt_prim_cons_opt_COLON_dele_specis_opt_type_consts_opt_class_body (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 =
        (match v1 with
        | Some x -> modifiers env x
        | None -> todo env ())
      in
      let v2 =
        (match v2 with
        | `Class tok -> token env tok (* "class" *)
        | `Inte tok -> token env tok (* "interface" *)
        )
      in
      let v3 = simple_identifier env v3 in
      let v4 =
        (match v4 with
        | Some x -> type_parameters env x
        | None -> todo env ())
      in
      let v5 =
        (match v5 with
        | Some x -> primary_constructor env x
        | None -> todo env ())
      in
      let v6 =
        (match v6 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* ":" *) in
            let v2 = delegation_specifiers env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v7 =
        (match v7 with
        | Some x -> type_constraints env x
        | None -> todo env ())
      in
      let v8 =
        (match v8 with
        | Some x -> class_body env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8)
  | `Opt_modifs_enum_class_simple_id_opt_type_params_opt_prim_cons_opt_COLON_dele_specis_opt_type_consts_opt_enum_class_body (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 =
        (match v1 with
        | Some x -> modifiers env x
        | None -> todo env ())
      in
      let v2 = token env v2 (* "enum" *) in
      let v3 = token env v3 (* "class" *) in
      let v4 = simple_identifier env v4 in
      let v5 =
        (match v5 with
        | Some x -> type_parameters env x
        | None -> todo env ())
      in
      let v6 =
        (match v6 with
        | Some x -> primary_constructor env x
        | None -> todo env ())
      in
      let v7 =
        (match v7 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* ":" *) in
            let v2 = delegation_specifiers env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v8 =
        (match v8 with
        | Some x -> type_constraints env x
        | None -> todo env ())
      in
      let v9 =
        (match v9 with
        | Some x -> enum_class_body env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9)
  )

and class_member_declaration (env : env) (x : CST.class_member_declaration) =
  (match x with
  | `Decl x -> declaration env x
  | `Comp_obj (v1, v2, v3, v4, v5, v6) ->
      let v1 =
        (match v1 with
        | Some x -> modifiers env x
        | None -> todo env ())
      in
      let v2 = token env v2 (* "companion" *) in
      let v3 = token env v3 (* "object" *) in
      let v4 =
        (match v4 with
        | Some x -> simple_identifier env x
        | None -> todo env ())
      in
      let v5 =
        (match v5 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* ":" *) in
            let v2 = delegation_specifiers env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v6 =
        (match v6 with
        | Some x -> class_body env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Anon_init (v1, v2) ->
      let v1 = token env v1 (* "init" *) in
      let v2 = block env v2 in
      todo env (v1, v2)
  | `Seco_cons (v1, v2, v3, v4, v5) ->
      let v1 =
        (match v1 with
        | Some x -> modifiers env x
        | None -> todo env ())
      in
      let v2 = token env v2 (* "constructor" *) in
      let v3 = function_value_parameters env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* ":" *) in
            let v2 = constructor_delegation_call env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v5 =
        (match v5 with
        | Some x -> block env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5)
  )

and class_member_declarations (env : env) (xs : CST.class_member_declarations) =
  List.map (fun (v1, v2) ->
    let v1 = class_member_declaration env v1 in
    let v2 = token env v2 (* pattern [\r\n]+ *) in
    todo env (v1, v2)
  ) xs

and class_parameter (env : env) ((v1, v2, v3, v4, v5, v6) : CST.class_parameter) =
  let v1 =
    (match v1 with
    | Some x -> modifiers env x
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | Some x -> anon_choice_val_2833752 env x
    | None -> todo env ())
  in
  let v3 = simple_identifier env v3 in
  let v4 = token env v4 (* ":" *) in
  let v5 = type_ env v5 in
  let v6 =
    (match v6 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "=" *) in
        let v2 = expression env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5, v6)

and class_parameters (env : env) ((v1, v2, v3) : CST.class_parameters) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = class_parameter env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = class_parameter env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

and constructor_delegation_call (env : env) ((v1, v2) : CST.constructor_delegation_call) =
  let v1 =
    (match v1 with
    | `This tok -> token env tok (* "this" *)
    | `Super tok -> token env tok (* "super" *)
    )
  in
  let v2 = value_arguments env v2 in
  todo env (v1, v2)

and constructor_invocation (env : env) ((v1, v2) : CST.constructor_invocation) =
  let v1 = user_type env v1 in
  let v2 = value_arguments env v2 in
  todo env (v1, v2)

and control_structure_body (env : env) (x : CST.control_structure_body) =
  (match x with
  | `Blk x -> block env x
  | `Stmt x -> statement env x
  )

and declaration (env : env) (x : CST.declaration) : AST.definition =
  (match x with
  | `Class_decl x -> class_declaration env x
  | `Obj_decl (v1, v2, v3, v4, v5) ->
      let v1 =
        (match v1 with
        | Some x -> modifiers env x
        | None -> todo env ())
      in
      let v2 = token env v2 (* "object" *) in
      let v3 = simple_identifier env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* ":" *) in
            let v2 = delegation_specifiers env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v5 =
        (match v5 with
        | Some x -> class_body env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5)
  | `Func_decl (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 =
        (match v1 with
        | Some x -> modifiers env x
        | None -> todo env ())
      in
      let v2 =
        (match v2 with
        | Some x -> type_parameters env x
        | None -> todo env ())
      in
      let v3 = token env v3 (* "fun" *) in
      let v4 = simple_identifier env v4 in
      let v5 = function_value_parameters env v5 in
      let v6 =
        (match v6 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* ":" *) in
            let v2 = type_ env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v7 =
        (match v7 with
        | Some x -> type_constraints env x
        | None -> todo env ())
      in
      let v8 =
        (match v8 with
        | Some x -> function_body env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8)
  | `Prop_decl (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 =
        (match v1 with
        | Some x -> modifiers env x
        | None -> todo env ())
      in
      let v2 = anon_choice_val_2833752 env v2 in
      let v3 =
        (match v3 with
        | Some x -> type_parameters env x
        | None -> todo env ())
      in
      let v4 = variable_declaration env v4 in
      let v5 =
        (match v5 with
        | Some x -> type_constraints env x
        | None -> todo env ())
      in
      let v6 =
        (match v6 with
        | Some x ->
            (match x with
            | `EQ_exp (v1, v2) ->
                let v1 = token env v1 (* "=" *) in
                let v2 = expression env v2 in
                todo env (v1, v2)
            | `Prop_dele x -> property_delegate env x
            )
        | None -> todo env ())
      in
      let v7 =
        (match v7 with
        | `Opt_getter opt ->
            (match opt with
            | Some x -> getter env x
            | None -> todo env ())
        | `Opt_setter opt ->
            (match opt with
            | Some x -> setter env x
            | None -> todo env ())
        )
      in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Type_alias (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "typealias" *) in
      let v2 = simple_identifier env v2 in
      let v3 = token env v3 (* "=" *) in
      let v4 = type_ env v4 in
      todo env (v1, v2, v3, v4)
  )

and delegation_specifier (env : env) (x : CST.delegation_specifier) =
  (match x with
  | `Cons_invo x -> constructor_invocation env x
  | `Expl_dele (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `User_type x -> user_type env x
        | `Func_type x -> function_type env x
        )
      in
      let v2 = token env v2 (* "by" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `User_type x -> user_type env x
  | `Func_type x -> function_type env x
  )

and delegation_specifiers (env : env) ((v1, v2) : CST.delegation_specifiers) =
  let v1 = delegation_specifier env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = delegation_specifier env v2 in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)

and enum_class_body (env : env) ((v1, v2, v3, v4) : CST.enum_class_body) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some x -> enum_entries env x
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* ";" *) in
        let v2 =
          (match v2 with
          | Some x -> class_member_declarations env x
          | None -> todo env ())
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v4 = token env v4 (* "}" *) in
  todo env (v1, v2, v3, v4)

and enum_entries (env : env) ((v1, v2, v3) : CST.enum_entries) =
  let v1 = enum_entry env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = enum_entry env v2 in
      todo env (v1, v2)
    ) v2
  in
  let v3 =
    (match v3 with
    | Some tok -> token env tok (* "," *)
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and enum_entry (env : env) ((v1, v2, v3, v4) : CST.enum_entry) =
  let v1 =
    (match v1 with
    | Some x -> modifiers env x
    | None -> todo env ())
  in
  let v2 = simple_identifier env v2 in
  let v3 =
    (match v3 with
    | Some x -> value_arguments env x
    | None -> todo env ())
  in
  let v4 =
    (match v4 with
    | Some x -> class_body env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)

and expression (env : env) (x : CST.expression) : AST.expr =
  (match x with
  | `Un_exp x -> unary_expression env x
  | `Bin_exp x -> binary_expression env x
  | `Prim_exp x -> primary_expression env x
  )

and finally_block (env : env) ((v1, v2) : CST.finally_block) =
  let v1 = token env v1 (* "finally" *) in
  let v2 = block env v2 in
  todo env (v1, v2)

and function_body (env : env) (x : CST.function_body) =
  (match x with
  | `Blk x -> block env x
  | `EQ_exp (v1, v2) ->
      let v1 = token env v1 (* "=" *) in
      let v2 = expression env v2 in
      todo env (v1, v2)
  )

and function_literal (env : env) (x : CST.function_literal) =
  let _ = (match x with
  | `Lambda_lit x -> lambda_literal env x
  | `Anon_func (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "fun" *) in
      let v2 =
        (match v2 with
        | Some (v1, v2, v3) ->
            let v1 = simple_user_type env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = token env v1 (* "." *) in
                let v2 = simple_user_type env v2 in
                todo env (v1, v2)
              ) v2
            in
            let v3 = token env v3 (* "." *) in
            todo env (v1, v2, v3)
        | None -> todo env ())
      in
      let v3 = token env v3 (* "(" *) in
      let v4 = token env v4 (* ")" *) in
      let v5 =
        (match v5 with
        | Some x -> function_body env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5)
  ) in
    raise Todo

and function_type (env : env) ((v1, v2, v3, v4) : CST.function_type) =
  let v1 =
    (match v1 with
    | Some (v1, v2) ->
        let v1 = simple_user_type env v1 in
        let v2 = token env v2 (* "." *) in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v2 = function_type_parameters env v2 in
  let v3 = token env v3 (* "->" *) in
  let v4 = type_ env v4 in
  todo env (v1, v2, v3, v4)

and function_type_parameters (env : env) ((v1, v2, v3) : CST.function_type_parameters) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = anon_choice_param_b77c1d8 env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = anon_choice_param_b77c1d8 env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

and function_value_parameter (env : env) ((v1, v2, v3) : CST.function_value_parameter) =
  let v1 =
    (match v1 with
    | Some x -> parameter_modifiers env x
    | None -> todo env ())
  in
  let v2 = parameter env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "=" *) in
        let v2 = expression env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and function_value_parameters (env : env) ((v1, v2, v3) : CST.function_value_parameters) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = function_value_parameter env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = function_value_parameter env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

and getter (env : env) ((v1, v2) : CST.getter) =
  let v1 = token env v1 (* "get" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4) ->
        let v1 = token env v1 (* "(" *) in
        let v2 = token env v2 (* ")" *) in
        let v3 =
          (match v3 with
          | Some (v1, v2) ->
              let v1 = token env v1 (* ":" *) in
              let v2 = type_ env v2 in
              todo env (v1, v2)
          | None -> todo env ())
        in
        let v4 = function_body env v4 in
        todo env (v1, v2, v3, v4)
    | None -> todo env ())
  in
  todo env (v1, v2)

and indexing_suffix (env : env) ((v1, v2, v3, v4) : CST.indexing_suffix) =
  let v1 = token env v1 (* "[" *) in
  let v2 = expression env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = expression env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 = token env v4 (* "]" *) in
  todo env (v1, v2, v3, v4)

and interpolation (env : env) (x : CST.interpolation) =
  (match x with
  | `DOLLARLCURL_exp_RCURL (v1, v2, v3) ->
      let v1 = token env v1 (* "${" *) in
      let v2 = expression env v2 in
      let v3 = token env v3 (* "}" *) in
      todo env (v1, v2, v3)
  | `DOLLAR_simple_id (v1, v2) ->
      let v1 = token env v1 (* "$" *) in
      let v2 = simple_identifier env v2 in
      todo env (v1, v2)
  )

and jump_expression (env : env) (x : CST.jump_expression) =
  let _ = (match x with
  | `Throw_exp (v1, v2) ->
      let v1 = token env v1 (* "throw" *) in
      let v2 = expression env v2 in
      todo env (v1, v2)
  | `Choice_ret_opt_exp (v1, v2) ->
      let v1 =
        (match v1 with
        | `Ret tok -> token env tok (* "return" *)
        | `Ret_at x -> return_at env x
        )
      in
      let v2 =
        (match v2 with
        | Some x -> expression env x
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Cont tok -> token env tok (* "continue" *)
  | `Cont_at (v1, v2) ->
      let v1 = token env v1 (* "continue@" *) in
      let v2 = simple_identifier env v2 in
      todo env (v1, v2)
  | `Brk tok -> token env tok (* "break" *)
  | `Brk_at (v1, v2) ->
      let v1 = token env v1 (* "break@" *) in
      let v2 = simple_identifier env v2 in
      todo env (v1, v2)
  ) in
    raise Todo

and lambda_literal (env : env) ((v1, v2, v3, v4) : CST.lambda_literal) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 =
          (match v1 with
          | Some x -> lambda_parameters env x
          | None -> todo env ())
        in
        let v2 = token env v2 (* "->" *) in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some x -> statements env x
    | None -> todo env ())
  in
  let v4 = token env v4 (* "}" *) in
  todo env (v1, v2, v3, v4)

and lambda_parameter (env : env) (x : CST.lambda_parameter) =
  (match x with
  | `Var_decl x -> variable_declaration env x
  )

and lambda_parameters (env : env) ((v1, v2) : CST.lambda_parameters) =
  let v1 = lambda_parameter env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = lambda_parameter env v2 in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)

and loop_statement (env : env) (x : CST.loop_statement) =
  (match x with
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 = token env v1 (* "for" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 = List.map (annotation env) v3 in
      let v4 = lambda_parameter env v4 in
      let v5 = token env v5 (* "in" *) in
      let v6 = expression env v6 in
      let v7 = token env v7 (* ")" *) in
      let v8 =
        (match v8 with
        | Some x -> control_structure_body env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8)
  | `While_stmt (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "while" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 = expression env v3 in
      let v4 = token env v4 (* ")" *) in
      let v5 =
        (match v5 with
        | `SEMI tok -> let _ = token env tok in  (* ";" *)
            raise Todo
        | `Cont_stru_body x -> control_structure_body env x
        )
      in
      todo env (v1, v2, v3, v4, v5)
  | `Do_while_stmt (v1, v2, v3, v4, v5, v6) ->
      let v1 = token env v1 (* "do" *) in
      let v2 =
        (match v2 with
        | Some x -> control_structure_body env x
        | None -> todo env ())
      in
      let v3 = token env v3 (* "while" *) in
      let v4 = token env v4 (* "(" *) in
      let v5 = expression env v5 in
      let v6 = token env v6 (* ")" *) in
      todo env (v1, v2, v3, v4, v5, v6)
  )

and modifiers (env : env) (x : CST.modifiers) =
  (match x with
  | `Anno x -> annotation env x
  | `Rep1_modi xs -> List.map (modifier env) xs
  )

and navigation_suffix (env : env) ((v1, v2) : CST.navigation_suffix) =
  let v1 = member_access_operator env v1 in
  let v2 =
    (match v2 with
    | `Simple_id x -> simple_identifier env x
    | `Paren_exp x -> parenthesized_expression env x
    | `Class tok -> token env tok (* "class" *)
    )
  in
  todo env (v1, v2)

and nullable_type (env : env) ((v1, v2) : CST.nullable_type) =
  let v1 =
    (match v1 with
    | `Type_ref x -> type_reference env x
    | `Paren_type x -> parenthesized_type env x
    )
  in
  let v2 = List.map (token env) (* "?" *) v2 in
  todo env (v1, v2)

and parameter (env : env) ((v1, v2, v3) : CST.parameter) : AST.parameter =
  let v1 = simple_identifier env v1 in
  let v2 = token env v2 (* ":" *) in
  let v3 = type_ env v3 in
  todo env (v1, v2, v3)

and parameter_modifiers (env : env) (x : CST.parameter_modifiers) =
  (match x with
  | `Anno x -> annotation env x
  | `Rep1_param_modi xs ->
      List.map (parameter_modifier env) xs
  )

and parameter_with_optional_type (env : env) ((v1, v2, v3) : CST.parameter_with_optional_type) =
  let v1 =
    (match v1 with
    | Some x -> parameter_modifiers env x
    | None -> todo env ())
  in
  let v2 = simple_identifier env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* ":" *) in
        let v2 = type_ env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and parenthesized_expression (env : env) ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 = token env v1 (* "(" *) in
  let v2 = expression env v2 in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

and parenthesized_type (env : env) ((v1, v2, v3) : CST.parenthesized_type) =
  let v1 = token env v1 (* "(" *) in
  let v2 = type_ env v2 in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

and primary_constructor (env : env) ((v1, v2) : CST.primary_constructor) =
  let v1 =
    (match v1 with
    | Some (v1, v2) ->
        let v1 =
          (match v1 with
          | Some x -> modifiers env x
          | None -> todo env ())
        in
        let v2 = token env v2 (* "constructor" *) in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v2 = class_parameters env v2 in
  todo env (v1, v2)

and primary_expression (env : env) (x : CST.primary_expression) : AST.expr =
  (match x with
  | `Paren_exp x -> let _ = parenthesized_expression env x in
      raise Todo
  | `Simple_id x -> simple_identifier env x
  | `Lit_cst x -> literal_constant env x
  | `Str_lit x -> string_literal env x
  | `Call_ref (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | Some x -> simple_identifier env x
        | None -> todo env ())
      in
      let v2 = token env v2 (* "::" *) in
      let v3 =
        (match v3 with
        | `Simple_id x -> simple_identifier env x
        | `Class tok -> token env tok (* "class" *)
        )
      in
      todo env (v1, v2, v3)
  | `Func_lit x -> function_literal env x
  | `Obj_lit (v1, v2, v3) ->
      let v1 = token env v1 (* "object" *) in
      let v2 =
        (match v2 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* ":" *) in
            let v2 = delegation_specifiers env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v3 = class_body env v3 in
      todo env (v1, v2, v3)
  | `Coll_lit (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "[" *) in
      let v2 = expression env v2 in
      let v3 =
        List.map (fun (v1, v2) ->
          let v1 = token env v1 (* "," *) in
          let v2 = expression env v2 in
          todo env (v1, v2)
        ) v3
      in
      let v4 = token env v4 (* "]" *) in
      todo env (v1, v2, v3, v4)
  | `This_exp tok -> let _ = token env tok in raise Todo (* "this" *)
  | `Super_exp v1 -> let _ = token env v1 in raise Todo(* "super" *)
  | `If_exp (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "if" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 = expression env v3 in
      let v4 = token env v4 (* ")" *) in
      let v5 =
        (match v5 with
          | `Cont_stru_body x -> control_structure_body env x
          | `SEMI tok -> token_todo env tok (* ";" *)
          | `Opt_cont_stru_body_opt_SEMI_else_choice_cont_stru_body (v1, v2, v3, v4) ->
              let v1 =
                (match v1 with
                  | Some x -> control_structure_body env x
                  | None -> todo env ())
              in
              let v2 =
                (match v2 with
                  | Some tok -> token env tok (* ";" *)
                  | None -> todo env ())
              in
              let v3 = token env v3 (* "else" *) in
              let v4 =
                (match v4 with
                  | `Cont_stru_body x -> control_structure_body env x
                  | `SEMI tok -> token_todo env tok (* ";" *)
                )
              in
              todo env (v1, v2, v3, v4)
        )
        in
        todo env (v1, v2, v3, v4, v5)
  | `When_exp (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "when" *) in
      let v2 =
        (match v2 with
        | Some x -> when_subject env x
        | None -> todo env ())
      in
      let v3 = token env v3 (* "{" *) in
      let v4 = List.map (when_entry env) v4 in
      let v5 = token env v5 (* "}" *) in
      todo env (v1, v2, v3, v4, v5)
  | `Try_exp (v1, v2, v3) ->
      let v1 = token env v1 (* "try" *) in
      let v2 = block env v2 in
      let v3 =
        (match v3 with
        | `Rep1_catch_blk_opt_fina_blk (v1, v2) ->
            let v1 = List.map (catch_block env) v1 in
            let v2 =
              (match v2 with
              | Some x -> finally_block env x
              | None -> todo env ())
            in
            todo env (v1, v2)
        | `Fina_blk x -> finally_block env x
        )
      in
      todo env (v1, v2, v3)
  | `Jump_exp x -> jump_expression env x
  )

and property_delegate (env : env) ((v1, v2) : CST.property_delegate) =
  let v1 = token env v1 (* "by" *) in
  let v2 = expression env v2 in
  todo env (v1, v2)

and range_test (env : env) ((v1, v2) : CST.range_test) =
  let v1 = in_operator env v1 in
  let v2 = expression env v2 in
  todo env (v1, v2)

and setter (env : env) ((v1, v2) : CST.setter) =
  let v1 = token env v1 (* "set" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4, v5) ->
        let v1 = token env v1 (* "(" *) in
        let v2 = parameter_with_optional_type env v2 in
        let v3 = token env v3 (* ")" *) in
        let v4 =
          (match v4 with
          | Some (v1, v2) ->
              let v1 = token env v1 (* ":" *) in
              let v2 = type_ env v2 in
              todo env (v1, v2)
          | None -> todo env ())
        in
        let v5 = function_body env v5 in
        todo env (v1, v2, v3, v4, v5)
    | None -> todo env ())
  in
  todo env (v1, v2)

and simple_user_type (env : env) ((v1, v2) : CST.simple_user_type) =
  let v1 = simple_identifier env v1 in
  let v2 =
    (match v2 with
    | Some x -> type_arguments env x
    | None -> todo env ())
  in
  todo env (v1, v2)

and statement (env : env) (x : CST.statement) : AST.stmt =
  (match x with
  | `Decl x -> let _ = declaration env x in 
      raise Todo
  | `Rep_choice_label_choice_assign (v1, v2) ->
      let v1 =
        List.map (fun x ->
          (match x with
          | `Label tok -> let t = token env tok in (* label *)
              raise Todo
          | `Anno x -> annotation env x
          )
        ) v1
      in
      let v2 =
        (match v2 with
        | `Assign x -> assignment env x
        | `Loop_stmt x -> loop_statement env x
        | `Exp x -> expression env x
        )
      in
      todo env (v1, v2)
  )

and statements (env : env) ((v1, v2, v3) : CST.statements) =
  let v1 = statement env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* pattern [\r\n]+ *) in
      let v2 = statement env v2 in
      todo env (v1, v2)
    ) v2
  in
  let v3 =
    (match v3 with
    | Some tok -> token env tok (* pattern [\r\n]+ *)
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and string_literal (env : env) (x : CST.string_literal) =
  let _ = (match x with
  | `Line_str_lit (v1, v2, v3) ->
      let v1 = token env v1 (* "\"" *) in
      let v2 =
        List.map (fun x ->
          (match x with
          | `Line_str_content x -> line_string_content env x
          | `Interp x -> interpolation env x
          )
        ) v2
      in
      let v3 = token env v3 (* "\"" *) in
      todo env (v1, v2, v3)
  | `Multi_line_str_lit (v1, v2, v3) ->
      let v1 = token env v1 (* "\"\"\"" *) in
      let v2 =
        List.map (fun x ->
          (match x with
          | `Multi_line_str_content x ->
              multi_line_string_content env x
          | `Interp x -> interpolation env x
          )
        ) v2
      in
      let v3 = token env v3 (* "\"\"\"" *) in
      todo env (v1, v2, v3)
  ) in 
    raise Todo

and type_ (env : env) ((v1, v2) : CST.type_) : AST.type_ =
  let v1 =
    (match v1 with
    | Some x -> type_modifiers env x
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | `Paren_type x -> parenthesized_type env x
    | `Null_type x -> nullable_type env x
    | `Type_ref x -> type_reference env x
    | `Func_type x -> function_type env x
    )
  in
  todo env (v1, v2)

and type_arguments (env : env) ((v1, v2, v3, v4) : CST.type_arguments) =
  let v1 = token env v1 (* "<" *) in
  let v2 = type_projection env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = type_projection env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 = token env v4 (* ">" *) in
  todo env (v1, v2, v3, v4)

and type_constraint (env : env) ((v1, v2, v3, v4) : CST.type_constraint) =
  let v1 = List.map (annotation env) v1 in
  let v2 = simple_identifier env v2 in
  let v3 = token env v3 (* ":" *) in
  let v4 = type_ env v4 in
  todo env (v1, v2, v3, v4)

and type_constraints (env : env) ((v1, v2, v3) : CST.type_constraints) =
  let v1 = token env v1 (* "where" *) in
  let v2 = type_constraint env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = type_constraint env v2 in
      todo env (v1, v2)
    ) v3
  in
  todo env (v1, v2, v3)

and type_modifier (env : env) (x : CST.type_modifier) =
  (match x with
  | `Anno x -> annotation env x
  | `Susp tok -> let t = token env tok (* "suspend" *) in 
      raise Todo
  )

and type_modifiers (env : env) (xs : CST.type_modifiers) =
  List.map (type_modifier env) xs

and type_parameter (env : env) ((v1, v2, v3) : CST.type_parameter) =
  let v1 =
    (match v1 with
    | Some x -> type_parameter_modifiers env x
    | None -> todo env ())
  in
  let v2 = simple_identifier env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* ":" *) in
        let v2 = type_ env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and type_parameter_modifier (env : env) (x : CST.type_parameter_modifier) =
  (match x with
  | `Reif_modi tok -> let t = token env tok in (* "reified" *) 
      raise Todo
  | `Vari_modi x -> type_projection_modifier env x
  | `Anno x -> annotation env x
  )

and type_parameter_modifiers (env : env) (xs : CST.type_parameter_modifiers) =
  List.map (type_parameter_modifier env) xs

and type_parameters (env : env) ((v1, v2, v3, v4) : CST.type_parameters) =
  let v1 = token env v1 (* "<" *) in
  let v2 = type_parameter env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = type_parameter env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 = token env v4 (* ">" *) in
  todo env (v1, v2, v3, v4)

and type_projection (env : env) (x : CST.type_projection) =
  (match x with
  | `Opt_type_proj_modifs_type (v1, v2) ->
      let v1 =
        (match v1 with
        | Some x -> type_projection_modifiers env x
        | None -> todo env ())
      in
      let v2 = type_ env v2 in
      todo env (v1, v2)
  | `STAR tok -> token env tok (* "*" *)
  )

and type_reference (env : env) (x : CST.type_reference) =
  (match x with
  | `User_type x -> user_type env x
  | `Dyna tok -> token env tok (* "dynamic" *)
  )

and type_test (env : env) ((v1, v2) : CST.type_test) =
  let v1 = is_operator env v1 in
  let v2 = expression env v2 in
  todo env (v1, v2)

and unary_expression (env : env) (x : CST.unary_expression) =
  (match x with
  | `Post_exp (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = postfix_unary_operator env v2 in
      todo env (v1, v2)
  | `Call_exp (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = call_suffix env v2 in
      todo env (v1, v2)
  | `Inde_exp (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = indexing_suffix env v2 in
      todo env (v1, v2)
  | `Navi_exp (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = navigation_suffix env v2 in
      todo env (v1, v2)
  | `Prefix_exp (v1, v2) ->
      let v1 =
        (match v1 with
        | `Anno x -> annotation env x
        | `Label tok -> let _ = token env tok in (* label *) 
            raise Todo
        | `Prefix_un_op x -> prefix_unary_operator env x
        )
      in
      let v2 = expression env v2 in
      todo env (v1, v2)
  | `As_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = as_operator env v2 in
      let v3 = type_ env v3 in
      todo env (v1, v2, v3)
  | `Spread_exp (v1, v2) ->
      let v1 = token env v1 (* "*" *) in
      let v2 = expression env v2 in
      todo env (v1, v2)
  )

and unescaped_annotation (env : env) (x : CST.unescaped_annotation) =
  (match x with
  | `Cons_invo x -> constructor_invocation env x
  | `User_type x -> user_type env x
  )

and user_type (env : env) ((v1, v2) : CST.user_type) =
  let v1 = simple_user_type env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "." *) in
      let v2 = simple_user_type env v2 in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)

and value_argument (env : env) ((v1, v2, v3, v4) : CST.value_argument) =
  let v1 =
    (match v1 with
    | Some x -> annotation env x
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = simple_identifier env v1 in
        let v2 = token env v2 (* "=" *) in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> token env tok (* "*" *)
    | None -> todo env ())
  in
  let v4 = expression env v4 in
  todo env (v1, v2, v3, v4)

and value_arguments (env : env) ((v1, v2, v3) : CST.value_arguments) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = value_argument env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = value_argument env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

and variable_declaration (env : env) ((v1, v2) : CST.variable_declaration) =
  let v1 = simple_identifier env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* ":" *) in
        let v2 = type_ env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2)

and when_condition (env : env) ((v1, v2, v3) : CST.when_condition) =
  let v1 = expression env v1 in
  let v2 = range_test env v2 in
  let v3 = type_test env v3 in
  todo env (v1, v2, v3)

and when_entry (env : env) ((v1, v2, v3, v4) : CST.when_entry) =
  let v1 =
    (match v1 with
    | `When_cond_rep_COMMA_when_cond (v1, v2) ->
        let v1 = when_condition env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = when_condition env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | `Else tok -> token env tok (* "else" *)
    )
  in
  let v2 = token env v2 (* "->" *) in
  let v3 = control_structure_body env v3 in
  let v4 =
    (match v4 with
    | Some tok -> token env tok (* pattern [\r\n]+ *)
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)

and when_subject (env : env) ((v1, v2, v3, v4) : CST.when_subject) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4) ->
        let v1 = List.map (annotation env) v1 in
        let v2 = token env v2 (* "val" *) in
        let v3 = variable_declaration env v3 in
        let v4 = token env v4 (* "=" *) in
        todo env (v1, v2, v3, v4)
    | None -> todo env ())
  in
  let v3 = expression env v3 in
  let v4 = token env v4 (* ")" *) in
  todo env (v1, v2, v3, v4)

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

let file_annotation (env : env) ((v1, v2, v3, v4) : CST.file_annotation) =
  let v1 = token env v1 (* "@" *) in
  let v2 = token env v2 (* "file" *) in
  let v3 = token env v3 (* ":" *) in
  let v4 =
    (match v4 with
    | `LBRACK_rep1_unes_anno_RBRACK (v1, v2, v3) ->
        let v1 = token env v1 (* "[" *) in
        let v2 = List.map (unescaped_annotation env) v2 in
        let v3 = token env v3 (* "]" *) in
        todo env (v1, v2, v3)
    | `Unes_anno x -> unescaped_annotation env x
    )
  in
  todo env (v1, v2, v3, v4)

let source_file (env : env) ((v1, v2, v3, v4, v5) : CST.source_file) : AST.program =
  let v1 =
    (match v1 with
    | Some x -> shebang_line env x
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = List.map (file_annotation env) v1 in
        let v2 = token env v2 (* pattern [\r\n]+ *) in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some x -> package_header env x
    | None -> todo env ())
  in
  let v4 = List.map (import_header env) v4 in
  let v5 =
    List.map (fun (v1, v2) ->
      let v1 = statement env v1 in
      let v2 = token env v2 (* pattern [\r\n]+ *) in
      todo env (v1, v2)
    ) v5
  in
  todo env (v1, v2, v3, v4, v5)


 (*****************************************************************************)
 (* Entry point *)
 (*****************************************************************************)
 let parse file =
   H.wrap_parser
     (fun () ->
        Parallel.backtrace_when_exn := false;
        Parallel.invoke Tree_sitter_kotlin.Parse.file file ()
     )
     (fun cst ->
      let env = { H.file; conv = H.line_col_to_pos file } in

      try
        source_file env cst
      with
        (Failure "not implemented") as exn ->
          let s = Printexc.get_backtrace () in
          pr2 "Some constructs are not handled yet";
          pr2 "CST was:";
          CST.dump_tree cst;
          pr2 "Original backtrace:";
          pr2 s;
          raise exn
   )