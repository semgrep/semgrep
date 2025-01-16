(* Colleen Dai
 * Yoann Padioleau
 *
 * Copyright (c) 2021, 2022 Semgrep Inc.
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
open Fpath_.Operators
module CST = Tree_sitter_kotlin.CST
module H = Parse_tree_sitter_helpers
open AST_generic
module G = AST_generic
module H2 = AST_generic_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Kotlin parser using tree-sitter-lang/semgrep-kotlin and converting
 * directly to AST_generic.ml
 *
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type context = Program | Pattern
type env = context H.env

let token = H.token
let str = H.str
let fb = Tok.unsafe_fake_bracket
let sc tok = Tok.sc tok

let in_pattern env =
  match env.H.extra with
  | Program -> false
  | Pattern -> true

let var_to_pattern (id, ptype) =
  let pat = PatId (id, empty_id_info ()) in
  match ptype with
  | Some t -> PatTyped (pat, t)
  | None -> pat

let vars_to_pattern (l, xs, r) =
  let ys = xs |> List_.map (fun (id, ptype) -> var_to_pattern (id, ptype)) in
  PatTuple (l, ys, r)

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)

(* The fake name @@@PARTIAL_CLASS_DECLARATION is used to distinguish
 * 1) two valid class declarations in a row vs
 * 2) a class declaration followed by a partial class declaration
 *
 * Valid kotlin programs cannot have class names that have @@@, so we will
 * know that this did not come from the source code.
 *
 * We will use this fake name to detect partial class
 * declarations in the function merge_class_declarations below.
 *)
let fake_name_for_partial_class_decl = "@@@PARTIAL_CLASS_DECLARATION"

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-kotlin/Boilerplate.ml *)

let visibility_modifier (env : env) (x : CST.visibility_modifier) =
  match x with
  | `Public tok -> KeywordAttr (Public, token env tok) (* "public" *)
  | `Priv tok -> KeywordAttr (Private, token env tok) (* "private" *)
  | `Inte tok -> G.unhandled_keywordattr (str env tok) (* "internal" *)
  | `Prot tok -> KeywordAttr (Protected, token env tok)

(* "protected" *)

let equality_operator (env : env) (x : CST.equality_operator) =
  match x with
  | `BANGEQ tok -> (NotEq, token env tok) (* "!=" *)
  | `BANGEQEQ tok -> (NotPhysEq, token env tok) (* "!==" *)
  | `EQEQ tok -> (Eq, token env tok) (* "==" *)
  | `EQEQEQ tok -> (PhysEq, token env tok)
(* "===" *)

let anon_choice_val_2833752 (env : env) (x : CST.anon_choice_val_2833752) =
  match x with
  | `Val tok -> (Const, token env tok) (* "val" *)
  | `Var tok -> (Mutable, token env tok)
(* "var" *)

let platform_modifier (env : env) (x : CST.platform_modifier) =
  match x with
  | `Expect tok -> G.unhandled_keywordattr (str env tok) (* "expect" *)
  | `Actual tok -> G.unhandled_keywordattr (str env tok)
(* "actual" *)

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
  | `Open tok -> G.unhandled_keywordattr (str env tok)
(* "open" *)

let postfix_unary_operator (env : env) (x : CST.postfix_unary_operator) =
  match x with
  | `PLUSPLUS tok ->
      fun e ->
        G.special (IncrDecr (Incr, Postfix), token env tok (* "++" *)) [ e ]
  | `DASHDASH tok ->
      fun e ->
        G.special (IncrDecr (Decr, Postfix), token env tok (* "--" *)) [ e ]
  | `BANGBANG tok ->
      fun e -> G.special (Op NotNullPostfix, token env tok (*!!*)) [ e ]

let variance_modifier (env : env) (x : CST.variance_modifier) : variance wrap =
  match x with
  | `In tok -> (Contravariant, token env tok) (* "in" *)
  | `Out tok -> (Covariant, token env tok)

(* "out" *)

let member_modifier (env : env) (x : CST.member_modifier) =
  match x with
  | `Over tok -> KeywordAttr (Override, token env tok) (* "override" *)
  | `Late tok -> G.unhandled_keywordattr (str env tok)

(* "lateinit" *)

let class_modifier (env : env) (x : CST.class_modifier) =
  match x with
  | `Sealed tok -> G.attr SealedClass (token env tok) (* "sealed" *)
  | `Anno tok -> G.attr AnnotationClass (token env tok) (* "annotation" *)
  | `Data tok -> G.attr RecordClass (token env tok) (* "data" *)
  | `Inner tok -> G.unhandled_keywordattr (str env tok)

(* "inner" *)

let boolean_literal (env : env) (x : CST.boolean_literal) =
  match x with
  | `True tok -> Bool (true, token env tok) (* "true" *)
  | `False tok -> Bool (false, token env tok)

(* "false" *)

let hex_literal (env : env) (tok : CST.hex_literal) =
  let s, t = str env tok (* hex_literal *) in
  Parsed_int.parse (s, t)

let use_site_target (env : env) ((v1, v2) : CST.use_site_target) =
  let s, t =
    match v1 with
    | `Field tok -> str env tok (* "field" *)
    | `Prop tok -> str env tok (* "property" *)
    | `Get tok -> str env tok (* "get" *)
    | `Set tok -> str env tok (* "set" *)
    | `Rece tok -> str env tok (* "receiver" *)
    | `Param tok -> str env tok (* "param" *)
    | `Setp tok -> str env tok (* "setparam" *)
    | `Dele tok -> str env tok
    (* "delegate" *)
  in
  let _v2 = token env v2 (* ":" *) in
  (s, t)

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
  Parsed_int.parse (s, t)

let as_operator (env : env) (x : CST.as_operator) =
  match x with
  | `As tok -> token env tok (* "as" *)
  | `AsQM tok -> token env tok

(* "as?" *)

let function_modifier (env : env) (x : CST.function_modifier) =
  match x with
  | `Tail tok -> G.unhandled_keywordattr (str env tok) (* "tailrec" *)
  | `Op tok -> G.unhandled_keywordattr (str env tok) (* "operator" *)
  | `Infix tok -> G.unhandled_keywordattr (str env tok) (* "infix" *)
  | `Inline tok -> KeywordAttr (Inline, token env tok) (* "inline" *)
  | `Exte tok -> KeywordAttr (Extern, token env tok) (* "external" *)
  | `Susp tok -> G.unhandled_keywordattr (str env tok)

(* "suspend" *)

let prefix_unary_operator (env : env) (x : CST.prefix_unary_operator) =
  match x with
  | `PLUSPLUS tok -> (Either.Left Incr, token env tok) (* "++" *)
  | `DASHDASH tok -> (Either.Left Decr, token env tok) (* "--" *)
  | `DASH tok -> (Either.Right Minus, token env tok) (* "-" *)
  | `PLUS tok -> (Either.Right Plus, token env tok) (* "+" *)
  | `BANG tok -> (Either.Right Not, token env tok)

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
  | `Vararg tok -> G.unhandled_keywordattr (str env tok) (* "vararg" *)
  | `Noin tok -> G.unhandled_keywordattr (str env tok) (* "noinline" *)
  | `Cros tok -> G.unhandled_keywordattr (str env tok)

(* "crossinline" *)

let bin_literal (env : env) (tok : CST.bin_literal) =
  let s, t = str env tok in
  (* bin_literal *)
  Parsed_int.parse (s, t)

(* "\"" *)

let uni_character_literal (env : env) ((v1, v2) : CST.uni_character_literal) =
  let v1 = token env v1 (* "\\u" *) in
  let v2 = str env v2 (* pattern [0-9a-fA-F]{4} *) in
  (fst v2, Tok.combine_toks v1 [ snd v2 ])

let type_projection_modifier (env : env) (x : CST.type_projection_modifier) =
  let x = variance_modifier env x in
  x

(* ignore, treat as a comment *)
let shebang_line (env : env) ((v1, v2) : CST.shebang_line) =
  let _v1 = token env v1 (* "#!" *) in
  let _v2 = token env v2 (* pattern [^\r\n]* *) in
  ()

let is_operator (env : env) (x : CST.is_operator) =
  match x with
  | `Is tok -> (Is, token env tok) (* "is" *)
  | `BANGis tok -> (NotIs, token env tok)

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
  | `DOT tok -> Either.Left (token env tok) (* "." *)
  | `Safe_nav tok -> Either.Right (str env tok) (* "?." *)
  | `COLONCOLON tok -> Either.Right (str env tok)

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
  | `Alpha_id tok -> str env tok (* pattern [a-zA-Z_][a-zA-Z_0-9]* *)
  | `Back_id tok -> str env tok

(* pattern `[^\r\n`]+` *)

let escape_seq (env : env) (x : CST.character_escape_seq) =
  match x with
  | `Uni_char_lit x -> uni_character_literal env x
  | `Esca_id tok -> str env tok

(* pattern "\\\\[tbrn'\dq\\\\$]" *)

let type_projection_modifiers (env : env) (xs : CST.type_projection_modifiers) =
  List_.map (type_projection_modifier env) xs

let simple_identifier (env : env) (x : CST.simple_identifier) : ident =
  match x with
  | `Choice_lexi_id (`Lexi_id x) -> lexical_identifier env x
  | `Choice_lexi_id
      (`Expect x | `Inner x | `Data x | `Actual x | `Get x | `Set x) ->
      str env x
  | `Pat_831065d x -> str env x

(* pattern \$[a-zA-Z_][a-zA-Z_0-9]* *)

let return_at (env : env) ((v1, v2) : CST.return_at) =
  let v1 = token env v1 (* "return@" *) in
  let v2 = lexical_identifier env v2 in
  (v1, Some v2)

let identifier (env : env) ((v1, v2) : CST.identifier) : dotted_ident =
  let v1 = simple_identifier env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "." *) in
        let v2 = simple_identifier env v2 in
        v2)
      v2
  in
  v1 :: v2

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
        | `Char_esc_seq x -> escape_seq env x
        | `Pat_b294348 tok -> str env tok
        (* pattern "[^\\n\\r'\\\\]" *)
      in
      let v3 = token env v3 (* "'" *) in
      let toks = [ snd v2 ] @ [ v3 ] in
      Char (fst v2, Tok.combine_toks v1 toks)
  | `Real_lit tok -> real_literal env tok (* real_literal *)
  | `Null tok -> Null (token env tok) (* "null" *)
  | `Long_lit (v1, v2) ->
      let v1 = anon_choice_int_lit_9015f32 env v1 in
      let _v2 = token env v2 (* "L" *) in
      Int v1
  | `Unsi_lit (v1, v2, v3) ->
      let v1 = anon_choice_int_lit_9015f32 env v1 in
      let v2 = str env v2 (* pattern [uU] *) in
      let _v3 =
        match v3 with
        | Some tok -> Some (str env tok) (* "L" *)
        | None -> None
      in
      (* let _str = Tok.content_of_tok v1 ^ fst v2 in
      *)
      Int (Parsed_int.map_tok (fun v1 -> Tok.combine_toks v1 [ snd v2 ]) v1)

let semi (env : env) x = token env x

let package_header (env : env) ((v1, v2, v3) : CST.package_header) : directive =
  let v1 = token env v1 (* "package" *) in
  let v2 = identifier env v2 in
  let _v3 = semi env v3 (* pattern [\r\n]+ *) in
  Package (v1, v2) |> G.d

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
            let _t, id = import_alias env x in
            ImportAs (v1, DottedName v2, Some (id, empty_id_info ())))
    | None ->
        let ident, module_name =
          match List.rev v2 with
          | [] -> raise Common.Impossible
          | x :: xs -> (x, List.rev xs)
        in
        ImportFrom
          (v1, DottedName module_name, [ H2.mk_import_from_kind ident None ])
  in
  let _v4 = semi env v4 (* pattern [\r\n]+ *) in
  v3 |> G.d

let rec annotated_lambda (env : env) ((v1, v2, v3) : CST.annotated_lambda) =
  let _v1TODO = List_.map (annotation env) v1 in
  let _v2TODO = v2 in
  lambda_literal env v3

and annotation (env : env) (x : CST.annotation) : attribute list =
  match x with
  | `Single_anno (origv1, v2, v3) ->
      let v1 = token env origv1 (* "@" *) in
      let _v2TODO =
        match v2 with
        | Some x ->
            let v2 = use_site_target env x in
            Some v2
        | None -> None
      in
      let n, args = unescaped_annotation env v3 in
      [ NamedAttr (v1, n, args) ]
  | `Multi_anno (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "@" *) in
      let _v2TODO =
        match v2 with
        | Some x ->
            let v2 = use_site_target env x in
            Some v2
        | None -> None
      in
      let _v3 = token env v3 (* "[" *) in
      let v4 = List_.map (unescaped_annotation env) v4 in
      let _v5 = token env v5 (* "]" *) in
      v4 |> List_.map (fun (n, args) -> NamedAttr (v1, n, args))

and anon_choice_param_b77c1d8 (env : env) (x : CST.anon_choice_param_b77c1d8) =
  match x with
  | `Param x ->
      let v1, v2 = parameter env x in
      let param = G.param_of_id v1 ~ptype:v2 in
      Param param
  | `Type x ->
      let v1 = type_ env x in
      Param (G.param_of_type v1)

and assignment (env : env) (x : CST.assignment) : expr =
  match x with
  | `Dire_assi_exp_assign_and_op_exp (v1, v2, v3) ->
      let v1 = directly_assignable_expression env v1 in
      let v2 = assignment_and_operator env v2 in
      let v3 = expression env v3 in
      AssignOp (v1, v2, v3) |> G.e
  | `Dire_assi_exp_EQ_exp (v1, v2, v3) ->
      let v1 = directly_assignable_expression env v1 in
      let v2 = token env v2 (* = *) in
      let v3 = expression env v3 in
      Assign (v1, v2, v3) |> G.e

and directly_assignable_expression (env : env)
    (x : CST.directly_assignable_expression) : expr =
  match x with
  | `Simple_id x ->
      let id = simple_identifier env x in
      N (H2.name_of_id id) |> G.e
  | `Post_un_exp (v1, v2) ->
      let v1 = primary_expression env v1 in
      let v2 = List_.map (postfix_unary_suffix env) v2 in
      v2 |> List.fold_left (fun acc f -> f acc) v1

and postfix_unary_suffix (env : env) (x : CST.postfix_unary_suffix) =
  match x with
  | `Post_un_op x -> postfix_unary_operator env x
  | `Navi_suffix x -> navigation_suffix env x
  | `Inde_suffix x -> indexing_suffix env x

and range_test (env : env) ((v1, v2) : CST.range_test) =
  let op, tok = in_operator env v1 in
  let e2 = expression env v2 in
  fun e1 -> G.opcall (op, tok) [ e1; e2 ]

and type_test (env : env) ((v1, v2) : CST.type_test) =
  let op, tok = is_operator env v1 in
  let t2 = type_ env v2 in
  fun e1 ->
    G.Call (G.IdSpecial (G.Op op, tok) |> G.e, fb [ G.Arg e1; G.ArgType t2 ])
    |> G.e

and binary_expression (env : env) (x : CST.binary_expression) =
  match x with
  | `Mult_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, tok = multiplicative_operator env v2 in
      let v3 = expression env v3 in
      G.opcall (v2, tok) [ v1; v3 ]
  | `Addi_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, tok = additive_operator env v2 in
      let v3 = expression env v3 in
      G.opcall (v2, tok) [ v1; v3 ]
  | `Range_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, tok = (Range, token env v2) (* ".." *) in
      let v3 = expression env v3 in
      G.opcall (v2, tok) [ v1; v3 ]
  | `Infix_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = simple_identifier env v2 in
      let v2_id = N (H2.name_of_id v2) |> G.e in
      let v3 = expression env v3 in
      Call (v2_id, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Elvis_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, tok = (Elvis, token env v2) (* "?:" *) in
      let v3 = expression env v3 in
      G.opcall (v2, tok) [ v1; v3 ]
  | `Check_exp (v1, v2) ->
      let v1 = expression env v1 in
      let v2 =
        match v2 with
        | `In_op_exp x -> range_test env x
        | `Is_op_type x -> type_test env x
      in
      v2 v1
  | `Comp_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, tok = comparison_operator env v2 in
      let v3 = expression env v3 in
      G.opcall (v2, tok) [ v1; v3 ]
  | `Equa_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, tok = equality_operator env v2 in
      let v3 = expression env v3 in
      G.opcall (v2, tok) [ v1; v3 ]
  | `Conj_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, tok = (And, token env v2) (* "&&" *) in
      let v3 = expression env v3 in
      G.opcall (v2, tok) [ v1; v3 ]
  | `Disj_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, tok = (Or, token env v2) (* "||" *) in
      let v3 = expression env v3 in
      G.opcall (v2, tok) [ v1; v3 ]

and block (env : env) ((v1, v2, v3) : CST.block) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    match v2 with
    | Some x -> statements env x
    | None -> []
  in
  let v3 = token env v3 (* "}" *) in
  Block (v1, v2, v3) |> G.s

and call_suffix (env : env) ((v1, v2) : CST.call_suffix) : G.arguments =
  let _v1TODO = Option.map (type_arguments env) v1 in
  let v2 =
    match v2 with
    | `Opt_value_args_anno_lambda (v1, v2) ->
        let l, args, r =
          match v1 with
          | Some x -> value_arguments env x
          | None -> fb []
        in
        (* https://kotlinlang.org/docs/lambdas.html#passing-trailing-lambdas *)
        let v2 = annotated_lambda env v2 in
        (l, args @ [ Arg v2 ], r)
    | `Value_args x -> value_arguments env x
  in
  v2

and catch_block (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.catch_block)
    =
  let v1 = token env v1 (* "catch" *) in
  let _v2 = token env v2 (* "(" *) in
  let _v3TODO = List_.map (annotation env) v3 in
  let v4 = simple_identifier env v4 in
  let _v5 = token env v5 (* ":" *) in
  let v6 = type_ env v6 in
  let _v7 = token env v7 (* ")" *) in
  let v8 = block env v8 in
  let exn = CatchParam (G.param_of_type v6 ~pname:v4) in
  (v1, exn, v8)

and class_body (env : env) ((v1, v2, v3) : CST.class_body) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    match v2 with
    | Some x -> class_member_declarations env x
    | None -> []
  in
  let v3 = token env v3 (* "}" *) in
  (v1, v2, v3)

and class_declaration (env : env) (x : CST.class_declaration) :
    entity * class_definition =
  match x with
  | `Opt_modifs_choice_class_simple_id_opt_type_params_opt_prim_cons_opt_COLON_dele_specis_opt_type_consts_opt_class_body
      (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 = modifiers_opt env v1 in
      let ckind =
        match v2 with
        | `Class tok -> (Class, token env tok) (* "class" *)
        | `Inte tok -> (Interface, token env tok)
        (* "interface" *)
      in
      let v3 = simple_identifier env v3 in
      let v4 =
        match v4 with
        | Some x -> Some (type_parameters env x)
        | None -> None
      in
      let cparams =
        match v5 with
        | Some x -> primary_constructor env x
        | None -> fb []
      in
      (* alt: we could identify in the list below the class with arguments,
       * which is the 'cextends', and put the rest in 'cimplements'.
       * Either.Right now we just put everything in 'cextends' and have
       * Generic_vs_generic.m_list__m_class_parent do clever matching.
       *)
      let cextends =
        match v6 with
        | Some (v1, v2) ->
            let _v1 = token env v1 (* ":" *) in
            let v2 = delegation_specifiers env v2 in
            v2
        | None -> []
      in
      let _v7TODO =
        match v7 with
        | Some x -> type_constraints env x
        | None -> []
      in
      let cbody =
        match v8 with
        | Some x -> class_body env x
        | None -> fb []
      in
      let ent = G.basic_entity v3 ~attrs:v1 ?tparams:v4 in
      let cdef =
        { ckind; cextends; cimplements = []; cmixins = []; cparams; cbody }
      in
      (ent, cdef)
  | `Opt_modifs_enum_class_simple_id_opt_type_params_opt_prim_cons_opt_COLON_dele_specis_opt_type_consts_opt_enum_class_body
      (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 = modifiers_opt env v1 in
      let v2 = token env v2 (* "enum" *) in
      let tclass = token env v3 (* "class" *) in
      let v4 = simple_identifier env v4 in
      let v5 = Option.map (type_parameters env) v5 in
      let cparams =
        match v6 with
        | Some x -> primary_constructor env x
        | None -> fb []
      in
      let cextends =
        match v7 with
        | Some (v1, v2) ->
            let _v1 = token env v1 (* ":" *) in
            let v2 = delegation_specifiers env v2 in
            v2
        | None -> []
      in
      let _v8 =
        match v8 with
        | Some x -> type_constraints env x
        | None -> []
      in
      let cbody =
        match v9 with
        | Some x -> enum_class_body env x
        | None -> fb []
      in
      let ent =
        G.basic_entity v4 ~attrs:(G.attr EnumClass v2 :: v1) ?tparams:v5
      in
      let cdef =
        {
          ckind = (Class, tclass);
          cextends;
          cimplements = [];
          cmixins = [];
          cparams;
          cbody;
        }
      in
      (ent, cdef)

and class_member_declaration (env : env) (x : CST.class_member_declaration) :
    field =
  match x with
  | `Choice_decl y -> (
      match y with
      | `Decl x ->
          let d = declaration env x in
          d |> G.fld
      | `Comp_obj (v1, v2, v3, v4, v5, v6) ->
          let v1 = modifiers_opt env v1 in
          let v2 = token env v2 (* "companion" *) in
          let v3 = token env v3 (* "object" *) in
          let v4 =
            match v4 with
            | Some x -> simple_identifier env x
            | None -> ("!companion!", v2)
          in
          let v5 =
            match v5 with
            | Some (v1, v2) ->
                let _v1 = token env v1 (* ":" *) in
                let v2 = delegation_specifiers env v2 in
                v2
            | None -> []
          in
          let v6 =
            match v6 with
            | Some x -> class_body env x
            | None -> fb []
          in
          let ent = G.basic_entity v4 ~attrs:v1 in
          let cdef =
            {
              ckind = (Object, v3);
              cextends = v5;
              cimplements = [];
              cmixins = [];
              cparams = fb [];
              cbody = v6;
            }
          in
          (ent, ClassDef cdef) |> G.fld
      | `Anon_init (v1, v2) ->
          let _v1 = token env v1 (* "init" *) in
          let v2 = block env v2 in
          F v2
      | `Seco_cons (v1, v2, v3, v4, v5) ->
          let v1 = modifiers_opt env v1 in
          let v2 = str env v2 (* "constructor" *) in
          let fparams = function_value_parameters env v3 in
          let _v4TODO =
            match v4 with
            | Some (v1, v2) ->
                let v1 = token env v1 (* ":" *) in
                let v2 = constructor_delegation_call env v2 in
                Some (v1, v2)
            | None -> None
          in
          let fbody =
            match v5 with
            | Some x -> G.FBStmt (block env x)
            | None -> G.FBDecl G.sc
          in
          let ent = G.basic_entity v2 ~attrs:v1 in
          let def =
            { fkind = (Method, snd v2); fparams; frettype = None; fbody }
          in
          (ent, FuncDef def) |> G.fld)
  | `Ellips x ->
      let x = token env x in
      G.field_ellipsis x

and class_member_declarations (env : env) (xs : CST.class_member_declarations) :
    field list =
  List_.map
    (fun (v1, v2) ->
      let v1 = class_member_declaration env v1 in
      let _v2 = semi env v2 (* pattern [\r\n]+ *) in
      v1)
    xs

and class_parameter (env : env) (x : CST.class_parameter) : G.parameter =
  match x with
  | `Opt_modifs_opt_choice_val_simple_id_COLON_type_opt_EQ_exp
      (v1, v2, v3, v4, v5, v6) ->
      let v1 = modifiers_opt env v1 in
      (* 'val' or 'var' *)
      let v2 =
        match v2 with
        | Some x -> [ KeywordAttr (anon_choice_val_2833752 env x) ]
        | None -> []
      in
      let v3 = simple_identifier env v3 in
      let _v4 = token env v4 (* ":" *) in
      let v5 = type_ env v5 in
      let v6 =
        match v6 with
        | Some (v1, v2) ->
            let _v1 = token env v1 (* "=" *) in
            let v2 = expression env v2 in
            Some v2
        | None -> None
      in
      Param (G.param_of_id v3 ?pdefault:v6 ~ptype:v5 ~pattrs:(v1 @ v2))
  | `Ellips v1 ->
      (* "..." *)
      let tk = token env v1 in
      ParamEllipsis tk

and class_parameters (env : env) ((v1, v2, v3, v4) : CST.class_parameters) :
    parameters =
  let l = token env v1 (* "(" *) in
  let params =
    match v2 with
    | Some (v1, v2) ->
        let v1 = class_parameter env v1 in
        let v2 =
          List_.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = class_parameter env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let _v3 =
    match v3 with
    | None -> None
    | Some v3 -> Some (token env v3)
    (* , *)
  in
  let r = token env v4 (* ")" *) in
  (l, params, r)

and constructor_delegation_call (env : env)
    ((v1, v2) : CST.constructor_delegation_call) : expr =
  let v1 =
    match v1 with
    | `This tok -> (This, token env tok) (* "this" *)
    | `Super tok -> (Super, token env tok)
    (* "super" *)
  in
  let v2 = value_arguments env v2 in
  let e = IdSpecial v1 |> G.e in
  Call (e, v2) |> G.e

and constructor_invocation (env : env) ((v1, v2) : CST.constructor_invocation) =
  let v1 = user_type env v1 in
  let v2 = value_arguments env v2 in
  (v1, v2)

and control_structure_body (env : env) (x : CST.control_structure_body) : stmt =
  match x with
  | `Blk x -> block env x
  | `Stmt x -> statement env x

and anon_opt_rece_type_opt_DOT_cc9388e (env : env)
    (opt : CST.anon_opt_rece_type_opt_DOT_cc9388e) =
  match opt with
  | Some (v1, v2) ->
      let v1 = receiver_type env v1 in
      let _v2 =
        match v2 with
        | Some tok -> (* "." *) Some (token env tok)
        | None -> None
      in
      Some v1
  | None -> None

and receiver_type (env : env) ((v1, v2) : CST.receiver_type) =
  let v1 =
    match v1 with
    | Some x -> type_modifiers env x
    | None -> []
  in
  let v2 =
    match v2 with
    | `Type_ref x -> type_reference env x
    | `Paren_type x -> parenthesized_type env x
    | `Null_type x -> nullable_type env x
  in
  (v1, v2)

and declaration (env : env) (x : CST.declaration) : definition =
  match x with
  (* TODO: ugly, this was put here but really it should be attached
   * to a Prop_decl. This was put at the declaration level because
   * of grammar ambiguity related to ASI. See grammar.js for more info.
   *)
  | `Getter x ->
      let mods, tget, _fun_optTODO = getter env x in
      let ent =
        {
          name = OtherEntity (("Getter", tget), []);
          attrs = mods;
          tparams = None;
        }
      in
      (ent, OtherDef (("Getter", tget), []))
  | `Setter x ->
      let mods, tset, _fun_optTODO = setter env x in
      let ent =
        {
          name = OtherEntity (("Setter", tset), []);
          attrs = mods;
          tparams = None;
        }
      in
      (ent, OtherDef (("Setter", tset), []))
  | `Class_decl x ->
      let ent, cdef = class_declaration env x in
      (ent, ClassDef cdef)
  | `Obj_decl (v1, v2, v3, v4, v5) ->
      let v1 = modifiers_opt env v1 in
      let v2 = token env v2 (* "object" *) in
      let v3 = simple_identifier env v3 in
      let v4 =
        match v4 with
        | Some (v1, v2) ->
            let _v1 = token env v1 (* ":" *) in
            let v2 = delegation_specifiers env v2 in
            v2
        | None -> []
      in
      let v5 =
        match v5 with
        | Some x -> class_body env x
        | None -> fb []
      in
      let ent = G.basic_entity v3 ~attrs:v1 in
      let cdef =
        {
          ckind = (Object, v2);
          cextends = v4;
          cimplements = [];
          cmixins = [];
          cparams = fb [];
          cbody = v5;
        }
      in
      (ent, ClassDef cdef)
  | `Func_decl (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 = modifiers_opt env v1 in
      let v2 = token env v2 (* "fun" *) in
      let v3 = Option.map (type_parameters env) v3 in
      (* TODO: receiver type, build a complex name with v5 *)
      let _v4TODO = anon_opt_rece_type_opt_DOT_cc9388e env v4 in
      let v5 = simple_identifier env v5 in
      let v6 = function_value_parameters env v6 in
      let v7 =
        match v7 with
        | Some (v1, v2) ->
            let _v1 = token env v1 (* ":" *) in
            let v2 = type_ env v2 in
            Some v2
        | None -> None
      in
      let _v8TODO =
        match v8 with
        | Some x -> type_constraints env x
        | None -> []
      in
      let v9 =
        match v9 with
        | Some x -> function_body env x
        | None -> G.FBDecl G.sc
      in
      let entity = basic_entity v5 ~attrs:v1 ?tparams:v3 in
      let func_def =
        { fkind = (Function, v2); fparams = v6; frettype = v7; fbody = v9 }
      in
      let def_kind = FuncDef func_def in
      (entity, def_kind)
  | `Prop_decl (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 = modifiers_opt env v1 in
      let v2 = KeywordAttr (anon_choice_val_2833752 env v2) in
      let v3 = Option.map (type_parameters env) v3 in
      (* TODO: distribute the name to all variable decls? *)
      let _v4TODO = anon_opt_rece_type_opt_DOT_cc9388e env v4 in
      let entname, typopt = lambda_parameter_for_property env v5 in
      let _v6TODO =
        match v6 with
        | Some x -> type_constraints env x
        | None -> []
      in
      let vinit =
        match v7 with
        | Some x -> (
            match x with
            | `EQ_exp (v1, v2) ->
                let _v1 = token env v1 (* "=" *) in
                let v2 = expression env v2 in
                Some v2
            | `Prop_dele x -> property_delegate env x)
        | None -> None
      in
      let vtok =
        match v8 with
        | Some tok -> (* ";" *) Some (token env tok)
        | None -> None
      in
      let _v9TODO =
        match v9 with
        | `Opt_getter opt -> (
            match opt with
            | Some x ->
                let x = getter env x in
                Some (Either.Left x)
            | None -> None)
        | `Opt_setter opt -> (
            match opt with
            | Some x ->
                let x = setter env x in
                Some (Either.Right x)
            | None -> None)
      in
      let vdef = { vinit; vtype = typopt; vtok } in
      let ent = { name = entname; attrs = v2 :: v1; tparams = v3 } in
      (ent, VarDef vdef)
  | `Type_alias (v0, v1, v2, v3, v4, v5) ->
      let attrs = modifiers_opt env v0 in
      let _kwd = token env v1 (* "typealias" *) in
      let id = simple_identifier env v2 in
      let tparams = Option.map (type_parameters env) v3 in
      let _eq = token env v4 (* "=" *) in
      let t = type_ env v5 in
      let ent = basic_entity ~attrs ?tparams id in
      let tdef = { tbody = AliasType t } in
      (ent, TypeDef tdef)

and delegation_specifier (env : env) (x : CST.delegation_specifier) :
    class_parent =
  match x with
  | `Cons_invo x ->
      let n, args = constructor_invocation env x in
      (TyN n |> G.t, Some args)
  | `Expl_dele (v1, v2, v3) ->
      let v1 =
        match v1 with
        | `User_type x ->
            let n = user_type env x in
            TyN n |> G.t
        | `Func_type x -> function_type env x
      in
      let v2 = token env v2 (* "by" *) in
      let v3 = expression env v3 in
      ( OtherType (("ByDelagation", v2), [ G.T v1 ]) |> G.t,
        Some (fb [ G.Arg v3 ]) )
  | `User_type x ->
      let n = user_type env x in
      (TyN n |> G.t, None)
  | `Func_type x -> (function_type env x, None)

and delegation_specifiers (env : env) ((v1, v2) : CST.delegation_specifiers) :
    class_parent list =
  let v1 = delegation_specifier env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = delegation_specifier env v2 in
        v2)
      v2
  in
  v1 :: v2

and enum_class_body (env : env) ((v1, v2, v3, v4) : CST.enum_class_body) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    match v2 with
    | Some x -> enum_entries env x
    | None -> []
  in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        let _v1 = token env v1 (* ";" *) in
        let v2 =
          match v2 with
          | Some x -> class_member_declarations env x
          | None -> []
        in
        v2
    | None -> []
  in
  let v4 = token env v4 (* "}" *) in
  (v1, v2 @ v3, v4)

and enum_entries (env : env) ((v1, v2, v3) : CST.enum_entries) : field list =
  let v1 = enum_entry env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = enum_entry env v2 in
        v2)
      v2
  in
  let _v3 =
    match v3 with
    | Some tok -> Some (token env tok) (* "," *)
    | None -> None
  in
  v1 :: v2

and enum_entry (env : env) ((v1, v2, v3, v4) : CST.enum_entry) : field =
  let v1 = modifiers_opt env v1 in
  let v2 = simple_identifier env v2 in
  let v3 =
    match v3 with
    | Some x -> Some (value_arguments env x)
    | None -> None
  in
  let v4 =
    match v4 with
    | Some x -> Some (class_body env x)
    | None -> None
  in
  let ent = G.basic_entity v2 ~attrs:v1 in
  let def = EnumEntryDef { ee_args = v3; ee_body = v4 } in
  (ent, def) |> G.fld

and expression (env : env) (x : CST.expression) : expr =
  match x with
  | `Choice_un_exp x -> (
      match x with
      | `Un_exp x -> unary_expression env x
      | `Bin_exp x -> binary_expression env x
      | `Prim_exp x -> primary_expression env x)
  | `Ellips x -> Ellipsis (token env x) |> G.e
  | `Deep_ellips (x1, x2, x3) ->
      let x1 = token env x1 in
      let x2 = expression env x2 in
      let x3 = token env x3 in
      G.DeepEllipsis (x1, x2, x3) |> G.e
  | `Typed_meta (v1, v2, v3, v4, v5) ->
      let _v1 = (* "(" *) token env v1 in
      let v2 = simple_identifier env v2 in
      let v3 = (* ":" *) token env v3 in
      let v4 = type_ env v4 in
      let _v5 = (* ")" *) token env v5 in
      TypedMetavar (v2, v3, v4) |> G.e
  | `Semg_named_ellips tok ->
      (* pattern \$\.\.\.[a-zA-Z_][a-zA-Z_0-9]* *)
      G.N (Id (str env tok, empty_id_info ())) |> G.e

and finally_block (env : env) ((v1, v2) : CST.finally_block) =
  let v1 = token env v1 (* "finally" *) in
  let v2 = block env v2 in
  (v1, v2)

and function_body (env : env) (x : CST.function_body) : G.function_body =
  match x with
  | `Blk x -> G.FBStmt (block env x)
  | `EQ_exp (v1, v2) ->
      let _v1 = token env v1 (* "=" *) in
      let v2 = expression env v2 in
      G.FBExpr v2

and function_literal (env : env) (x : CST.function_literal) =
  match x with
  | `Lambda_lit x -> lambda_literal env x
  | `Anon_func (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "fun" *) in
      let _v2TODO =
        match v2 with
        | Some (v1, v2, v3) ->
            let v1 = simple_user_type env v1 in
            let v2 =
              List_.map
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
      let v3 = function_value_parameters env v3 (* "(" *) in
      let v4 =
        match v4 with
        | None -> None
        | Some (_, ty) -> Some (type_ env ty)
      in
      let v5 =
        match v5 with
        | Some x -> function_body env x
        | None -> G.FBDecl G.sc
      in
      let kind = (Function, v1) in
      let func_def =
        { fkind = kind; fparams = v3; frettype = v4; fbody = v5 }
      in
      Lambda func_def |> G.e

and function_type (env : env) ((v1, v2, v3, v4) : CST.function_type) =
  let _v1TODO =
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
  TyFun (v2, v4) |> G.t

and function_type_parameters (env : env)
    ((v1, v2, v3) : CST.function_type_parameters) =
  let _v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = anon_choice_param_b77c1d8 env v1 in
        let v2 =
          List_.map
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

and function_value_parameter (env : env) (x : CST.function_value_parameter) =
  match x with
  | `Opt_param_modifs_param_opt_EQ_exp (v1, v2, v3) ->
      let pattrs =
        match v1 with
        | Some x -> parameter_modifiers env x
        | None -> []
      in
      let pname, ptype = parameter env v2 in
      let pdefault =
        match v3 with
        | Some (tok, expr) ->
            let _tok = token env tok (* "=" *) in
            let e = expression env expr in
            Some e
        | None -> None
      in
      let param = G.param_of_id pname ~ptype ?pdefault ~pattrs in
      Param param
  | `Ellips tok ->
      (* "..." *)
      let t = token env tok in
      ParamEllipsis t

and function_value_parameters (env : env)
    ((v1, v2, _v3, v4) : CST.function_value_parameters) : G.parameters =
  let l = token env v1 (* "(" *) in
  let params =
    match v2 with
    | Some (v1, v2) ->
        let v1 = function_value_parameter env v1 in
        let v2 =
          List_.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = function_value_parameter env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let r = token env v4 (* ")" *) in
  (l, params, r)

and getter (env : env) ((v0, v1, v2) : CST.getter) =
  let mods = modifiers_opt env v0 in
  let tget = token env v1 (* "get" *) in
  let fun_opt =
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
  (mods, tget, fun_opt)

and indexing_suffix (env : env) ((v1, v2, v3, v4) : CST.indexing_suffix) =
  let v1 = token env v1 (* "[" *) in
  let v2 = expression env v2 in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let v1 = token env v1 (* "," *) in
        let v2 = expression env v2 in
        (v1, v2))
      v3
  in
  (* indices with only the left part '[' or left ',' *)
  let indices = (v1, v2) :: v3 in

  let v4 = token env v4 (* "]" *) in

  let rec aux_add_right_part xs =
    match xs with
    | [] -> raise Impossible
    | [ (tleft, e) ] -> [ (tleft, e, v4) ]
    | (tleft, e) :: (tright, e2) :: xs ->
        (tleft, e, tright) :: aux_add_right_part ((tright, e2) :: xs)
  in
  (* indices with both parts *)
  let indices = aux_add_right_part indices in
  fun e ->
    indices
    |> List.fold_left
         (fun acc (tleft, e, tright) ->
           G.ArrayAccess (acc, (tleft, e, tright)) |> G.e)
         e

and interpolation (env : env) (x : CST.interpolation) =
  match x with
  | `DOLLARLCURL_exp_RCURL (v1, v2, v3) ->
      let v1 = token env v1 (* "${" *) in
      let v2 = expression env v2 in
      let v3 = token env v3 (* "}" *) in
      Either_.Right3 (v1, Some v2, v3)
  | `DOLLAR_simple_id (v1, v2) ->
      let v1 = token env v1 (* "$" *) in
      let v2 = simple_identifier env v2 in
      Either_.Right3 (v1, Some (N (Id (v2, empty_id_info ())) |> G.e), v1)

and jump_expression (env : env) (x : CST.jump_expression) =
  match x with
  | `Throw_exp (v1, v2) ->
      let v1 = token env v1 (* "throw" *) in
      let v2 = expression env v2 in
      Throw (v1, v2, sc v1) |> G.s
  | `Choice_ret_opt_exp (v1, v2) -> (
      let tret, id_opt =
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
      match id_opt with
      | None -> Return (tret, v2, sc tret) |> G.s
      | Some id ->
          let n = N (H2.name_of_id id) |> G.e in
          let any = Option.to_list v2 |> List_.map (fun e -> E e) in
          OtherStmt (OS_Todo, TodoK ("return@", tret) :: E n :: any) |> G.s)
  | `Cont tok ->
      let v1 = token env tok (* "continue" *) in
      Continue (v1, LNone, sc v1) |> G.s
  | `Cont_at (v1, v2) ->
      let v1 = token env v1 (* "continue@" *) in
      let v2 = lexical_identifier env v2 in
      let ident = LId v2 in
      Continue (v1, ident, sc v1) |> G.s
  | `Brk tok ->
      let v1 = token env tok (* "break" *) in
      Break (v1, LNone, sc v1) |> G.s
  | `Brk_at (v1, v2) ->
      let v1 = token env v1 (* "break@" *) in
      let v2 = lexical_identifier env v2 in
      let ident = LId v2 in
      Break (v1, ident, sc v1) |> G.s

and lambda_literal (env : env) ((v1, v2, v3, v4) : CST.lambda_literal) =
  let v1 = token env v1 (* "{" *) in
  let params, lbracket_block_start =
    match v2 with
    | Some (v1, v2) ->
        let v1 =
          match v1 with
          | Some x -> lambda_parameters env x
          | None -> []
        in
        (* use this to delimit the Block below. *)
        let v2 = token env v2 (* "->" *) in
        (v1, v2)
    (* note that even without parameters, 'it' can be used to
     * represent an anonymous parameter.
     *)
    | None -> ([], v1)
  in
  let v3 =
    match v3 with
    | Some x -> statements env x
    | None -> []
  in
  let v4 = token env v4 (* "}" *) in
  let fbody = G.FBStmt (Block (lbracket_block_start, v3, v4) |> G.s) in
  let kind = (LambdaKind, v1) in
  let func_def =
    { fkind = kind; fparams = fb params; frettype = None; fbody }
  in
  Lambda func_def |> G.e

and var_or_multivar (env : env) (x : CST.lambda_parameter) =
  match x with
  | `Var_decl x ->
      let id, ptype = variable_declaration env x in
      Either.Left (id, ptype)
  | `Multi_var_decl (v1, v2, v3, v4) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = variable_declaration env v2 in
      let v3 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = variable_declaration env v2 in
            v2)
          v3
      in
      let v4 = (* ")" *) token env v4 in
      Either.Right (v1, v2 :: v3, v4)

and lambda_parameter (env : env) (x : CST.lambda_parameter) : G.parameter =
  match var_or_multivar env x with
  | Either.Left (id, ptype) -> G.Param (G.param_of_id id ?ptype)
  | Either.Right (l, xs, r) ->
      let pat = vars_to_pattern (l, xs, r) in
      G.ParamPattern pat

and lambda_parameter_for_loop (env : env) (x : CST.lambda_parameter) =
  match var_or_multivar env x with
  | Either.Left (id, ptype) -> var_to_pattern (id, ptype)
  | Either.Right (l, xs, r) -> vars_to_pattern (l, xs, r)

and lambda_parameter_for_property (env : env) (x : CST.lambda_parameter) =
  match var_or_multivar env x with
  | Either.Left (id, ptype) ->
      let n = H2.name_of_id id in
      (G.EN n, ptype)
  | Either.Right (l, xs, r) ->
      let pat = vars_to_pattern (l, xs, r) in
      (G.EPattern pat, None)

and lambda_parameters (env : env) ((v1, v2) : CST.lambda_parameters) :
    G.parameter list =
  let v1 = lambda_parameter env v1 in
  let v2 =
    List_.map
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
      let _v3TODO = List_.map (annotation env) v3 in
      let pat = lambda_parameter_for_loop env v4 in
      let v5 = token env v5 (* "in" *) in
      let v6 = expression env v6 in
      let _v7 = token env v7 (* ")" *) in
      let v8 =
        match v8 with
        | Some x -> control_structure_body env x
        | None -> Block (fb []) |> G.s
      in
      let header = ForEach (pat, v5, v6) in
      For (v1, header, v8) |> G.s
  | `While_stmt (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "while" *) in
      let _v2 = token env v2 (* "(" *) in
      let v3 = expression env v3 in
      let _v4 = token env v4 (* ")" *) in
      let v5 =
        match v5 with
        | `SEMI v1 ->
            let v1 = token env v1 in
            G.emptystmt v1
        | `Cont_stru_body x -> control_structure_body env x
      in
      While (v1, G.Cond v3, v5) |> G.s
  | `Do_while_stmt (v1, v2, v3, v4, v5, v6) ->
      let v1 = token env v1 (* "do" *) in
      let v2 =
        match v2 with
        | Some x -> control_structure_body env x
        | None -> G.Block (fb []) |> G.s
      in
      let _v3 = token env v3 (* "while" *) in
      let _v4 = token env v4 (* "(" *) in
      let v5 = expression env v5 in
      let _v6 = token env v6 (* ")" *) in
      DoWhile (v1, v2, v5) |> G.s

and modifiers (env : env) (xs : CST.modifiers) : attribute list =
  List.concat_map
    (function
      | `Anno x -> annotation env x
      | `Modi x -> [ modifier env x ])
    xs

and modifiers_opt env x =
  match x with
  | None -> []
  | Some x -> modifiers env x

and navigation_suffix (env : env) (x : CST.navigation_suffix) =
  match x with
  | `Member_access_op_choice_simple_id (v1, v2) -> (
      let op = member_access_operator env v1 in
      let fld =
        match v2 with
        | `Simple_id x ->
            let id = simple_identifier env x in
            FN (Id (id, empty_id_info ()))
        | `Paren_exp x ->
            let e = parenthesized_expression env x in
            FDynamic e
        | `Class tok ->
            let id = str env tok in
            (* "class" *)
            FN (Id (id, empty_id_info ()))
      in
      fun e ->
        match op with
        | Either.Left tdot -> DotAccess (e, tdot, fld) |> G.e
        | Either.Right otherop ->
            let any_fld =
              match fld with
              | FN n -> E (N n |> G.e)
              | FDynamic e -> E e
            in
            OtherExpr (otherop, [ any_fld; E e ]) |> G.e)
  | `Member_access_op_ellips (v1, v2) -> (
      let op = member_access_operator env v1 in
      let tellipsis = token env v2 in
      fun e ->
        match op with
        | Either.Left _tdot -> DotAccessEllipsis (e, tellipsis) |> G.e
        | Either.Right otherop -> OtherExpr (otherop, [ Tk tellipsis ]) |> G.e)

and nullable_type (env : env) ((v1, v2) : CST.nullable_type) =
  let v1 =
    match v1 with
    | `Type_ref x -> type_reference env x
    | `Paren_type x -> parenthesized_type env x
  in
  let v2 = List_.map (token env) (* "?" *) v2 in
  match v2 with
  | hd :: _tl -> TyQuestion (v1, hd) |> G.t
  | [] -> raise Impossible

(* see repeat1($._quest) in grammar.js *)
and parameter (env : env) ((v1, v2, v3) : CST.parameter) =
  let v1 = simple_identifier env v1 in
  let _v2 = token env v2 (* ":" *) in
  let v3 = type_ env v3 in
  (v1, v3)

and parameter_modifiers (env : env) (xs : CST.parameter_modifiers) =
  List.concat_map
    (function
      | `Anno x -> annotation env x
      | `Param_modi x -> [ parameter_modifier env x ])
    xs

and parameter_with_optional_type (env : env)
    ((v1, v2, v3) : CST.parameter_with_optional_type) : parameter_classic =
  let v1 =
    match v1 with
    | Some x -> parameter_modifiers env x
    | None -> []
  in
  let v2 = simple_identifier env v2 in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        let _v1 = token env v1 (* ":" *) in
        let v2 = type_ env v2 in
        Some v2
    | None -> None
  in
  G.param_of_id v2 ~pattrs:v1 ?ptype:v3

and parenthesized_expression (env : env)
    ((v1, v2, v3) : CST.parenthesized_expression) : G.expr =
  let _v1 = token env v1 (* "(" *) in
  let v2 = expression env v2 in
  let _v3 = token env v3 (* ")" *) in
  v2

and parenthesized_type (env : env) ((v1, v2, v3) : CST.parenthesized_type) =
  let _v1 = token env v1 (* "(" *) in
  let v2 = type_ env v2 in
  let _v3 = token env v3 (* ")" *) in
  v2

and primary_constructor (env : env) ((v1, v2) : CST.primary_constructor) :
    parameters =
  let _v1TODO =
    match v1 with
    | Some (v1, v2) ->
        let _v1TODO = modifiers_opt env v1 in
        let v2 = token env v2 (* "constructor" *) in
        Some v2
    | None -> None
  in
  let v2 = class_parameters env v2 in
  v2

and primary_expression (env : env) (x : CST.primary_expression) : expr =
  match x with
  | `Paren_exp x ->
      let x = parenthesized_expression env x in
      x
  | `Simple_id x ->
      let id = simple_identifier env x in
      G.N (H2.name_of_id id) |> G.e
  | `Lit_cst x -> L (literal_constant env x) |> G.e
  | `Str_lit x -> string_literal env x
  | `Call_ref (v1, v2, v3) ->
      let v1 =
        match v1 with
        | Some x ->
            let id = simple_identifier env x in
            Some id
        | None -> None
      in
      let v2 = token env v2 (* "::" *) in
      let v3 =
        match v3 with
        | `Simple_id x -> simple_identifier env x
        (* TODO? use G.OE_ClassLiteral like for Java? *)
        | `Class tok -> str env tok
        (* "class" *)
      in
      let name_info =
        match (v1, v2, v3) with
        | _ ->
            {
              name_last = (v3, None);
              name_middle = None (* TODO*);
              name_top = None;
              name_info = empty_id_info ();
            }
        (* TODO use qualifiers, with v1TODO above *)
      in
      G.N (IdQualified name_info) |> G.e
  | `Func_lit x -> function_literal env x
  | `Obj_lit (v1, v2, v3) ->
      let v1 = token env v1 (* "object" *) in
      let v2 =
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
          cextends = v2;
          cimplements = [];
          cmixins = [];
          cparams = fb [];
          cbody = v3;
        }
      |> G.e
  | `Coll_lit (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "[" *) in
      let v2 = expression env v2 in
      let v3 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = token env v1 (* "," *) in
            let v2 = expression env v2 in
            v2)
          v3
      in
      let v4 = token env v4 (* "]" *) in
      Container (List, (v1, v2 :: v3, v4)) |> G.e
  | `This_exp tok ->
      let tok = token env tok in
      IdSpecial (This, tok) |> G.e
      (* "this" *)
  | `Super_exp v1 ->
      let tok = token env v1 in
      IdSpecial (Super, tok) |> G.e
  | `Call_exp (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = call_suffix env v2 in
      Call (v1, v2) |> G.e
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
        | `SEMI t -> (G.emptystmt (token env t), None)
        | `Opt_cont_stru_body_opt_SEMI_else_choice_cont_stru_body
            (v1, v2, v3, v4) ->
            let v1 =
              match v1 with
              | Some x -> control_structure_body env x
              | None -> G.Block (fb []) |> G.s
            in
            let _v2 =
              match v2 with
              | Some t -> Some (G.emptystmt (token env t)) (* ";" *)
              | None -> None
            in
            let _v3 = token env v3 (* "else" *) in
            let v4 =
              match v4 with
              | `Cont_stru_body x -> control_structure_body env x
              | `SEMI tok -> G.emptystmt (token env tok)
              (* ";" *)
            in
            (v1, Some v4)
      in
      let v6, v7 = v5 in
      let if_stmt = If (v1, G.Cond v3, v6, v7) |> G.s in
      stmt_to_expr if_stmt
  | `When_exp (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "when" *) in
      let v2 =
        match v2 with
        | Some x -> Some (when_subject env x)
        | None -> None
      in
      let _v3 = token env v3 (* "{" *) in
      let v4 = List_.map (when_entry env) v4 in
      let _v5 = token env v5 (* "}" *) in
      let switch_stmt = Switch (v1, v2, v4) |> G.s in
      stmt_to_expr switch_stmt
  | `Try_exp (v1, v2, v3) ->
      let v1 = token env v1 (* "try" *) in
      let v2 = block env v2 in
      let catch, finally =
        match v3 with
        | `Rep1_catch_blk_opt_fina_blk (v1, v2) ->
            let v1 = List_.map (catch_block env) v1 in
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
      let try_stmt = Try (v1, v2, catch, None, finally) |> G.s in
      stmt_to_expr try_stmt
  | `Jump_exp x ->
      let v1 = jump_expression env x in
      stmt_to_expr v1

(* TODO: right now it's transform in '=' but should be different? *)
and property_delegate (env : env) ((v1, v2) : CST.property_delegate) =
  let _v1 = token env v1 (* "by" *) in
  let v2 = expression env v2 in
  Some v2

and setter (env : env) ((v0, v1, v2) : CST.setter) =
  let mods = modifiers_opt env v0 in
  let tset = token env v1 (* "set" *) in
  let fun_opt =
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
  (mods, tset, fun_opt)

and simple_user_type (env : env) ((v1, v2) : CST.simple_user_type) :
    ident * type_arguments option =
  let v1 = simple_identifier env v1 in
  let v2 =
    match v2 with
    | Some x ->
        let args = type_arguments env x in
        Some args
    | None -> None
  in
  (v1, v2)

and statement (env : env) (x : CST.statement) : stmt =
  match x with
  | `Decl x ->
      let dec = declaration env x in
      DefStmt dec |> G.s
  | `Rep_choice_label_choice_assign (_v1, v2) ->
      (*TODO let v1 =
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
  | `Part_class_decl (v1, v2, v3, v4, v5, v6, v7) ->
      let tparams =
        match v1 with
        | Some x -> Some (type_parameters env x)
        | None -> None
      in
      let cparams = primary_constructor env (Some (v2, v3), v4) in
      let cextends =
        match v5 with
        | Some (v1, v2) ->
            let _v1 = token env v1 (* ":" *) in
            let v2 = delegation_specifiers env v2 in
            v2
        | None -> []
      in
      let _type_constraints_TODO =
        match v6 with
        | Some x -> type_constraints env x
        | None -> []
      in
      let cbody =
        match v7 with
        | Some x -> class_body env x
        | None -> fb []
      in
      let ckind = (Class, fake "class") in
      (* See Constants section above why we use fake_name_for_partial_class_decl. *)
      let fake_ident = (fake_name_for_partial_class_decl, fake "fake_tok") in
      let ent = basic_entity fake_ident ?tparams in
      let cdef =
        { ckind; cextends; cimplements = []; cmixins = []; cparams; cbody }
      in
      let def = (ent, ClassDef cdef) in
      DefStmt def |> G.s

and statements (env : env) ((v1, v2, v3) : CST.statements) =
  let v1 = statement env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = semi env v1 (* pattern [\r\n]+ *) in
        let v2 = statement env v2 in
        v2)
      v2
  in
  let () =
    match v3 with
    | Some tok ->
        let _ = semi env tok (* pattern [\r\n]+ *) in
        ()
    | None -> ()
  in
  v1 :: v2

and string_literal (env : env) (v1, v2, v3) : expr =
  let l = token env v1 in
  let r = token env v3 in
  match v2 with
  | [ `Interp (`DOLLAR_simple_id (v1, v2)) ] when in_pattern env ->
      (* This is something of the form "$X". This is interpreted as an interpolated
         string of a single identifier, but if this is the pattern,
         it's probably not what the person writing the rule meant.
         Instead, we interpret it as a string literal containing a metavariable,
         which allows the existing literal metavariable machinery to run.

         This does not affect Semgrep's expressive power, because a string containing
         an interpolated identifier `X` can also be expressed in a Semgrep pattern via
         `"{X}"`.
      *)
      let s1, t1 = str env v1 (* "$" *) in
      let s2, t2 = simple_identifier env v2 in
      G.L (G.String (l, (s1 ^ s2, Tok.combine_toks l [ t1; t2; r ]), r)) |> G.e
  | _ ->
      let v2 =
        List_.map
          (fun x ->
            match x with
            | `Str_content tok ->
                (* string_content *)
                Either_.Left3 (str env tok)
            | `Interp x -> interpolation env x)
          v2
      in
      G.interpolated (l, v2, r)

and type_ (env : env) ((v1, v2) : CST.type_) : type_ =
  let v1 =
    match v1 with
    | Some x -> type_modifiers env x
    | None -> []
  in
  let v2 =
    match v2 with
    | `Paren_type x -> parenthesized_type env x
    | `Null_type x -> nullable_type env x
    | `Type_ref x -> type_reference env x
    | `Func_type x -> function_type env x
  in
  { v2 with t_attrs = v1 }

and type_arguments (env : env) ((v1, v2, v3, v4) : CST.type_arguments) =
  let v1 = token env v1 (* "<" *) in
  let v2 = type_projection env ~tok:v1 v2 in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let v1 = token env v1 (* "," *) in
        let v2 = type_projection env ~tok:v1 v2 in
        v2)
      v3
  in
  let v4 = token env v4 (* ">" *) in
  (v1, v2 :: v3, v4)

and type_constraint (env : env) ((v1, v2, v3, v4) : CST.type_constraint) :
    ident * type_ =
  let _v1TODO = List_.map (annotation env) v1 in
  let v2 = simple_identifier env v2 in
  let _v3 = token env v3 (* ":" *) in
  let v4 = type_ env v4 in
  (v2, v4)

and type_constraints (env : env) ((v1, v2, v3) : CST.type_constraints) =
  let _v1 = token env v1 (* "where" *) in
  let v2 = type_constraint env v2 in
  let v3 =
    List_.map
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
  List.concat_map (type_modifier env) xs

and type_parameter (env : env) ((v1, v2, v3) : CST.type_parameter) =
  let tp_modifiers =
    match v1 with
    | Some x -> type_parameter_modifiers env x
    | None -> []
  in
  let tp_id = simple_identifier env v2 in
  let tp_bounds =
    match v3 with
    | Some (v1, v2) ->
        let _v1 = token env v1 (* ":" *) in
        let v2 = type_ env v2 in
        [ v2 ]
    | None -> []
  in
  let tp_attrs, variances = Either_.partition (fun x -> x) tp_modifiers in
  let tp_attrs = List_.flatten tp_attrs in
  let tp_variance =
    match variances with
    | [] -> None
    (* can have multiple variance? I just keep the first one *)
    | x :: _ -> Some x
  in
  G.tparam_of_id tp_id ~tp_attrs ~tp_bounds ?tp_variance

and type_parameter_modifier (env : env) (x : CST.type_parameter_modifier) =
  match x with
  | `Reif_modi tok ->
      let x = str env tok in
      (* "reified" *)
      Either.Left [ unhandled_keywordattr x ]
  | `Vari_modi x -> Either.Right (type_projection_modifier env x)
  | `Anno x -> Either.Left (annotation env x)

and type_parameter_modifiers (env : env) (xs : CST.type_parameter_modifiers) =
  List_.map (type_parameter_modifier env) xs

and type_parameters (env : env) ((v1, v2, v3, v4) : CST.type_parameters) :
    G.type_parameters =
  let lt = token env v1 (* "<" *) in
  let v2 = type_parameter env v2 in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = type_parameter env v2 in
        v2)
      v3
  in
  let gt = token env v4 (* ">" *) in
  (lt, v2 :: v3, gt)

and type_projection (env : env) ~tok (x : CST.type_projection) =
  match x with
  | `Opt_type_proj_modifs_type (v1, v2) ->
      let _v1 =
        match v1 with
        | Some x -> type_projection_modifiers env x
        | None -> []
      in
      let v2 = type_ env v2 in
      OtherTypeArg (("Projection", tok), [ T v2 ])
  | `STAR tok ->
      let star = str env tok (* "*" *) in
      OtherTypeArg (star, [])

and type_reference (env : env) (x : CST.type_reference) : G.type_ =
  match x with
  | `User_type x ->
      let n = user_type env x in
      TyN n |> G.t
  | `Dyna tok (* "dynamic" *) -> TyAny (token env tok) |> G.t

and unary_expression (env : env) (x : CST.unary_expression) =
  match x with
  | `Post_exp (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = postfix_unary_operator env v2 in
      v2 v1
  | `Inde_exp (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = indexing_suffix env v2 in
      v2 v1
  | `Navi_exp (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = navigation_suffix env v2 in
      v2 v1
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
      | Some (Either.Left incr_decr, tok) ->
          G.special (IncrDecr (incr_decr, Postfix), tok) [ v2 ]
      | Some (Either.Right operator, tok) -> G.opcall (operator, tok) [ v2 ])
  | `As_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = as_operator env v2 in
      let v3 = type_ env v3 in
      Cast (v3, v2, v1) |> G.e
  | `Spread_exp (v1, v2) ->
      let v1 = token env v1 (* "*" *) in
      let v2 = expression env v2 in
      G.special (Spread, v1) [ v2 ]

and unescaped_annotation (env : env) (x : CST.unescaped_annotation) :
    name * arguments =
  match x with
  | `Cons_invo x ->
      let v1 = constructor_invocation env x in
      v1
  | `User_type x ->
      let v1 = user_type env x in
      (v1, fb [])

and user_type (env : env) ((v1, v2) : CST.user_type) : name =
  let v1 = simple_user_type env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "." *) in
        let v2 = simple_user_type env v2 in
        v2)
      v2
  in
  H2.name_of_ids_with_opt_typeargs (v1 :: v2)

and value_argument (env : env) ((v1, v2, v3, v4) : CST.value_argument) :
    argument =
  let _v1TODO =
    match v1 with
    | Some x -> [ annotation env x ]
    | None -> []
  in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = simple_identifier env v1 in
        let v2 = token env v2 (* "=" *) in
        Some (v1, v2)
    | None -> None
  in
  let v3 =
    match v3 with
    | Some tok -> Some (token env tok) (* "*" *)
    | None -> None
  in
  let v4 = expression env v4 in
  let e =
    match v3 with
    | None -> v4
    | Some t -> G.special (Spread, t) [ v4 ]
  in
  match v2 with
  | None -> Arg e
  | Some (id, _t) -> ArgKwd (id, e)

and value_arguments (env : env) ((v1, v2, v3) : CST.value_arguments) : arguments
    =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, v2, _v3) ->
        let v1 = value_argument env v1 in
        let v2 =
          List_.map
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

and when_condition (env : env) (x : CST.when_condition) : G.expr =
  match x with
  | `Exp v1 -> expression env v1
  (* TODO: there is an implicit first argument that is the thing
   * we call 'when' on
   *)
  | `Range_test (v1, v2) ->
      let op, tok = in_operator env v1 in
      let v2 = expression env v2 in
      G.opcall (op, tok) [ v2 ]
  | `Type_test (v1, v2) ->
      let op, tok = is_operator env v1 in
      let t = type_ env v2 in
      G.Call (G.IdSpecial (G.Op op, tok) |> G.e, fb [ G.ArgType t ]) |> G.e

and when_entry (env : env) ((v1, v2, v3, v4) : CST.when_entry) =
  let v1 =
    match v1 with
    | `When_cond_rep_COMMA_when_cond (v1, v2) ->
        let v1 = when_condition env v1 in
        let v2 =
          List_.map
            (fun (v1, v2) ->
              let v1 = token env v1 (* "," *) in
              let v2 = when_condition env v2 in
              (v1, v2))
            v2
        in
        let cond =
          v2
          |> List.fold_left (fun acc (t, e) -> G.opcall (And, t) [ acc; e ]) v1
        in
        Case (fake "case", PatWhen (PatWildcard (fake "_"), cond))
    | `Else tok -> Default (token env tok)
    (* "else" *)
  in
  let _v2 = token env v2 (* "->" *) in
  let v3 = control_structure_body env v3 in
  let () =
    match v4 with
    | Some tok ->
        let _ = semi env tok (* pattern [\r\n]+ *) in
        ()
    | None -> ()
  in
  CasesAndBody ([ v1 ], v3)

and when_subject (env : env) ((v1, v2, v3, v4) : CST.when_subject) : condition =
  let _v1 = token env v1 (* "(" *) in
  let _v2TODO =
    match v2 with
    | Some (v1, v2, v3, v4) ->
        let _v1 = List_.map (annotation env) v1 in
        let v2 = token env v2 (* "val" *) in
        let v3 = variable_declaration env v3 in
        let v4 = token env v4 (* "=" *) in
        Some (v1, v2, v3, v4)
    | None -> None
  in
  let v3 = expression env v3 in
  let _v4 = token env v4 (* ")" *) in
  (* TODO: use CondWithDecl *)
  G.Cond v3

let import_list (env : env) ((v1, v2) : CST.import_list) =
  let v1 = List_.map (import_header env) v1 in
  let _v2 = (* import_list_delimiter *) token env v2 in
  v1

let file_annotation (env : env) ((v1, v2, v3, v4, v5) : CST.file_annotation) =
  let _at = token env v1 (* "@" *) in
  let _file = token env v2 (* "file" *) in
  let _colon = token env v3 (* ":" *) in
  let _annot =
    match v4 with
    | `LBRACK_rep1_unes_anno_RBRACK (v1, v2, v3) ->
        let _v1 = token env v1 (* "[" *) in
        let v2 = List_.map (unescaped_annotation env) v2 in
        let _v3 = token env v3 (* "]" *) in
        v2
    | `Unes_anno x ->
        let v1 = unescaped_annotation env x in
        [ v1 ]
  in
  let _semi = semi env v5 in
  ()

let merge_class_declarations (xs : stmt list) : stmt list =
  let rec merge rev_acc (xs : stmt list) =
    match xs with
    (* Merge two consecutive class declarations, if the second one
     * is a partial class declaration.
     *
     * For the first class declaration, only the name, kind of class,
     * attributes, and type parameters will/may be populated because
     * it will come from the code
     *   @SomeAttribute class SomeClassName < TypeParam >
     * or
     *   @SomeAttribute enum SomeClassName < TypeParam >
     *
     * The rest is populated by the second part which is a partial
     * class declaration.
     *
     * We expect that each class declaration will have at most one
     * corresponding partial class declaration that follows immediately.
     *
     * If we missed something and somehow this is not the case, we will
     * let the partial class declaration remain in the Generic AST.
     *)
    | { s = DefStmt ({ name; attrs; tparams }, ClassDef { ckind; _ }); _ }
      :: {
           s =
             DefStmt
               ( {
                   name = EN (Id ((class_name, _), _));
                   attrs = [];
                   tparams = new_tparams;
                 },
                 ClassDef
                   { ckind = _; cextends; cimplements; cmixins; cparams; cbody }
               );
           _;
         }
      :: rest
      when class_name = fake_name_for_partial_class_decl ->
        let tparams =
          match tparams with
          | Some _ -> tparams
          | None -> new_tparams
        in
        let ent = { name; attrs; tparams } in
        let cdef =
          ClassDef { ckind; cextends; cimplements; cmixins; cparams; cbody }
        in
        let stmt = DefStmt (ent, cdef) |> G.s in
        merge (stmt :: rev_acc) rest
    | x :: rest -> merge (x :: rev_acc) rest
    | [] -> List.rev rev_acc
  in
  merge [] xs

let source_file (env : env) (x : CST.source_file) : any =
  match x with
  | `Opt_sheb_line_rep_file_anno_opt_pack_header_rep_import_list_rep_stmt_semi
      (v1, v2, v3, v4, v5) ->
      let _v1 =
        match v1 with
        | Some x -> shebang_line env x
        | None -> ()
      in
      let _v2 = List_.map (file_annotation env) v2 in
      let v3 =
        match v3 with
        | Some x -> [ package_header env x ]
        | None -> []
      in
      let v4 = List.concat_map (import_list env) v4 in
      let v5 =
        List_.map
          (fun (v1, v2) ->
            let v1 = statement env v1 in
            let _v2 = semi env v2 (* pattern [\r\n]+ *) in
            v1)
          v5
      in
      let xs = merge_class_declarations v5 in
      let dirs = v3 @ v4 |> List_.map (fun d -> DirectiveStmt d |> G.s) in
      Pr (dirs @ xs)
  | `Semg_exp (_v1, v2) ->
      let v2 = expression env v2 in
      E v2

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_kotlin.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = Program } in
      match source_file env cst with
      | G.Pr xs -> xs
      | _ -> failwith "not a program")

let parse_expression_or_source_file str =
  let res = Tree_sitter_kotlin.Parse.string str in
  match res.errors with
  | [] -> res
  | _ ->
      let expr_str = "__SEMGREP_EXPRESSION " ^ str in
      Tree_sitter_kotlin.Parse.string expr_str

let parse_pattern str =
  H.wrap_parser
    (fun () -> parse_expression_or_source_file str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env =
        { H.file; conv = H.line_col_to_pos_pattern str; extra = Pattern }
      in
      source_file env cst)
