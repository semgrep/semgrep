(* Yoann Padioleau
 *
 * Copyright (c) 2021-2022 R2C
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
module CST = Tree_sitter_cpp.CST
module H = Parse_tree_sitter_helpers
module HPfff = Parser_cpp_mly_helper
open Ast_cpp
module R = Tree_sitter_run.Raw_tree

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* OCaml parser using tree-sitter-lang/semgrep-cpp and converting
 * to ../ast/ast_cpp.ml
 *
 * The resulting AST can then be converted to the generic AST by using
 * ../generic/cpp_to_generic.ml
 *
 *)

(* to avoid cascading error effects when code is partially parsed by
 * tree-sitter *)
let recover_when_partial_error = ref true

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type env = unit H.env

let token = H.token
let _fake = Tok.fake_tok
let fb = Tok.fake_bracket
let str = H.str

(* for declarators *)
let id x = x
let error t s = raise (Parsing_error.Other_error (s, t))

(* To use in situations where we should raise an error, unless
 * tree-sitter partially parsed the file, in which case the AST may
 * be weird because tree-sitter may have skipped or inserted some tokens.
 *)
let error_unless_partial_error _env t s =
  (* TODO: we should add and check in env whether the file was partially
   * parsed and contained some ERROR CST nodes around t instead of
   * using a hardcoded boolean below.
   *)
  if not !recover_when_partial_error then error t s
  else
    logger#error "error_unless_partial_error: %s, at %s" s
      (Tok.stringpos_of_tok t)

let map_fold_operator (env : env) (x : CST.fold_operator) =
  match x with
  | `PLUS tok ->
      let t = (* "+" *) token env tok in
      (BinaryOp (Arith Plus), t)
  | `DASH tok ->
      let t = (* "-" *) token env tok in
      (BinaryOp (Arith Minus), t)
  | `STAR tok ->
      let t = (* "*" *) token env tok in
      (BinaryOp (Arith Mul), t)
  | `SLASH tok ->
      let t = (* "/" *) token env tok in
      (BinaryOp (Arith Div), t)
  | `PERC tok ->
      let t = (* "%" *) token env tok in
      (BinaryOp (Arith Mod), t)
  | `HAT tok ->
      let t = (* "^" *) token env tok in
      (BinaryOp (Arith Xor), t)
  | `AMP tok ->
      let t = (* "&" *) token env tok in
      (BinaryOp (Arith And), t)
  | `BAR tok ->
      let t = (* "|" *) token env tok in
      (BinaryOp (Arith Or), t)
  | `EQ tok ->
      let t = (* "=" *) token env tok in
      (AssignOp (SimpleAssign t), t)
  | `LT tok ->
      let t = (* "<" *) token env tok in
      (BinaryOp (Logical Inf), t)
  | `GT tok ->
      let t = (* ">" *) token env tok in
      (BinaryOp (Logical Sup), t)
  | `LTLT tok ->
      let t = (* "<<" *) token env tok in
      (BinaryOp (Arith DecLeft), t)
  | `GTGT tok ->
      let t = (* ">>" *) token env tok in
      (BinaryOp (Arith DecRight), t)
  | `PLUSEQ tok ->
      let t = (* "+=" *) token env tok in
      (AssignOp (OpAssign (Plus, t)), t)
  | `DASHEQ tok ->
      let t = (* "-=" *) token env tok in
      (AssignOp (OpAssign (Minus, t)), t)
  | `STAREQ tok ->
      let t = (* "*=" *) token env tok in
      (AssignOp (OpAssign (Mul, t)), t)
  | `SLASHEQ tok ->
      let t = (* "/=" *) token env tok in
      (AssignOp (OpAssign (Div, t)), t)
  | `PERCEQ tok ->
      let t = (* "%=" *) token env tok in
      (AssignOp (OpAssign (Mod, t)), t)
  | `HATEQ tok ->
      let t = (* "^=" *) token env tok in
      (AssignOp (OpAssign (Xor, t)), t)
  | `AMPEQ tok ->
      let t = (* "&=" *) token env tok in
      (AssignOp (OpAssign (And, t)), t)
  | `BAREQ tok ->
      let t = (* "|=" *) token env tok in
      (AssignOp (OpAssign (Or, t)), t)
  | `GTGTEQ tok ->
      let t = (* ">>=" *) token env tok in
      (AssignOp (OpAssign (DecRight, t)), t)
  | `LTLTEQ tok ->
      let t = (* "<<=" *) token env tok in
      (AssignOp (OpAssign (DecLeft, t)), t)
  | `EQEQ tok ->
      let t = (* "==" *) token env tok in
      (BinaryOp (Logical Eq), t)
  | `BANGEQ tok ->
      let t = (* "!=" *) token env tok in
      (BinaryOp (Logical NotEq), t)
  | `LTEQ tok ->
      let t = (* "<=" *) token env tok in
      (BinaryOp (Logical InfEq), t)
  | `GTEQ tok ->
      let t = (* ">=" *) token env tok in
      (BinaryOp (Logical SupEq), t)
  | `AMPAMP tok ->
      let t = (* "&&" *) token env tok in
      (BinaryOp (Logical AndLog), t)
  | `BARBAR tok ->
      let t = (* "||" *) token env tok in
      (BinaryOp (Logical OrLog), t)
  | `COMMA tok ->
      let t = (* "," *) token env tok in
      (CommaOp, t)
  | `DOTSTAR tok ->
      let t = (* ".*" *) token env tok in
      (DotStarOp, t)
  | `DASHGTSTAR tok ->
      let t = (* "->*" *) token env tok in
      (PtrOpOp PtrStarOp, t)

(* like Parse_c_tree_sitter.number_literal and H.parse_number_literal
 * but for ast_cpp.ml, not AST_generic.ml
 *)
let parse_number_literal (s, t) =
  match Common2.int_of_string_c_octal_opt s with
  | Some i -> Int (Some i, t)
  | None -> (
      match float_of_string_opt s with
      | Some f -> Float (Some f, t)
      (* could be None because of a suffix in the string *)
      | None -> Int (None, t))

(* see tree-sitter-c/grammar.js *)
let parse_primitive_type _env (s, t) =
  match s with
  (* size_t, ssize_t, intptr_t, int8_t, int16_t, ... *)
  | _ -> (nQ, TypeName (name_of_id (s, t)))

(* name builder helpers *)

let id_of_dname_for_parameter env dname =
  match dname with
  | DN (None, [], IdIdent id) -> id
  | DN (None, [], IdTemplated (IdIdent id, _args)) ->
      logger#error "Weird IdTemplated in id_of_dname_for_parameter";
      logger#error "Probably tree-sitter partial error: %s"
        (Ast_cpp.show_declarator_name dname);
      id
  | _ ->
      logger#error "Weird dname for parameter: %s"
        (Ast_cpp.show_declarator_name dname);
      error_unless_partial_error env (ii_of_dname dname)
        "expecting an ident for parameter";
      let ii = ii_of_dname dname in
      (Tok.content_of_tok ii, ii)

let name_of_dname_for_function dn =
  match dn with
  | DN n -> n
  | DNStructuredBinding (l, _, _) ->
      error l "single name expected for a function"

let name_of_dname_for_var _env dn =
  match dn with
  | DN n -> n
  | DNStructuredBinding (l, (_id, _xs), _) ->
      (* TODO: this can happen in ForRange; we should change
       * the type of ForRange to accept possible StructuredBindings.
       *)
      error l "single name expected for simple var"

(* name_of_id id *)

let _name_scoped nameopt tcolcol id_or_op : name =
  match nameopt with
  | None -> (Some tcolcol, [], id_or_op)
  | Some ((tcolcol1, xs1, id_or_op1) as name) ->
      let lastxs =
        match id_or_op1 with
        | IdIdent id -> QClassname id
        | IdTemplated (IdIdent id, args) -> QTemplateId (id, args)
        | IdTemplated _
        | IdOperator _
        | IdDestructor _
        | IdConverter _ ->
            error (ii_of_name name)
              "invalid operator/destructor/converter qualifier"
      in
      (tcolcol1, xs1 @ [ lastxs ], id_or_op)

let name_add_template_qual (id, args) (_tcolon, quals, base) : name =
  (None, QTemplateId (id, args) :: quals, base)

let name_add_class_qual id (_tcolon, quals, base) : name =
  (None, QClassname id :: quals, base)

let name_add_tcolon tcolon (_old_tcolon, quals, base) : name =
  (tcolon, quals, base)

let name_add_template_args name args =
  let top, qu, id_or_op = name in
  let id_or_op =
    match id_or_op with
    | IdTemplated _ ->
        error (ii_of_name name) "Impossible, already templated name"
    | _ -> IdTemplated (id_or_op, args)
  in
  (top, qu, id_or_op)

let trailing_comma env v =
  match v with
  | Some tok -> token env tok (* "," *) |> ignore
  | None -> ()

let make_onedecl ~v_name ~v_type ~v_init ~v_specs =
  match (v_name, v_init) with
  | DN n, _ -> V ({ name = n; specs = v_specs }, { v_type; v_init })
  | DNStructuredBinding (l, (id, ids), r), Some ini ->
      StructuredBinding (v_type, (l, id :: ids, r), ini)
  | DNStructuredBinding (_, (id, _), _), None ->
      (* see expecting_init.cpp for example of code badly parsed
       * by tree-sitter which then leads to this error. Note that
       * I don't use error_unless_partial_error because even without
       * any error in the file, tree-sitter still wrongly parses some
       * code as a StructuredBinding when it's not.
       *)
      logger#error "Weird DNStructuredBinding without an init at %s"
        (Tok.stringpos_of_tok (snd id));
      V ({ name = name_of_id id; specs = v_specs }, { v_type; v_init })

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-cpp/Boilerplate.ml *)

(**
   Boilerplate to be used as a template when mapping the cpp CST
   to another type of tree.
*)

let map_ms_call_modifier (env : env) (x : CST.ms_call_modifier) =
  match x with
  | `X___cdecl tok -> str env tok (* "__cdecl" *)
  | `X___clrc tok -> str env tok (* "__clrcall" *)
  | `X___stdc tok -> str env tok (* "__stdcall" *)
  | `X___fast tok -> str env tok (* "__fastcall" *)
  | `X___this tok -> str env tok (* "__thiscall" *)
  | `X___vect tok -> str env tok

(* "__vectorcall" *)

let map_anon_choice_DASHDASH_d11def2 (env : env)
    (x : CST.anon_choice_DASHDASH_d11def2) =
  match x with
  | `DASHDASH tok -> (Dec, token env tok) (* "--" *)
  | `PLUSPLUS tok -> (Inc, token env tok)

(* "++" *)

let map_lambda_default_capture (env : env) (x : CST.lambda_default_capture) =
  match x with
  | `EQ tok -> CaptureEq (token env tok) (* "=" *)
  | `AMP tok -> CaptureRef (token env tok)

(* "&" *)

let map_virtual_specifier (env : env) (x : CST.virtual_specifier) =
  match x with
  | `Final tok -> Final (token env tok) (* "final" *)
  | `Over tok -> Override (token env tok)

(* "override" *)

let map_type_qualifier (env : env) (x : CST.type_qualifier) :
    type_qualifier wrap =
  match x with
  | `Choice_const x -> (
      match x with
      | `Const tok -> (Const, token env tok) (* "const" *)
      | `Vola tok -> (Volatile, token env tok) (* "volatile" *)
      | `Rest tok -> (Restrict, token env tok) (* "restrict" *)
      | `X__Atomic tok -> (Atomic, token env tok) (* "_Atomic" *))
  | `Muta tok -> (Mutable, token env tok) (* "mutable" *)
  | `Cons_5014e42 tok -> (Constexpr, (* "constexpr" *) token env tok)
  | `Cons_36fe86c tok -> (Constinit, (* "constinit" *) token env tok)
  | `Cons_a25342f tok -> (Consteval, (* "consteval" *) token env tok)

(* "constexpr" *)

let map_virtual_function_specifier (env : env)
    (x : CST.virtual_function_specifier) =
  match x with
  | `Virt tok -> Virtual (token env tok)

(* "virtual" *)

let map_storage_class_specifier (env : env) (x : CST.storage_class_specifier) :
    storage wrap =
  match x with
  | `Extern tok -> (Extern, token env tok) (* "extern" *)
  | `Static tok -> (Static, token env tok) (* "static" *)
  | `Regi tok -> (Register, token env tok) (* "register" *)
  | `Inline tok -> (StoInline, token env tok)
  | `Thread_local tok -> (ThreadLocal, token env tok)
(* "thread_local" *)

(* "inline" *)

let map_anon_choice_DOT_2ad1dab (env : env) (x : CST.anon_choice_DOT_2ad1dab) =
  match x with
  | `DOT tok -> (Dot, token env tok) (* "." *)
  | `DASHGT tok -> (Arrow, token env tok)

(* "->" *)

let map_anon_choice_BANG_67174d6 (env : env) (x : CST.anon_choice_BANG_67174d6)
    =
  match x with
  | `BANG tok -> (Not, token env tok) (* "!" *)
  | `TILDE tok -> (Tilde, token env tok) (* "~" *)
  | `DASH tok -> (UnMinus, token env tok) (* "-" *)
  | `PLUS tok -> (UnPlus, token env tok)

(* "+" *)

let map_ms_unaligned_ptr_modifier (env : env)
    (x : CST.ms_unaligned_ptr_modifier) =
  match x with
  | `X__unal tok -> Unaligned (token env tok) (* "_unaligned" *)
  | `X___unal tok -> Unaligned (token env tok)

(* "__unaligned" *)

let map_anon_choice_type_a2fe5d4 (env : env) (x : CST.anon_choice_type_a2fe5d4)
    =
  match x with
  | `Type tok -> token env tok (* "typename" *)
  | `Class tok -> token env tok

(* "class" *)

let map_anon_choice_public_c9638d9 (env : env)
    (x : CST.anon_choice_public_c9638d9) =
  match x with
  | `Public tok -> (Public, token env tok) (* "public" *)
  | `Priv tok -> (Private, token env tok) (* "private" *)
  | `Prot tok -> (Protected, token env tok)

(* "protected" *)

let map_ref_qualifier (env : env) (x : CST.ref_qualifier) =
  match x with
  | `AMP tok ->
      let t = token env tok (* "&" *) in
      fun x -> (nQ, TReference (t, x))
  | `AMPAMP tok ->
      let t = token env tok (* "&&" *) in
      fun x -> (nQ, TRefRef (t, x))

let map_anon_choice_pat_25b90ba_4a37f8c (env : env)
    (x : CST.anon_choice_pat_25b90ba_4a37f8c) =
  match x with
  | `Pat_25b90ba tok -> Ifdef (token env tok) (* pattern #[ 	]*ifdef *)
  (* TODO Ifndef *)
  | `Pat_9d92f6a tok -> Ifdef (token env tok)

(* pattern #[ 	]*ifndef *)

let map_char_literal (env : env) ((v1, v2, v3) : CST.char_literal) =
  let v1 =
    match v1 with
    | `LSQUOT tok -> token env tok (* "L'" *)
    | `USQUOT_d861d39 tok -> token env tok (* "u'" *)
    | `USQUOT_2701bdc tok -> token env tok (* "U'" *)
    | `U8SQUOT tok -> token env tok (* "u8'" *)
    | `SQUOT tok -> token env tok
    (* "'" *)
  in
  let s, v2 =
    match v2 with
    | `Esc_seq tok -> str env tok (* escape_sequence *)
    | `Imm_tok_pat_36637e2 tok -> str env tok
    (* pattern "[^\\n']" *)
  in
  let v3 = token env v3 (* "'" *) in
  let t = Tok.combine_toks v1 [ v2; v3 ] in
  Char (s, t)

let map_decltype_auto (env : env) ((v1, v2, v3, v4) : CST.decltype_auto) : typeC
    =
  (* The distinction seems subtle here, but `decltype(auto)` allows us to
     capture more categories of values than `auto`. In particular, you
     can use it to note either a reference or non-reference type.
     https://cplusplus.com/forum/general/188645/
     TODO: I don't think the distinction is important atm, so let's just work with
     this.
  *)
  let _v1 = (* "decltype" *) token env v1 in
  let _v2 = (* "(" *) token env v2 in
  let v3 = (* "auto" *) token env v3 in
  let _v4 = (* ")" *) token env v4 in
  TAuto v3

let map_preproc_call (env : env) ((v1, v2, v3) : CST.preproc_call) =
  let v1 = token env v1 (* pattern #[ \t]*[a-zA-Z]\w* *) in
  let _v2 =
    match v2 with
    | Some tok -> Some (str env tok) (* preproc_arg *)
    | None -> None
  in
  let _v3 = token env v3 (* "\n" *) in
  PragmaAndCo v1

let map_ms_pointer_modifier (env : env) (x : CST.ms_pointer_modifier) =
  match x with
  | `Ms_unal_ptr_modi x -> map_ms_unaligned_ptr_modifier env x
  | `Ms_rest_modi tok -> PtrRestrict (token env tok) (* "__restrict" *)
  | `Ms_unsi_ptr_modi tok -> Uptr (token env tok) (* "__uptr" *)
  | `Ms_signed_ptr_modi tok -> Sptr (token env tok)

(* "__sptr" *)

let map_string_literal (env : env) ((v1, v2, v3) : CST.string_literal) :
    string wrap =
  let v1 =
    match v1 with
    | `LDQUOT tok -> token env tok (* "L\"" *)
    | `UDQUOT_c163aae tok -> token env tok (* "u\"" *)
    | `UDQUOT_df3447d tok -> token env tok (* "U\"" *)
    | `U8DQUOT tok -> token env tok (* "u8\"" *)
    | `DQUOT tok -> token env tok
    (* "\"" *)
  in
  let v2 =
    Common.map
      (fun x ->
        match x with
        | `Imm_tok_prec_p1_pat_c7f65b4 tok ->
            str env tok (* pattern "[^\\\\\"\\n]+" *)
        | `Esc_seq tok -> str env tok
        (* escape_sequence *))
      v2
  in
  let s = v2 |> Common.map fst |> String.concat "" in
  let xs = v2 |> Common.map snd in
  let v3 = token env v3 (* "\"" *) in
  let t = Tok.combine_toks v1 (xs @ [ v3 ]) in
  (s, t)

let map_preproc_def (env : env) ((v1, v2, v3, v4) : CST.preproc_def) =
  let v1 = token env v1 (* pattern #[ 	]*define *) in
  let v2 = str env v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 =
    match v3 with
    | Some tok ->
        let x = token env tok (* preproc_arg *) in
        (* TODO: we should parse this x! can be DefineExpr, etc. *)
        DefineTodo ("MacroBody", x)
    | None -> DefineEmpty
  in
  let _v4 = token env v4 (* "\n" *) in
  Define (v1, v2, DefineVar, v3)

let map_preproc_defined (env : env) (x : CST.preproc_defined) =
  match x with
  | `Defi_LPAR_id_RPAR (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "defined" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 = str env v3 (* pattern [a-zA-Z_]\w* *) in
      let v4 = token env v4 (* ")" *) in
      Call (IdSpecial (Defined, v1), (v2, [ Arg (expr_of_id v3) ], v4))
  | `Defi_id (v1, v2) ->
      let v1 = token env v1 (* "defined" *) in
      let v2 = str env v2 (* pattern [a-zA-Z_]\w* *) in
      Call (IdSpecial (Defined, v1), fb v1 [ Arg (expr_of_id v2) ])

let map_variadic_declarator (env : env) ((v1, v2) : CST.variadic_declarator) =
  let v1 = token env v1 (* "..." *) in
  let v2 =
    match v2 with
    | Some tok -> Some (str env tok) (* pattern [a-zA-Z_]\w* *)
    | None -> None
  in
  (v1, v2)

let map_field_designator (env : env) ((v1, v2) : CST.field_designator) :
    designator =
  let v1 = token env v1 (* "." *) in
  let v2 = str env v2 (* pattern [a-zA-Z_]\w* *) in
  DesignatorField (v1, v2)

let map_variadic_type_parameter_declaration (env : env)
    ((v1, v2, v3) : CST.variadic_type_parameter_declaration) =
  let v1 = map_anon_choice_type_a2fe5d4 env v1 in
  let v2 = token env v2 (* "..." *) in
  let v3 =
    match v3 with
    | Some tok -> Some (str env tok) (* pattern [a-zA-Z_]\w* *)
    | None -> None
  in
  TPVariadic (v1, v2, v3)

let map_type_parameter_declaration (env : env)
    ((v1, v2) : CST.type_parameter_declaration) : template_parameter =
  let v1 = map_anon_choice_type_a2fe5d4 env v1 in
  let v2 =
    match v2 with
    | Some tok -> Some (str env tok) (* pattern [a-zA-Z_]\w* *)
    | None -> None
  in
  TPClass (v1, v2, None)

let map_ms_declspec_modifier (env : env)
    ((v1, v2, v3, v4) : CST.ms_declspec_modifier) =
  let v1 = token env v1 (* "__declspec" *) in
  let v2 = token env v2 (* "(" *) in
  let v3 = str env v3 (* pattern [a-zA-Z_]\w* *) in
  let v4 = token env v4 (* ")" *) in
  DeclSpec (v1, (v2, v3, v4))

let map_anon_choice_name_id_d3c4b5f (env : env)
    (x : CST.anon_choice_name_id_d3c4b5f) =
  match x with
  | `Id tok -> str env tok (* pattern [a-zA-Z_]\w* *)
  (* TODO: should return an either? *)
  | `DOTDOTDOT tok -> str env tok

(* "..." *)

let map_sized_type_specifier (env : env) ((v1, v2) : CST.sized_type_specifier) :
    type_ =
  let v1 =
    Common.map
      (fun x ->
        match x with
        | `Signed tok -> (TSigned, token env tok) (* "signed" *)
        | `Unsi tok -> (TUnsigned, token env tok) (* "unsigned" *)
        | `Long tok -> (TLong, token env tok) (* "long" *)
        | `Short tok -> (TShort, token env tok)
        (* "short" *))
      v1
  in
  let v2 =
    match v2 with
    | Some x -> (
        match x with
        | `Id tok ->
            let id = str env tok (* pattern [a-zA-Z_]\w* *) in
            Some (nQ, TypeName (name_of_id id))
        | `Prim_type tok ->
            let x = str env tok (* primitive_type *) in
            Some (parse_primitive_type env x))
    | None -> None
  in
  (nQ, TSized (v1, v2))

let map_destructor_name (env : env) ((v1, v2) : CST.destructor_name) =
  let v1 = token env v1 (* "~" *) in
  let v2 = str env v2 (* pattern [a-zA-Z_]\w* *) in
  IdDestructor (v1, v2)

let map_anon_choice_raw_str_lit_28125b5 (env : env)
    (x : CST.anon_choice_raw_str_lit_28125b5) =
  match x with
  | `Raw_str_lit tok ->
      let x = str env tok (* raw_string_literal *) in
      x
  | `Str_lit x -> map_string_literal env x

let rec map_preproc_argument_list (env : env)
    ((v1, v2, v3) : CST.preproc_argument_list) : expr list paren =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_preproc_expression env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = map_preproc_expression env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 = token env v3 (* ")" *) in
  (v1, v2, v3)

and map_preproc_binary_expression (env : env)
    (x : CST.preproc_binary_expression) =
  match x with
  | `Prep_exp_PLUS_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "+" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Arith Plus, v2), v3)
  | `Prep_exp_DASH_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "-" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Arith Minus, v2), v3)
  | `Prep_exp_STAR_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "*" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Arith Mul, v2), v3)
  | `Prep_exp_SLASH_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "/" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Arith Div, v2), v3)
  | `Prep_exp_PERC_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "%" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Arith Mod, v2), v3)
  | `Prep_exp_BARBAR_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "||" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Logical OrLog, v2), v3)
  | `Prep_exp_AMPAMP_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "&&" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Logical AndLog, v2), v3)
  | `Prep_exp_BAR_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "|" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Arith Or, v2), v3)
  | `Prep_exp_HAT_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "^" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Arith Xor, v2), v3)
  | `Prep_exp_AMP_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "&" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Arith And, v2), v3)
  | `Prep_exp_EQEQ_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "==" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Logical Eq, v2), v3)
  | `Prep_exp_BANGEQ_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "!=" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Logical NotEq, v2), v3)
  | `Prep_exp_GT_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* ">" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Logical Sup, v2), v3)
  | `Prep_exp_GTEQ_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* ">=" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Logical SupEq, v2), v3)
  | `Prep_exp_LTEQ_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "<=" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Logical InfEq, v2), v3)
  | `Prep_exp_LT_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "<" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Logical Inf, v2), v3)
  | `Prep_exp_LTLT_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "<<" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Arith DecLeft, v2), v3)
  | `Prep_exp_GTGT_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* ">>" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Arith DecRight, v2), v3)

and map_preproc_call_expression (env : env)
    ((v1, v2) : CST.preproc_call_expression) =
  let v1 = str env v1 (* pattern [a-zA-Z_]\w* *) in
  let v2 = map_preproc_argument_list env v2 in
  (v1, v2)

and map_preproc_expression (env : env) (x : CST.preproc_expression) : expr =
  match x with
  | `Id tok ->
      let x = str env tok (* pattern [a-zA-Z_]\w* *) in
      expr_of_id x
  | `Prep_call_exp x ->
      let id, (l, xs, r) = map_preproc_call_expression env x in
      let args = xs |> Common.map expr_to_arg in
      Call (expr_of_id id, (l, args, r))
  | `Num_lit tok ->
      let x = str env tok (* number_literal *) in
      C (parse_number_literal x)
  | `Char_lit x ->
      let c = map_char_literal env x in
      C c
  | `Prep_defi x ->
      let x = map_preproc_defined env x in
      x
  | `Prep_un_exp (v1, v2) ->
      let op = map_anon_choice_BANG_67174d6 env v1 in
      let v2 = map_preproc_expression env v2 in
      Unary (op, v2)
  | `Prep_bin_exp x -> map_preproc_binary_expression env x
  | `Prep_paren_exp (v1, v2, v3) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_preproc_expression env v2 in
      let v3 = token env v3 (* ")" *) in
      ParenExpr (v1, v2, v3)

let map_variadic_reference_declarator (env : env)
    ((v1, v2) : CST.variadic_reference_declarator) =
  let v1 =
    match v1 with
    | `AMPAMP tok -> token env tok (* "&&" *)
    | `AMP tok -> token env tok
    (* "&" *)
  in
  let v2 = map_variadic_declarator env v2 in
  (v1, v2)

let map_preproc_params (env : env) ((v1, v2, v3) : CST.preproc_params) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_name_id_d3c4b5f env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = map_anon_choice_name_id_d3c4b5f env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 = token env v3 (* ")" *) in
  (v1, v2, v3)

let rec map_anon_choice_name_id_ba1b968_rev (env : env)
    (x : CST.anon_choice_name_id_ba1b968) =
  match x with
  | `Id tok ->
      (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *)
      [ str env tok ]
  | `Name_defi_name (v1, v2, v3, v4) ->
      let v1 = map_anon_choice_name_id_ba1b968_rev env v1 in
      let _v2 = (* "::" *) token env v2 in
      let _v3 =
        match v3 with
        | Some tok -> Some ((* "inline" *) token env tok)
        | None -> None
      in
      let v4 =
        (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *)
        str env v4
      in
      v4 :: v1

let map_anon_choice_name_id_ba1b968 (env : env)
    (x : CST.anon_choice_name_id_ba1b968) =
  let ids = map_anon_choice_name_id_ba1b968_rev env x in
  List.rev ids

let map_operator_name (env : env) ((v1, v2) : CST.operator_name) =
  let v1 = (* "operator" *) token env v1 in
  let v2 =
    match v2 with
    | `Co_await tok ->
        let t = (* "co_await" *) token env tok in
        (CoAwaitOp, t)
    | `PLUS tok ->
        let t = (* "+" *) token env tok in
        (BinaryOp (Arith Plus), t)
    | `DASH tok ->
        let t = (* "-" *) token env tok in
        (BinaryOp (Arith Minus), t)
    | `STAR tok ->
        let t = (* "*" *) token env tok in
        (BinaryOp (Arith Mul), t)
    | `SLASH tok ->
        let t = (* "/" *) token env tok in
        (BinaryOp (Arith Div), t)
    | `PERC tok ->
        let t = (* "%" *) token env tok in
        (BinaryOp (Arith Mod), t)
    | `HAT tok ->
        let t = (* "^" *) token env tok in
        (BinaryOp (Arith Xor), t)
    | `AMP tok ->
        let t = (* "&" *) token env tok in
        (BinaryOp (Arith And), t)
    | `BAR tok ->
        let t = (* "|" *) token env tok in
        (BinaryOp (Arith Or), t)
    | `TILDE tok ->
        let t = (* "~" *) token env tok in
        (UnaryTildeOp, t)
    | `BANG tok ->
        let t = (* "!" *) token env tok in
        (UnaryNotOp, t)
    | `EQ tok ->
        let t = (* "=" *) token env tok in
        (AssignOp (SimpleAssign t), t)
    | `LT tok ->
        let t = (* "<" *) token env tok in
        (BinaryOp (Logical Inf), t)
    | `GT tok ->
        let t = (* ">" *) token env tok in
        (BinaryOp (Logical Sup), t)
    | `PLUSEQ tok ->
        let t = (* "+=" *) token env tok in
        (AssignOp (OpAssign (Plus, t)), t)
    | `DASHEQ tok ->
        let t = (* "-=" *) token env tok in
        (AssignOp (OpAssign (Minus, t)), t)
    | `STAREQ tok ->
        let t = (* "*=" *) token env tok in
        (AssignOp (OpAssign (Mul, t)), t)
    | `SLASHEQ tok ->
        let t = (* "/=" *) token env tok in
        (AssignOp (OpAssign (Div, t)), t)
    | `PERCEQ tok ->
        let t = (* "%=" *) token env tok in
        (AssignOp (OpAssign (Mod, t)), t)
    | `HATEQ tok ->
        let t = (* "^=" *) token env tok in
        (AssignOp (OpAssign (Xor, t)), t)
    | `AMPEQ tok ->
        let t = (* "&=" *) token env tok in
        (AssignOp (OpAssign (And, t)), t)
    | `BAREQ tok ->
        let t = (* "|=" *) token env tok in
        (AssignOp (OpAssign (Or, t)), t)
    | `LTLT tok ->
        let t = (* "<<" *) token env tok in
        (BinaryOp (Arith DecLeft), t)
    | `GTGT tok ->
        let t = (* ">>" *) token env tok in
        (BinaryOp (Arith DecRight), t)
    | `GTGTEQ tok ->
        let t = (* ">>=" *) token env tok in
        (AssignOp (OpAssign (DecRight, t)), t)
    | `LTLTEQ tok ->
        let t = (* "<<=" *) token env tok in
        (AssignOp (OpAssign (DecLeft, t)), t)
    | `EQEQ tok ->
        let t = (* "==" *) token env tok in
        (BinaryOp (Logical Eq), t)
    | `BANGEQ tok ->
        let t = (* "!=" *) token env tok in
        (BinaryOp (Logical NotEq), t)
    | `LTEQ tok ->
        let t = (* "<=" *) token env tok in
        (BinaryOp (Logical InfEq), t)
    | `GTEQ tok ->
        let t = (* ">=" *) token env tok in
        (BinaryOp (Logical SupEq), t)
    | `LTEQGT tok ->
        let t = (* "<=>" *) token env tok in
        (BinaryOp (Logical Spaceship), t)
    | `AMPAMP tok ->
        let t = (* "&&" *) token env tok in
        (BinaryOp (Logical AndLog), t)
    | `BARBAR tok ->
        let t = (* "||" *) token env tok in
        (BinaryOp (Logical OrLog), t)
    | `PLUSPLUS tok ->
        let t = (* "++" *) token env tok in
        (FixOp Inc, t)
    | `DASHDASH tok ->
        let t = (* "--" *) token env tok in
        (FixOp Dec, t)
    | `COMMA tok ->
        let t = (* "," *) token env tok in
        (CommaOp, t)
    | `DASHGTSTAR tok ->
        let t = (* "->*" *) token env tok in
        (PtrOpOp PtrStarOp, t)
    | `DASHGT tok ->
        let t = (* "->" *) token env tok in
        (PtrOpOp PtrOp, t)
    | `LPARRPAR tok ->
        let t = (* "()" *) token env tok in
        (AccessOp ParenOp, t)
    | `LBRACKRBRACK tok ->
        let t = (* "[]" *) token env tok in
        (AccessOp ArrayOp, t)
    | `Choice_new_opt_LBRACKRBRACK (v1, v2) ->
        let is_new, v1, t1 =
          match v1 with
          | `New tok -> (true, AllocOp NewOp, (* "new" *) token env tok)
          | `Delete tok ->
              (false, AllocOp DeleteOp, (* "delete" *) token env tok)
        in
        let v2 =
          match v2 with
          | Some tok ->
              let t2 = (* "[]" *) token env tok in
              let t = Tok.combine_toks t1 [ t2 ] in
              if is_new then (AllocOp NewArrayOp, t)
              else (AllocOp DeleteArrayOp, t)
          | None -> (v1, t1)
        in
        v2
    | `DQUOTDQUOT_id (v1, v2) ->
        (* This seems to be used in the context of literal operators.
           https://en.cppreference.com/w/cpp/language/user_literal
        *)
        let v1 = (* "\"\"" *) token env v1 in
        let v2 =
          (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *)
          token env v2
        in
        (DQuoteOp, Tok.combine_toks v1 [ v2 ])
  in
  (v1, v2)

let map_concatenated_string (env : env) ((v1, v2) : CST.concatenated_string) =
  let v1 = map_anon_choice_raw_str_lit_28125b5 env v1 in
  let v2 = Common.map (map_anon_choice_raw_str_lit_28125b5 env) v2 in
  match v2 with
  | [] -> String v1
  | _ -> MultiString (v1 :: v2)

let map_user_defined_literal (env : env) ((v1, v2) : CST.user_defined_literal) =
  let v1 =
    match v1 with
    | `Num_lit tok -> parse_number_literal ((* number_literal *) str env tok)
    | `Char_lit x -> map_char_literal env x
    | `Str_lit x -> String (map_string_literal env x)
    | `Raw_str_lit tok -> String ((* raw_string_literal *) str env tok)
    | `Conc_str x -> map_concatenated_string env x
  in
  let v2 = (* pattern [a-zA-Z_]\w* *) str env v2 in
  UserDefined (v1, v2)

let map_preproc_include (env : env) ((v1, v2, v3) : CST.preproc_include) =
  let v1 = token env v1 (* pattern #[ 	]*include *) in
  let v2 =
    match v2 with
    (* TODO: in pfff the string in the string wrap does not contain
     * the enclosing chars *)
    | `Str_lit x ->
        let x = map_string_literal env x in
        IncLocal x
    | `System_lib_str tok ->
        let x = str env tok (* system_lib_string *) in
        IncSystem x
    | `Id tok ->
        let id = str env tok (* pattern [a-zA-Z_]\w* *) in
        IncOther (expr_of_id id)
    | `Prep_call_exp x ->
        let id, (l, xs, r) = map_preproc_call_expression env x in
        let args = xs |> Common.map expr_to_arg in
        let x = Call (expr_of_id id, (l, args, r)) in
        IncOther x
  in
  let _v3 = token env v3 (* "\n" *) in
  Include (v1, v2)

let map_preproc_function_def (env : env)
    ((v1, v2, v3, v4, v5) : CST.preproc_function_def) =
  let v1 = token env v1 (* pattern #[ 	]*define *) in
  let v2 = str env v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 = map_preproc_params env v3 in
  let v4 =
    match v4 with
    (* TODO: we should parse this x! can be DefineExpr, etc. *)
    | Some tok ->
        let x = token env tok (* preproc_arg *) in
        DefineTodo ("MacroBody", x)
    | None -> DefineEmpty
  in
  let _v5 = token env v5 (* "\n" *) in
  Define (v1, v2, DefineMacro v3, v4)

let rec map_abstract_array_declarator (env : env)
    ((v1, v2, v3, v4, v5) : CST.abstract_array_declarator) : abstract_declarator
    =
  let v1 =
    match v1 with
    | Some x -> map_abstract_declarator env x
    | None -> id
  in
  let v2 = token env v2 (* "[" *) in
  let v3 = Common.map (map_type_qualifier env) v3 in
  let v4 =
    match v4 with
    | Some x -> Some (map_anon_choice_exp_508611b env x)
    | None -> None
  in
  let v5 = token env v5 (* "]" *) in
  fun x -> v1 (v3, TArray ((v2, v4, v5), x))

and map_abstract_declarator (env : env) (x : CST.abstract_declarator) :
    abstract_declarator =
  match x with
  | `Choice_abst_poin_decl x -> (
      match x with
      | `Abst_poin_decl x -> map_abstract_pointer_declarator env x
      | `Abst_func_decl x -> map_abstract_function_declarator env x
      | `Abst_array_decl x -> map_abstract_array_declarator env x
      | `Abst_paren_decl x -> map_abstract_parenthesized_declarator env x)
  | `Abst_ref_decl (v1, v2) ->
      let v1 = map_ref_qualifier env v1 in
      let v2 =
        match v2 with
        | Some x -> map_abstract_declarator env x
        | None -> id
      in
      fun x -> x |> v2 |> v1

and map_requirement_clause_constraint (env : env)
    (x : CST.requirement_clause_constraint) : expr =
  match x with
  | `True tok ->
      let x = token env tok (* true *) in
      C (Bool (true, x))
  | `False tok ->
      let x = token env tok (* false *) in
      C (Bool (false, x))
  | `Class_name x -> N (map_class_name env x)
  | `Fold_exp x -> map_fold_expression env x
  | `Lambda_exp x -> map_lambda_expression env x
  | `Requis_exp x -> map_requires_expression env x
  | `LPAR_exp_RPAR (v1, v2, v3) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* ")" *) token env v3 in
      ParenExpr (v1, v2, v3)
  | `Cons_conj (v1, v2, v3) ->
      let v1 = map_requirement_clause_constraint env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_requirement_clause_constraint env v3 in
      Binary (v1, (Logical AndLog, v2), v3)
  | `Cons_disj (v1, v2, v3) ->
      let v1 = map_requirement_clause_constraint env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_requirement_clause_constraint env v3 in
      Binary (v1, (Logical OrLog, v2), v3)

and map_unary_left_fold (env : env) ((v1, v2, v3) : CST.unary_left_fold) =
  let v1 = (* "..." *) token env v1 in
  let v2 = map_fold_operator env v2 in
  let v3 = map_expression env v3 in
  LeftFold (v1, v2, v3)

and map_unary_right_fold (env : env) ((v1, v2, v3) : CST.unary_right_fold) =
  let v1 = map_expression env v1 in
  let v2 = map_fold_operator env v2 in
  let v3 = (* "..." *) token env v3 in
  RightFold (v1, v2, v3)

and map_binary_fold_operator (env : env)
    ((v1, v2, v3) : CST.binary_fold_operator) =
  let v1 = map_fold_operator env v1 in
  let v2 = (* "..." *) token env v2 in
  let v3 = map_fold_operator env v3 in
  (v1, v2, v3)

and map_binary_fold (env : env) ((v1, v2, v3) : CST.binary_fold) =
  let v1 = map_expression env v1 in
  let v2 = map_binary_fold_operator env v2 in
  let v3 = map_expression env v3 in
  BinaryFold (v1, v2, v3)

and map_fold_expression (env : env) ((v1, v2, v3) : CST.fold_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | `Un_right_fold x -> map_unary_right_fold env x
    | `Un_left_fold x -> map_unary_left_fold env x
    | `Bin_fold x -> map_binary_fold env x
  in
  let v3 = (* ")" *) token env v3 in
  FoldExpr (v1, v2, v3)

and map_requires_clause (env : env) ((v1, v2) : CST.requires_clause) =
  let _v1 = (* "requires" *) token env v1 in
  let v2 = map_requirement_clause_constraint env v2 in
  v2

and map_requires_expression (env : env) ((v1, v2, v3) : CST.requires_expression)
    =
  let v1 = (* "requires" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_requires_parameter_list env x
    | None -> fb v1 []
  in
  let v3 = map_requirement_seq env v3 in
  RequiresExpr (v1, v2, v3)

and map_requires_parameter_list (env : env)
    ((v1, v2, v3) : CST.requires_parameter_list) : parameter list paren =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_param_decl_1a61eef env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let _v1 = (* "," *) token env v1 in
              let v2 = map_anon_choice_param_decl_1a61eef env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 = (* ")" *) token env v3 in
  (v1, v2, v3)

and map_requirement_seq (env : env) ((v1, v2, v3) : CST.requirement_seq) =
  let v1 = (* "{" *) token env v1 in
  let v2 = List.map (map_requirement env) v2 in
  let v3 = (* "}" *) token env v3 in
  (v1, v2, v3)

and map_requirement (env : env) (x : CST.requirement) : requirement =
  match x with
  | `Exp_stmt x ->
      let expropt, sc = map_expression_statement env x in
      ExprReq (expropt, sc)
  | `Type_requ (v1, v2) ->
      let v1 = (* "typename" *) token env v1 in
      let v2 = map_class_name env v2 in
      TypeNameReq (v1, v2)
  | `Comp_requ (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "{" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "}" *) token env v3 in
      let v4 =
        match v4 with
        | Some tok -> Some ((* "noexcept" *) token env tok)
        | None -> None
      in
      let v5 =
        match v5 with
        | Some x -> Some (map_trailing_return_type env x)
        | None -> None
      in
      let v6 = (* ";" *) token env v6 in
      CompoundReq ((v1, v2, v3), v4, v5, v6)

and map_abstract_function_declarator (env : env)
    ((v1, v2, v3, v4, v5) : CST.abstract_function_declarator) :
    abstract_declarator =
  let v1 =
    match v1 with
    | Some x -> map_abstract_declarator env x
    | None -> id
  in
  let v2 = map_parameter_list env v2 in
  let v3 =
    Common.map
      (fun x ->
        match x with
        | `Type_qual x ->
            let x = map_type_qualifier env x in
            Left3 (TQ x)
        | `Ref_qual x ->
            (* This is a ref qualifier, which has to do with whether or not a function can be
               used as an lvalue or rvalue.
               https://stackoverflow.com/questions/21861148/what-does-the-single-ampersand-after-the-parameter-list-of-a-member-function-dec
               This modifies the return type, as we have chosen to represent it in ast_cpp (TReference and TRefRef)
            *)
            Middle3 (map_ref_qualifier env x)
        | `Noex x ->
            let x = map_noexcept env x in
            Right3 x
        | `Throw_spec x ->
            let x = map_throw_specifier env x in
            Right3 x)
      v3
  in
  let v4 =
    match v4 with
    | Some x -> Some (map_trailing_return_type env x)
    | None -> None
  in
  let v5 =
    match v5 with
    | Some x -> Some (map_requires_clause env x)
    | None -> None
  in
  let ft_specs, ft_ref_qualifiers, ft_throw = Common.partition_either3 id v3 in
  fun x ->
    let t =
      let init_t =
        match v4 with
        | None -> x
        | Some t -> t
      in
      match ft_ref_qualifiers with
      | [] -> init_t
      (* Shouldn't be possible to have more than 1. *)
      | ref_qual :: _ -> ref_qual init_t
    in
    v1
      ( nQ,
        TFunction
          {
            (* override ft_ret with trailing_return_type as x can actually
             * be a fake type sometimes (e.g., in lambdas)
             *)
            ft_ret = t;
            ft_params = v2;
            ft_specs;
            ft_const = None (* TODO *);
            ft_throw;
            ft_requires = v5;
          } )

and map_abstract_parenthesized_declarator (env : env)
    ((v1, v2, v3) : CST.abstract_parenthesized_declarator) : abstract_declarator
    =
  let v1 = token env v1 (* "(" *) in
  let v2 = map_abstract_declarator env v2 in
  let v3 = token env v3 (* ")" *) in
  fun x -> (nQ, ParenType (v1, v2 x, v3))

and map_abstract_pointer_declarator (env : env)
    ((v1, v2, v3) : CST.abstract_pointer_declarator) : abstract_declarator =
  let v1 = token env v1 (* "*" *) in
  let v2 = Common.map (map_type_qualifier env) v2 in
  let f1 x = (v2, TPointer (v1, x, [])) in
  let v3 =
    match v3 with
    | Some x -> map_abstract_declarator env x
    | None -> id
  in
  (* TODO: bug? seems different order than in declarator *)
  fun x -> x |> v3 |> f1

and map_alias_declaration (env : env)
    ((v1, v2, v3, v4, v5) : CST.alias_declaration) : using =
  let v1 = token env v1 (* "using" *) in
  let v2 = str env v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 = token env v3 (* "=" *) in
  let v4 = map_type_descriptor env v4 in
  let v5 = token env v5 (* ";" *) in
  (v1, UsingAlias (v2, v3, v4), v5)

(* for New and ObjInit *)
and map_anon_choice_arg_list_e4b6f8f (env : env)
    (x : CST.anon_choice_arg_list_e4b6f8f) : obj_init =
  match x with
  | `Arg_list x ->
      let x = map_argument_list env x in
      Args x
  | `Init_list x ->
      let x = map_initializer_list env x in
      Inits x

(* after a struct/union/class keyword *)
and map_anon_choice_class_name_d6703e6 (env : env)
    (x : CST.anon_choice_class_name_d6703e6) : class_key wrap -> type_ =
  match x with
  | `Class_name x ->
      let x = map_class_name env x in
      fun ckey -> (nQ, ClassName (ckey, x))
  | `Opt_class_name_opt_virt_spec_opt_base_class_clause_field_decl_list
      (v1, v2, v3, v4) ->
      let v1 =
        match v1 with
        | Some x -> Some (map_class_name env x)
        | None -> None
      in
      let v2 =
        match v2 with
        | Some x -> Some (map_virtual_specifier env x)
        | None -> None
      in
      let v3 =
        match v3 with
        | Some x -> map_base_class_clause env x
        | None -> []
      in
      let v4 = map_field_declaration_list env v4 in
      fun ckey ->
        let cdef =
          ( v1,
            {
              c_kind = ckey;
              c_inherit =
                v3 |> Common.map (fun clause -> { clause with i_virtual = v2 });
              c_members = v4;
            } )
        in
        (nQ, ClassDef cdef)

(* for map_declaration *)
and map_anon_choice_decl_f8b0ff3 (env : env) (x : CST.anon_choice_decl_f8b0ff3)
    =
  match x with
  | `Decl x ->
      let x = map_declarator env x in
      fun t specs ->
        let v_specs = specs in
        make_onedecl ~v_name:x.dn ~v_type:(x.dt t) ~v_init:None ~v_specs
  | `Init_decl x ->
      let x, init = map_init_declarator env x in
      fun t specs ->
        let v_specs = specs in
        make_onedecl ~v_name:x.dn ~v_type:(x.dt t) ~v_init:(Some init) ~v_specs

and map_anon_choice_exp_3078596 (env : env) (x : CST.anon_choice_exp_3078596) :
    initialiser =
  match x with
  | `Exp x ->
      let x = map_expression env x in
      InitExpr x
  | `Init_list x ->
      let x = map_initializer_list env x in
      InitList x

and map_anon_choice_exp_508611b (env : env) (x : CST.anon_choice_exp_508611b) =
  match x with
  | `Exp x ->
      let x = map_expression env x in
      x
  | `STAR tok ->
      let t = token env tok in
      ExprTodo (("StarTArray", t), [])

(* "*" *)
and map_anon_choice_exp_55b4dba (env : env) (x : CST.anon_choice_exp_55b4dba) :
    expr =
  match x with
  | `Exp x -> map_expression env x
  | `Comma_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "," *) in
      let v3 = map_anon_choice_exp_55b4dba env v3 in
      Sequence (v1, v2, v3)

and map_anon_choice_init_pair_1a6981e (env : env)
    (x : CST.anon_choice_init_pair_1a6981e) : initialiser =
  match x with
  | `Init_pair (v1, v2, v3) ->
      let v1 =
        Common.map
          (fun x ->
            match x with
            | `Subs_desi x -> map_subscript_designator env x
            | `Field_desi x -> map_field_designator env x)
          v1
      in
      let v2 = token env v2 (* "=" *) in
      let v3 = map_anon_choice_exp_3078596 env v3 in
      InitDesignators (v1, v2, v3)
  | `Exp x ->
      let x = map_expression env x in
      InitExpr x
  | `Init_list x ->
      let x = map_initializer_list env x in
      InitList x

and map_anon_choice_param_decl_1a61eef (env : env)
    (x : CST.anon_choice_param_decl_1a61eef) =
  match x with
  | `Param_decl x -> P (map_parameter_declaration env x)
  | `Opt_param_decl x -> P (map_optional_parameter_declaration env x)
  | `Vari_param_decl x -> map_variadic_parameter_declaration env x

and map_anon_choice_name_id_1d0ba77 (env : env)
    (x : CST.anon_choice_name_id_1d0ba77) =
  match x with
  | `Id tok ->
      (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *)
      name_of_id (str env tok)
  | `Qual_id x -> map_qualified_identifier env x

and map_anon_choice_op_cast_b108b62 (env : env)
    (x : CST.anon_choice_op_cast_b108b62) =
  match x with
  | `Op_cast x -> map_operator_cast env x
  | `Qual_op_cast_id x -> map_qualified_operator_cast_identifier env x

(* inside template parameters *)
and map_anon_choice_param_decl_13b5913 (env : env)
    (x : CST.anon_choice_param_decl_13b5913) : template_parameter =
  match x with
  | `Param_decl x ->
      let x = map_parameter_declaration env x in
      TP (P x)
  | `Opt_param_decl x ->
      let x = map_optional_parameter_declaration env x in
      TP (P x)
  | `Type_param_decl x ->
      let x = map_type_parameter_declaration env x in
      x
  | `Vari_param_decl x ->
      let x = map_variadic_parameter_declaration env x in
      TP x
  | `Vari_type_param_decl x ->
      let x = map_variadic_type_parameter_declaration env x in
      x
  | `Opt_type_param_decl x ->
      let x = map_optional_type_parameter_declaration env x in
      x
  | `Temp_temp_param_decl (v1, v2, v3) ->
      let v1 = token env v1 (* "template" *) in
      let v2 = map_template_parameter_list env v2 in
      let v3 =
        match v3 with
        | `Type_param_decl x -> map_type_parameter_declaration env x
        | `Vari_type_param_decl x ->
            map_variadic_type_parameter_declaration env x
        | `Opt_type_param_decl x ->
            map_optional_type_parameter_declaration env x
      in
      TPNested (v1, v2, v3)

and map_anon_choice_param_decl_d9083af (env : env)
    (x : CST.anon_choice_param_decl_d9083af) : parameter =
  match x with
  | `Param_decl x ->
      let x = map_parameter_declaration env x in
      P x
  | `Opt_param_decl x ->
      let x = map_optional_parameter_declaration env x in
      P x
  | `Vari_param_decl x ->
      let x = map_variadic_parameter_declaration env x in
      x
  | `DOTDOTDOT tok ->
      let x = token env tok in
      ParamEllipsis x

(* "..." *)
and map_anon_choice_prep_else_8b52b0f (env : env)
    (x : CST.anon_choice_prep_else_8b52b0f) : toplevel list =
  match x with
  | `Prep_else (v1, v2) ->
      let v1 = token env v1 (* pattern #[ 	]*else *) in
      let v2 = map_translation_unit env v2 in
      let dir = CppIfdef (IfdefElse v1) in
      dir :: v2
  | `Prep_elif (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* pattern #[ 	]*elif *) in
      let _v2 = map_preproc_expression env v2 in
      let _v3 = token env v3 (* "\n" *) in
      let v4 = map_translation_unit env v4 in
      let v5 =
        match v5 with
        | Some x -> map_anon_choice_prep_else_8b52b0f env x
        | None -> []
      in
      let dir = CppIfdef (IfdefElseif v1) in
      (dir :: v4) @ v5

and map_anon_choice_prep_else_in_field_decl_list_97ea65e (env : env)
    (x : CST.anon_choice_prep_else_in_field_decl_list_97ea65e) :
    class_member sequencable list =
  match x with
  | `Prep_else_in_field_decl_list (v1, v2) ->
      let v1 = token env v1 (* pattern #[ 	]*else *) in
      let v2 = List.concat_map (map_field_declaration_list_item env) v2 in
      let dir = CppIfdef (IfdefElse v1) in
      dir :: v2
  | `Prep_elif_in_field_decl_list (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* pattern #[ 	]*elif *) in
      let _v2 = map_preproc_expression env v2 in
      let _v3 = token env v3 (* "\n" *) in
      let v4 = List.concat_map (map_field_declaration_list_item env) v4 in
      let v5 =
        match v5 with
        | Some x -> map_anon_choice_prep_else_in_field_decl_list_97ea65e env x
        | None -> []
      in
      let dir = CppIfdef (IfdefElseif v1) in
      (dir :: v4) @ v5

and map_anon_choice_type_desc_4d9cafa (env : env)
    (x : CST.anon_choice_type_desc_4d9cafa) : template_argument =
  match x with
  | `Type_desc x ->
      let x = map_type_descriptor env x in
      Left x
  | `Type_param_pack_expa (v1, v2) ->
      let v1 = map_type_descriptor env v1 in
      let v2 = token env v2 (* "..." *) in
      let t = (nQ, TypeTodo (("TypeDots", v2), [ v1 ])) in
      Left t
  | `Exp x ->
      let x = map_expression env x in
      Right x

and map_anon_choice_type_qual_c8e0748 (env : env)
    (x : CST.anon_choice_type_qual_c8e0748) :
    (specifier, type_ -> type_, exn_spec) Common.either3 =
  match x with
  | `Type_qual x ->
      let x = map_type_qualifier env x in
      Left3 (TQ x)
  | `Virt_spec x ->
      let x = map_virtual_specifier env x in
      Left3 (M x)
  | `Noex x ->
      let x = map_noexcept env x in
      Right3 x
  | `Throw_spec x ->
      let x = map_throw_specifier env x in
      Right3 x
  | `Ref_qual x ->
      let x = map_ref_qualifier env x in
      Middle3 x

and map_argument_list (env : env) ((v1, v2, v3) : CST.argument_list) :
    argument list paren =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_exp_3078596 env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = map_anon_choice_exp_3078596 env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 = token env v3 (* ")" *) in
  ( v1,
    v2
    |> Common.map (function
         | InitExpr e -> Arg e
         | InitList (l, xs, r) -> ArgInits (l, xs, r)
         | _ -> raise Impossible (* see map_anon_choice_exp_3078596 *)),
    v3 )

and map_array_declarator (env : env)
    ((v1, v2, v3, v4, v5) : CST.array_declarator) : declarator =
  let v1 = map_declarator env v1 in
  let v2 = token env v2 (* "[" *) in
  let v3 = Common.map (map_type_qualifier env) v3 in
  let v4 =
    match v4 with
    | Some x -> Some (map_anon_choice_exp_508611b env x)
    | None -> None
  in
  let v5 = token env v5 (* "]" *) in
  { v1 with dt = (fun x -> v1.dt (v3, TArray ((v2, v4, v5), x))) }

and map_array_field_declarator (env : env)
    ((v1, v2, v3, v4, v5) : CST.array_field_declarator) =
  let v1 = map_field_declarator env v1 in
  let v2 = token env v2 (* "[" *) in
  let v3 = Common.map (map_type_qualifier env) v3 in
  let v4 =
    match v4 with
    | Some x -> Some (map_anon_choice_exp_508611b env x)
    | None -> None
  in
  let v5 = token env v5 (* "]" *) in
  { v1 with dt = (fun x -> v1.dt (v3, TArray ((v2, v4, v5), x))) }

and map_assignment_expression (env : env)
    ((v1, v2, v3) : CST.assignment_expression) : expr =
  let v1 = map_assignment_left_expression env v1 in
  let v2 =
    match v2 with
    | `EQ tok -> SimpleAssign (token env tok) (* "=" *)
    | `STAREQ tok -> OpAssign (Mul, token env tok) (* "*=" *)
    | `SLASHEQ tok -> OpAssign (Div, token env tok) (* "/=" *)
    | `PERCEQ tok -> OpAssign (Mod, token env tok) (* "%=" *)
    | `PLUSEQ tok -> OpAssign (Plus, token env tok) (* "+=" *)
    | `DASHEQ tok -> OpAssign (Minus, token env tok) (* "-=" *)
    | `LTLTEQ tok -> OpAssign (DecLeft, token env tok) (* "<<=" *)
    | `GTGTEQ tok -> OpAssign (DecRight, token env tok) (* ">>=" *)
    | `AMPEQ tok -> OpAssign (And, token env tok) (* "&=" *)
    | `HATEQ tok -> OpAssign (Xor, token env tok) (* "^=" *)
    | `BAREQ tok -> OpAssign (Or, token env tok)
    (* "|=" *)
  in
  let v3 = map_expression env v3 in
  Assign (v1, v2, v3)

and map_assignment_left_expression (env : env)
    (x : CST.assignment_left_expression) : a_lhs =
  match x with
  | `Choice_id x -> (
      match x with
      | `Id tok ->
          let x = str env tok (* pattern [a-zA-Z_]\w* *) in
          expr_of_id x
      | `Call_exp x -> map_call_expression env x
      | `Field_exp x -> map_field_expression env x
      | `Poin_exp x -> map_pointer_expression env x
      | `Subs_exp x -> map_subscript_expression env x
      | `Paren_exp x ->
          let l, e, r = map_parenthesized_expression env x in
          ParenExpr (l, e, r))
  | `Qual_id x ->
      let x = map_qualified_identifier env x in
      N x

and map_attribute (env : env) ((v1, v2, v3) : CST.attribute) =
  let ((_, base_tok) as base) =
    (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *)
    str env v2
  in
  let base =
    match v1 with
    | Some (v1, v2) ->
        let v1 =
          (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *)
          str env v1
        in
        let _v2 = (* "::" *) token env v2 in
        (None, [ QClassname v1 ], IdIdent base)
    | None -> (None, [], IdIdent base)
  in
  let v3 =
    match v3 with
    | Some x -> map_argument_list env x
    | None -> fb base_tok []
  in
  Call (N base, v3)

and map_attribute_declaration (env : env)
    ((v1, v2, v3, v4) : CST.attribute_declaration) : attribute =
  let v1 = token env v1 (* "[[" *) in
  let v2 = map_attribute env v2 in
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = map_attribute env v2 in
        v2)
      v3
  in
  let v4 = token env v4 (* "]]" *) in
  BracketsAttr (v1, v2 :: v3, v4)

and map_attribute_specifier (env : env)
    ((v1, v2, v3, v4) : CST.attribute_specifier) : attribute =
  let v1 = token env v1 (* "__attribute__" *) in
  let v2 = token env v2 (* "(" *) in
  let v3 = map_argument_list env v3 in
  let v4 = token env v4 (* ")" *) in
  UnderscoresAttr (v1, (v2, v3, v4))

and map_attributed_declarator (env : env) ((v1, v2) : CST.attributed_declarator)
    =
  let v1 = map_declarator env v1 in
  let _v2_TODO = Common.map (map_attribute_declaration env) v2 in
  v1

and map_attributed_field_declarator (env : env)
    ((v1, v2) : CST.attributed_field_declarator) =
  let v1 = map_field_declarator env v1 in
  let _v2_TODO = List.map (map_attribute_declaration env) v2 in
  v1

and map_attributed_statement (env : env) ((v1, v2) : CST.attributed_statement) =
  let _v1_TODO = List.map (map_attribute_declaration env) v1 in
  let v2 = map_statement env v2 in
  v2

and map_base_class_clause (env : env)
    ((v1, v2, v3, v4, v5) : CST.base_class_clause) : base_clause list =
  let _v1 = token env v1 (* ":" *) in
  let v2 =
    match v2 with
    | Some x -> Some (map_anon_choice_public_c9638d9 env x)
    | None -> None
  in
  let v3 = map_class_name env v3 in
  let _v4TODO =
    match v4 with
    | Some tok -> Some (token env tok) (* "..." *)
    | None -> None
  in
  let base1 = { i_name = v3; i_access = v2; i_virtual = None } in
  let v5 =
    Common.map
      (fun (v1, v2, v3, v4) ->
        let _v1 = token env v1 (* "," *) in
        let v2 =
          match v2 with
          | Some x -> Some (map_anon_choice_public_c9638d9 env x)
          | None -> None
        in
        let v3 = map_class_name env v3 in
        let _v4 =
          match v4 with
          | Some tok -> Some (token env tok) (* "..." *)
          | None -> None
        in
        { i_name = v3; i_access = v2; i_virtual = None })
      v5
  in
  base1 :: v5

and map_binary_expression (env : env) (x : CST.binary_expression) : expr =
  match x with
  | `Exp_LTEQGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "<=>" *) in
      let v3 = map_expression env v3 in
      Binary (v1, (Logical Spaceship, v2), v3)
  | `Choice_exp_PLUS_exp x -> (
      match x with
      | `Exp_PLUS_exp (v1, v2, v3) ->
          let v1 = map_expression env v1 in
          let v2 = token env v2 (* "+" *) in
          let v3 = map_expression env v3 in
          Binary (v1, (Arith Plus, v2), v3)
      | `Exp_DASH_exp (v1, v2, v3) ->
          let v1 = map_expression env v1 in
          let v2 = token env v2 (* "-" *) in
          let v3 = map_expression env v3 in
          Binary (v1, (Arith Minus, v2), v3)
      | `Exp_STAR_exp (v1, v2, v3) ->
          let v1 = map_expression env v1 in
          let v2 = token env v2 (* "*" *) in
          let v3 = map_expression env v3 in
          Binary (v1, (Arith Mul, v2), v3)
      | `Exp_SLASH_exp (v1, v2, v3) ->
          let v1 = map_expression env v1 in
          let v2 = token env v2 (* "/" *) in
          let v3 = map_expression env v3 in
          Binary (v1, (Arith Div, v2), v3)
      | `Exp_PERC_exp (v1, v2, v3) ->
          let v1 = map_expression env v1 in
          let v2 = token env v2 (* "%" *) in
          let v3 = map_expression env v3 in
          Binary (v1, (Arith Mod, v2), v3)
      | `Exp_BARBAR_exp (v1, v2, v3) ->
          let v1 = map_expression env v1 in
          let v2 = token env v2 (* "||" *) in
          let v3 = map_expression env v3 in
          Binary (v1, (Logical OrLog, v2), v3)
      | `Exp_AMPAMP_exp (v1, v2, v3) ->
          let v1 = map_expression env v1 in
          let v2 = token env v2 (* "&&" *) in
          let v3 = map_expression env v3 in
          Binary (v1, (Logical AndLog, v2), v3)
      | `Exp_BAR_exp (v1, v2, v3) ->
          let v1 = map_expression env v1 in
          let v2 = token env v2 (* "|" *) in
          let v3 = map_expression env v3 in
          Binary (v1, (Arith Or, v2), v3)
      | `Exp_HAT_exp (v1, v2, v3) ->
          let v1 = map_expression env v1 in
          let v2 = token env v2 (* "^" *) in
          let v3 = map_expression env v3 in
          Binary (v1, (Arith Xor, v2), v3)
      | `Exp_AMP_exp (v1, v2, v3) ->
          let v1 = map_expression env v1 in
          let v2 = token env v2 (* "&" *) in
          let v3 = map_expression env v3 in
          Binary (v1, (Arith And, v2), v3)
      | `Exp_EQEQ_exp (v1, v2, v3) ->
          let v1 = map_expression env v1 in
          let v2 = token env v2 (* "==" *) in
          let v3 = map_expression env v3 in
          Binary (v1, (Logical Eq, v2), v3)
      | `Exp_BANGEQ_exp (v1, v2, v3) ->
          let v1 = map_expression env v1 in
          let v2 = token env v2 (* "!=" *) in
          let v3 = map_expression env v3 in
          Binary (v1, (Logical NotEq, v2), v3)
      | `Exp_GT_exp (v1, v2, v3) ->
          let v1 = map_expression env v1 in
          let v2 = token env v2 (* ">" *) in
          let v3 = map_expression env v3 in
          Binary (v1, (Logical Sup, v2), v3)
      | `Exp_GTEQ_exp (v1, v2, v3) ->
          let v1 = map_expression env v1 in
          let v2 = token env v2 (* ">=" *) in
          let v3 = map_expression env v3 in
          Binary (v1, (Logical SupEq, v2), v3)
      | `Exp_LTEQ_exp (v1, v2, v3) ->
          let v1 = map_expression env v1 in
          let v2 = token env v2 (* "<=" *) in
          let v3 = map_expression env v3 in
          Binary (v1, (Logical InfEq, v2), v3)
      | `Exp_LT_exp (v1, v2, v3) ->
          let v1 = map_expression env v1 in
          let v2 = token env v2 (* "<" *) in
          let v3 = map_expression env v3 in
          Binary (v1, (Logical Inf, v2), v3)
      | `Exp_LTLT_exp (v1, v2, v3) ->
          let v1 = map_expression env v1 in
          let v2 = token env v2 (* "<<" *) in
          let v3 = map_expression env v3 in
          Binary (v1, (Arith DecLeft, v2), v3)
      | `Exp_GTGT_exp (v1, v2, v3) ->
          let v1 = map_expression env v1 in
          let v2 = token env v2 (* ">>" *) in
          let v3 = map_expression env v3 in
          Binary (v1, (Arith DecRight, v2), v3))

and map_bitfield_clause (env : env) ((v1, v2) : CST.bitfield_clause) =
  let v1 = token env v1 (* ":" *) in
  let v2 = map_expression env v2 in
  (v1, v2)

and map_call_expression (env : env) (x : CST.call_expression) : expr =
  match x with
  | `Exp_arg_list (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = map_argument_list env v2 in
      Call (v1, v2)
  | `Prim_type_arg_list (v1, v2) ->
      let v1 = str env v1 (* primitive_type *) in
      let t = parse_primitive_type env v1 in
      let v2 = map_argument_list env v2 in
      ConstructedObject (t, Args v2)

and map_cast_expression (env : env) ((v1, v2, v3, v4) : CST.cast_expression) =
  let v1 = token env v1 (* "(" *) in
  let v2 = map_type_descriptor env v2 in
  let v3 = token env v3 (* ")" *) in
  let v4 = map_expression env v4 in
  Cast ((v1, v2, v3), v4)

and map_catch_clause (env : env) ((v1, v2, v3) : CST.catch_clause) : handler =
  let v1 = token env v1 (* "catch" *) in
  let l, v2, r = map_parameter_list env v2 in
  let v3 = map_compound_statement env v3 in
  let param =
    match v2 with
    | [ p ] -> p
    | xs -> ParamTodo (("MultiParamExn", v1), xs)
  in
  (v1, (l, ExnDecl param, r), v3)

and map_class_name (env : env) (x : CST.class_name) : a_class_name =
  match x with
  | `Id tok ->
      let x = str env tok (* pattern [a-zA-Z_]\w* *) in
      name_of_id x
  | `Qual_type_id x ->
      let x = map_qualified_type_identifier env x in
      x
  | `Temp_type x ->
      let x = map_template_type env x in
      x

and map_co_await_expression (env : env) ((v1, v2) : CST.co_await_expression) =
  let v1 = (* "co_await" *) token env v1 in
  let v2 = map_expression env v2 in
  CoAwait (v1, v2)

and map_comma_expression (env : env) ((v1, v2, v3) : CST.comma_expression) =
  let v1 = map_expression env v1 in
  let v2 = (* "," *) token env v2 in
  let v3 = map_anon_choice_exp_55b4dba env v3 in
  Sequence (v1, v2, v3)

and map_compound_literal_expression (env : env)
    (x : CST.compound_literal_expression) =
  match x with
  | `LPAR_type_desc_RPAR_init_list (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_type_descriptor env v2 in
      let v3 = token env v3 (* ")" *) in
      let v4 = map_initializer_list env v4 in
      GccConstructor ((v1, v2, v3), v4)
  | `Class_name_init_list (v1, v2) ->
      let v1 = map_class_name env v1 in
      let t = (nQ, TypeName v1) in
      let l, xs, r = map_initializer_list env v2 in
      ConstructedObject (t, Inits (l, xs, r))

and map_compound_statement (env : env) ((v1, v2, v3) : CST.compound_statement) :
    compound =
  let v1 = token env v1 (* "{" *) in
  let v2 = map_translation_unit env v2 in
  let v3 = token env v3 (* "}" *) in
  (v1, v2, v3)

and map_concept_definition (env : env)
    ((v1, v2, v3, v4, v5) : CST.concept_definition) =
  (* A "concept" is a named set of requirements which may be imposed upon
     templates, functions, classes, etc.
     https://en.cppreference.com/w/cpp/language/constraints
  *)
  let v1 = (* "concept" *) token env v1 in
  let v2 =
    (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *)
    str env v2
  in
  let v3 = (* "=" *) token env v3 in
  let v4 = map_expression env v4 in
  let v5 = (* ";" *) token env v5 in
  Concept (v1, v2, v3, v4, v5)

and map_condition_clause (env : env) ((v1, v2, v3, v4) : CST.condition_clause) :
    condition_clause paren =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some x -> Some (map_init_statement env x)
    | None -> None
  in
  let v3 =
    match v3 with
    | `Exp x -> CondClassic (map_expression env x)
    | `Comma_exp x -> CondClassic (map_comma_expression env x)
    | `Cond_decl x -> map_condition_declaration env x
  in
  let v4 = token env v4 (* ")" *) in
  (v1, (v2, v3), v4)

and map_condition_declaration (env : env)
    ((v1, v2, v3) : CST.condition_declaration) : condition_subject =
  let t, specs = map_declaration_specifiers env v1 in
  let { dn; dt } = map_declarator env v2 in
  let v3 =
    match v3 with
    | `EQ_exp (v1, v2) ->
        let v1 = token env v1 (* "=" *) in
        let v2 = map_expression env v2 in
        EqInit (v1, InitExpr v2)
    | `Init_list x -> ObjInit (Inits (map_initializer_list env x))
  in
  let var =
    match dn with
    | DN n -> ({ name = n; specs }, { v_init = Some v3; v_type = dt t })
    | DNStructuredBinding _ ->
        error (ii_of_dname dn)
          "not expecting a structured_binding in a condition"
  in
  CondOneDecl var

and map_conditional_expression (env : env)
    ((v1, v2, v3, v4, v5) : CST.conditional_expression) =
  let v1 = map_expression env v1 in
  let v2 = token env v2 (* "?" *) in
  let v3 = map_expression env v3 in
  let v4 = token env v4 (* ":" *) in
  let v5 = map_expression env v5 in
  CondExpr (v1, v2, Some v3, v4, v5)

and map_constructor_or_destructor_declaration (env : env)
    ((v1, v2, v3) : CST.constructor_or_destructor_declaration) =
  let v1 = Common.map (map_constructor_specifiers env) v1 in
  let { dn; dt } = map_function_declarator env v2 in
  let v3 = token env v3 (* ";" *) in
  let n = name_of_dname_for_function dn in
  let t = dt (tvoid (ii_of_name n)) in
  let ent, def = HPfff.fixFunc ((n, t, []), FBDecl v3) in
  ({ ent with specs = v1 @ ent.specs }, def)

and map_anon_choice_comp_stmt_e6a11e2 (env : env)
    (x : CST.anon_choice_comp_stmt_e6a11e2) : function_body =
  match x with
  | `Comp_stmt x ->
      let l, xs, r = map_compound_statement env x in
      FBDef (l, xs, r)
  | `Try_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "try" *) in
      let v2 = map_compound_statement env v2 in
      let v3 = Common.map (map_catch_clause env) v3 in
      FBTry (v1, v2, v3)

and map_try_statement (env : env) ((v1, v2, v3) : CST.try_statement) =
  let v1 = token env v1 (* "try" *) in
  let v2 = map_compound_statement env v2 in
  let v3 = Common.map (map_catch_clause env) v3 in
  Try (v1, v2, v3)

and map_default_method_clause (env : env) (v1, v2, v3) =
  let v1 = token env v1 (* "=" *) in
  let v2 = token env v2 (* "default" *) in
  let v3 = token env v3 (* ";" *) in
  FBDefault (v1, v2, v3)

and map_delete_method_clause (env : env) (v1, v2, v3) =
  let v1 = token env v1 (* "=" *) in
  let v2 = token env v2 (* "delete" *) in
  let v3 = token env v3 (* ";" *) in
  FBDelete (v1, v2, v3)

and map_dependent_identifier (env : env) ((v1, v2) : CST.dependent_identifier) =
  (* brandon: I'm not actually sure if this is different than the non-template
     version.
  *)
  let _v1 = (* "template" *) token env v1 in
  let v2 = map_template_function env v2 in
  v2

and map_dependent_field_identifier (env : env)
    ((v1, v2) : CST.dependent_field_identifier) =
  (* brandon: I'm not actually sure if this is different than the non-template
     version.
  *)
  let _v1 = (* "template" *) token env v1 in
  let v2 = map_template_method env v2 in
  v2

and map_dependent_type_identifier (env : env)
    ((v1, v2) : CST.dependent_type_identifier) =
  let _v1 = (* "template" *) token env v1 in
  let v2 = map_template_type env v2 in
  v2

and map_dependent_type_identifier_with_name (env : env)
    ((v1, v2) : CST.dependent_type_identifier) name =
  let _v1 = (* "template" *) token env v1 in
  map_template_type_with_name env v2 name

and map_constructor_or_destructor_definition (env : env)
    ((v1, v2, v3, v4) : CST.constructor_or_destructor_definition) =
  let v1 = Common.map (map_constructor_specifiers env) v1 in
  let { dn; dt } = map_function_declarator env v2 in
  let _v3TODO =
    match v3 with
    | Some x -> map_field_initializer_list env x
    | None -> []
  in
  let n = name_of_dname_for_function dn in
  let t = dt (tvoid (ii_of_name n)) in
  let v4 =
    match v4 with
    | `Choice_comp_stmt x -> map_anon_choice_comp_stmt_e6a11e2 env x
    | `Defa_meth_clause x -> map_default_method_clause env x
    | `Delete_meth_clause x -> map_delete_method_clause env x
  in
  let ent, def = HPfff.fixFunc ((n, t, []), v4) in
  ({ ent with specs = v1 @ ent.specs }, def)

and map_constructor_specifiers (env : env) (x : CST.constructor_specifiers) :
    specifier =
  match x with
  | `Decl_modifs x -> map_declaration_modifiers env x
  | `Expl_func_spec x -> M (map_explicit_function_specifier env x)

and map_declaration_modifiers (env : env) (x : CST.declaration_modifiers) =
  match x with
  | `Choice_stor_class_spec x -> (
      match x with
      | `Stor_class_spec x ->
          let x = map_storage_class_specifier env x in
          ST x
      | `Type_qual x ->
          let x = map_type_qualifier env x in
          TQ x
      | `Attr_spec x ->
          let x = map_attribute_specifier env x in
          A x
      | `Attr_decl x -> A (map_attribute_declaration env x)
      | `Ms_decl_modi x -> A (map_ms_declspec_modifier env x))
  | `Virt_func_spec x ->
      let x = map_virtual_function_specifier env x in
      M x

and map_declaration (env : env) ((v1, v2, v3, v4) : CST.declaration) : vars_decl
    =
  let t, specs = map_declaration_specifiers env v1 in
  let v2 = map_anon_choice_decl_f8b0ff3 env v2 in
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = map_anon_choice_decl_f8b0ff3 env v2 in
        v2)
      v3
  in
  let v4 = token env v4 (* ";" *) in
  let xs = v2 :: v3 |> Common.map (fun f -> f t specs) in
  (xs, v4)

and map_declaration_list (env : env) ((v1, v2, v3) : CST.declaration_list) :
    declarations =
  let v1 = token env v1 (* "{" *) in
  let v2 = map_translation_unit env v2 in
  let v3 = token env v3 (* "}" *) in
  (v1, v2, v3)

and map_declaration_specifiers (env : env)
    ((v1, v2, v3) : CST.declaration_specifiers) : type_ * specifier list =
  let specs1 = Common.map (map_declaration_modifiers env) v1 in
  let t = map_type_specifier env v2 in
  let specs2 = Common.map (map_declaration_modifiers env) v3 in

  (* adjustments for 'const int foo', where the const
   * actually applies to the type (this is what we do in parse_cpp.mly)
   * not the decl *)
  let t, specs =
    let tqs, other =
      specs1 @ specs2
      |> Common.partition_either (function
           | TQ x -> Left x
           | (A _ | M _ | ST _) as x -> Right x)
    in
    let tqs2, tc = t in
    ((tqs @ tqs2, tc), other)
  in
  (t, specs)

and map_declarator (env : env) (x : CST.declarator) : declarator =
  match x with
  | `Choice_attr_decl x -> (
      match x with
      | `Attr_decl x -> map_attributed_declarator env x
      | `Poin_decl x -> map_pointer_declarator env x
      | `Func_decl x -> map_function_declarator env x
      | `Array_decl x -> map_array_declarator env x
      | `Paren_decl x -> map_parenthesized_declarator env x
      | `Id tok ->
          let x = str env tok (* pattern [a-zA-Z_]\w* *) in
          { dn = DN (name_of_id x); dt = id })
  | `Ref_decl (v1, v2) ->
      let v1 = map_ref_qualifier env v1 in
      let v2 = map_declarator env v2 in
      { v2 with dt = (fun x -> x |> v1 |> v2.dt) }
  | `Qual_id x ->
      let x = map_qualified_identifier env x in
      { dn = DN x; dt = id }
  | `Temp_func x ->
      let x = map_template_function env x in
      { dn = DN x; dt = id }
  | `Op_name tok ->
      let operator_tk, opwrap = map_operator_name env tok (* operator_name *) in
      let dn = DN (None, noQscope, IdOperator (operator_tk, opwrap)) in
      { dn; dt = id }
  | `Dest_name x ->
      let x = map_destructor_name env x in
      let dn = DN (None, noQscope, x) in
      { dn; dt = id }
  (* c++17: complex pattern assign *)
  | `Stru_bind_decl (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "[" *) in
      let v2 = str env v2 (* pattern [a-zA-Z_]\w* *) in
      let v3 =
        Common.map
          (fun (v1, v2) ->
            let _v1 = token env v1 (* "," *) in
            let v2 = str env v2 (* pattern [a-zA-Z_]\w* *) in
            v2)
          v3
      in
      let v4 = token env v4 (* "]" *) in
      { dn = DNStructuredBinding (v1, (v2, v3), v4); dt = id }

and map_empty_declaration (env : env) ((v1, v2) : CST.empty_declaration) : decl
    =
  let v1 = map_type_specifier env v1 in
  let v2 = token env v2 (* ";" *) in
  let one = EmptyDecl v1 in
  DeclList ([ one ], v2)

and map_enum_base_clause (env : env) ((v1, v2) : CST.enum_base_clause) =
  let _v1 = token env v1 (* ":" *) in
  let v2 =
    match v2 with
    | `Qual_type_id x ->
        let x = map_qualified_type_identifier env x in
        (nQ, TypeName x)
    | `Id tok ->
        let x = str env tok (* pattern [a-zA-Z_]\w* *) in
        (nQ, TypeName (name_of_id x))
    | `Sized_type_spec x ->
        let x = map_sized_type_specifier env x in
        x
  in
  v2

and map_enumerator (env : env) ((v1, v2) : CST.enumerator) : enum_elem =
  let v1 = str env v1 (* pattern [a-zA-Z_]\w* *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "=" *) in
        let v2 = map_expression env v2 in
        Some (v1, v2)
    | None -> None
  in
  { e_name = v1; e_val = v2 }

and map_enumerator_list (env : env) ((v1, v2, v3, v4) : CST.enumerator_list) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_enumerator env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = map_enumerator env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let _v3 = trailing_comma env v3 in
  let v4 = token env v4 (* "}" *) in
  (v1, v2, v4)

and map_explicit_function_specifier (env : env)
    (x : CST.explicit_function_specifier) : modifier =
  match x with
  | `Expl tok -> Explicit (token env tok, (* "explicit" *) None)
  | `Expl_LPAR_exp_RPAR (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "explicit" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 = map_expression env v3 in
      let v4 = token env v4 (* ")" *) in
      Explicit (v1, Some (v2, v3, v4))

and map_expression (env : env) (x : CST.expression) : expr =
  match x with
  | `Choice_choice_cond_exp x -> map_expression_bis env x
  | `Semg_ellips v1 ->
      let t = token env v1 in
      Ellipsis t
  | `Deep_ellips (v1, v2, v3) ->
      let l = token env v1 in
      let e = map_expression env v2 in
      let r = token env v3 in
      DeepEllipsis (l, e, r)

and map_expression_bis (env : env) x : expr =
  match x with
  | `Choice_cond_exp x -> (
      match x with
      | `Cond_exp x -> map_conditional_expression env x
      | `Assign_exp x -> map_assignment_expression env x
      | `Bin_exp x -> map_binary_expression env x
      | `Un_exp x -> map_unary_expression env x
      | `Update_exp x -> map_update_expression env x
      | `Cast_exp x -> map_cast_expression env x
      | `Poin_exp x -> map_pointer_expression env x
      | `Sizeof_exp x -> map_sizeof_expression env x
      | `Subs_exp x -> map_subscript_expression env x
      | `Call_exp x -> map_call_expression env x
      | `Field_exp x -> map_field_expression env x
      | `Comp_lit_exp x -> map_compound_literal_expression env x
      | `Id tok ->
          let x = str env tok (* pattern [a-zA-Z_]\w* *) in
          expr_of_id x
      | `Num_lit tok ->
          let x = str env tok (* number_literal *) in
          C (parse_number_literal x)
      | `Str_lit x ->
          let x = map_string_literal env x in
          C (String x)
      | `True tok ->
          let x = token env tok (* true *) in
          C (Bool (true, x))
      | `False tok ->
          let x = token env tok (* false *) in
          C (Bool (false, x))
      | `Null tok ->
          let x = token env tok (* "NULL" *) in
          C (Nullptr x)
      | `Conc_str x ->
          let x = map_concatenated_string env x in
          C x
      | `Char_lit x ->
          let x = map_char_literal env x in
          C x
      | `Paren_exp x ->
          let l, e, r = map_parenthesized_expression env x in
          ParenExpr (l, e, r))
  | `Co_await_exp x -> map_co_await_expression env x
  (* I'm pretty sure these two can't appear in normal places that expressions
     can, but only in the context of concepts or templates.
     But the grammar permits it. C'est la vie.
  *)
  | `Requis_exp x -> map_requires_expression env x
  | `Requis_clause x -> map_requires_clause env x
  | `Temp_func x ->
      let x = map_template_function env x in
      N x
  | `Qual_id x ->
      let x = map_qualified_identifier env x in
      N x
  | `New_exp (v1, v2, v3, v4, v5, v6) ->
      let v1 =
        match v1 with
        | Some tok -> Some (token env tok) (* "::" *)
        | None -> None
      in
      let v2 = token env v2 (* "new" *) in
      let v3 =
        match v3 with
        | Some x -> Some (map_argument_list env x)
        | None -> None
      in
      let t = map_type_specifier env v4 in
      let v5 =
        match v5 with
        | Some x -> map_new_declarator env x
        | None -> id
      in
      let t = v5 t in
      let v6 =
        match v6 with
        | Some x -> Some (map_anon_choice_arg_list_e4b6f8f env x)
        | None -> None
      in
      New (v1, v2, v3, t, v6)
  | `Delete_exp (v1, v2, v3, v4) ->
      let v1 =
        match v1 with
        | Some tok -> Some (token env tok) (* "::" *)
        | None -> None
      in
      let v2 = token env v2 (* "delete" *) in
      let v3 =
        match v3 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* "[" *) in
            let v2 = token env v2 (* "]" *) in
            fun tcolcol tkwd e -> Delete (tcolcol, tkwd, Some (v1, (), v2), e)
        | None -> fun tcolcol tkwd e -> Delete (tcolcol, tkwd, None, e)
      in
      let v4 = map_expression env v4 in
      v3 v1 v2 v4
  | `Lambda_exp x -> map_lambda_expression env x
  | `Param_pack_expa (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "..." *) in
      ParamPackExpansion (v1, v2)
  | `Null tok ->
      let x = token env tok (* "nullptr" *) in
      C (Nullptr x)
  | `This tok ->
      let x = token env tok (* "this" *) in
      IdSpecial (This, x)
  | `Raw_str_lit tok ->
      let x = str env tok in
      C (String x)
  | `User_defi_lit x -> map_user_defined_literal env x
  | `Fold_exp x -> map_fold_expression env x

(* raw_string_literal *)
and map_expression_statement (env : env) ((v1, v2) : CST.expression_statement) :
    expr option * sc =
  let v1 =
    match v1 with
    | Some x -> Some (map_anon_choice_exp_55b4dba env x)
    | None -> None
  in
  let v2 = token env v2 (* ";" *) in
  (v1, v2)

and map_field_declaration (env : env) ((v1, v2, v3, v4) : CST.field_declaration)
    : class_member =
  let t, specs = map_declaration_specifiers env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_field_declarator env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = map_field_declarator env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 =
    match v3 with
    | Some x -> (
        match x with
        | `Bitf_clause x ->
            let t, e = map_bitfield_clause env x in
            Some (Bitfield (t, e))
        | `Init_list x ->
            let x = map_initializer_list env x in
            Some (ObjInit (Inits x))
        | `EQ_choice_exp (v1, v2) ->
            let v1 = token env v1 (* "=" *) in
            let v2 = map_anon_choice_exp_3078596 env v2 in
            Some (EqInit (v1, v2)))
    | None -> None
  in
  let v4 = token env v4 (* ";" *) in
  (* TODO? v4 can be empty? *)
  let xs =
    v2
    |> Common.map (fun { dn; dt } ->
           make_onedecl ~v_name:dn ~v_init:v3 ~v_type:(dt t) ~v_specs:specs)
  in
  F (DeclList (xs, v4))

and map_field_declaration_list (env : env)
    ((v1, v2, v3) : CST.field_declaration_list) :
    class_member sequencable list brace =
  let v1 = token env v1 (* "{" *) in
  let v2 = List.concat_map (map_field_declaration_list_item env) v2 in
  let v3 = token env v3 (* "}" *) in
  (v1, v2, v3)

and map_field_declaration_list_item (env : env)
    (x : CST.field_declaration_list_item) : class_member sequencable list =
  match x with
  | `Choice_field_decl x -> (
      match x with
      | `Field_decl x ->
          let x = map_field_declaration env x in
          [ X x ]
      | `Prep_def x ->
          let x = map_preproc_def env x in
          [ CppDirective x ]
      | `Prep_func_def x ->
          let x = map_preproc_function_def env x in
          [ CppDirective x ]
      | `Prep_call x ->
          let x = map_preproc_call env x in
          [ CppDirective x ]
      | `Prep_if_in_field_decl_list x ->
          let x = map_preproc_if_in_field_declaration_list env x in
          x
      | `Prep_ifdef_in_field_decl_list x ->
          let x = map_preproc_ifdef_in_field_declaration_list env x in
          x)
  | `Temp_decl x ->
      let x = map_template_declaration env x in
      [ X (F x) ]
  | `Inline_meth_defi (v1, v2, v3) ->
      let t, specs = map_declaration_specifiers env v1 in
      let { dn; dt } = map_field_declarator env v2 in
      let v3 =
        match v3 with
        | `Comp_stmt x ->
            let x = map_compound_statement env x in
            FBDef x
        | `Defa_meth_clause x -> map_default_method_clause env x
        | `Delete_meth_clause x -> map_delete_method_clause env x
      in
      let n = name_of_dname_for_function dn in
      let t = dt t in
      let ent, def = HPfff.fixFunc ((n, t, []), v3) in
      let fdef = ({ ent with specs = ent.specs @ specs }, def) in
      [ X (F (Func fdef)) ]
  | `Cons_or_dest_defi x ->
      let x = map_constructor_or_destructor_definition env x in
      [ X (F (Func x)) ]
  | `Cons_or_dest_decl x ->
      let x = map_constructor_or_destructor_declaration env x in
      [ X (F (Func x)) ]
  | `Op_cast_defi x ->
      let x = map_operator_cast_definition env x in
      [ X (F (Func x)) ]
  | `Op_cast_decl x ->
      let x = map_operator_cast_declaration env x in
      [ X (F (DeclList x)) ]
  | `Friend_decl (v1, v2) ->
      let v1 = token env v1 (* "friend" *) in
      let v2 =
        match v2 with
        | `Decl x ->
            let x = map_declaration env x in
            DeclList x
        | `Func_defi x ->
            let x = map_function_definition env x in
            Func x
        | `Opt_choice_class_class_name_SEMI (v1, v2, v3) ->
            let v1 =
              match v1 with
              | Some x -> (
                  match x with
                  | `Class tok -> Some (Class, token env tok) (* "class" *)
                  | `Struct tok -> Some (Struct, token env tok) (* "struct" *)
                  | `Union tok -> Some (Union, token env tok) (* "union" *))
              | None -> None
            in
            let v2 = map_class_name env v2 in
            let v3 = token env v3 (* ";" *) in
            let t =
              match v1 with
              | None -> (nQ, TypeName v2)
              | Some class_key -> (nQ, ClassName (class_key, v2))
            in
            let one = EmptyDecl t in
            DeclList ([ one ], v3)
      in
      [ X (Friend (v1, v2)) ]
  | `Access_spec (v1, v2) ->
      let v1 = map_anon_choice_public_c9638d9 env v1 in
      let v2 = token env v2 (* ":" *) in
      [ X (Access (v1, v2)) ]
  | `Alias_decl x ->
      let x = map_alias_declaration env x in
      [ X (F (UsingDecl x)) ]
  | `Using_decl x ->
      let x = map_using_declaration env x in
      [ X (F (UsingDecl x)) ]
  | `Type_defi x ->
      let x = map_type_definition env x in
      [ X (F (DeclList x)) ]
  | `Static_assert_decl x ->
      let x = map_static_assert_declaration env x in
      [ X (F x) ]

and map_field_declarator (env : env) (x : CST.field_declarator) : declarator =
  match x with
  | `Choice_attr_field_decl x -> (
      match x with
      | `Attr_field_decl x -> map_attributed_field_declarator env x
      | `Poin_field_decl x -> map_pointer_field_declarator env x
      | `Func_field_decl x -> map_function_field_declarator env x
      | `Array_field_decl x -> map_array_field_declarator env x
      | `Paren_field_decl x -> map_parenthesized_field_declarator env x
      | `Id tok ->
          let x = str env tok (* pattern [a-zA-Z_]\w* *) in
          { dn = DN (name_of_id x); dt = id })
  | `Ref_field_decl (v1, v2) ->
      let v1 = map_ref_qualifier env v1 in
      let v2 = map_field_declarator env v2 in
      { v2 with dt = (fun x -> x |> v1 |> v2.dt) }
  | `Temp_meth x ->
      let x = map_template_method env x in
      { dn = DN x; dt = id }
  | `Op_name tok ->
      let operator_tk, opwrap = map_operator_name env tok (* operator_name *) in
      { dn = DN (None, [], IdOperator (operator_tk, opwrap)); dt = id }

and map_field_expression (env : env) (x : CST.field_expression) : expr =
  match x with
  | `Exp_choice_DOT_id (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_anon_choice_DOT_2ad1dab env v2 in
      let v3 = str env v3 (* pattern [a-zA-Z_]\w* *) in
      DotAccess (v1, v2, name_of_id v3)
  | `Exp_choice_DOT_choice_dest_name (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_anon_choice_DOT_2ad1dab env v2 in
      let v3 =
        match v3 with
        | `Dest_name x ->
            let x = map_destructor_name env x in
            (None, [], x)
        | `Temp_meth x ->
            let x = map_template_method env x in
            x
        | `Depe_field_id x -> map_dependent_field_identifier env x
      in
      DotAccess (v1, v2, v3)

and map_field_initializer (env : env) ((v1, v2, v3) : CST.field_initializer) =
  let v1 =
    match v1 with
    | `Id tok ->
        (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *)
        name_of_id (str env tok)
    | `Temp_meth x -> map_template_method env x
    | `Qual_field_id x -> map_qualified_field_identifier env x
  in
  let v2 =
    match v2 with
    | `Init_list x ->
        let x = map_initializer_list env x in
        Inits x
    | `Arg_list x ->
        let x = map_argument_list env x in
        Args x
  in
  let _v3TODO =
    match v3 with
    | Some tok -> Some (token env tok) (* "..." *)
    | None -> None
  in
  (v1, v2)

and map_field_initializer_list (env : env)
    ((v1, v2, v3) : CST.field_initializer_list) =
  let _v1 = token env v1 (* ":" *) in
  let v2 = map_field_initializer env v2 in
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = map_field_initializer env v2 in
        v2)
      v3
  in
  v2 :: v3

and map_function_declarator (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.function_declarator) : declarator =
  let v1 = map_declarator env v1 in
  let v2 = map_parameter_list env v2 in
  let _v3 = Common.map (map_attribute_specifier env) v3 in
  let ft_specs, ft_rets, ft_throw =
    Common.partition_either3 (map_anon_choice_type_qual_c8e0748 env) v4
  in
  let v5 =
    match v5 with
    | Some x -> Some (map_trailing_return_type env x)
    | None -> None
  in
  let v6 =
    match v6 with
    | Some x -> Some (map_requires_clause env x)
    | None -> None
  in
  {
    v1 with
    dt =
      (fun ty ->
        let ty =
          match v5 with
          | None -> ty
          | Some ty' -> ty'
        in
        v1.dt
          ( nQ,
            TFunction
              {
                ft_ret =
                  (match ft_rets with
                  | [] -> ty
                  | f :: _ -> f ty);
                ft_params = v2;
                ft_specs;
                ft_const = None;
                ft_throw;
                ft_requires = v6;
              } ));
  }

and map_function_definition (env : env)
    ((v1, v2, v3, v4) : CST.function_definition) : func_definition =
  let v1 =
    match v1 with
    | Some x ->
        let s, t = map_ms_call_modifier env x in
        [ M (MsCall (s, t)) ]
    | None -> []
  in
  let t, specs = map_declaration_specifiers env v2 in
  let { dn; dt } = map_declarator env v3 in
  let v4 = map_anon_choice_comp_stmt_e6a11e2 env v4 in
  let n = name_of_dname_for_function dn in
  let t = dt t in
  let ent, def = HPfff.fixFunc ((n, t, []), v4) in
  ({ ent with specs = ent.specs @ specs @ v1 }, def)

and map_function_field_declarator (env : env)
    ((v1, v2, v3, v4, v5) : CST.function_field_declarator) =
  let v1 = map_field_declarator env v1 in
  let v2 = map_parameter_list env v2 in
  let ft_specs, ft_rets, ft_throw =
    Common.partition_either3 (map_anon_choice_type_qual_c8e0748 env) v3
  in
  let v4 =
    match v4 with
    | Some x -> Some (map_trailing_return_type env x)
    | None -> None
  in
  let v5 =
    match v5 with
    | Some x -> Some (map_requires_clause env x)
    | None -> None
  in
  {
    v1 with
    dt =
      (fun ty ->
        let ty =
          match v4 with
          | None -> ty
          | Some ty' -> ty'
        in
        v1.dt
          ( nQ,
            TFunction
              {
                ft_ret =
                  (match ft_rets with
                  | [] -> ty
                  | f :: _ -> f ty);
                ft_params = v2;
                ft_specs;
                ft_const = None;
                ft_throw;
                ft_requires = v5;
              } ));
  }

and map_init_declarator (env : env) (x : CST.init_declarator) =
  match x with
  | `Decl_EQ_choice_init_list (v1, v2, v3) ->
      let v1 = map_declarator env v1 in
      let v2 = token env v2 (* "=" *) in
      let v3 =
        match v3 with
        | `Init_list x ->
            let x = map_initializer_list env x in
            InitList x
        | `Exp x ->
            let x = map_expression env x in
            InitExpr x
      in
      (v1, EqInit (v2, v3))
  | `Decl_choice_arg_list (v1, v2) ->
      let v1 = map_declarator env v1 in
      let v2 = map_anon_choice_arg_list_e4b6f8f env v2 in
      (v1, ObjInit v2)

and map_init_statement (env : env) (x : CST.init_statement) =
  match x with
  | `Alias_decl x ->
      let c = map_alias_declaration env x in
      InitUsing c
  | `Type_defi x -> InitVarsDecl (map_type_definition env x)
  | `Decl x -> InitVarsDecl (map_declaration env x)
  | `Exp_stmt x ->
      let eopt, sc = map_expression_statement env x in
      InitExprStmt (eopt, sc)

and map_initializer_list (env : env) ((v1, v2, v3, v4) : CST.initializer_list) :
    initialiser list brace =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_init_pair_1a6981e env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = map_anon_choice_init_pair_1a6981e env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let _v3 = trailing_comma env v3 in
  let v4 = token env v4 (* "}" *) in
  (v1, v2, v4)

and map_lambda_expression (env : env) ((v1, v2, v3, v4) : CST.lambda_expression)
    =
  let l, xs, r = map_lambda_capture_specifier env v1 in
  let _v2, ft_requires =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_template_parameter_list env v1 in
        let v2 =
          match v2 with
          | Some x -> Some (map_requires_clause env x)
          | None -> None
        in
        (v1, v2)
    | None -> (fb l [], None)
  in
  let v3 =
    match v3 with
    | Some x -> Some (map_abstract_function_declarator env x)
    | None -> None
  in
  let v4 = map_compound_statement env v4 in
  let ft_ret = (nQ, TAuto l) in
  let f_type =
    match v3 with
    | Some ft -> (
        let t = ft ft_ret in
        match t with
        | _, TFunction ft -> ft
        | _ -> error r "expecting a function type for a lambda")
    | None ->
        {
          ft_ret;
          ft_params = (l, [], r);
          ft_specs = [];
          ft_const = None;
          ft_throw = [];
          ft_requires;
        }
  in
  let fdef = { f_type; f_body = FBDef v4; f_specs = [] } in
  Lambda ((l, xs, r), fdef)

and map_lambda_capture_specifier (env : env)
    ((v1, v2, v3) : CST.lambda_capture_specifier) : lambda_capture list bracket
    =
  let v1 = token env v1 (* "[" *) in
  let v2 =
    match v2 with
    | `Lambda_defa_capt x ->
        let x = map_lambda_default_capture env x in
        [ x ]
    | `Opt_exp_rep_COMMA_exp opt -> (
        match opt with
        | Some (v1, v2) ->
            let v1 = map_expression env v1 in
            let v2 =
              Common.map
                (fun (v1, v2) ->
                  let _v1 = token env v1 (* "," *) in
                  let v2 = map_expression env v2 in
                  v2)
                v2
            in
            v1 :: v2 |> Common.map (fun e -> CaptureOther e)
        | None -> [])
    | `Lambda_defa_capt_COMMA_exp_rep_COMMA_exp (v1, v2, v3, v4) ->
        let v1 = map_lambda_default_capture env v1 in
        let _v2 = token env v2 (* "," *) in
        let v3 = map_expression env v3 in
        let v4 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = map_expression env v2 in
              v2)
            v4
        in
        v1 :: (v3 :: v4 |> Common.map (fun e -> CaptureOther e))
  in
  let v3 = token env v3 (* "]" *) in
  (v1, v2, v3)

and map_linkage_specification (env : env)
    ((v1, v2, v3) : CST.linkage_specification) =
  let v1 = token env v1 (* "extern" *) in
  let v2 = map_string_literal env v2 in
  let v3 =
    match v3 with
    | `Func_defi x ->
        let x = map_function_definition env x in
        ExternDecl (v1, v2, Func x)
    | `Decl x ->
        let x = map_declaration env x in
        ExternDecl (v1, v2, DeclList x)
    | `Decl_list x ->
        let x = map_declaration_list env x in
        ExternList (v1, v2, x)
  in
  v3

and map_ms_based_modifier (env : env) ((v1, v2) : CST.ms_based_modifier) =
  let v1 = token env v1 (* "__based" *) in
  let v2 = map_argument_list env v2 in
  Based (v1, v2)

and map_new_declarator (env : env) (x : CST.new_declarator) :
    abstract_declarator =
  match x with
  | `Rectype (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "[" *) in
      let v2 = map_expression env v2 in
      let v3 = token env v3 (* "]" *) in
      let v4 =
        match v4 with
        | Some x -> map_new_declarator env x
        | None -> id
      in
      fun t -> (nQ, TArray ((v1, Some v2, v3), v4 t))

and map_noexcept (env : env) ((v1, v2) : CST.noexcept) : exn_spec =
  let v1 = token env v1 (* "noexcept" *) in
  let v2 =
    match v2 with
    | Some (v1, v2, v3) ->
        let v1 = token env v1 (* "(" *) in
        let v2 =
          match v2 with
          | Some x -> Some (map_expression env x)
          | None -> None
        in
        let v3 = token env v3 (* ")" *) in
        Some (v1, v2, v3)
    | None -> None
  in
  Noexcept (v1, v2)

and map_non_case_statement (env : env) (x : CST.non_case_statement) : stmt =
  match x with
  | `Choice_attr_stmt x -> (
      match x with
      | `Attr_stmt x -> map_attributed_statement env x
      | `Labe_stmt (v1, v2, v3) ->
          let v1 = str env v1 (* pattern [a-zA-Z_]\w* *) in
          let v2 = token env v2 (* ":" *) in
          let v3 = map_statement env v3 in
          Label (v1, v2, v3)
      | `Comp_stmt x ->
          let l, xs, r = map_compound_statement env x in
          Compound (l, xs, r)
      | `Exp_stmt x ->
          let eopt, sc = map_expression_statement env x in
          ExprStmt (eopt, sc)
      | `If_stmt (v1, v2, v3, v4, v5) ->
          let v1 = token env v1 (* "if" *) in
          let v2 =
            match v2 with
            | Some tok -> Some (token env tok) (* "constexpr" *)
            | None -> None
          in
          let v3 = map_condition_clause env v3 in
          let v4 = map_statement env v4 in
          let v5 =
            match v5 with
            | Some (v1, v2) ->
                let v1 = token env v1 (* "else" *) in
                let v2 = map_statement env v2 in
                Some (v1, v2)
            | None -> None
          in
          If (v1, v2, v3, v4, v5)
      | `Switch_stmt (v1, v2, v3) ->
          let v1 = token env v1 (* "switch" *) in
          let v2 = map_condition_clause env v2 in
          let l, xs, r = map_compound_statement env v3 in
          Switch (v1, v2, Compound (l, xs, r))
      | `Do_stmt (v1, v2, v3, v4, v5) ->
          let v1 = token env v1 (* "do" *) in
          let v2 = map_statement env v2 in
          let v3 = token env v3 (* "while" *) in
          let v4 = map_parenthesized_expression env v4 in
          let v5 = token env v5 (* ";" *) in
          DoWhile (v1, v2, v3, v4, v5)
      | `While_stmt (v1, v2, v3) ->
          let v1 = token env v1 (* "while" *) in
          let v2 = map_condition_clause env v2 in
          let v3 = map_statement env v3 in
          While (v1, v2, v3)
      | `For_stmt (v1, v2, v3, v4, v5, v6, v7, v8) ->
          let v1 = token env v1 (* "for" *) in
          let v2 = token env v2 (* "(" *) in
          let v3 =
            match v3 with
            | `Decl x ->
                let x = map_declaration env x in
                Right x
            | `Opt_choice_exp_SEMI x ->
                let eopt, sc = map_expression_statement env x in
                Left (eopt, sc)
          in
          let v4 =
            match v4 with
            | Some x -> Some (map_anon_choice_exp_55b4dba env x)
            | None -> None
          in
          let _v5 = token env v5 (* ";" *) in
          let v6 =
            match v6 with
            | Some x -> Some (map_anon_choice_exp_55b4dba env x)
            | None -> None
          in
          let v7 = token env v7 (* ")" *) in
          let v8 = map_statement env v8 in
          For (v1, (v2, ForClassic (v3, v4, v6), v7), v8)
      | `Ret_stmt x -> map_return_statement env x
      | `Brk_stmt (v1, v2) ->
          let v1 = token env v1 (* "break" *) in
          let v2 = token env v2 (* ";" *) in
          Jump (Break v1, v2)
      | `Cont_stmt (v1, v2) ->
          let v1 = token env v1 (* "continue" *) in
          let v2 = token env v2 (* ";" *) in
          Jump (Continue v1, v2)
      | `Goto_stmt (v1, v2, v3) ->
          let v1 = token env v1 (* "goto" *) in
          let v2 = str env v2 (* pattern [a-zA-Z_]\w* *) in
          let v3 = token env v3 (* ";" *) in
          Jump (Goto (v1, v2), v3))
  | `Co_ret_stmt (v1, v2, v3) ->
      let v1 = (* "co_return" *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> Some (map_expression env x)
        | None -> None
      in
      let _v3 = (* ";" *) token env v3 in
      CoStmt ((Co_return, v1), v2)
  | `Co_yield_stmt (v1, v2, v3) ->
      let v1 = (* "co_yield" *) token env v1 in
      let v2 = map_expression env v2 in
      let _v3 = (* ";" *) token env v3 in
      CoStmt ((Co_yield, v1), Some v2)
  | `For_range_loop (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 = token env v1 (* "for" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 =
        match v3 with
        | Some x -> Some (map_init_statement env x)
        | None -> None
      in
      let t, specs = map_declaration_specifiers env v4 in
      let v5 = map_declarator env v5 in
      let v6 = token env v6 (* ":" *) in
      let v7 = map_anon_choice_exp_3078596 env v7 in
      let v8 = token env v8 (* ")" *) in
      let v9 = map_statement env v9 in
      let n = name_of_dname_for_var env v5.dn in
      let ent = { name = n; specs } in
      let var = { v_type = v5.dt t; v_init = None } in
      let for_header = ForRange (v3, (ent, var), v6, v7) in
      For (v1, (v2, for_header, v8), v9)
  | `Try_stmt x -> map_try_statement env x
  | `Throw_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "throw" *) in
      let v2 =
        match v2 with
        | Some x -> Some (map_expression env x)
        | None -> None
      in
      let v3 = token env v3 (* ";" *) in
      ExprStmt (Some (Throw (v1, v2)), v3)

and map_operator_cast (env : env) ((v1, v2, v3) : CST.operator_cast) : name =
  let v1 = token env v1 (* "operator" *) in
  let t, _specs = map_declaration_specifiers env v2 in
  let v3 = map_abstract_declarator env v3 in
  let t = v3 t in
  let id_or_op = IdConverter (v1, t) in
  (None, [], id_or_op)

and map_operator_cast_declaration (env : env)
    ((v1, v2, v3, v4) : CST.operator_cast_declaration) : vars_decl =
  let v1 = List.map (map_constructor_specifiers env) v1 in
  let name = map_anon_choice_op_cast_b108b62 env v2 in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "=" *) in
        let v2 = map_expression env v2 in
        Some (EqInit (v1, InitExpr v2))
    | None -> None
  in
  let t = tvoid (ii_of_name name) in
  let one = V ({ name; specs = v1 }, { v_init = v3; v_type = t }) in
  let v4 = token env v4 (* ";" *) in
  ([ one ], v4)

and map_operator_cast_definition (env : env)
    ((v1, v2, v3) : CST.operator_cast_definition) =
  let v1 = List.map (map_constructor_specifiers env) v1 in
  let n = map_anon_choice_op_cast_b108b62 env v2 in
  let v3 = map_compound_statement env v3 in

  let t = tvoid (ii_of_name n) in
  let ent, def = HPfff.fixFunc ((n, t, []), FBDef v3) in
  ({ ent with specs = v1 @ ent.specs }, def)

and map_optional_parameter_declaration (env : env)
    ((v1, v2, v3, v4) : CST.optional_parameter_declaration) : parameter_classic
    =
  let t, p_specs = map_declaration_specifiers env v1 in
  let v3 = token env v3 (* "=" *) in
  let v4 = map_expression env v4 in
  let v2 =
    match v2 with
    | Some x ->
        let { dn; dt } = map_declarator env x in
        let id = id_of_dname_for_parameter env dn in
        make_param (dt t) ~p_name:(Some id) ~p_specs ~p_val:(Some (v3, v4))
    | None -> make_param t ~p_specs ~p_val:(Some (v3, v4))
  in
  v2

and map_optional_type_parameter_declaration (env : env)
    ((v1, v2, v3, v4) : CST.optional_type_parameter_declaration) =
  let v1 = map_anon_choice_type_a2fe5d4 env v1 in
  let v2 =
    match v2 with
    | Some tok -> Some (str env tok) (* pattern [a-zA-Z_]\w* *)
    | None -> None
  in
  let _v3 = token env v3 (* "=" *) in
  let v4 = map_type_specifier env v4 in
  TPClass (v1, v2, Some v4)

and map_parameter_declaration (env : env) ((v1, v2) : CST.parameter_declaration)
    : parameter_classic =
  let t, p_specs = map_declaration_specifiers env v1 in
  let v3 =
    match v2 with
    | Some x -> (
        match x with
        | `Decl x ->
            let { dn; dt } = map_declarator env x in
            let id = id_of_dname_for_parameter env dn in
            make_param (dt t) ~p_name:(Some id) ~p_specs
        | `Abst_decl x ->
            let dt = map_abstract_declarator env x in
            make_param (dt t) ~p_specs)
    | None -> make_param t ~p_specs
  in
  v3

and map_parameter_list (env : env) ((v1, v2, v3) : CST.parameter_list) :
    parameter list paren =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_param_decl_d9083af env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = map_anon_choice_param_decl_d9083af env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 = token env v3 (* ")" *) in
  (v1, v2, v3)

and map_parenthesized_declarator (env : env)
    ((v1, v2, v3) : CST.parenthesized_declarator) : declarator =
  let v1 = token env v1 (* "(" *) in
  let v2 = map_declarator env v2 in
  let v3 = token env v3 (* ")" *) in
  { v2 with dt = (fun x -> (nQ, ParenType (v1, v2.dt x, v3))) }

and map_parenthesized_expression (env : env)
    ((v1, v2, v3) : CST.parenthesized_expression) : expr paren =
  let v1 = token env v1 (* "(" *) in
  let v2 = map_anon_choice_exp_55b4dba env v2 in
  let v3 = token env v3 (* ")" *) in
  (v1, v2, v3)

and map_parenthesized_field_declarator (env : env)
    ((v1, v2, v3) : CST.parenthesized_field_declarator) =
  let v1 = token env v1 (* "(" *) in
  let v2 = map_field_declarator env v2 in
  let v3 = token env v3 (* ")" *) in
  { v2 with dt = (fun x -> (nQ, ParenType (v1, v2.dt x, v3))) }

and map_pointer_declarator (env : env)
    ((v1, v2, v3, v4, v5) : CST.pointer_declarator) : declarator =
  let v1 =
    match v1 with
    | Some x -> [ map_ms_based_modifier env x ]
    | None -> []
  in
  let v2 = token env v2 (* "*" *) in
  let v3 = Common.map (map_ms_pointer_modifier env) v3 in
  let v4 = Common.map (map_type_qualifier env) v4 in
  let f1 x = (v4, TPointer (v2, x, v1 @ v3)) in
  let v5 = map_declarator env v5 in
  { v5 with dt = (fun x -> x |> f1 |> v5.dt) }

and map_pointer_expression (env : env) ((v1, v2) : CST.pointer_expression) :
    expr =
  let v1 =
    match v1 with
    | `STAR tok -> (DeRef, token env tok) (* "*" *)
    | `AMP tok -> (GetRef, token env tok)
    (* "&" *)
  in
  let v2 = map_expression env v2 in
  Unary (v1, v2)

and map_pointer_field_declarator (env : env)
    ((v1, v2, v3, v4, v5) : CST.pointer_field_declarator) : declarator =
  let v1 =
    match v1 with
    | Some x -> [ map_ms_based_modifier env x ]
    | None -> []
  in
  let v2 = token env v2 (* "*" *) in
  let v3 = Common.map (map_ms_pointer_modifier env) v3 in
  let v4 = Common.map (map_type_qualifier env) v4 in
  let f1 x = (v4, TPointer (v2, x, v1 @ v3)) in
  let v5 = map_field_declarator env v5 in
  { v5 with dt = (fun x -> x |> f1 |> v5.dt) }

and map_preproc_if (env : env) ((v1, v2, v3, v4, v5, v6) : CST.preproc_if) :
    toplevel list =
  (* TODO: IfIf *)
  let v1 = token env v1 (* pattern #[ 	]*if *) in
  let _v2TODO = map_preproc_expression env v2 in
  let _v3 = token env v3 (* "\n" *) in
  let dir1 = Ifdef v1 in
  let v4 = map_translation_unit env v4 in
  let v5 =
    match v5 with
    | Some x -> map_anon_choice_prep_else_8b52b0f env x
    | None -> []
  in
  let v6 = token env v6 (* pattern #[ 	]*endif *) in
  let dir2 = IfdefEndif v6 in
  (CppIfdef dir1 :: v4) @ v5 @ [ CppIfdef dir2 ]

and map_preproc_if_in_field_declaration_list (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.preproc_if_in_field_declaration_list) :
    class_member sequencable list =
  (* TODO: IfIf *)
  let v1 = token env v1 (* pattern #[ 	]*if *) in
  let _v2TODO = map_preproc_expression env v2 in
  let _v3 = token env v3 (* "\n" *) in
  let dir1 = Ifdef v1 in
  let v4 = List.concat_map (map_field_declaration_list_item env) v4 in
  let v5 =
    match v5 with
    | Some x -> map_anon_choice_prep_else_in_field_decl_list_97ea65e env x
    | None -> []
  in
  let v6 = token env v6 (* pattern #[ 	]*endif *) in
  let dir2 = IfdefEndif v6 in
  (CppIfdef dir1 :: v4) @ v5 @ [ CppIfdef dir2 ]

and map_preproc_ifdef (env : env) ((v1, v2, v3, v4, v5) : CST.preproc_ifdef) :
    toplevel list =
  let v1 = map_anon_choice_pat_25b90ba_4a37f8c env v1 in
  let dir1 = v1 in
  let _v2 = token env v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 = map_translation_unit env v3 in
  let v4 =
    match v4 with
    | Some x -> map_anon_choice_prep_else_8b52b0f env x
    | None -> []
  in
  let v5 = token env v5 (* pattern #[ 	]*endif *) in
  let dir2 = IfdefEndif v5 in
  (CppIfdef dir1 :: v3) @ v4 @ [ CppIfdef dir2 ]

and map_preproc_ifdef_in_field_declaration_list (env : env)
    ((v1, v2, v3, v4, v5) : CST.preproc_ifdef_in_field_declaration_list) :
    class_member sequencable list =
  let v1 = map_anon_choice_pat_25b90ba_4a37f8c env v1 in
  let dir1 = v1 in
  let _v2 = token env v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 = List.concat_map (map_field_declaration_list_item env) v3 in
  let v4 =
    match v4 with
    | Some x -> map_anon_choice_prep_else_in_field_decl_list_97ea65e env x
    | None -> []
  in
  let v5 = token env v5 (* pattern #[ 	]*endif *) in
  let dir2 = IfdefEndif v5 in
  (CppIfdef dir1 :: v3) @ v4 @ [ CppIfdef dir2 ]

and map_return_statement (env : env) (x : CST.return_statement) : stmt =
  match x with
  | `Ret_opt_choice_exp_SEMI (v1, v2, v3) ->
      let v1 = token env v1 (* "return" *) in
      let v2 =
        match v2 with
        | Some x -> Some (Arg (map_anon_choice_exp_55b4dba env x))
        | None -> None
      in
      let v3 = token env v3 (* ";" *) in
      Jump (Return (v1, v2), v3)
  | `Ret_init_list_SEMI (v1, v2, v3) ->
      let v1 = token env v1 (* "return" *) in
      let v2 = map_initializer_list env v2 in
      let v3 = token env v3 (* ";" *) in
      Jump (Return (v1, Some (ArgInits v2)), v3)

(* This entire scope resolution parsing logic is pretty darn annoying.
   The basic gist is that a name consists of a possible starting dcolon,
   some stuff in the middle, and a base at the far right.
   The actual scope resolution type does not contain the RHS that it is
   applied to, however, so we need to pass in the name that it modifies
   directly.
*)
and map_scope_resolution (env : env) ((v1, v2) : CST.scope_resolution) name =
  let v2 = (* "::" *) token env v2 in
  match v1 with
  | Some x -> (
      match x with
      | `Id tok ->
          (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *)
          name_add_class_qual (str env tok) name
      | `Temp_type (v1, v2) -> map_template_type_with_name env (v1, v2) name
      | `Depe_type_id x -> map_dependent_type_identifier_with_name env x name)
  | None -> name_add_tcolon (Some v2) name

and map_qualified_field_identifier (env : env)
    ((v1, v2) : CST.qualified_field_identifier) =
  let v1 = map_scope_resolution env v1 in
  let v2 =
    match v2 with
    | `Depe_field_id x -> map_dependent_field_identifier env x
    | `Qual_field_id x -> map_qualified_field_identifier env x
    | `Temp_meth x -> map_template_method env x
    | `Id tok ->
        (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *)
        name_of_id (str env tok)
  in
  v1 v2

(* This is the main workhorse dealing with the recursive nature of qualified
   identifiers.
*)
and map_qualified_identifier (env : env) ((v1, v2) : CST.qualified_identifier) :
    name =
  let v1 = map_scope_resolution env v1 in
  let v2 =
    match v2 with
    | `Depe_id x -> map_dependent_identifier env x
    | `Qual_id x -> map_qualified_identifier env x
    | `Temp_func x -> map_template_function env x
    | `Id tok ->
        (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *)
        name_of_id (str env tok)
    | `Op_name x ->
        let tk, opwrap = map_operator_name env x in
        (None, [], IdOperator (tk, opwrap))
    | `Dest_name x ->
        let x = map_destructor_name env x in
        (None, [], x)
  in
  v1 v2

and map_qualified_operator_cast_identifier (env : env)
    ((v1, v2) : CST.qualified_operator_cast_identifier) : name =
  let v1 = map_scope_resolution env v1 in
  let v2 =
    match v2 with
    | `Qual_op_cast_id x -> map_qualified_operator_cast_identifier env x
    | `Op_cast x -> map_operator_cast env x
  in
  v1 v2

and map_qualified_type_identifier (env : env)
    ((v1, v2) : CST.qualified_type_identifier) : name =
  let v1 = map_scope_resolution env v1 in
  let v2 =
    match v2 with
    | `Depe_type_id x -> map_dependent_type_identifier env x
    | `Qual_type_id x -> map_qualified_type_identifier env x
    | `Temp_type x -> map_template_type env x
    | `Id tok ->
        (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *)
        name_of_id (str env tok)
  in
  v1 v2

and map_sizeof_expression (env : env) (x : CST.sizeof_expression) : expr =
  match x with
  | `Sizeof_choice_exp (v1, v2) ->
      let v1top = token env v1 (* "sizeof" *) in
      let v2 =
        match v2 with
        | `Exp x ->
            let x = map_expression env x in
            SizeOf (v1top, Left x)
        | `LPAR_type_desc_RPAR (v1, v2, v3) ->
            let v1 = token env v1 (* "(" *) in
            let v2 = map_type_descriptor env v2 in
            let v3 = token env v3 (* ")" *) in
            SizeOf (v1top, Right (v1, v2, v3))
      in
      v2
  | `Sizeof_DOTDOTDOT_LPAR_id_RPAR (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "sizeof" *) in
      let _v2 = token env v2 (* "..." *) in
      let _v3 = token env v3 (* "(" *) in
      let _v4 = str env v4 (* pattern [a-zA-Z_]\w* *) in
      let _v5 = token env v5 (* ")" *) in
      ExprTodo (("SizeofDots", v1), [])

and map_statement (env : env) (x : CST.statement) : stmt =
  match x with
  | `Case_stmt (v1, v2, v3) -> (
      let tcolon = (* ":" *) token env v2 in
      let body =
        List.map
          (fun x ->
            match x with
            | `Choice_choice_attr_stmt x -> S (map_non_case_statement env x)
            | `Decl x -> D (DeclList (map_declaration env x))
            | `Type_defi x -> D (DeclList (map_type_definition env x)))
          v3
      in
      match v1 with
      | `Case_exp (v1, v2) ->
          let v1 = (* "case" *) token env v1 in
          let v2 = map_expression env v2 in
          Case (v1, v2, tcolon, body)
      | `Defa tok ->
          let v1 = (* "default" *) token env tok in
          Default (v1, tcolon, body))
  | `Choice_choice_attr_stmt x -> map_non_case_statement env x

and map_static_assert_declaration (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.static_assert_declaration) =
  let v1 = token env v1 (* "static_assert" *) in
  let v2 = token env v2 (* "(" *) in
  let v3 = map_expression env v3 in
  let v4 =
    match v4 with
    | Some (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 =
          match v2 with
          | `Str_lit x ->
              let x = map_string_literal env x in
              String x
          | `Raw_str_lit tok ->
              let x = str env tok (* raw_string_literal *) in
              String x
          | `Conc_str x ->
              let x = map_concatenated_string env x in
              x
        in
        [ Arg (C v2) ]
    | None -> []
  in
  let v5 = token env v5 (* ")" *) in
  let _v6 = token env v6 (* ";" *) in
  let args = (v2, Arg v3 :: v4, v5) in
  StaticAssert (v1, args)

and map_subscript_designator (env : env)
    ((v1, v2, v3) : CST.subscript_designator) : designator =
  let v1 = token env v1 (* "[" *) in
  let v2 = map_expression env v2 in
  let v3 = token env v3 (* "]" *) in
  DesignatorIndex (v1, v2, v3)

and map_subscript_expression (env : env)
    ((v1, v2, v3, v4) : CST.subscript_expression) : expr =
  let v1 = map_expression env v1 in
  let v2 = token env v2 (* "[" *) in
  let v3 = map_anon_choice_exp_3078596 env v3 in
  let v4 = token env v4 (* "]" *) in
  ArrayAccess (v1, (v2, v3, v4))

and map_template_argument_list (env : env)
    ((v1, v2, v3) : CST.template_argument_list) : template_arguments =
  let v1 = token env v1 (* "<" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_type_desc_4d9cafa env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = map_anon_choice_type_desc_4d9cafa env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 = token env v3 (* tok_GT *) in
  (v1, v2, v3)

and map_template_declaration (env : env)
    ((v1, v2, v3, v4) : CST.template_declaration) : decl =
  let v1 = token env v1 (* "template" *) in
  let v2 = map_template_parameter_list env v2 in
  let v3 =
    match v3 with
    | Some x -> Some (map_requires_clause env x)
    | None -> None
  in
  let v4 =
    match v4 with
    | `Empty_decl x ->
        let x = map_empty_declaration env x in
        x
    | `Alias_decl x ->
        let x = map_alias_declaration env x in
        UsingDecl x
    | `Decl x ->
        let x = map_declaration env x in
        DeclList x
    | `Temp_decl x ->
        let x = map_template_declaration env x in
        x
    | `Func_defi x ->
        let x = map_function_definition env x in
        Func x
    | `Conc_defi x -> map_concept_definition env x
    | `Cons_or_dest_decl x ->
        let x = map_constructor_or_destructor_declaration env x in
        Func x
    | `Cons_or_dest_defi x ->
        let x = map_constructor_or_destructor_definition env x in
        Func x
    | `Op_cast_decl x ->
        let x = map_operator_cast_declaration env x in
        DeclList x
    | `Op_cast_defi x ->
        let x = map_operator_cast_definition env x in
        Func x
  in
  TemplateDecl (v1, v2, v3, v4)

and map_template_function (env : env) ((v1, v2) : CST.template_function) : name
    =
  let v1 =
    (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *)
    str env v1
  in
  let v2 = map_template_argument_list env v2 in
  name_add_template_args (name_of_id v1) v2

and map_template_method (env : env) ((v1, v2) : CST.template_method) : name =
  let v1 =
    (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *)
    str env v1
  in
  let v2 = map_template_argument_list env v2 in
  name_add_template_args (name_of_id v1) v2

and map_template_parameter_list (env : env)
    ((v1, v2, v3) : CST.template_parameter_list) : template_parameters =
  let v1 = token env v1 (* "<" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_param_decl_13b5913 env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = map_anon_choice_param_decl_13b5913 env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 = token env v3 (* tok_GT *) in
  (v1, v2, v3)

and map_template_type (env : env) ((v1, v2) : CST.template_type) : name =
  let v1 =
    (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *)
    name_of_id (str env v1)
  in
  let v2 = map_template_argument_list env v2 in
  name_add_template_args v1 v2

and map_template_type_with_name (env : env) ((v1, v2) : CST.template_type) name
    : name =
  let v1 =
    (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *)
    str env v1
  in
  let v2 = map_template_argument_list env v2 in
  name_add_template_qual (v1, v2) name

and map_throw_specifier (env : env) ((v1, v2, v3, v4) : CST.throw_specifier) :
    exn_spec =
  let v1 = token env v1 (* "throw" *) in
  let v2 = token env v2 (* "(" *) in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        let v1 = map_type_descriptor env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = map_type_descriptor env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v4 = token env v4 (* ")" *) in
  ThrowSpec (v1, (v2, v3, v4))

and map_top_level_item (env : env) (x : CST.top_level_item) : toplevel list =
  match x with
  | `Choice_func_defi x -> (
      match x with
      | `Func_defi x ->
          let x = map_function_definition env x in
          [ X (D (Func x)) ]
      | `Link_spec x ->
          let x = map_linkage_specification env x in
          [ X (D x) ]
      | `Decl x ->
          let x = map_declaration env x in
          [ X (D (DeclList x)) ]
      | `Choice_case_stmt x ->
          let x = map_statement env x in
          [ X (S x) ]
      | `Attr_stmt x -> [ X (S (map_attributed_statement env x)) ]
      | `Type_defi x ->
          let x = map_type_definition env x in
          [ X (D (DeclList x)) ]
      | `Empty_decl x ->
          let x = map_empty_declaration env x in
          [ X (D x) ]
      | `Prep_if x ->
          let x = map_preproc_if env x in
          x
      | `Prep_ifdef x ->
          let x = map_preproc_ifdef env x in
          x
      | `Prep_incl x ->
          let x = map_preproc_include env x in
          [ CppDirective x ]
      | `Prep_def x ->
          let x = map_preproc_def env x in
          [ CppDirective x ]
      | `Prep_func_def x ->
          let x = map_preproc_function_def env x in
          [ CppDirective x ]
      | `Prep_call x ->
          let x = map_preproc_call env x in
          [ CppDirective x ])
  | `Name_defi (v1, v2, v3) ->
      let v1 = token env v1 (* "namespace" *) in
      let v2 =
        match v2 with
        | Some x -> map_anon_choice_name_id_ba1b968 env x
        | None -> []
      in
      let v3 = map_declaration_list env v3 in
      [ X (D (Namespace (v1, v2, v3))) ]
  | `Conc_defi x -> [ X (D (map_concept_definition env x)) ]
  | `Name_alias_defi (v1, v2, v3, v4, v5) ->
      let v1 = (* "namespace" *) token env v1 in
      let v2 =
        (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *)
        str env v2
      in
      let v3 = (* "=" *) token env v3 in
      let v4 = map_anon_choice_name_id_1d0ba77 env v4 in
      let v5 = (* ";" *) token env v5 in
      [ X (D (NamespaceAlias (v1, v2, v3, v4, v5))) ]
  | `Using_decl x ->
      let x = map_using_declaration env x in
      [ X (D (UsingDecl x)) ]
  | `Alias_decl x ->
      let x = map_alias_declaration env x in
      [ X (D (UsingDecl x)) ]
  | `Static_assert_decl x ->
      let x = map_static_assert_declaration env x in
      [ X (D x) ]
  | `Temp_decl x ->
      let x = map_template_declaration env x in
      [ X (D x) ]
  | `Temp_inst (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "template" *) in
      let t, specs =
        match v2 with
        | Some x ->
            let t, specs = map_declaration_specifiers env x in
            (t, specs)
        | None -> (tvoid v1, [])
      in
      let { dn; dt } = map_declarator env v3 in
      let t = dt t in
      let n = name_of_dname_for_var env dn in
      let ent = { name = n; specs } in
      let v4 = token env v4 (* ";" *) in
      [
        X
          (D
             (TemplateInstanciation
                (v1, (ent, { v_type = t; v_init = None }), v4)));
      ]
  | `Cons_or_dest_defi x ->
      let x = map_constructor_or_destructor_definition env x in
      [ X (D (Func x)) ]
  | `Op_cast_defi x ->
      let x = map_operator_cast_definition env x in
      [ X (D (Func x)) ]
  | `Op_cast_decl x ->
      let x = map_operator_cast_declaration env x in
      [ X (D (DeclList x)) ]

and map_trailing_return_type (env : env) ((v1, v2) : CST.trailing_return_type) =
  let _v1 = (* "->" *) token env v1 in
  let v2 = map_type_descriptor env v2 in
  v2

and map_translation_unit (env : env) (xs : CST.translation_unit) : program =
  List.concat_map (map_top_level_item env) xs

(* actually dn is an ident here, never a complex name *)
and map_type_declarator (env : env) (x : CST.type_declarator) : declarator =
  match x with
  | `Attr_type_decl (v1, v2) ->
      let v1 = map_type_declarator env v1 in
      let _v2_TODO = Common.map (map_attribute_declaration env) v2 in
      v1
  | `Poin_type_decl (v1, v2, v3, v4, v5) ->
      let v1 =
        match v1 with
        | Some x -> [ map_ms_based_modifier env x ]
        | None -> []
      in
      let v2 = token env v2 (* "*" *) in
      let v3 = Common.map (map_ms_pointer_modifier env) v3 in
      let v4 = Common.map (map_type_qualifier env) v4 in
      let f1 x = (v4, TPointer (v2, x, v1 @ v3)) in
      let v5 = map_type_declarator env v5 in
      { v5 with dt = (fun x -> x |> f1 |> v5.dt) }
  | `Func_type_decl (v1, v2) ->
      let v1 = map_type_declarator env v1 in
      let v2 = map_parameter_list env v2 in
      {
        v1 with
        dt =
          (fun x ->
            v1.dt
              ( nQ,
                TFunction
                  {
                    ft_ret = x;
                    ft_params = v2;
                    ft_specs = [];
                    ft_const = None;
                    ft_throw = [];
                    ft_requires = None;
                  } ));
      }
  | `Array_type_decl (v1, v2, v3, v4, v5) ->
      let v1 = map_type_declarator env v1 in
      let v2 = token env v2 (* "[" *) in
      let v3 = Common.map (map_type_qualifier env) v3 in
      let v4 =
        match v4 with
        | Some x -> Some (map_anon_choice_exp_508611b env x)
        | None -> None
      in
      let v5 = token env v5 (* "]" *) in
      { v1 with dt = (fun x -> v1.dt (v3, TArray ((v2, v4, v5), x))) }
  | `Paren_type_decl (v1, v2, v3) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_type_declarator env v2 in
      let v3 = token env v3 (* ")" *) in
      { v2 with dt = (fun x -> (nQ, ParenType (v1, v2.dt x, v3))) }
  | `Id tok ->
      let x = str env tok in
      { dn = DN (name_of_id x); dt = id }

(* pattern [a-zA-Z_]\w* *)
and map_type_definition (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.type_definition) : vars_decl =
  let v1 = token env v1 (* "typedef" *) in
  let _v2TODO = Common.map (map_type_qualifier env) v2 in
  let v3 = map_type_specifier env v3 in
  let v4 = map_type_declarator env v4 in
  let v5 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = map_type_declarator env v2 in
        v2)
      v5
  in
  let v6 = token env v6 (* ";" *) in

  let xs =
    v4 :: v5
    |> Common.map (fun { dn; dt } ->
           let id = HPfff.id_of_dname_for_typedef dn in
           TypedefDecl (v1, dt v3, id))
  in
  (xs, v6)

and map_type_descriptor (env : env) ((v1, v2, v3, v4) : CST.type_descriptor) :
    type_ =
  let v1 = Common.map (map_type_qualifier env) v1 in
  let qs, tc = map_type_specifier env v2 in
  let v3 = Common.map (map_type_qualifier env) v3 in
  let t = (v1 @ qs @ v3, tc) in
  let v4 =
    match v4 with
    | Some x -> map_abstract_declarator env x
    | None -> id
  in
  v4 t

and map_type_specifier (env : env) (x : CST.type_specifier) : type_ =
  match x with
  | `Struct_spec (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "struct" *) in
      let _v2 =
        match v2 with
        | Some x -> [ map_ms_declspec_modifier env x ]
        | None -> []
      in
      let _v3_TODO =
        match v3 with
        | Some x -> Some (map_attribute_declaration env x)
        | None -> None
      in
      let v4 = map_anon_choice_class_name_d6703e6 env v4 in
      v4 (Struct, v1)
  | `Union_spec (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "union" *) in
      let _v2 =
        match v2 with
        | Some x -> [ map_ms_declspec_modifier env x ]
        | None -> []
      in
      let _v3_TODO =
        match v3 with
        | Some x -> Some (map_attribute_declaration env x)
        | None -> None
      in
      let v4 = map_anon_choice_class_name_d6703e6 env v4 in
      v4 (Union, v1)
  | `Enum_spec (v1, v2, v3) ->
      let tenum = token env v1 (* "enum" *) in
      let _v2TODO =
        match v2 with
        | Some x -> (
            match x with
            | `Class tok -> Some (Class, token env tok) (* "class" *)
            | `Struct tok -> Some (Struct, token env tok) (* "struct" *))
        | None -> None
      in
      let v3 =
        match v3 with
        | `Class_name_opt_enum_base_clause_opt_enum_list (v1, v2, v3) ->
            let n = map_class_name env v1 in
            let _toptTODO =
              match v2 with
              | Some x -> Some (map_enum_base_clause env x)
              | None -> None
            in
            let v3 =
              match v3 with
              | Some x ->
                  let enum_body = map_enumerator_list env x in
                  EnumDef { enum_kind = tenum; enum_name = Some n; enum_body }
              | None -> EnumName (tenum, n)
            in
            v3
        | `Enum_list x ->
            let enum_body = map_enumerator_list env x in
            EnumDef { enum_kind = tenum; enum_name = None; enum_body }
      in
      (nQ, v3)
  | `Class_spec (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "class" *) in
      let _v2 =
        match v2 with
        | Some x -> [ map_ms_declspec_modifier env x ]
        | None -> []
      in
      let _v3_TODO =
        match v3 with
        | Some x -> Some (map_attribute_declaration env x)
        | None -> None
      in
      let v4 = map_anon_choice_class_name_d6703e6 env v4 in
      v4 (Class, v1)
  | `Sized_type_spec x ->
      let x = map_sized_type_specifier env x in
      x
  | `Prim_type tok ->
      let x = str env tok (* primitive_type *) in
      parse_primitive_type env x
  | `Temp_type x ->
      let x = map_template_type env x in
      (nQ, TypeName x)
  | `Plac_type_spec (v1, v2) ->
      let _v1_TODO =
        match v1 with
        | Some x -> Some (map_type_specifier env x)
        | None -> None
      in
      (* TODO: I do not understand why you can both have a real type and an
         `auto` after it.
      *)
      let v2 =
        match v2 with
        | `Auto tok -> TAuto (* "auto" *) (token env tok)
        | `Decl_auto x -> map_decltype_auto env x
      in
      (nQ, v2)
  | `Depe_type (v1, v2) ->
      let v1 = token env v1 (* "typename" *) in
      let v2 = map_type_specifier env v2 in
      (nQ, TypenameKwd (v1, v2))
  | `Decl (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "decltype" *) in
      let _v2 = token env v2 (* "(" *) in
      let _v3 = map_expression env v3 in
      let _v4 = token env v4 (* ")" *) in
      (nQ, TypeTodo (("decltype", v1), []))
  | `Choice_qual_type_id x ->
      let name =
        match x with
        | `Qual_type_id x -> map_qualified_type_identifier env x
        | `Id tok ->
            let x = str env tok (* pattern [a-zA-Z_]\w* *) in
            name_of_id x
      in
      (nQ, TypeName name)

and map_unary_expression (env : env) ((v1, v2) : CST.unary_expression) =
  let v1 = map_anon_choice_BANG_67174d6 env v1 in
  let v2 = map_expression env v2 in
  Unary (v1, v2)

and map_update_expression (env : env) (x : CST.update_expression) : expr =
  match x with
  | `Choice_DASHDASH_exp (v1, v2) ->
      let v1 = map_anon_choice_DASHDASH_d11def2 env v1 in
      let v2 = map_expression env v2 in
      Prefix (v1, v2)
  | `Exp_choice_DASHDASH (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = map_anon_choice_DASHDASH_d11def2 env v2 in
      Postfix (v1, v2)

and map_using_declaration (env : env) ((v1, v2, v3, v4) : CST.using_declaration)
    : using =
  let v1 = token env v1 (* "using" *) in
  let v3 = map_anon_choice_name_id_1d0ba77 env v3 in
  let v4 = token env v4 (* ";" *) in
  let v2 =
    match v2 with
    | Some tok ->
        let n = token env tok (* "namespace" *) in
        (v1, UsingNamespace (n, v3), v4)
    | None -> (v1, UsingName v3, v4)
  in
  v2

and map_variadic_parameter_declaration (env : env)
    ((v1, v2) : CST.variadic_parameter_declaration) =
  let t, p_specs = map_declaration_specifiers env v1 in
  let v2 =
    match v2 with
    | `Vari_decl x ->
        let tdots, p_name = map_variadic_declarator env x in
        let p = make_param t ~p_name ~p_specs in
        ParamVariadic (None, tdots, p)
    | `Vari_ref_decl x ->
        let ampersand, (tdots, p_name) =
          map_variadic_reference_declarator env x
        in
        let p = make_param t ~p_name ~p_specs in
        ParamVariadic (Some ampersand, tdots, p)
  in
  v2

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_cpp.Parse.file file)
    (fun cst ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      map_translation_unit env cst)

let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_cpp.Parse.string str)
    (fun cst ->
      let file = "<pattern>" in
      let env = { H.file; conv = Hashtbl.create 0; extra = () } in
      let xs = map_translation_unit env cst in
      match xs with
      | [ s ] -> Toplevel s
      | xs -> Toplevels xs)
