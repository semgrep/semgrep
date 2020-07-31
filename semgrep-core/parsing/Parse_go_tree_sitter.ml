(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
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
module AST = Ast_go
module CST = Tree_sitter_go.CST
module PI = Parse_info
open Ast_go
module G = AST_generic
module H = Parse_tree_sitter_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Go parser using ocaml-tree-sitter-lang/go and converting
 * to pfff/lang_go/parsing/ast_go.ml
 *
 * The resulting AST can then be converted to the generic AST by using
 * pfff/lang_go/analyze/go_to_generic.ml
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = H.env
let _fake = G.fake
let token = H.token
let str = H.str

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying ocaml-tree-sitter-lang/go/.../Boilerplate.ml *)

(**
   Boilerplate to be used as a template when mapping the go CST
   to another type of tree.
*)

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

[@@@warning "-32"]

let blank (env : env) () = ()

let todo (env : env) _ =
   failwith "not implemented"

let identifier (env : env) (tok : CST.identifier) =
  str env tok (* identifier *)



let qualified_type (env : env) ((v1, v2, v3) : CST.qualified_type) =
  let v1 = str env v1 (* identifier *) in
  let _v2 = token env v2 (* "." *) in
  let v3 = str env v3 (* identifier *) in
  [v1; v3]

let package_clause (env : env) ((v1, v2) : CST.package_clause) =
  let v1 = token env v1 (* "package" *) in
  let v2 = str env v2 (* identifier *) in
  Package (v1, v2)

let empty_labeled_statement (env : env) ((v1, v2) : CST.empty_labeled_statement) =
  let v1 = token env v1 (* identifier *) in
  let v2 = token env v2 (* ":" *) in
  todo env (v1, v2)

let interpreted_string_literal (env : env) ((v1, v2, v3) : CST.interpreted_string_literal) =
  let v1 = token env v1 (* "\"" *) in
  let v2 =
    List.map (fun x ->
      (match x with
      | `Blank () -> todo env ()
      | `Esc_seq tok -> token env tok (* escape_sequence *)
      )
    ) v2
  in
  let v3 = token env v3 (* "\"" *) in
  todo env (v1, v2, v3)

let import_spec (env : env) ((v1, v2) : CST.import_spec) =
  let v1 =
    (match v1 with
    | Some x ->
        (match x with
        | `Dot tok -> token env tok (* "." *)
        | `Blank_id tok -> token env tok (* "_" *)
        | `Id tok -> token env tok (* identifier *)
        )
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | `Raw_str_lit tok -> token env tok (* raw_string_literal *)
    | `Inte_str_lit x -> interpreted_string_literal env x
    )
  in
  todo env (v1, v2)

let rec declaration (env : env) (x : CST.declaration) =
  (match x with
  | `Const_decl (v1, v2) ->
      let v1 = token env v1 (* "const" *) in
      let v2 =
        (match v2 with
        | `Const_spec x -> const_spec env x
        | `LPAR_rep_const_spec_choice_LF_RPAR (v1, v2, v3) ->
            let v1 = token env v1 (* "(" *) in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = const_spec env v1 in
                let v2 =
                  (match v2 with
                  | `LF tok -> token env tok (* "\n" *)
                  | `SEMI tok -> token env tok (* ";" *)
                  )
                in
                todo env (v1, v2)
              ) v2
            in
            let v3 = token env v3 (* ")" *) in
            todo env (v1, v2, v3)
        )
      in
      todo env (v1, v2)
  | `Type_decl (v1, v2) ->
      let v1 = token env v1 (* "type" *) in
      let v2 =
        (match v2 with
        | `Type_spec x -> type_spec env x
        | `Type_alias x -> type_alias env x
        | `LPAR_rep_choice_type_spec_choice_LF_RPAR (v1, v2, v3) ->
            let v1 = token env v1 (* "(" *) in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 =
                  (match v1 with
                  | `Type_spec x -> type_spec env x
                  | `Type_alias x -> type_alias env x
                  )
                in
                let v2 =
                  (match v2 with
                  | `LF tok -> token env tok (* "\n" *)
                  | `SEMI tok -> token env tok (* ";" *)
                  )
                in
                todo env (v1, v2)
              ) v2
            in
            let v3 = token env v3 (* ")" *) in
            todo env (v1, v2, v3)
        )
      in
      todo env (v1, v2)
  | `Var_decl (v1, v2) ->
      let v1 = token env v1 (* "var" *) in
      let v2 =
        (match v2 with
        | `Var_spec x -> var_spec env x
        | `LPAR_rep_var_spec_choice_LF_RPAR (v1, v2, v3) ->
            let v1 = token env v1 (* "(" *) in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = var_spec env v1 in
                let v2 =
                  (match v2 with
                  | `LF tok -> token env tok (* "\n" *)
                  | `SEMI tok -> token env tok (* ";" *)
                  )
                in
                todo env (v1, v2)
              ) v2
            in
            let v3 = token env v3 (* ")" *) in
            todo env (v1, v2, v3)
        )
      in
      todo env (v1, v2)
  )


and const_spec (env : env) ((v1, v2, v3) : CST.const_spec) =
  let v1 = token env v1 (* identifier *) in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = token env v2 (* identifier *) in
      todo env (v1, v2)
    ) v2
  in
  let v3 =
    (match v3 with
    | Some (v1, v2, v3) ->
        let v1 =
          (match v1 with
          | Some x ->
              (match x with
              | `Simple_type x -> simple_type env x
              | `Paren_type x -> parenthesized_type env x
              )
          | None -> todo env ())
        in
        let v2 = token env v2 (* "=" *) in
        let v3 = expression_list env v3 in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  todo env (v1, v2, v3)


and var_spec (env : env) ((v1, v2, v3) : CST.var_spec) =
  let v1 = token env v1 (* identifier *) in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = token env v2 (* identifier *) in
      todo env (v1, v2)
    ) v2
  in
  let v3 =
    (match v3 with
    | `Choice_simple_type_opt_EQ_exp_list (v1, v2) ->
        let v1 =
          (match v1 with
          | `Simple_type x -> simple_type env x
          | `Paren_type x -> parenthesized_type env x
          )
        in
        let v2 =
          (match v2 with
          | Some (v1, v2) ->
              let v1 = token env v1 (* "=" *) in
              let v2 = expression_list env v2 in
              todo env (v1, v2)
          | None -> todo env ())
        in
        todo env (v1, v2)
    | `EQ_exp_list (v1, v2) ->
        let v1 = token env v1 (* "=" *) in
        let v2 = expression_list env v2 in
        todo env (v1, v2)
    )
  in
  todo env (v1, v2, v3)


and parameter_list (env : env) ((v1, v2, v3) : CST.parameter_list) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 =
          (match v1 with
          | Some (v1, v2) ->
              let v1 =
                (match v1 with
                | `Param_decl x -> parameter_declaration env x
                | `Vari_param_decl x ->
                    variadic_parameter_declaration env x
                )
              in
              let v2 =
                List.map (fun (v1, v2) ->
                  let v1 = token env v1 (* "," *) in
                  let v2 =
                    (match v2 with
                    | `Param_decl x -> parameter_declaration env x
                    | `Vari_param_decl x ->
                        variadic_parameter_declaration env x
                    )
                  in
                  todo env (v1, v2)
                ) v2
              in
              todo env (v1, v2)
          | None -> todo env ())
        in
        let v2 =
          (match v2 with
          | Some tok -> token env tok (* "," *)
          | None -> todo env ())
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)


and parameter_declaration (env : env) ((v1, v2) : CST.parameter_declaration) =
  let v1 =
    (match v1 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* identifier *) in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = token env v2 (* identifier *) in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | `Simple_type x -> simple_type env x
    | `Paren_type x -> parenthesized_type env x
    )
  in
  todo env (v1, v2)


and variadic_parameter_declaration (env : env) ((v1, v2, v3) : CST.variadic_parameter_declaration) =
  let v1 =
    (match v1 with
    | Some tok -> token env tok (* identifier *)
    | None -> todo env ())
  in
  let v2 = token env v2 (* "..." *) in
  let v3 =
    (match v3 with
    | `Simple_type x -> simple_type env x
    | `Paren_type x -> parenthesized_type env x
    )
  in
  todo env (v1, v2, v3)


and type_alias (env : env) ((v1, v2, v3) : CST.type_alias) =
  let v1 = token env v1 (* identifier *) in
  let v2 = token env v2 (* "=" *) in
  let v3 =
    (match v3 with
    | `Simple_type x -> simple_type env x
    | `Paren_type x -> parenthesized_type env x
    )
  in
  todo env (v1, v2, v3)


and type_spec (env : env) ((v1, v2) : CST.type_spec) =
  let v1 = token env v1 (* identifier *) in
  let v2 =
    (match v2 with
    | `Simple_type x -> simple_type env x
    | `Paren_type x -> parenthesized_type env x
    )
  in
  todo env (v1, v2)


and expression_list (env : env) ((v1, v2) : CST.expression_list) =
  let v1 = expression env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = expression env v2 in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)


and parenthesized_type (env : env) ((v1, v2, v3) : CST.parenthesized_type) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | `Simple_type x -> simple_type env x
    | `Paren_type x -> parenthesized_type env x
    )
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)


and simple_type (env : env) (x : CST.simple_type) =
  (match x with
  | `Id tok -> TName [str env tok] (* identifier *)
  | `Qual_type x -> TName (qualified_type env x)
  | `Poin_type (v1, v2) ->
      let v1 = token env v1 (* "*" *) in
      let v2 =
        (match v2 with
        | `Simple_type x -> simple_type env x
        | `Paren_type x -> parenthesized_type env x
        )
      in
      todo env (v1, v2)
  | `Struct_type x -> struct_type env x
  | `Inte_type (v1, v2) ->
      let v1 = token env v1 (* "interface" *) in
      let v2 = method_spec_list env v2 in
      todo env (v1, v2)
  | `Array_type x -> array_type env x
  | `Slice_type x -> slice_type env x
  | `Map_type x -> map_type env x
  | `Chan_type x -> channel_type env x
  | `Func_type (v1, v2, v3) ->
      let v1 = token env v1 (* "func" *) in
      let v2 = parameter_list env v2 in
      let v3 =
        (match v3 with
        | Some x ->
            (match x with
            | `Param_list x -> parameter_list env x
            | `Simple_type x -> simple_type env x
            )
        | None -> todo env ())
      in
      todo env (v1, v2, v3)
  )


and array_type (env : env) ((v1, v2, v3, v4) : CST.array_type) =
  let v1 = token env v1 (* "[" *) in
  let v2 = expression env v2 in
  let v3 = token env v3 (* "]" *) in
  let v4 =
    (match v4 with
    | `Simple_type x -> simple_type env x
    | `Paren_type x -> parenthesized_type env x
    )
  in
  todo env (v1, v2, v3, v4)


and implicit_length_array_type (env : env) ((v1, v2, v3, v4) : CST.implicit_length_array_type) =
  let v1 = token env v1 (* "[" *) in
  let v2 = token env v2 (* "..." *) in
  let v3 = token env v3 (* "]" *) in
  let v4 =
    (match v4 with
    | `Simple_type x -> simple_type env x
    | `Paren_type x -> parenthesized_type env x
    )
  in
  todo env (v1, v2, v3, v4)


and slice_type (env : env) ((v1, v2, v3) : CST.slice_type) =
  let v1 = token env v1 (* "[" *) in
  let v2 = token env v2 (* "]" *) in
  let v3 =
    (match v3 with
    | `Simple_type x -> simple_type env x
    | `Paren_type x -> parenthesized_type env x
    )
  in
  todo env (v1, v2, v3)


and struct_type (env : env) ((v1, v2) : CST.struct_type) =
  let v1 = token env v1 (* "struct" *) in
  let v2 = field_declaration_list env v2 in
  todo env (v1, v2)


and field_declaration_list (env : env) ((v1, v2, v3) : CST.field_declaration_list) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 = field_declaration env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 =
              (match v1 with
              | `LF tok -> token env tok (* "\n" *)
              | `SEMI tok -> token env tok (* ";" *)
              )
            in
            let v2 = field_declaration env v2 in
            todo env (v1, v2)
          ) v2
        in
        let v3 =
          (match v3 with
          | Some x ->
              (match x with
              | `LF tok -> token env tok (* "\n" *)
              | `SEMI tok -> token env tok (* ";" *)
              )
          | None -> todo env ())
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)


and field_declaration (env : env) ((v1, v2) : CST.field_declaration) =
  let v1 =
    (match v1 with
    | `Id_rep_COMMA_id_choice_simple_type (v1, v2, v3) ->
        let v1 = token env v1 (* identifier *) in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = token env v2 (* identifier *) in
            todo env (v1, v2)
          ) v2
        in
        let v3 =
          (match v3 with
          | `Simple_type x -> simple_type env x
          | `Paren_type x -> parenthesized_type env x
          )
        in
        todo env (v1, v2, v3)
    | `Opt_STAR_choice_id (v1, v2) ->
        let v1 =
          (match v1 with
          | Some tok -> token env tok (* "*" *)
          | None -> todo env ())
        in
        let v2 =
          (match v2 with
          | `Id tok -> [str env tok] (* identifier *)
          | `Qual_type x -> qualified_type env x
          )
        in
        todo env (v1, v2)
    )
  in
  let v2 =
    (match v2 with
    | Some x ->
        (match x with
        | `Raw_str_lit tok -> token env tok (* raw_string_literal *)
        | `Inte_str_lit x -> interpreted_string_literal env x
        )
    | None -> todo env ())
  in
  todo env (v1, v2)


and method_spec_list (env : env) ((v1, v2, v3) : CST.method_spec_list) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 =
          (match v1 with
          | `Id tok -> [str env tok] (* identifier *)
          | `Qual_type x -> qualified_type env x
          | `Meth_spec x -> method_spec env x
          )
        in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 =
              (match v1 with
              | `LF tok -> token env tok (* "\n" *)
              | `SEMI tok -> token env tok (* ";" *)
              )
            in
            let v2 =
              (match v2 with
              | `Id tok -> [str env tok] (* identifier *)
              | `Qual_type x -> qualified_type env x
              | `Meth_spec x -> method_spec env x
              )
            in
            todo env (v1, v2)
          ) v2
        in
        let v3 =
          (match v3 with
          | Some x ->
              (match x with
              | `LF tok -> token env tok (* "\n" *)
              | `SEMI tok -> token env tok (* ";" *)
              )
          | None -> todo env ())
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)


and method_spec (env : env) ((v1, v2, v3) : CST.method_spec) =
  let v1 = token env v1 (* identifier *) in
  let v2 = parameter_list env v2 in
  let v3 =
    (match v3 with
    | Some x ->
        (match x with
        | `Param_list x -> parameter_list env x
        | `Simple_type x -> simple_type env x
        )
    | None -> todo env ())
  in
  todo env (v1, v2, v3)


and map_type (env : env) ((v1, v2, v3, v4, v5) : CST.map_type) =
  let v1 = token env v1 (* "map" *) in
  let v2 = token env v2 (* "[" *) in
  let v3 =
    (match v3 with
    | `Simple_type x -> simple_type env x
    | `Paren_type x -> parenthesized_type env x
    )
  in
  let v4 = token env v4 (* "]" *) in
  let v5 =
    (match v5 with
    | `Simple_type x -> simple_type env x
    | `Paren_type x -> parenthesized_type env x
    )
  in
  todo env (v1, v2, v3, v4, v5)


and channel_type (env : env) (x : CST.channel_type) =
  (match x with
  | `Chan_choice_simple_type (v1, v2) ->
      let v1 = token env v1 (* "chan" *) in
      let v2 =
        (match v2 with
        | `Simple_type x -> simple_type env x
        | `Paren_type x -> parenthesized_type env x
        )
      in
      todo env (v1, v2)
  | `Chan_LTDASH_choice_simple_type (v1, v2, v3) ->
      let v1 = token env v1 (* "chan" *) in
      let v2 = token env v2 (* "<-" *) in
      let v3 =
        (match v3 with
        | `Simple_type x -> simple_type env x
        | `Paren_type x -> parenthesized_type env x
        )
      in
      todo env (v1, v2, v3)
  | `LTDASH_chan_choice_simple_type (v1, v2, v3) ->
      let v1 = token env v1 (* "<-" *) in
      let v2 = token env v2 (* "chan" *) in
      let v3 =
        (match v3 with
        | `Simple_type x -> simple_type env x
        | `Paren_type x -> parenthesized_type env x
        )
      in
      todo env (v1, v2, v3)
  )


and block (env : env) ((v1, v2, v3) : CST.block) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some x -> statement_list env x
    | None -> todo env ())
  in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)


and statement_list (env : env) (x : CST.statement_list) =
  (match x with
  | `Stmt_rep_choice_LF_stmt_opt_choice_LF_opt_empty_labe_stmt (v1, v2, v3) ->
      let v1 = statement env v1 in
      let v2 =
        List.map (fun (v1, v2) ->
          let v1 =
            (match v1 with
            | `LF tok -> token env tok (* "\n" *)
            | `SEMI tok -> token env tok (* ";" *)
            )
          in
          let v2 = statement env v2 in
          todo env (v1, v2)
        ) v2
      in
      let v3 =
        (match v3 with
        | Some (v1, v2) ->
            let v1 =
              (match v1 with
              | `LF tok -> token env tok (* "\n" *)
              | `SEMI tok -> token env tok (* ";" *)
              )
            in
            let v2 =
              (match v2 with
              | Some x -> empty_labeled_statement env x
              | None -> todo env ())
            in
            todo env (v1, v2)
        | None -> todo env ())
      in
      todo env (v1, v2, v3)
  | `Empty_labe_stmt x ->
      empty_labeled_statement env x
  )


and statement (env : env) (x : CST.statement) =
  (match x with
  | `Decl x -> declaration env x
  | `Simple_stmt x -> simple_statement env x
  | `Ret_stmt (v1, v2) ->
      let v1 = token env v1 (* "return" *) in
      let v2 =
        (match v2 with
        | Some x -> expression_list env x
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Go_stmt (v1, v2) ->
      let v1 = token env v1 (* "go" *) in
      let v2 = expression env v2 in
      todo env (v1, v2)
  | `Defer_stmt (v1, v2) ->
      let v1 = token env v1 (* "defer" *) in
      let v2 = expression env v2 in
      todo env (v1, v2)
  | `If_stmt x -> if_statement env x
  | `For_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "for" *) in
      let v2 =
        (match v2 with
        | Some x ->
            (match x with
            | `Exp x -> expression env x
            | `For_clause x -> for_clause env x
            | `Range_clause x -> range_clause env x
            )
        | None -> todo env ())
      in
      let v3 = block env v3 in
      todo env (v1, v2, v3)
  | `Exp_switch_stmt (v1, v2, v3, v4, v5, v6) ->
      let v1 = token env v1 (* "switch" *) in
      let v2 =
        (match v2 with
        | Some (v1, v2) ->
            let v1 = simple_statement env v1 in
            let v2 = token env v2 (* ";" *) in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | Some x -> expression env x
        | None -> todo env ())
      in
      let v4 = token env v4 (* "{" *) in
      let v5 =
        List.map (fun x ->
          (match x with
          | `Exp_case x -> expression_case env x
          | `Defa_case x -> default_case env x
          )
        ) v5
      in
      let v6 = token env v6 (* "}" *) in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Type_switch_stmt (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "switch" *) in
      let v2 = type_switch_header env v2 in
      let v3 = token env v3 (* "{" *) in
      let v4 =
        List.map (fun x ->
          (match x with
          | `Type_case x -> type_case env x
          | `Defa_case x -> default_case env x
          )
        ) v4
      in
      let v5 = token env v5 (* "}" *) in
      todo env (v1, v2, v3, v4, v5)
  | `Select_stmt (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "select" *) in
      let v2 = token env v2 (* "{" *) in
      let v3 =
        List.map (fun x ->
          (match x with
          | `Comm_case x -> communication_case env x
          | `Defa_case x -> default_case env x
          )
        ) v3
      in
      let v4 = token env v4 (* "}" *) in
      todo env (v1, v2, v3, v4)
  | `Labe_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* identifier *) in
      let v2 = token env v2 (* ":" *) in
      let v3 = statement env v3 in
      todo env (v1, v2, v3)
  | `Fall_stmt tok -> token env tok (* "fallthrough" *)
  | `Brk_stmt (v1, v2) ->
      let v1 = token env v1 (* "break" *) in
      let v2 =
        (match v2 with
        | Some tok -> token env tok (* identifier *)
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Cont_stmt (v1, v2) ->
      let v1 = token env v1 (* "continue" *) in
      let v2 =
        (match v2 with
        | Some tok -> token env tok (* identifier *)
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Goto_stmt (v1, v2) ->
      let v1 = token env v1 (* "goto" *) in
      let v2 = token env v2 (* identifier *) in
      todo env (v1, v2)
  | `Blk x -> block env x
  | `Empty_stmt tok -> token env tok (* ";" *)
  )


and simple_statement (env : env) (x : CST.simple_statement) =
  (match x with
  | `Exp x -> expression env x
  | `Send_stmt x -> send_statement env x
  | `Inc_stmt (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "++" *) in
      todo env (v1, v2)
  | `Dec_stmt (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "--" *) in
      todo env (v1, v2)
  | `Assign_stmt (v1, v2, v3) ->
      let v1 = expression_list env v1 in
      let v2 =
        (match v2 with
        | `STAREQ tok -> token env tok (* "*=" *)
        | `SLASHEQ tok -> token env tok (* "/=" *)
        | `PERCEQ tok -> token env tok (* "%=" *)
        | `LTLTEQ tok -> token env tok (* "<<=" *)
        | `GTGTEQ tok -> token env tok (* ">>=" *)
        | `AMPEQ tok -> token env tok (* "&=" *)
        | `AMPHATEQ tok -> token env tok (* "&^=" *)
        | `PLUSEQ tok -> token env tok (* "+=" *)
        | `DASHEQ tok -> token env tok (* "-=" *)
        | `BAREQ tok -> token env tok (* "|=" *)
        | `HATEQ tok -> token env tok (* "^=" *)
        | `EQ tok -> token env tok (* "=" *)
        )
      in
      let v3 = expression_list env v3 in
      todo env (v1, v2, v3)
  | `Short_var_decl (v1, v2, v3) ->
      let v1 = expression_list env v1 in
      let v2 = token env v2 (* ":=" *) in
      let v3 = expression_list env v3 in
      todo env (v1, v2, v3)
  )


and send_statement (env : env) ((v1, v2, v3) : CST.send_statement) =
  let v1 = expression env v1 in
  let v2 = token env v2 (* "<-" *) in
  let v3 = expression env v3 in
  todo env (v1, v2, v3)


and receive_statement (env : env) ((v1, v2) : CST.receive_statement) =
  let v1 =
    (match v1 with
    | Some (v1, v2) ->
        let v1 = expression_list env v1 in
        let v2 =
          (match v2 with
          | `EQ tok -> token env tok (* "=" *)
          | `COLONEQ tok -> token env tok (* ":=" *)
          )
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v2 = expression env v2 in
  todo env (v1, v2)


and if_statement (env : env) ((v1, v2, v3, v4, v5) : CST.if_statement) =
  let v1 = token env v1 (* "if" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = simple_statement env v1 in
        let v2 = token env v2 (* ";" *) in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = expression env v3 in
  let v4 = block env v4 in
  let v5 =
    (match v5 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "else" *) in
        let v2 =
          (match v2 with
          | `Blk x -> block env x
          | `If_stmt x -> if_statement env x
          )
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5)


and for_clause (env : env) ((v1, v2, v3, v4, v5) : CST.for_clause) =
  let v1 =
    (match v1 with
    | Some x -> simple_statement env x
    | None -> todo env ())
  in
  let v2 = token env v2 (* ";" *) in
  let v3 =
    (match v3 with
    | Some x -> expression env x
    | None -> todo env ())
  in
  let v4 = token env v4 (* ";" *) in
  let v5 =
    (match v5 with
    | Some x -> simple_statement env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5)


and range_clause (env : env) ((v1, v2, v3) : CST.range_clause) =
  let v1 =
    (match v1 with
    | Some (v1, v2) ->
        let v1 = expression_list env v1 in
        let v2 =
          (match v2 with
          | `EQ tok -> token env tok (* "=" *)
          | `COLONEQ tok -> token env tok (* ":=" *)
          )
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v2 = token env v2 (* "range" *) in
  let v3 = expression env v3 in
  todo env (v1, v2, v3)


and expression_case (env : env) ((v1, v2, v3, v4) : CST.expression_case) =
  let v1 = token env v1 (* "case" *) in
  let v2 = expression_list env v2 in
  let v3 = token env v3 (* ":" *) in
  let v4 =
    (match v4 with
    | Some x -> statement_list env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)


and default_case (env : env) ((v1, v2, v3) : CST.default_case) =
  let v1 = token env v1 (* "default" *) in
  let v2 = token env v2 (* ":" *) in
  let v3 =
    (match v3 with
    | Some x -> statement_list env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3)


and type_switch_header (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.type_switch_header) =
  let v1 =
    (match v1 with
    | Some (v1, v2) ->
        let v1 = simple_statement env v1 in
        let v2 = token env v2 (* ";" *) in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = expression_list env v1 in
        let v2 = token env v2 (* ":=" *) in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = expression env v3 in
  let v4 = token env v4 (* "." *) in
  let v5 = token env v5 (* "(" *) in
  let v6 = token env v6 (* "type" *) in
  let v7 = token env v7 (* ")" *) in
  todo env (v1, v2, v3, v4, v5, v6, v7)


and type_case (env : env) ((v1, v2, v3, v4, v5) : CST.type_case) =
  let v1 = token env v1 (* "case" *) in
  let v2 =
    (match v2 with
    | `Simple_type x -> simple_type env x
    | `Paren_type x -> parenthesized_type env x
    )
  in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 =
        (match v2 with
        | `Simple_type x -> simple_type env x
        | `Paren_type x -> parenthesized_type env x
        )
      in
      todo env (v1, v2)
    ) v3
  in
  let v4 = token env v4 (* ":" *) in
  let v5 =
    (match v5 with
    | Some x -> statement_list env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5)


and communication_case (env : env) ((v1, v2, v3, v4) : CST.communication_case) =
  let v1 = token env v1 (* "case" *) in
  let v2 =
    (match v2 with
    | `Send_stmt x -> send_statement env x
    | `Rece_stmt x -> receive_statement env x
    )
  in
  let v3 = token env v3 (* ":" *) in
  let v4 =
    (match v4 with
    | Some x -> statement_list env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)


and expression (env : env) (x : CST.expression) =
  (match x with
  | `Un_exp (v1, v2) ->
      let v1 =
        (match v1 with
        | `PLUS tok -> token env tok (* "+" *)
        | `DASH tok -> token env tok (* "-" *)
        | `BANG tok -> token env tok (* "!" *)
        | `HAT tok -> token env tok (* "^" *)
        | `STAR tok -> token env tok (* "*" *)
        | `AMP tok -> token env tok (* "&" *)
        | `LTDASH tok -> token env tok (* "<-" *)
        )
      in
      let v2 = expression env v2 in
      todo env (v1, v2)
  | `Bin_exp x -> binary_expression env x
  | `Sele_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "." *) in
      let v3 = token env v3 (* identifier *) in
      todo env (v1, v2, v3)
  | `Index_exp (v1, v2, v3, v4) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "[" *) in
      let v3 = expression env v3 in
      let v4 = token env v4 (* "]" *) in
      todo env (v1, v2, v3, v4)
  | `Slice_exp (v1, v2, v3, v4) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "[" *) in
      let v3 =
        (match v3 with
        | `Opt_exp_COLON_opt_exp (v1, v2, v3) ->
            let v1 =
              (match v1 with
              | Some x -> expression env x
              | None -> todo env ())
            in
            let v2 = token env v2 (* ":" *) in
            let v3 =
              (match v3 with
              | Some x -> expression env x
              | None -> todo env ())
            in
            todo env (v1, v2, v3)
        | `Opt_exp_COLON_exp_COLON_exp (v1, v2, v3, v4, v5) ->
            let v1 =
              (match v1 with
              | Some x -> expression env x
              | None -> todo env ())
            in
            let v2 = token env v2 (* ":" *) in
            let v3 = expression env v3 in
            let v4 = token env v4 (* ":" *) in
            let v5 = expression env v5 in
            todo env (v1, v2, v3, v4, v5)
        )
      in
      let v4 = token env v4 (* "]" *) in
      todo env (v1, v2, v3, v4)
  | `Call_exp x -> call_expression env x
  | `Type_asse_exp (v1, v2, v3, v4, v5) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "." *) in
      let v3 = token env v3 (* "(" *) in
      let v4 =
        (match v4 with
        | `Simple_type x -> simple_type env x
        | `Paren_type x -> parenthesized_type env x
        )
      in
      let v5 = token env v5 (* ")" *) in
      todo env (v1, v2, v3, v4, v5)
  | `Type_conv_exp (v1, v2, v3, v4, v5) ->
      let v1 =
        (match v1 with
        | `Simple_type x -> simple_type env x
        | `Paren_type x -> parenthesized_type env x
        )
      in
      let v2 = token env v2 (* "(" *) in
      let v3 = expression env v3 in
      let v4 =
        (match v4 with
        | Some tok -> token env tok (* "," *)
        | None -> todo env ())
      in
      let v5 = token env v5 (* ")" *) in
      todo env (v1, v2, v3, v4, v5)
  | `Id tok -> token env tok (* identifier *)
  | `Choice_new x ->
      (match x with
      | `New tok -> token env tok (* "new" *)
      | `Make tok -> token env tok (* "make" *)
      )
  | `Comp_lit (v1, v2) ->
      let v1 =
        (match v1 with
        | `Map_type x -> map_type env x
        | `Slice_type x -> slice_type env x
        | `Array_type x -> array_type env x
        | `Impl_len_array_type x ->
            implicit_length_array_type env x
        | `Struct_type x -> struct_type env x
        | `Id tok -> TName [str env tok] (* identifier *)
        | `Qual_type x -> TName (qualified_type env x)
        )
      in
      let v2 = literal_value env v2 in
      todo env (v1, v2)
  | `Func_lit (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "func" *) in
      let v2 = parameter_list env v2 in
      let v3 =
        (match v3 with
        | Some x ->
            (match x with
            | `Param_list x -> parameter_list env x
            | `Simple_type x -> simple_type env x
            )
        | None -> todo env ())
      in
      let v4 = block env v4 in
      todo env (v1, v2, v3, v4)
  | `Choice_raw_str_lit x ->
      (match x with
      | `Raw_str_lit tok -> token env tok (* raw_string_literal *)
      | `Inte_str_lit x -> interpreted_string_literal env x
      )
  | `Int_lit tok -> token env tok (* int_literal *)
  | `Float_lit tok -> token env tok (* float_literal *)
  | `Imag_lit tok -> token env tok (* imaginary_literal *)
  | `Rune_lit tok -> token env tok (* rune_literal *)
  | `Nil tok -> token env tok (* "nil" *)
  | `True tok -> token env tok (* "true" *)
  | `False tok -> token env tok (* "false" *)
  | `Paren_exp (v1, v2, v3) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = expression env v2 in
      let v3 = token env v3 (* ")" *) in
      todo env (v1, v2, v3)
  )


and call_expression (env : env) (x : CST.call_expression) =
  (match x with
  | `Choice_new_spec_arg_list (v1, v2) ->
      let v1 =
        (match v1 with
        | `New tok -> token env tok (* "new" *)
        | `Make tok -> token env tok (* "make" *)
        )
      in
      let v2 = special_argument_list env v2 in
      todo env (v1, v2)
  | `Exp_arg_list (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = argument_list env v2 in
      todo env (v1, v2)
  )


and variadic_argument (env : env) ((v1, v2) : CST.variadic_argument) =
  let v1 = expression env v1 in
  let v2 = token env v2 (* "..." *) in
  todo env (v1, v2)


and special_argument_list (env : env) ((v1, v2, v3, v4, v5) : CST.special_argument_list) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | `Simple_type x -> simple_type env x
    | `Paren_type x -> parenthesized_type env x
    )
  in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = expression env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 =
    (match v4 with
    | Some tok -> token env tok (* "," *)
    | None -> todo env ())
  in
  let v5 = token env v5 (* ")" *) in
  todo env (v1, v2, v3, v4, v5)


and argument_list (env : env) ((v1, v2, v3) : CST.argument_list) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 =
          (match v1 with
          | `Exp x -> expression env x
          | `Vari_arg x -> variadic_argument env x
          )
        in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 =
              (match v2 with
              | `Exp x -> expression env x
              | `Vari_arg x -> variadic_argument env x
              )
            in
            todo env (v1, v2)
          ) v2
        in
        let v3 =
          (match v3 with
          | Some tok -> token env tok (* "," *)
          | None -> todo env ())
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)


and literal_value (env : env) ((v1, v2, v3) : CST.literal_value) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 =
          (match v1 with
          | `Elem x -> element env x
          | `Keyed_elem x -> keyed_element env x
          )
        in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 =
              (match v2 with
              | `Elem x -> element env x
              | `Keyed_elem x -> keyed_element env x
              )
            in
            todo env (v1, v2)
          ) v2
        in
        let v3 =
          (match v3 with
          | Some tok -> token env tok (* "," *)
          | None -> todo env ())
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)


and keyed_element (env : env) ((v1, v2) : CST.keyed_element) =
  let v1 =
    (match v1 with
    | `Exp_COLON (v1, v2) ->
        let v1 = expression env v1 in
        let v2 = token env v2 (* ":" *) in
        todo env (v1, v2)
    | `Lit_value_COLON (v1, v2) ->
        let v1 = literal_value env v1 in
        let v2 = token env v2 (* ":" *) in
        todo env (v1, v2)
    | `Id_COLON (v1, v2) ->
        let v1 = token env v1 (* identifier *) in
        let v2 = token env v2 (* ":" *) in
        todo env (v1, v2)
    )
  in
  let v2 =
    (match v2 with
    | `Exp x -> expression env x
    | `Lit_value x -> literal_value env x
    )
  in
  todo env (v1, v2)


and element (env : env) (x : CST.element) =
  (match x with
  | `Exp x -> expression env x
  | `Lit_value x -> literal_value env x
  )


and binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Exp_choice_STAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        (match v2 with
        | `STAR tok -> token env tok (* "*" *)
        | `SLASH tok -> token env tok (* "/" *)
        | `PERC tok -> token env tok (* "%" *)
        | `LTLT tok -> token env tok (* "<<" *)
        | `GTGT tok -> token env tok (* ">>" *)
        | `AMP tok -> token env tok (* "&" *)
        | `AMPHAT tok -> token env tok (* "&^" *)
        )
      in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_choice_PLUS_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        (match v2 with
        | `PLUS tok -> token env tok (* "+" *)
        | `DASH tok -> token env tok (* "-" *)
        | `BAR tok -> token env tok (* "|" *)
        | `HAT tok -> token env tok (* "^" *)
        )
      in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_choice_EQEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        (match v2 with
        | `EQEQ tok -> token env tok (* "==" *)
        | `BANGEQ tok -> token env tok (* "!=" *)
        | `LT tok -> token env tok (* "<" *)
        | `LTEQ tok -> token env tok (* "<=" *)
        | `GT tok -> token env tok (* ">" *)
        | `GTEQ tok -> token env tok (* ">=" *)
        )
      in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "&&" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "||" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  )

let import_spec_list (env : env) ((v1, v2, v3) : CST.import_spec_list) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = import_spec env v1 in
      let v2 =
        (match v2 with
        | `LF tok -> token env tok (* "\n" *)
        | `SEMI tok -> token env tok (* ";" *)
        )
      in
      todo env (v1, v2)
    ) v2
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

let method_declaration (env : env) ((v1, v2, v3, v4, v5, v6) : CST.method_declaration) =
  let v1 = token env v1 (* "func" *) in
  let v2 = parameter_list env v2 in
  let v3 = token env v3 (* identifier *) in
  let v4 = parameter_list env v4 in
  let v5 =
    (match v5 with
    | Some x ->
        (match x with
        | `Param_list x -> parameter_list env x
        | `Simple_type x -> simple_type env x
        )
    | None -> todo env ())
  in
  let v6 =
    (match v6 with
    | Some x -> block env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5, v6)

let function_declaration (env : env) ((v1, v2, v3, v4, v5) : CST.function_declaration) =
  let v1 = token env v1 (* "func" *) in
  let v2 = token env v2 (* identifier *) in
  let v3 = parameter_list env v3 in
  let v4 =
    (match v4 with
    | Some x ->
        (match x with
        | `Param_list x -> parameter_list env x
        | `Simple_type x -> simple_type env x
        )
    | None -> todo env ())
  in
  let v5 =
    (match v5 with
    | Some x -> block env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5)

let import_declaration (env : env) ((v1, v2) : CST.import_declaration) =
  let v1 = token env v1 (* "import" *) in
  let v2 =
    (match v2 with
    | `Import_spec x -> import_spec env x
    | `Import_spec_list x -> import_spec_list env x
    )
  in
  todo env (v1, v2)

let source_file (env : env) (xs : CST.source_file) =
  List.map (fun x ->
    (match x with
    | `Stmt_choice_LF (v1, v2) ->
        let v1 = statement env v1 in
        let v2 =
          (match v2 with
          | `LF tok -> token env tok (* "\n" *)
          | `SEMI tok -> token env tok (* ";" *)
          )
        in
        todo env (v1, v2)
    | `Choice_pack_clause_opt_choice_LF (v1, v2) ->
        let v1 =
          (match v1 with
          | `Pack_clause x -> package_clause env x
          | `Func_decl x -> function_declaration env x
          | `Meth_decl x -> method_declaration env x
          | `Import_decl x -> import_declaration env x
          )
        in
        let v2 =
          (match v2 with
          | Some x ->
              (match x with
              | `LF tok -> token env tok (* "\n" *)
              | `SEMI tok -> token env tok (* ";" *)
              )
          | None -> todo env ())
        in
        todo env (v1, v2)
    )
  ) xs


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  let ast =
    Parallel.backtrace_when_exn := false;
    Parallel.invoke Tree_sitter_go.Parse.file file ()
  in
  let env = { H.file; conv = H.line_col_to_pos file } in
  let _x = source_file env ast in
  raise Todo
