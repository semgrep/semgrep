(* Brandon Wu
 *
 * Copyright (C) 2024 r2c
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
module CST = Tree_sitter_ql.CST
module H = Parse_tree_sitter_helpers
open AST_ql

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* QL parser using tree-sitter-lang/semgrep-ql and converting
 * to ../ast/AST_ql.ml
 *
 * The resulting AST can then be converted to the generic AST by using
 * QL_to_generic.ml
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
(* This was started by copying tree-sitter-lang/semgrep-python/Boilerplate.ml *)

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

let token (env : env) (tok : Tree_sitter_run.Token.t) = R.Token tok
let blank (env : env) () = R.Tuple []

let map_quantifier (env : env) (x : CST.quantifier) =
  match x with
  | `Exists tok -> R.Case ("Exists", (* "exists" *) token env tok)
  | `Forall tok -> R.Case ("Forall", (* "forall" *) token env tok)
  | `Forex tok -> R.Case ("Forex", (* "forex" *) token env tok)

let map_qldoc (env : env) (tok : CST.qldoc) =
  (* pattern \/\*\*[^*]*\*+([^/*][^*]*\*+)*\/ *) token env tok

let map_semgrep_metavariable (env : env) (tok : CST.semgrep_metavariable) =
  (* semgrep_metavariable *) token env tok

let map_integer (env : env) (tok : CST.integer) =
  (* pattern [0-9]+ *) token env tok

let map_semgrep_ellipsis_metavar (env : env)
    (tok : CST.semgrep_ellipsis_metavar) =
  (* pattern \$\.\.\.[a-zA-Z_][a-zA-Z_0-9]* *) token env tok

let map_addop (env : env) (x : CST.addop) =
  match x with
  | `PLUS tok -> R.Case ("PLUS", (* "+" *) token env tok)
  | `DASH tok -> R.Case ("DASH", (* "-" *) token env tok)

let map_direction (env : env) (x : CST.direction) =
  match x with
  | `Asc tok -> R.Case ("Asc", (* "asc" *) token env tok)
  | `Desc tok -> R.Case ("Desc", (* "desc" *) token env tok)

let map_closure (env : env) (x : CST.closure) =
  match x with
  | `STAR tok -> R.Case ("STAR", (* "*" *) token env tok)
  | `PLUS tok -> R.Case ("PLUS", (* "+" *) token env tok)

let map_unop (env : env) (x : CST.unop) =
  match x with
  | `PLUS tok -> R.Case ("PLUS", (* "+" *) token env tok)
  | `DASH tok -> R.Case ("DASH", (* "-" *) token env tok)

let map_compop (env : env) (x : CST.compop) =
  match x with
  | `EQ tok -> R.Case ("EQ", (* "=" *) token env tok)
  | `BANGEQ tok -> R.Case ("BANGEQ", (* "!=" *) token env tok)
  | `LT tok -> R.Case ("LT", (* "<" *) token env tok)
  | `GT tok -> R.Case ("GT", (* ">" *) token env tok)
  | `LTEQ tok -> R.Case ("LTEQ", (* "<=" *) token env tok)
  | `GTEQ tok -> R.Case ("GTEQ", (* ">=" *) token env tok)

let map_string_ (env : env) (tok : CST.string_) =
  (* pattern "\"([^\"\\\\\\r\\n\\t]|\\\\[\"\\\\nrt])*\"" *) token env tok

let map_float_ (env : env) (tok : CST.float_) =
  (* pattern [0-9]+\.[0-9]+ *) token env tok

let map_dbtype (env : env) (tok : CST.dbtype) =
  (* pattern @[a-z][A-Za-z0-9_]* *) token env tok

let map_primitivetype (env : env) (x : CST.primitivetype) =
  match x with
  | `Bool tok -> R.Case ("Bool", (* "boolean" *) token env tok)
  | `Date tok -> R.Case ("Date", (* "date" *) token env tok)
  | `Float tok -> R.Case ("Float", (* "float" *) token env tok)
  | `Int tok -> R.Case ("Int", (* "int" *) token env tok)
  | `Str tok -> R.Case ("Str", (* "string" *) token env tok)

let map_mulop (env : env) (x : CST.mulop) =
  match x with
  | `STAR tok -> R.Case ("STAR", (* "*" *) token env tok)
  | `SLASH tok -> R.Case ("SLASH", (* "/" *) token env tok)
  | `PERC tok -> R.Case ("PERC", (* "%" *) token env tok)

let map_lower_id (env : env) (tok : CST.lower_id) =
  (* pattern [a-z][A-Za-z0-9_]* *) token env tok

let map_pat_3bf1220 (env : env) (tok : CST.pat_3bf1220) =
  (* pattern [A-Z][A-Za-z0-9_]* *) token env tok

let map_aggid (env : env) (x : CST.aggid) =
  match x with
  | `Avg tok -> R.Case ("Avg", (* "avg" *) token env tok)
  | `Concat tok -> R.Case ("Concat", (* "concat" *) token env tok)
  | `Stri_18c266c tok ->
      R.Case ("Stri_18c266c", (* "strictconcat" *) token env tok)
  | `Count tok -> R.Case ("Count", (* "count" *) token env tok)
  | `Max tok -> R.Case ("Max", (* "max" *) token env tok)
  | `Min tok -> R.Case ("Min", (* "min" *) token env tok)
  | `Rank tok -> R.Case ("Rank", (* "rank" *) token env tok)
  | `Stri_8bc2381 tok ->
      R.Case ("Stri_8bc2381", (* "strictcount" *) token env tok)
  | `Stri_a43e15b tok -> R.Case ("Stri_a43e15b", (* "strictsum" *) token env tok)
  | `Sum tok -> R.Case ("Sum", (* "sum" *) token env tok)
  | `Any tok -> R.Case ("Any", (* "any" *) token env tok)
  | `Unique tok -> R.Case ("Unique", (* "unique" *) token env tok)

let map_bool_ (env : env) (x : CST.bool_) =
  match x with
  | `True tok -> R.Case ("True", (* "true" *) token env tok)
  | `False tok -> R.Case ("False", (* "false" *) token env tok)

let map_annotname (env : env) (x : CST.annotname) =
  match x with
  | `Lower_id tok ->
      R.Case ("Lower_id", (* pattern [a-z][A-Za-z0-9_]* *) token env tok)
  | `Semg_meta tok ->
      R.Case ("Semg_meta", (* semgrep_metavariable *) token env tok)

let map_predicatename (env : env) (x : CST.predicatename) =
  match x with
  | `Lower_id tok ->
      R.Case ("Lower_id", (* pattern [a-z][A-Za-z0-9_]* *) token env tok)
  | `Semg_meta tok ->
      R.Case ("Semg_meta", (* semgrep_metavariable *) token env tok)

let map_upper_id (env : env) (x : CST.upper_id) =
  match x with
  | `Pat_3bf1220 x -> R.Case ("Pat_3bf1220", map_pat_3bf1220 env x)
  | `Semg_meta tok ->
      R.Case ("Semg_meta", (* semgrep_metavariable *) token env tok)

let map_literal (env : env) (x : CST.literal) =
  match x with
  | `Int tok -> R.Case ("Int", (* pattern [0-9]+ *) token env tok)
  | `Float tok -> R.Case ("Float", (* pattern [0-9]+\.[0-9]+ *) token env tok)
  | `Bool x -> R.Case ("Bool", map_bool_ env x)
  | `Str tok ->
      R.Case
        ( "Str",
          (* pattern "\"([^\"\\\\\\r\\n\\t]|\\\\[\"\\\\nrt])*\"" *)
          token env tok )

let map_classname (env : env) (x : CST.classname) = map_upper_id env x

let map_simpleid (env : env) (x : CST.simpleid) =
  match x with
  | `Lower_id tok ->
      R.Case ("Lower_id", (* pattern [a-z][A-Za-z0-9_]* *) token env tok)
  | `Upper_id x -> R.Case ("Upper_id", map_classname env x)

let map_literalid (env : env) (x : CST.literalid) =
  match x with
  | `Lower_id tok ->
      R.Case ("Lower_id", (* pattern [a-z][A-Za-z0-9_]* *) token env tok)
  | `Upper_id x -> R.Case ("Upper_id", map_classname env x)

let map_varname (env : env) (x : CST.varname) = map_simpleid env x
let map_modulename (env : env) (x : CST.modulename) = map_simpleid env x

let map_variable (env : env) (x : CST.variable) =
  match x with
  | `This tok -> R.Case ("This", (* "this" *) token env tok)
  | `Result tok -> R.Case ("Result", (* "result" *) token env tok)
  | `Varn x -> R.Case ("Varn", map_varname env x)

let rec map_aritylesspredicateexpr (env : env)
    ((v1, v2) : CST.aritylesspredicateexpr) =
  let v1 =
    match v1 with
    | Some (v1, v2) ->
        R.Option
          (Some
             (let v1 = map_moduleexpr env v1 in
              let v2 = (* "::" *) token env v2 in
              R.Tuple [ v1; v2 ]))
    | None -> R.Option None
  in
  let v2 = map_literalid env v2 in
  R.Tuple [ v1; v2 ]

and map_moduleexpr (env : env) (x : CST.moduleexpr) =
  match x with
  | `Simp x -> R.Case ("Simp", map_modulename env x)
  | `Modu x -> R.Case ("Modu", map_moduleinstantiation env x)
  | `Modu_COLONCOLON_choice_simp (v1, v2, v3) ->
      R.Case
        ( "Modu_COLONCOLON_choice_simp",
          let v1 = map_moduleexpr env v1 in
          let v2 = (* "::" *) token env v2 in
          let v3 =
            match v3 with
            | `Simp x -> R.Case ("Simp", map_modulename env x)
            | `Modu x -> R.Case ("Modu", map_moduleinstantiation env x)
          in
          R.Tuple [ v1; v2; v3 ] )

and map_moduleinstantiation (env : env)
    ((v1, v2, v3, v4, v5) : CST.moduleinstantiation) =
  let v1 = map_modulename env v1 in
  let v2 = (* "<" *) token env v2 in
  let v3 = map_signatureexpr env v3 in
  let v4 =
    R.List
      (List.map
         (fun (v1, v2) ->
           let v1 = (* "," *) token env v1 in
           let v2 = map_signatureexpr env v2 in
           R.Tuple [ v1; v2 ])
         v4)
  in
  let v5 = (* ">" *) token env v5 in
  R.Tuple [ v1; v2; v3; v4; v5 ]

and map_predicateexpr (env : env) ((v1, v2, v3) : CST.predicateexpr) =
  let v1 = map_aritylesspredicateexpr env v1 in
  let v2 = (* "/" *) token env v2 in
  let v3 = (* pattern [0-9]+ *) token env v3 in
  R.Tuple [ v1; v2; v3 ]

and map_signatureexpr (env : env) (x : CST.signatureexpr) =
  match x with
  | `Type x -> R.Case ("Type", map_typeexpr env x)
  | `Modu x -> R.Case ("Modu", map_moduleexpr env x)
  | `Pred x -> R.Case ("Pred", map_predicateexpr env x)

and map_typeexpr (env : env) (x : CST.typeexpr) =
  match x with
  | `Opt_modu_COLONCOLON_clas (v1, v2) ->
      R.Case
        ( "Opt_modu_COLONCOLON_clas",
          let v1 =
            match v1 with
            | Some (v1, v2) ->
                R.Option
                  (Some
                     (let v1 = map_moduleexpr env v1 in
                      let v2 = (* "::" *) token env v2 in
                      R.Tuple [ v1; v2 ]))
            | None -> R.Option None
          in
          let v2 = map_classname env v2 in
          R.Tuple [ v1; v2 ] )
  | `Dbtype tok ->
      R.Case ("Dbtype", (* pattern @[a-z][A-Za-z0-9_]* *) token env tok)
  | `Prim x -> R.Case ("Prim", map_primitivetype env x)

let map_annotarg (env : env) (x : CST.annotarg) =
  match x with
  | `Simp x -> R.Case ("Simp", map_modulename env x)
  | `This tok -> R.Case ("This", (* "this" *) token env tok)
  | `Result tok -> R.Case ("Result", (* "result" *) token env tok)

let map_importmoduleexpr (env : env) ((v1, v2) : CST.importmoduleexpr) =
  let v1 =
    R.List
      (List.map
         (fun (v1, v2) ->
           let v1 = map_modulename env v1 in
           let v2 = (* "." *) token env v2 in
           R.Tuple [ v1; v2 ])
         v1)
  in
  let v2 = map_moduleexpr env v2 in
  R.Tuple [ v1; v2 ]

let map_predicatealiasbody (env : env) ((v1, v2, v3) : CST.predicatealiasbody) =
  let v1 = (* "=" *) token env v1 in
  let v2 = map_predicateexpr env v2 in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [ v1; v2; v3 ]

let map_vardecl (env : env) ((v1, v2) : CST.vardecl) =
  let v1 = map_typeexpr env v1 in
  let v2 = map_varname env v2 in
  R.Tuple [ v1; v2 ]

let map_moduleparam (env : env) ((v1, v2) : CST.moduleparam) =
  let v1 = map_signatureexpr env v1 in
  let v2 = map_modulename env v2 in
  R.Tuple [ v1; v2 ]

let map_anon_choice_pred_44fe1b2 (env : env) (x : CST.anon_choice_pred_44fe1b2)
    =
  match x with
  | `Pred tok -> R.Case ("Pred", (* "predicate" *) token env tok)
  | `Type x -> R.Case ("Type", map_typeexpr env x)

let map_modulealiasbody (env : env) ((v1, v2, v3) : CST.modulealiasbody) =
  let v1 = (* "=" *) token env v1 in
  let v2 = map_moduleexpr env v2 in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [ v1; v2; v3 ]

let map_typealiasbody (env : env) ((v1, v2, v3) : CST.typealiasbody) =
  let v1 = (* "=" *) token env v1 in
  let v2 = map_typeexpr env v2 in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [ v1; v2; v3 ]

let map_typeunionbody (env : env) ((v1, v2, v3, v4, v5) : CST.typeunionbody) =
  let v1 = (* "=" *) token env v1 in
  let v2 = map_typeexpr env v2 in
  let v3 = (* "or" *) token env v3 in
  let v4 =
    match v4 with
    | Some (v1, v2) ->
        R.Option
          (Some
             (let v1 = map_typeexpr env v1 in
              let v2 =
                R.List
                  (List.map
                     (fun (v1, v2) ->
                       let v1 = (* "or" *) token env v1 in
                       let v2 = map_typeexpr env v2 in
                       R.Tuple [ v1; v2 ])
                     v2)
              in
              R.Tuple [ v1; v2 ]))
    | None -> R.Option None
  in
  let v5 = (* ";" *) token env v5 in
  R.Tuple [ v1; v2; v3; v4; v5 ]

let map_annotation (env : env) (x : CST.annotation) =
  match x with
  | `Anno x -> R.Case ("Anno", map_annotname env x)
  | `Anno_LBRACK_anno_rep_COMMA_anno_RBRACK (v1, v2, v3, v4, v5) ->
      R.Case
        ( "Anno_LBRACK_anno_rep_COMMA_anno_RBRACK",
          let v1 = map_annotname env v1 in
          let v2 = (* "[" *) token env v2 in
          let v3 = map_annotarg env v3 in
          let v4 =
            R.List
              (List.map
                 (fun (v1, v2) ->
                   let v1 = (* "," *) token env v1 in
                   let v2 = map_annotarg env v2 in
                   R.Tuple [ v1; v2 ])
                 v4)
          in
          let v5 = (* "]" *) token env v5 in
          R.Tuple [ v1; v2; v3; v4; v5 ] )

let map_importdirective (env : env) ((v1, v2, v3) : CST.importdirective) =
  let v1 = (* "import" *) token env v1 in
  let v2 = map_importmoduleexpr env v2 in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        R.Option
          (Some
             (let v1 = (* "as" *) token env v1 in
              let v2 = map_modulename env v2 in
              R.Tuple [ v1; v2 ]))
    | None -> R.Option None
  in
  R.Tuple [ v1; v2; v3 ]

let map_field (env : env) ((v1, v2) : CST.field) =
  let v1 = map_vardecl env v1 in
  let v2 = (* ";" *) token env v2 in
  R.Tuple [ v1; v2 ]

let map_anon_vard_rep_COMMA_vard_76ab5f3 (env : env)
    ((v1, v2) : CST.anon_vard_rep_COMMA_vard_76ab5f3) =
  let v1 = map_vardecl env v1 in
  let v2 =
    R.List
      (List.map
         (fun (v1, v2) ->
           let v1 = (* "," *) token env v1 in
           let v2 = map_vardecl env v2 in
           R.Tuple [ v1; v2 ])
         v2)
  in
  R.Tuple [ v1; v2 ]

let rec map_anon_call_arg_rep_COMMA_call_arg_25882ee (env : env)
    ((v1, v2) : CST.anon_call_arg_rep_COMMA_call_arg_25882ee) =
  let v1 = map_call_arg env v1 in
  let v2 =
    R.List
      (List.map
         (fun (v1, v2) ->
           let v1 = (* "," *) token env v1 in
           let v2 = map_call_arg env v2 in
           R.Tuple [ v1; v2 ])
         v2)
  in
  R.Tuple [ v1; v2 ]

and map_asexpr (env : env) ((v1, v2) : CST.asexpr) =
  let v1 = map_exprorterm env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        R.Option
          (Some
             (let v1 = (* "as" *) token env v1 in
              let v2 = map_varname env v2 in
              R.Tuple [ v1; v2 ]))
    | None -> R.Option None
  in
  R.Tuple [ v1; v2 ]

and map_asexprs (env : env) ((v1, v2) : CST.asexprs) =
  let v1 = map_asexpr env v1 in
  let v2 =
    R.List
      (List.map
         (fun (v1, v2) ->
           let v1 = (* "," *) token env v1 in
           let v2 = map_asexpr env v2 in
           R.Tuple [ v1; v2 ])
         v2)
  in
  R.Tuple [ v1; v2 ]

and map_call_arg (env : env) (x : CST.call_arg) =
  match x with
  | `Expr x -> R.Case ("Expr", map_exprorterm env x)
  | `Unde tok -> R.Case ("Unde", (* "_" *) token env tok)

and map_call_or_unqual_agg_body (env : env) (x : CST.call_or_unqual_agg_body) =
  match x with
  | `Call_body (v1, v2, v3) ->
      R.Case
        ( "Call_body",
          let v1 = (* "(" *) token env v1 in
          let v2 =
            match v2 with
            | Some x ->
                R.Option
                  (Some (map_anon_call_arg_rep_COMMA_call_arg_25882ee env x))
            | None -> R.Option None
          in
          let v3 = (* ")" *) token env v3 in
          R.Tuple [ v1; v2; v3 ] )
  | `Unqual_agg_body (v1, v2, v3, v4, v5, v6) ->
      R.Case
        ( "Unqual_agg_body",
          let v1 = (* "(" *) token env v1 in
          let v2 =
            match v2 with
            | Some x ->
                R.Option (Some (map_anon_vard_rep_COMMA_vard_76ab5f3 env x))
            | None -> R.Option None
          in
          let v3 = (* "|" *) token env v3 in
          let v4 =
            match v4 with
            | Some x -> R.Option (Some (map_exprorterm env x))
            | None -> R.Option None
          in
          let v5 =
            match v5 with
            | Some (v1, v2) ->
                R.Option
                  (Some
                     (let v1 = (* "|" *) token env v1 in
                      let v2 = map_asexprs env v2 in
                      R.Tuple [ v1; v2 ]))
            | None -> R.Option None
          in
          let v6 = (* ")" *) token env v6 in
          R.Tuple [ v1; v2; v3; v4; v5; v6 ] )

and map_expr_aggregate_body (env : env) ((v1, v2) : CST.expr_aggregate_body) =
  let v1 = map_asexprs env v1 in
  let v2 =
    match v2 with
    | Some x -> R.Option (Some (map_orderbys env x))
    | None -> R.Option None
  in
  R.Tuple [ v1; v2 ]

and map_exprorterm (env : env) (x : CST.exprorterm) =
  match x with
  | `Spec_call (v1, v2, v3) ->
      R.Case
        ( "Spec_call",
          let v1 = (* "none" *) token env v1 in
          let v2 = (* "(" *) token env v2 in
          let v3 = (* ")" *) token env v3 in
          R.Tuple [ v1; v2; v3 ] )
  | `Prefix_cast (v1, v2, v3, v4) ->
      R.Case
        ( "Prefix_cast",
          let v1 = (* "(" *) token env v1 in
          let v2 = map_typeexpr env v2 in
          let v3 = (* ")" *) token env v3 in
          let v4 = map_exprorterm env v4 in
          R.Tuple [ v1; v2; v3; v4 ] )
  | `Prim x -> R.Case ("Prim", map_primary env x)
  | `Un_expr (v1, v2) ->
      R.Case
        ( "Un_expr",
          let v1 = map_unop env v1 in
          let v2 = map_exprorterm env v2 in
          R.Tuple [ v1; v2 ] )
  | `Mul_expr (v1, v2, v3) ->
      R.Case
        ( "Mul_expr",
          let v1 = map_exprorterm env v1 in
          let v2 = map_mulop env v2 in
          let v3 = map_exprorterm env v3 in
          R.Tuple [ v1; v2; v3 ] )
  | `Add_expr (v1, v2, v3) ->
      R.Case
        ( "Add_expr",
          let v1 = map_exprorterm env v1 in
          let v2 = map_addop env v2 in
          let v3 = map_exprorterm env v3 in
          R.Tuple [ v1; v2; v3 ] )
  | `In_expr (v1, v2, v3) ->
      R.Case
        ( "In_expr",
          let v1 = map_exprorterm env v1 in
          let v2 = (* "in" *) token env v2 in
          let v3 = map_primary env v3 in
          R.Tuple [ v1; v2; v3 ] )
  | `Comp_term (v1, v2, v3) ->
      R.Case
        ( "Comp_term",
          let v1 = map_exprorterm env v1 in
          let v2 = map_compop env v2 in
          let v3 = map_exprorterm env v3 in
          R.Tuple [ v1; v2; v3 ] )
  | `Inst_of (v1, v2, v3) ->
      R.Case
        ( "Inst_of",
          let v1 = map_exprorterm env v1 in
          let v2 = (* "instanceof" *) token env v2 in
          let v3 = map_typeexpr env v3 in
          R.Tuple [ v1; v2; v3 ] )
  | `Nega (v1, v2) ->
      R.Case
        ( "Nega",
          let v1 = (* "not" *) token env v1 in
          let v2 = map_exprorterm env v2 in
          R.Tuple [ v1; v2 ] )
  | `If_term (v1, v2, v3, v4, v5, v6) ->
      R.Case
        ( "If_term",
          let v1 = (* "if" *) token env v1 in
          let v2 = map_exprorterm env v2 in
          let v3 = (* "then" *) token env v3 in
          let v4 = map_exprorterm env v4 in
          let v5 = (* "else" *) token env v5 in
          let v6 = map_exprorterm env v6 in
          R.Tuple [ v1; v2; v3; v4; v5; v6 ] )
  | `Conj (v1, v2, v3) ->
      R.Case
        ( "Conj",
          let v1 = map_exprorterm env v1 in
          let v2 = (* "and" *) token env v2 in
          let v3 = map_exprorterm env v3 in
          R.Tuple [ v1; v2; v3 ] )
  | `Disj (v1, v2, v3) ->
      R.Case
        ( "Disj",
          let v1 = map_exprorterm env v1 in
          let v2 = (* "or" *) token env v2 in
          let v3 = map_exprorterm env v3 in
          R.Tuple [ v1; v2; v3 ] )
  | `Impl (v1, v2, v3) ->
      R.Case
        ( "Impl",
          let v1 = map_exprorterm env v1 in
          let v2 = (* "implies" *) token env v2 in
          let v3 = map_exprorterm env v3 in
          R.Tuple [ v1; v2; v3 ] )
  | `Quan (v1, v2, v3, v4) ->
      R.Case
        ( "Quan",
          let v1 = map_quantifier env v1 in
          let v2 = (* "(" *) token env v2 in
          let v3 =
            match v3 with
            | `Opt_vard_rep_COMMA_vard_opt_BAR_expr_opt_BAR_expr (v1, v2) ->
                R.Case
                  ( "Opt_vard_rep_COMMA_vard_opt_BAR_expr_opt_BAR_expr",
                    let v1 =
                      match v1 with
                      | Some x ->
                          R.Option
                            (Some (map_anon_vard_rep_COMMA_vard_76ab5f3 env x))
                      | None -> R.Option None
                    in
                    let v2 =
                      match v2 with
                      | Some (v1, v2, v3) ->
                          R.Option
                            (Some
                               (let v1 = (* "|" *) token env v1 in
                                let v2 = map_exprorterm env v2 in
                                let v3 =
                                  match v3 with
                                  | Some (v1, v2) ->
                                      R.Option
                                        (Some
                                           (let v1 = (* "|" *) token env v1 in
                                            let v2 = map_exprorterm env v2 in
                                            R.Tuple [ v1; v2 ]))
                                  | None -> R.Option None
                                in
                                R.Tuple [ v1; v2; v3 ]))
                      | None -> R.Option None
                    in
                    R.Tuple [ v1; v2 ] )
            | `Expr x -> R.Case ("Expr", map_exprorterm env x)
          in
          let v4 = (* ")" *) token env v4 in
          R.Tuple [ v1; v2; v3; v4 ] )

and map_full_aggregate_body (env : env) (x : CST.full_aggregate_body) =
  match x with
  | `Opt_vard_rep_COMMA_vard_BAR_opt_expr_opt_BAR_asexprs_opt_orders
      (v1, v2, v3, v4) ->
      R.Case
        ( "Opt_vard_rep_COMMA_vard_BAR_opt_expr_opt_BAR_asexprs_opt_orders",
          let v1 =
            match v1 with
            | Some x ->
                R.Option (Some (map_anon_vard_rep_COMMA_vard_76ab5f3 env x))
            | None -> R.Option None
          in
          let v2 = (* "|" *) token env v2 in
          let v3 =
            match v3 with
            | Some x -> R.Option (Some (map_exprorterm env x))
            | None -> R.Option None
          in
          let v4 =
            match v4 with
            | Some (v1, v2, v3) ->
                R.Option
                  (Some
                     (let v1 = (* "|" *) token env v1 in
                      let v2 = map_asexprs env v2 in
                      let v3 =
                        match v3 with
                        | Some x -> R.Option (Some (map_orderbys env x))
                        | None -> R.Option None
                      in
                      R.Tuple [ v1; v2; v3 ]))
            | None -> R.Option None
          in
          R.Tuple [ v1; v2; v3; v4 ] )
  | `Vard_rep_COMMA_vard x ->
      R.Case ("Vard_rep_COMMA_vard", map_anon_vard_rep_COMMA_vard_76ab5f3 env x)

and map_orderby (env : env) ((v1, v2) : CST.orderby) =
  let v1 = map_exprorterm env v1 in
  let v2 =
    match v2 with
    | Some x -> R.Option (Some (map_direction env x))
    | None -> R.Option None
  in
  R.Tuple [ v1; v2 ]

and map_orderbys (env : env) ((v1, v2, v3, v4) : CST.orderbys) =
  let v1 = (* "order" *) token env v1 in
  let v2 = (* "by" *) token env v2 in
  let v3 = map_orderby env v3 in
  let v4 =
    R.List
      (List.map
         (fun (v1, v2) ->
           let v1 = (* "," *) token env v1 in
           let v2 = map_orderby env v2 in
           R.Tuple [ v1; v2 ])
         v4)
  in
  R.Tuple [ v1; v2; v3; v4 ]

and map_par_expr (env : env) (x : CST.par_expr) =
  match x with
  | `LPAR_simp_semg_meta_RPAR (v1, v2, v3, v4) ->
      R.Case
        ( "LPAR_simp_semg_meta_RPAR",
          let v1 = (* "(" *) token env v1 in
          let v2 = map_modulename env v2 in
          let v3 = (* semgrep_metavariable *) token env v3 in
          let v4 = (* ")" *) token env v4 in
          R.Tuple [ v1; v2; v3; v4 ] )
  | `LPAR_expr_RPAR (v1, v2, v3) ->
      R.Case
        ( "LPAR_expr_RPAR",
          let v1 = (* "(" *) token env v1 in
          let v2 = map_exprorterm env v2 in
          let v3 = (* ")" *) token env v3 in
          R.Tuple [ v1; v2; v3 ] )

and map_primary (env : env) (x : CST.primary) =
  match x with
  | `Semg_ellips tok -> R.Case ("Semg_ellips", (* "..." *) token env tok)
  | `Semg_ellips_meta tok ->
      R.Case
        ( "Semg_ellips_meta",
          (* pattern \$\.\.\.[a-zA-Z_][a-zA-Z_0-9]* *) token env tok )
  | `Call_or_unqual_agg_expr (v1, v2, v3) ->
      R.Case
        ( "Call_or_unqual_agg_expr",
          let v1 = map_aritylesspredicateexpr env v1 in
          let v2 =
            match v2 with
            | Some x -> R.Option (Some (map_closure env x))
            | None -> R.Option None
          in
          let v3 = map_call_or_unqual_agg_body env v3 in
          R.Tuple [ v1; v2; v3 ] )
  | `Qual_expr (v1, v2, v3) ->
      R.Case
        ( "Qual_expr",
          let v1 = map_primary env v1 in
          let v2 = (* "." *) token env v2 in
          let v3 = map_qualifiedrhs env v3 in
          R.Tuple [ v1; v2; v3 ] )
  | `Lit x -> R.Case ("Lit", map_literal env x)
  | `Var x -> R.Case ("Var", map_variable env x)
  | `Super_ref (v1, v2) ->
      R.Case
        ( "Super_ref",
          let v1 =
            match v1 with
            | Some (v1, v2) ->
                R.Option
                  (Some
                     (let v1 = map_typeexpr env v1 in
                      let v2 = (* "." *) token env v2 in
                      R.Tuple [ v1; v2 ]))
            | None -> R.Option None
          in
          let v2 = (* "super" *) token env v2 in
          R.Tuple [ v1; v2 ] )
  | `Aggr (v1, v2, v3, v4, v5) ->
      R.Case
        ( "Aggr",
          let v1 = map_aggid env v1 in
          let v2 =
            match v2 with
            | Some (v1, v2, v3, v4) ->
                R.Option
                  (Some
                     (let v1 = (* "[" *) token env v1 in
                      let v2 = map_exprorterm env v2 in
                      let v3 =
                        R.List
                          (List.map
                             (fun (v1, v2) ->
                               let v1 = (* "," *) token env v1 in
                               let v2 = map_exprorterm env v2 in
                               R.Tuple [ v1; v2 ])
                             v3)
                      in
                      let v4 = (* "]" *) token env v4 in
                      R.Tuple [ v1; v2; v3; v4 ]))
            | None -> R.Option None
          in
          let v3 = (* "(" *) token env v3 in
          let v4 =
            match v4 with
            | Some x ->
                R.Option
                  (Some
                     (match x with
                     | `Full_aggr_body x ->
                         R.Case ("Full_aggr_body", map_full_aggregate_body env x)
                     | `Expr_aggr_body x ->
                         R.Case ("Expr_aggr_body", map_expr_aggregate_body env x)))
            | None -> R.Option None
          in
          let v5 = (* ")" *) token env v5 in
          R.Tuple [ v1; v2; v3; v4; v5 ] )
  | `Range (v1, v2, v3, v4, v5) ->
      R.Case
        ( "Range",
          let v1 = (* "[" *) token env v1 in
          let v2 = map_exprorterm env v2 in
          let v3 = (* ".." *) token env v3 in
          let v4 = map_exprorterm env v4 in
          let v5 = (* "]" *) token env v5 in
          R.Tuple [ v1; v2; v3; v4; v5 ] )
  | `Set_lit (v1, v2, v3, v4) ->
      R.Case
        ( "Set_lit",
          let v1 = (* "[" *) token env v1 in
          let v2 =
            match v2 with
            | Some (v1, v2) ->
                R.Option
                  (Some
                     (let v1 = map_exprorterm env v1 in
                      let v2 =
                        R.List
                          (List.map
                             (fun (v1, v2) ->
                               let v1 = (* "," *) token env v1 in
                               let v2 = map_exprorterm env v2 in
                               R.Tuple [ v1; v2 ])
                             v2)
                      in
                      R.Tuple [ v1; v2 ]))
            | None -> R.Option None
          in
          let v3 =
            match v3 with
            | Some tok -> R.Option (Some ((* "," *) token env tok))
            | None -> R.Option None
          in
          let v4 = (* "]" *) token env v4 in
          R.Tuple [ v1; v2; v3; v4 ] )
  | `Par_expr x -> R.Case ("Par_expr", map_par_expr env x)
  | `Expr_anno (v1, v2, v3, v4, v5, v6, v7) ->
      R.Case
        ( "Expr_anno",
          let v1 = map_annotname env v1 in
          let v2 = (* "[" *) token env v2 in
          let v3 = map_annotname env v3 in
          let v4 = (* "]" *) token env v4 in
          let v5 = (* "(" *) token env v5 in
          let v6 = map_exprorterm env v6 in
          let v7 = (* ")" *) token env v7 in
          R.Tuple [ v1; v2; v3; v4; v5; v6; v7 ] )

and map_qualifiedrhs (env : env) (x : CST.qualifiedrhs) =
  match x with
  | `Pred_opt_clos_LPAR_opt_call_arg_rep_COMMA_call_arg_RPAR (v1, v2, v3, v4, v5)
    ->
      R.Case
        ( "Pred_opt_clos_LPAR_opt_call_arg_rep_COMMA_call_arg_RPAR",
          let v1 = map_predicatename env v1 in
          let v2 =
            match v2 with
            | Some x -> R.Option (Some (map_closure env x))
            | None -> R.Option None
          in
          let v3 = (* "(" *) token env v3 in
          let v4 =
            match v4 with
            | Some x ->
                R.Option
                  (Some (map_anon_call_arg_rep_COMMA_call_arg_25882ee env x))
            | None -> R.Option None
          in
          let v5 = (* ")" *) token env v5 in
          R.Tuple [ v1; v2; v3; v4; v5 ] )
  | `LPAR_type_RPAR (v1, v2, v3) ->
      R.Case
        ( "LPAR_type_RPAR",
          let v1 = (* "(" *) token env v1 in
          let v2 = map_typeexpr env v2 in
          let v3 = (* ")" *) token env v3 in
          R.Tuple [ v1; v2; v3 ] )

let map_select (env : env) ((v1, v2, v3, v4, v5) : CST.select) =
  let v1 =
    match v1 with
    | Some (v1, v2) ->
        R.Option
          (Some
             (let v1 = (* "from" *) token env v1 in
              let v2 =
                match v2 with
                | Some x ->
                    R.Option (Some (map_anon_vard_rep_COMMA_vard_76ab5f3 env x))
                | None -> R.Option None
              in
              R.Tuple [ v1; v2 ]))
    | None -> R.Option None
  in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        R.Option
          (Some
             (let v1 = (* "where" *) token env v1 in
              let v2 = map_exprorterm env v2 in
              R.Tuple [ v1; v2 ]))
    | None -> R.Option None
  in
  let v3 = (* "select" *) token env v3 in
  let v4 = map_asexprs env v4 in
  let v5 =
    match v5 with
    | Some x -> R.Option (Some (map_orderbys env x))
    | None -> R.Option None
  in
  R.Tuple [ v1; v2; v3; v4; v5 ]

let map_charpred (env : env) ((v1, v2, v3, v4, v5, v6) : CST.charpred) =
  let v1 = map_classname env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = (* ")" *) token env v3 in
  let v4 = (* "{" *) token env v4 in
  let v5 = map_exprorterm env v5 in
  let v6 = (* "}" *) token env v6 in
  R.Tuple [ v1; v2; v3; v4; v5; v6 ]

let map_body (env : env) ((v1, v2, v3) : CST.body) =
  let v1 = (* "{" *) token env v1 in
  let v2 = map_exprorterm env v2 in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [ v1; v2; v3 ]

let map_optbody (env : env) (x : CST.optbody) =
  match x with
  | `Empty tok -> R.Case ("Empty", (* ";" *) token env tok)
  | `Body x -> R.Case ("Body", map_body env x)
  | `High (v1, v2, v3, v4, v5, v6, v7, v8) ->
      R.Case
        ( "High",
          let v1 = (* "=" *) token env v1 in
          let v2 = map_literalid env v2 in
          let v3 = (* "(" *) token env v3 in
          let v4 =
            match v4 with
            | Some (v1, v2) ->
                R.Option
                  (Some
                     (let v1 = map_predicateexpr env v1 in
                      let v2 =
                        R.List
                          (List.map
                             (fun (v1, v2) ->
                               let v1 = (* "," *) token env v1 in
                               let v2 = map_predicateexpr env v2 in
                               R.Tuple [ v1; v2 ])
                             v2)
                      in
                      R.Tuple [ v1; v2 ]))
            | None -> R.Option None
          in
          let v5 = (* ")" *) token env v5 in
          let v6 = (* "(" *) token env v6 in
          let v7 =
            match v7 with
            | Some x ->
                R.Option
                  (Some (map_anon_call_arg_rep_COMMA_call_arg_25882ee env x))
            | None -> R.Option None
          in
          let v8 = (* ")" *) token env v8 in
          R.Tuple [ v1; v2; v3; v4; v5; v6; v7; v8 ] )

let map_datatypebranch (env : env)
    ((v1, v2, v3, v4, v5, v6, v7) : CST.datatypebranch) =
  let v1 =
    match v1 with
    | Some tok ->
        R.Option
          (Some ((* pattern \/\*\*[^*]*\*+([^/*][^*]*\*+)*\/ *) token env tok))
    | None -> R.Option None
  in
  let v2 =
    match v2 with
    | Some x -> R.Option (Some (map_annotation env x))
    | None -> R.Option None
  in
  let v3 = map_classname env v3 in
  let v4 = (* "(" *) token env v4 in
  let v5 =
    match v5 with
    | Some x -> R.Option (Some (map_anon_vard_rep_COMMA_vard_76ab5f3 env x))
    | None -> R.Option None
  in
  let v6 = (* ")" *) token env v6 in
  let v7 =
    match v7 with
    | Some x -> R.Option (Some (map_body env x))
    | None -> R.Option None
  in
  R.Tuple [ v1; v2; v3; v4; v5; v6; v7 ]

let map_memberpredicate (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.memberpredicate) =
  let v1 = map_anon_choice_pred_44fe1b2 env v1 in
  let v2 = map_predicatename env v2 in
  let v3 = (* "(" *) token env v3 in
  let v4 =
    match v4 with
    | Some x -> R.Option (Some (map_anon_vard_rep_COMMA_vard_76ab5f3 env x))
    | None -> R.Option None
  in
  let v5 = (* ")" *) token env v5 in
  let v6 = map_optbody env v6 in
  R.Tuple [ v1; v2; v3; v4; v5; v6 ]

let map_classlesspredicate (env : env) ((v1, v2, v3) : CST.classlesspredicate) =
  let v1 = map_anon_choice_pred_44fe1b2 env v1 in
  let v2 = map_predicatename env v2 in
  let v3 =
    match v3 with
    | `LPAR_opt_vard_rep_COMMA_vard_RPAR_optb (v1, v2, v3, v4) ->
        R.Case
          ( "LPAR_opt_vard_rep_COMMA_vard_RPAR_optb",
            let v1 = (* "(" *) token env v1 in
            let v2 =
              match v2 with
              | Some x ->
                  R.Option (Some (map_anon_vard_rep_COMMA_vard_76ab5f3 env x))
              | None -> R.Option None
            in
            let v3 = (* ")" *) token env v3 in
            let v4 = map_optbody env v4 in
            R.Tuple [ v1; v2; v3; v4 ] )
    | `Pred x -> R.Case ("Pred", map_predicatealiasbody env x)
  in
  R.Tuple [ v1; v2; v3 ]

let map_datatypebranches (env : env) ((v1, v2) : CST.datatypebranches) =
  let v1 = map_datatypebranch env v1 in
  let v2 =
    R.List
      (List.map
         (fun (v1, v2) ->
           let v1 = (* "or" *) token env v1 in
           let v2 = map_datatypebranch env v2 in
           R.Tuple [ v1; v2 ])
         v2)
  in
  R.Tuple [ v1; v2 ]

let map_classmember (env : env) (x : CST.classmember) =
  match x with
  | `Semg_ellips tok -> R.Case ("Semg_ellips", (* "..." *) token env tok)
  | `Rep_anno_choice_char (v1, v2) ->
      R.Case
        ( "Rep_anno_choice_char",
          let v1 = R.List (List.map (map_annotation env) v1) in
          let v2 =
            match v2 with
            | `Char x -> R.Case ("Char", map_charpred env x)
            | `Memb x -> R.Case ("Memb", map_memberpredicate env x)
            | `Field x -> R.Case ("Field", map_field env x)
          in
          R.Tuple [ v1; v2 ] )
  | `Qldoc tok ->
      R.Case
        ("Qldoc", (* pattern \/\*\*[^*]*\*+([^/*][^*]*\*+)*\/ *) token env tok)

let map_datatype (env : env) ((v1, v2, v3, v4) : CST.datatype) =
  let v1 = (* "newtype" *) token env v1 in
  let v2 = map_classname env v2 in
  let v3 = (* "=" *) token env v3 in
  let v4 = map_datatypebranches env v4 in
  R.Tuple [ v1; v2; v3; v4 ]

let map_dataclass (env : env) ((v1, v2, v3) : CST.dataclass) =
  let v1 = (* "class" *) token env v1 in
  let v2 = map_classname env v2 in
  let v3 =
    match v3 with
    | `Opt_extends_type_rep_COMMA_type_opt_inst_type_rep_COMMA_type_choice_LCURL_rep_clas_RCURL
        (v1, v2, v3) ->
        R.Case
          ( "Opt_extends_type_rep_COMMA_type_opt_inst_type_rep_COMMA_type_choice_LCURL_rep_clas_RCURL",
            let v1 =
              match v1 with
              | Some (v1, v2, v3) ->
                  R.Option
                    (Some
                       (let v1 = (* "extends" *) token env v1 in
                        let v2 = map_typeexpr env v2 in
                        let v3 =
                          R.List
                            (List.map
                               (fun (v1, v2) ->
                                 let v1 = (* "," *) token env v1 in
                                 let v2 = map_typeexpr env v2 in
                                 R.Tuple [ v1; v2 ])
                               v3)
                        in
                        R.Tuple [ v1; v2; v3 ]))
              | None -> R.Option None
            in
            let v2 =
              match v2 with
              | Some (v1, v2, v3) ->
                  R.Option
                    (Some
                       (let v1 = (* "instanceof" *) token env v1 in
                        let v2 = map_typeexpr env v2 in
                        let v3 =
                          R.List
                            (List.map
                               (fun (v1, v2) ->
                                 let v1 = (* "," *) token env v1 in
                                 let v2 = map_typeexpr env v2 in
                                 R.Tuple [ v1; v2 ])
                               v3)
                        in
                        R.Tuple [ v1; v2; v3 ]))
              | None -> R.Option None
            in
            let v3 =
              match v3 with
              | `LCURL_rep_clas_RCURL (v1, v2, v3) ->
                  R.Case
                    ( "LCURL_rep_clas_RCURL",
                      let v1 = (* "{" *) token env v1 in
                      let v2 = R.List (List.map (map_classmember env) v2) in
                      let v3 = (* "}" *) token env v3 in
                      R.Tuple [ v1; v2; v3 ] )
              | `SEMI tok -> R.Case ("SEMI", (* ";" *) token env tok)
            in
            R.Tuple [ v1; v2; v3 ] )
    | `Type_e6aca0c x -> R.Case ("Type_e6aca0c", map_typealiasbody env x)
    | `Type_9cc1977 x -> R.Case ("Type_9cc1977", map_typeunionbody env x)
  in
  R.Tuple [ v1; v2; v3 ]

let rec map_module_ (env : env) ((v1, v2, v3, v4, v5) : CST.module_) =
  let v1 = (* "module" *) token env v1 in
  let v2 = map_modulename env v2 in
  let v3 =
    match v3 with
    | Some (v1, v2, v3, v4) ->
        R.Option
          (Some
             (let v1 = (* "<" *) token env v1 in
              let v2 = map_moduleparam env v2 in
              let v3 =
                R.List
                  (List.map
                     (fun (v1, v2) ->
                       let v1 = (* "," *) token env v1 in
                       let v2 = map_moduleparam env v2 in
                       R.Tuple [ v1; v2 ])
                     v3)
              in
              let v4 = (* ">" *) token env v4 in
              R.Tuple [ v1; v2; v3; v4 ]))
    | None -> R.Option None
  in
  let v4 =
    match v4 with
    | Some (v1, v2, v3) ->
        R.Option
          (Some
             (let v1 = (* "implements" *) token env v1 in
              let v2 = map_signatureexpr env v2 in
              let v3 =
                R.List
                  (List.map
                     (fun (v1, v2) ->
                       let v1 = (* "," *) token env v1 in
                       let v2 = map_signatureexpr env v2 in
                       R.Tuple [ v1; v2 ])
                     v3)
              in
              R.Tuple [ v1; v2; v3 ]))
    | None -> R.Option None
  in
  let v5 =
    match v5 with
    | `LCURL_rep_modu_RCURL (v1, v2, v3) ->
        R.Case
          ( "LCURL_rep_modu_RCURL",
            let v1 = (* "{" *) token env v1 in
            let v2 = R.List (List.map (map_modulemember env) v2) in
            let v3 = (* "}" *) token env v3 in
            R.Tuple [ v1; v2; v3 ] )
    | `Modu x -> R.Case ("Modu", map_modulealiasbody env x)
  in
  R.Tuple [ v1; v2; v3; v4; v5 ]

and map_modulemember (env : env) (x : CST.modulemember) =
  match x with
  | `Semg_ellips tok -> R.Case ("Semg_ellips", (* "..." *) token env tok)
  | `Rep_anno_choice_impo (v1, v2) ->
      R.Case
        ( "Rep_anno_choice_impo",
          let v1 = R.List (List.map (map_annotation env) v1) in
          let v2 =
            match v2 with
            | `Impo x -> R.Case ("Impo", map_importdirective env x)
            | `Clas x -> R.Case ("Clas", map_classlesspredicate env x)
            | `Data_cb44f91 x -> R.Case ("Data_cb44f91", map_dataclass env x)
            | `Data_3931108 x -> R.Case ("Data_3931108", map_datatype env x)
            | `Select x -> R.Case ("Select", map_select env x)
            | `Module x -> R.Case ("Module", map_module_ env x)
          in
          R.Tuple [ v1; v2 ] )
  | `Qldoc tok ->
      R.Case
        ("Qldoc", (* pattern \/\*\*[^*]*\*+([^/*][^*]*\*+)*\/ *) token env tok)

let map_ql (env : env) (x : CST.ql) =
  match x with
  | `Semg_exp (v1, v2) ->
      R.Case
        ( "Semg_exp",
          let v1 = (* "__SEMGREP_EXPRESSION" *) token env v1 in
          let v2 = map_exprorterm env v2 in
          R.Tuple [ v1; v2 ] )
  | `Rep_modu xs ->
      R.Case ("Rep_modu", R.List (List.map (map_modulemember env) xs))

let dump_tree root =
  map_ql () root |> Tree_sitter_run.Raw_tree.to_string |> print_string
