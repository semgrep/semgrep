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
open Common
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
let name_of_id id = { name_last = (id, None); name_middle = [] }

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-ql/Boilerplate.ml *)

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

let map_quantifier (env : env) (x : CST.quantifier) =
  match x with
  | `Exists tok -> (Exists, token env tok)
  | `Forall tok -> (Forall, token env tok)
  | `Forex tok -> (Forex, token env tok)

let _map_qldoc (env : env) (tok : CST.qldoc) =
  (* pattern \/\*\*[^*]*\*+([^/*][^*]*\*+)*\/ *) token env tok

let map_integer (env : env) (tok : CST.integer) =
  Int (Parsed_int.parse ((* pattern [0-9]+ *) str env tok))

let map_addop (env : env) (x : CST.addop) =
  match x with
  | `PLUS tok -> (Add, token env tok)
  | `DASH tok -> (Sub, token env tok)

let map_direction (env : env) (x : CST.direction) =
  match x with
  | `Asc tok -> (Asc, token env tok)
  | `Desc tok -> (Desc, token env tok)

let map_closure (env : env) (x : CST.closure) =
  match x with
  | `STAR tok -> token env tok
  | `PLUS tok -> token env tok

let map_unop (env : env) (x : CST.unop) =
  match x with
  | `PLUS tok -> (UAdd, token env tok)
  | `DASH tok -> (USub, token env tok)

let map_compop (env : env) (x : CST.compop) =
  match x with
  | `EQ tok -> (Eq, token env tok)
  | `BANGEQ tok -> (NotEq, token env tok)
  | `LT tok -> (Lt, token env tok)
  | `GT tok -> (Gt, token env tok)
  | `LTEQ tok -> (LtEq, token env tok)
  | `GTEQ tok -> (GtEq, token env tok)

let map_primitivetype (env : env) (x : CST.primitivetype) =
  match x with
  | `Bool tok -> str env tok
  | `Date tok -> str env tok
  | `Float tok -> str env tok
  | `Int tok -> str env tok
  | `Str tok -> str env tok

let map_mulop (env : env) (x : CST.mulop) =
  match x with
  | `STAR tok -> (Mult, token env tok)
  | `SLASH tok -> (Div, token env tok)
  | `PERC tok -> (Mod, token env tok)

let map_pat_3bf1220 (env : env) (tok : CST.pat_3bf1220) : ident =
  (* pattern [A-Z][A-Za-z0-9_]* *) str env tok

let map_aggid (env : env) (x : CST.aggid) =
  match x with
  | `Avg tok -> str env tok
  | `Concat tok -> str env tok
  | `Stri_18c266c tok -> str env tok
  | `Count tok -> str env tok
  | `Max tok -> str env tok
  | `Min tok -> str env tok
  | `Rank tok -> str env tok
  | `Stri_8bc2381 tok -> str env tok
  | `Stri_a43e15b tok -> str env tok
  | `Sum tok -> str env tok
  | `Any tok -> str env tok
  | `Unique tok -> str env tok
  | `Semg_meta tok -> str env tok

let map_bool_ (env : env) (x : CST.bool_) =
  match x with
  | `True tok -> (true, token env tok)
  | `False tok -> (false, token env tok)

let map_annotname (env : env) (x : CST.annotname) =
  match x with
  | `Lower_id tok -> str env tok
  | `Semg_meta tok -> str env tok

let map_predicatename (env : env) (x : CST.predicatename) =
  match x with
  | `Lower_id tok -> str env tok
  | `Semg_meta tok -> str env tok

let map_upper_id (env : env) (x : CST.upper_id) =
  match x with
  | `Pat_3bf1220 x -> map_pat_3bf1220 env x
  | `Semg_meta tok -> str env tok

let map_literal (env : env) (x : CST.literal) : literal =
  match x with
  | `Int tok -> map_integer env tok
  | `Float tok ->
      let s, t = str env tok in
      Float (float_of_string_opt s, t)
  | `Bool x -> Bool (map_bool_ env x)
  | `Str tok ->
      let s, t = str env tok in
      (* Remove leading and trailing backticks. The grammar guarantees that raw
         * string literals will always have leading and trailing backticks, so this
         * String.sub call should be safe. Let's check just to be sure. *)
      if
        not
          (String.length s >= 2
          && String.get s 0 =*= '"'
          && String.get s (String.length s - 1) =*= '"')
      then
        failwith @@ "Found unexpected raw string literal without delimiters: "
        ^ s;
      let s = String.sub s 1 (String.length s - 2) in
      String (s, token env tok)

let map_classname (env : env) (x : CST.classname) = map_upper_id env x

let map_simpleid (env : env) (x : CST.simpleid) =
  match x with
  | `Lower_id tok -> str env tok
  | `Upper_id x -> map_classname env x

let map_literalid (env : env) (x : CST.literalid) =
  match x with
  | `Lower_id tok -> str env tok
  | `Upper_id x -> map_classname env x

let map_varname (env : env) (x : CST.varname) = map_simpleid env x
let map_modulename (env : env) (x : CST.modulename) = map_simpleid env x

let map_variable (env : env) (x : CST.variable) =
  match x with
  | `This tok -> IdSpecial (This, token env tok)
  | `Result tok -> IdSpecial (Result, token env tok)
  | `Varn x -> N (Id (map_varname env x))

let rec map_aritylesspredicateexpr (env : env)
    ((v1, v2) : CST.aritylesspredicateexpr) : qualified_info =
  let id = map_literalid env v2 in
  match v1 with
  | Some (v1, v2) ->
      let v1 = map_moduleexpr ~last:(id, None) env v1 in
      let v2 = (* "::" *) token env v2 in
      v1
  | None -> name_of_id id

and map_moduleexpr ?last (env : env) (x : CST.moduleexpr) : qualified_info =
  match x with
  | `Simp x -> (
      let id = map_modulename env x in
      match last with
      | None -> name_of_id id
      | Some (id, tyargs) ->
          { name_last = (id, tyargs); name_middle = [ (id, None) ] })
  | `Modu x -> { name_last = map_moduleinstantiation env x; name_middle = [] }
  | `Modu_COLONCOLON_choice_simp (v1, v2, v3) -> (
      (* doing this first so we can pass it back down to map_moduleexpr*)
      let v3 =
        match v3 with
        | `Simp x -> (map_modulename env x, None)
        | `Modu x -> map_moduleinstantiation env x
      in
      let v1 = map_moduleexpr ~last:v3 env v1 in
      let _v2 = (* "::" *) token env v2 in
      match last with
      | None -> v1
      | Some (id, tyargs) ->
          { v1 with name_middle = (id, tyargs) :: v1.name_middle })

and map_moduleinstantiation (env : env)
    ((v1, v2, v3, v4, v5) : CST.moduleinstantiation) :
    ident * type_arguments option =
  let v1 = map_modulename env v1 in
  let v2 = (* "<" *) token env v2 in
  let v3 = map_signatureexpr env v3 in
  let v4 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_signatureexpr env v2 in
        v2)
      v4
  in
  let v5 = (* ">" *) token env v5 in
  (v1, Some (v3 :: v4))

and map_predicateexpr (env : env) ((v1, v2, v3) : CST.predicateexpr) :
    qualified_info * Parsed_int.t =
  let v1 = map_aritylesspredicateexpr env v1 in
  let v2 = (* "/" *) token env v2 in
  let v3 = Parsed_int.parse ((* pattern [0-9]+ *) str env v3) in
  (v1, v3)

and map_signatureexpr (env : env) (x : CST.signatureexpr) : type_argument =
  match x with
  | `Type x -> (map_typeexpr env x, None)
  | `Modu x -> (IdQualified (map_moduleexpr env x), None)
  | `Pred x ->
      let v1, v2 = map_predicateexpr env x in
      (IdQualified v1, Some v2)

and map_typeexpr (env : env) (x : CST.typeexpr) : name =
  match x with
  | `Opt_modu_COLONCOLON_clas (v1, v2) -> (
      let id = map_classname env v2 in
      match v1 with
      | Some (v1, v2) ->
          let v1 = map_moduleexpr ~last:(id, None) env v1 in
          let v2 = (* "::" *) token env v2 in
          IdQualified v1
      | None -> Id id)
  | `Dbtype tok -> Id (str env tok)
  | `Prim x -> Id (map_primitivetype env x)

let map_annotarg (env : env) (x : CST.annotarg) : expr =
  match x with
  | `Simp x -> N (Id (map_modulename env x))
  | `This tok -> IdSpecial (This, token env tok)
  | `Result tok -> IdSpecial (Result, token env tok)

let map_importmoduleexpr (env : env) ((v1, v2) : CST.importmoduleexpr) =
  let v1 =
    List_.map
      (fun (v1, v2) ->
        let v1 = map_modulename env v1 in
        let v2 = (* "." *) token env v2 in
        v1)
      v1
  in
  let v2 = map_moduleexpr env v2 in
  IdQualified
    {
      v2 with
      name_middle = List_.map (fun id -> (id, None)) v1 @ v2.name_middle;
    }

let map_predicatealiasbody (env : env) ((v1, v2, v3) : CST.predicatealiasbody) =
  let v1 = (* "=" *) token env v1 in
  let v2 = map_predicateexpr env v2 in
  let v3 = (* ";" *) token env v3 in
  v2

let map_vardecl (env : env) (x : CST.vardecl) =
  match x with
  | `Semg_ellips tok -> VardeclEllipsis ((* "..." *) token env tok)
  | `Type_varn (v1, v2) ->
      let v1 = map_typeexpr env v1 in
      let v2 = map_varname env v2 in
      VardeclInit (v1, v2)

let map_moduleparam (env : env) ((v1, v2) : CST.moduleparam) :
    type_argument * ident =
  let v1 = map_signatureexpr env v1 in
  let v2 = map_modulename env v2 in
  (v1, v2)

let map_anon_choice_pred_44fe1b2 (env : env) (x : CST.anon_choice_pred_44fe1b2)
    =
  match x with
  | `Pred tok ->
      let _v1 = (* predicate *) token env tok in
      None
  | `Type x -> Some (map_typeexpr env x)

let map_modulealiasbody (env : env) ((v1, v2, v3) : CST.modulealiasbody) =
  let v1 = (* "=" *) token env v1 in
  let v2 = map_moduleexpr env v2 in
  let v3 = (* ";" *) token env v3 in
  v2

let map_typealiasbody (env : env) ((v1, v2, v3) : CST.typealiasbody) =
  let v1 = (* "=" *) token env v1 in
  let v2 = map_typeexpr env v2 in
  let v3 = (* ";" *) token env v3 in
  v2

let map_typeunionbody ~tok ~name (env : env)
    ((v1, v2, v3, v4, v5) : CST.typeunionbody) : stmt =
  let v1 = (* "=" *) token env v1 in
  let v2 = map_typeexpr env v2 in
  let v3 = (* "or" *) token env v3 in
  let v4 =
    match v4 with
    | Some (v1, v2) ->
        let v1 = map_typeexpr env v1 in
        let v2 =
          List_.map
            (fun (v1, v2) ->
              let v1 = (* "or" *) token env v1 in
              let v2 = map_typeexpr env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v5 = (* ";" *) token env v5 in
  TypeUnion (tok, name, v2 :: v4)

let map_annotation (env : env) (x : CST.annotation) =
  match x with
  | `Anno x -> map_annotname env x |> ignore
  | `Anno_LBRACK_anno_rep_COMMA_anno_RBRACK (v1, v2, v3, v4, v5) ->
      let v1 = map_annotname env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 = map_annotarg env v3 in
      let v4 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_annotarg env v2 in
            v2)
          v4
      in
      let v5 = (* "]" *) token env v5 in
      ()

let map_importdirective (env : env) ((v1, v2, v3) : CST.importdirective) : stmt
    =
  let v1 = (* "import" *) token env v1 in
  let v2 = map_importmoduleexpr env v2 in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        let v1 = (* "as" *) token env v1 in
        let v2 = map_modulename env v2 in
        Some v2
    | None -> None
  in
  ImportAs (v1, v2, v3)

let map_field (env : env) ((v1, v2) : CST.field) =
  let v1 = map_vardecl env v1 in
  let v2 = (* ";" *) token env v2 in
  v1

let map_anon_vard_rep_COMMA_vard_76ab5f3 (env : env)
    ((v1, v2) : CST.anon_vard_rep_COMMA_vard_76ab5f3) =
  let v1 = map_vardecl env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_vardecl env v2 in
        v2)
      v2
  in
  v1 :: v2

let rec map_anon_call_arg_rep_COMMA_call_arg_25882ee (env : env)
    ((v1, v2) : CST.anon_call_arg_rep_COMMA_call_arg_25882ee) =
  let v1 = map_call_arg env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_call_arg env v2 in
        v2)
      v2
  in
  v1 :: v2

and map_asexpr (env : env) ((v1, v2) : CST.asexpr) =
  let v1 = map_exprorterm env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        Some
          (let v1 = (* "as" *) token env v1 in
           let v2 = map_varname env v2 in
           v2)
    | None -> None
  in
  (v1, v2)

and map_asexprs (env : env) ((v1, v2) : CST.asexprs) =
  let v1 = map_asexpr env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_asexpr env v2 in
        v2)
      v2
  in
  v1 :: v2

and map_call_arg (env : env) (x : CST.call_arg) : argument =
  match x with
  | `Semg_ellips tok -> Arg (Ellipsis ((* "..." *) token env tok))
  | `Choice_expr x -> (
      match x with
      | `Expr x -> Arg (map_exprorterm env x)
      | `Unde tok -> ArgUnderscore (token env tok))

and map_call_or_unqual_agg_body ~lhs (env : env)
    (x : CST.call_or_unqual_agg_body) =
  match x with
  | `Call_body (v1, v2, v3) ->
      let v1 = (* "(" *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> map_anon_call_arg_rep_COMMA_call_arg_25882ee env x
        | None -> []
      in
      let v3 = (* ")" *) token env v3 in
      Call (lhs, [], (v1, v2, v3))
  | `Unqual_agg_body (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "(" *) token env v1 in
      let vardecls =
        match v2 with
        | Some x -> map_anon_vard_rep_COMMA_vard_76ab5f3 env x
        | None -> []
      in
      let v3 = (* "|" *) token env v3 in
      let formula =
        match v4 with
        | Some x -> Some (map_exprorterm env x)
        | None -> None
      in
      let as_exprs =
        match v5 with
        | Some (v1, v2) ->
            let v1 = (* "|" *) token env v1 in
            let v2 = map_asexprs env v2 in
            v2
        | None -> []
      in
      let v6 = (* ")" *) token env v6 in
      Aggregation
        {
          expr = lhs;
          rank_exprs = [];
          body = (v1, { vardecls; formula; as_exprs; agg_orderbys = [] }, v6);
        }

and map_expr_aggregate_body (env : env) ((v1, v2) : CST.expr_aggregate_body) =
  let as_exprs = map_asexprs env v1 in
  let agg_orderbys =
    match v2 with
    | Some x -> map_orderbys env x
    | None -> []
  in
  (as_exprs, agg_orderbys)

and map_exprorterm (env : env) (x : CST.exprorterm) : expr =
  match x with
  | `Spec_call (v1, v2, v3) ->
      let v1 = (* "none" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = (* ")" *) token env v3 in
      Call (IdSpecial (NoneId, v1), [], (v2, [], v3))
  | `Prefix_cast (v1, v2, v3, v4) ->
      let _v1 = (* "(" *) token env v1 in
      let v2 = map_typeexpr env v2 in
      let _v3 = (* ")" *) token env v3 in
      let v4 = map_exprorterm env v4 in
      Cast (v2, v4)
  | `Prim x -> map_primary env x
  | `Un_expr (v1, v2) ->
      let v1 = map_unop env v1 in
      let v2 = map_exprorterm env v2 in
      UnaryOp (v1, v2)
  | `Mul_expr (v1, v2, v3) ->
      let v1 = map_exprorterm env v1 in
      let v2 = map_mulop env v2 in
      let v3 = map_exprorterm env v3 in
      BinOp (v1, v2, v3)
  | `Add_expr (v1, v2, v3) ->
      let v1 = map_exprorterm env v1 in
      let v2 = map_addop env v2 in
      let v3 = map_exprorterm env v3 in
      BinOp (v1, v2, v3)
  | `In_expr (v1, v2, v3) ->
      let v1 = map_exprorterm env v1 in
      let v2 = (* "in" *) token env v2 in
      let v3 = map_primary env v3 in
      BinOp (v1, (In, v2), v3)
  | `Comp_term (v1, v2, v3) ->
      let v1 = map_exprorterm env v1 in
      let v2 = map_compop env v2 in
      let v3 = map_exprorterm env v3 in
      BinOp (v1, v2, v3)
  | `Inst_of (v1, v2, v3) ->
      let v1 = map_exprorterm env v1 in
      let v2 = (* "instanceof" *) token env v2 in
      let v3 = map_typeexpr env v3 in
      BinOp (v1, (InstanceOf, v2), N v3)
  | `Nega (v1, v2) ->
      let v1 = (* "not" *) token env v1 in
      let v2 = map_exprorterm env v2 in
      Call (IdSpecial (Not, v1), [], Tok.unsafe_fake_bracket [ Arg v2 ])
  | `If_term (v1, v2, v3, v4, v5, v6) ->
      let _v1 = (* "if" *) token env v1 in
      let v2 = map_exprorterm env v2 in
      let _v3 = (* "then" *) token env v3 in
      let v4 = map_exprorterm env v4 in
      let _v5 = (* "else" *) token env v5 in
      let v6 = map_exprorterm env v6 in
      IfExp (v2, v4, v6)
  | `Conj (v1, v2, v3) ->
      let v1 = map_exprorterm env v1 in
      let v2 = (* "and" *) token env v2 in
      let v3 = map_exprorterm env v3 in
      BinOp (v1, (And, v2), v3)
  | `Disj (v1, v2, v3) ->
      let v1 = map_exprorterm env v1 in
      let v2 = (* "or" *) token env v2 in
      let v3 = map_exprorterm env v3 in
      BinOp (v1, (Or, v2), v3)
  | `Impl (v1, v2, v3) ->
      let v1 = map_exprorterm env v1 in
      let v2 = (* "implies" *) token env v2 in
      let v3 = map_exprorterm env v3 in
      BinOp (v1, (Implies, v2), v3)
  | `Quan (v1, v2, v3, v4) ->
      let v1 = map_quantifier env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        match v3 with
        | `Opt_vard_rep_COMMA_vard_opt_BAR_expr_opt_BAR_expr (v1, v2) -> (
            let vardecls =
              match v1 with
              | Some x -> map_anon_vard_rep_COMMA_vard_76ab5f3 env x
              | None -> []
            in
            match v2 with
            | Some (v1, v2, v3) ->
                let v1 = (* "|" *) token env v1 in
                let left = map_exprorterm env v2 in
                let right =
                  match v3 with
                  | Some (v1, v2) ->
                      let v1 = (* "|" *) token env v1 in
                      let right = map_exprorterm env v2 in
                      Some right
                  | None -> None
                in
                Declared (vardecls, Some (left, right))
            | None -> Declared (vardecls, None))
        | `Expr x -> Bare (map_exprorterm env x)
      in
      let v4 = (* ")" *) token env v4 in
      Quantified (v1, (v2, v3, v4))

and map_full_aggregate_body (env : env) (x : CST.full_aggregate_body) =
  match x with
  | `Opt_vard_rep_COMMA_vard_BAR_opt_expr_opt_BAR_asexprs_opt_orders
      (v1, v2, v3, v4) ->
      let vardecls =
        match v1 with
        | Some x -> map_anon_vard_rep_COMMA_vard_76ab5f3 env x
        | None -> []
      in
      let v2 = (* "|" *) token env v2 in
      let formula =
        match v3 with
        | Some x -> Some (map_exprorterm env x)
        | None -> None
      in
      let as_exprs, agg_orderbys =
        match v4 with
        | Some (v1, v2, v3) ->
            let v1 = (* "|" *) token env v1 in
            let v2 = map_asexprs env v2 in
            let v3 =
              match v3 with
              | Some x -> map_orderbys env x
              | None -> []
            in
            (v2, v3)
        | None -> ([], [])
      in
      { vardecls; formula; as_exprs; agg_orderbys }
  | `Vard_rep_COMMA_vard x ->
      {
        vardecls = map_anon_vard_rep_COMMA_vard_76ab5f3 env x;
        formula = None;
        as_exprs = [];
        agg_orderbys = [];
      }

and map_orderby (env : env) ((v1, v2) : CST.orderby) =
  let v1 = map_exprorterm env v1 in
  let v2 =
    match v2 with
    | Some x -> Some (map_direction env x)
    | None -> None
  in
  (v1, v2)

and map_orderbys (env : env) ((v1, v2, v3, v4) : CST.orderbys) =
  let v1 = (* "order" *) token env v1 in
  let v2 = (* "by" *) token env v2 in
  let v3 = map_orderby env v3 in
  let v4 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_orderby env v2 in
        v2)
      v4
  in
  v3 :: v4

and map_par_expr (env : env) (x : CST.par_expr) : expr =
  match x with
  | `LPAR_simp_semg_meta_RPAR (v1, v2, v3, v4) ->
      let _v1 = (* "(" *) token env v1 in
      let v2 = map_modulename env v2 in
      let v3 = (* semgrep_metavariable *) str env v3 in
      let _v4 = (* ")" *) token env v4 in
      TypedMetavar (Id v2, v3)
  | `LPAR_expr_RPAR (v1, v2, v3) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = map_exprorterm env v2 in
      let v3 = (* ")" *) token env v3 in
      ParenExpr (v1, v2, v3)

and map_primary (env : env) (x : CST.primary) : expr =
  match x with
  | `Semg_ellips tok -> Ellipsis (token env tok)
  | `Semg_ellips_meta tok -> MetavarEllipsis (str env tok)
  | `Semg_deep_exp (v1, v2, v3) ->
      let v1 = (* "<..." *) token env v1 in
      let v2 = map_exprorterm env v2 in
      let v3 = (* "...>" *) token env v3 in
      DeepEllipsis (v1, v2, v3)
  | `Call_or_unqual_agg_expr (v1, v2, v3) ->
      let v1 = map_aritylesspredicateexpr env v1 in
      let v2 =
        match v2 with
        | Some x -> Some (map_closure env x)
        | None -> None
      in
      map_call_or_unqual_agg_body ~lhs:(N (IdQualified v1)) env v3
  | `Qual_expr (v1, v2, v3) ->
      let v1 = map_primary env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = map_qualifiedrhs env v3 in
      DotAccess (v1, v2, v3)
  | `Lit x -> L (map_literal env x)
  | `Var x -> map_variable env x
  | `Super_ref (v1, v2) -> (
      let super = (* "super" *) str env v2 in
      match v1 with
      | Some (v1, v2) ->
          let v1 = map_typeexpr env v1 in
          let v2 = (* "." *) token env v2 in
          DotAccess (N v1, v2, RhsSuper (snd super))
      | None -> N (Id super))
  | `Aggr (v1, v2, v3, v4, v5) ->
      let v1 = N (Id (map_aggid env v1)) in
      (* the specification says there can only be one of these, but whatever *)
      let v2 =
        match v2 with
        | Some (v1, v2, v3, v4) ->
            let v1 = (* "[" *) token env v1 in
            let v2 = map_exprorterm env v2 in
            let v3 =
              List_.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = map_exprorterm env v2 in
                  v2)
                v3
            in
            let v4 = (* "]" *) token env v4 in
            v2 :: v3
        | None -> []
      in
      let v3 = (* "(" *) token env v3 in
      let v5 = (* ")" *) token env v5 in
      let body =
        match v4 with
        | Some x -> (
            match x with
            | `Full_aggr_body x -> map_full_aggregate_body env x
            | `Expr_aggr_body x ->
                let as_exprs, agg_orderbys = map_expr_aggregate_body env x in
                { vardecls = []; formula = None; as_exprs; agg_orderbys })
        | None ->
            { vardecls = []; formula = None; as_exprs = []; agg_orderbys = [] }
      in
      Aggregation { expr = v1; rank_exprs = v2; body = (v3, body, v5) }
  | `Range (v1, v2, v3, v4, v5) ->
      let v1 = (* "[" *) token env v1 in
      let v2 = map_exprorterm env v2 in
      let v3 = (* ".." *) token env v3 in
      let v4 = map_exprorterm env v4 in
      let v5 = (* "]" *) token env v5 in
      Range (v2, v3, v4)
  | `Set_lit (v1, v2, v3, v4) ->
      let v1 = (* "[" *) token env v1 in
      let v2 =
        match v2 with
        | Some (v1, v2) ->
            let v1 = map_exprorterm env v1 in
            let v2 =
              List_.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = map_exprorterm env v2 in
                  v2)
                v2
            in
            v1 :: v2
        | None -> []
      in
      let v3 =
        match v3 with
        | Some tok -> Some ((* "," *) token env tok)
        | None -> None
      in
      let v4 = (* "]" *) token env v4 in
      Set (v1, v2, v4)
  | `Par_expr x -> map_par_expr env x
  | `Expr_anno (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = map_annotname env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 = map_annotname env v3 in
      let v4 = (* "]" *) token env v4 in
      let v5 = (* "(" *) token env v5 in
      let v6 = map_exprorterm env v6 in
      let v7 = (* ")" *) token env v7 in
      AnnotatedExpr (v1, v3, v6)

and map_qualifiedrhs (env : env) (x : CST.qualifiedrhs) =
  match x with
  | `Pred_opt_clos_LPAR_opt_call_arg_rep_COMMA_call_arg_RPAR (v1, v2, v3, v4, v5)
    ->
      let v1 = map_predicatename env v1 in
      let _v2 =
        match v2 with
        | Some x -> Some (map_closure env x)
        | None -> None
      in
      let v3 = (* "(" *) token env v3 in
      let v4 =
        match v4 with
        | Some x -> map_anon_call_arg_rep_COMMA_call_arg_25882ee env x
        | None -> []
      in
      let v5 = (* ")" *) token env v5 in
      RhsPredicate (v1, v4)
  | `LPAR_type_RPAR (v1, v2, v3) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = map_typeexpr env v2 in
      let v3 = (* ")" *) token env v3 in
      RhsCast v2

let map_select (env : env) ((v1, v2, v3, v4, v5) : CST.select) =
  let v1 =
    match v1 with
    | Some (v1, v2) ->
        Some
          (let v1 = (* "from" *) token env v1 in
           let v2 =
             match v2 with
             | Some x -> map_anon_vard_rep_COMMA_vard_76ab5f3 env x
             | None -> []
           in
           (v1, v2))
    | None -> None
  in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        Some
          (let v1 = (* "where" *) token env v1 in
           let v2 = map_exprorterm env v2 in
           v2)
    | None -> None
  in
  let v3 = (* "select" *) token env v3 in
  let v4 = map_asexprs env v4 in
  let v5 =
    match v5 with
    | Some x -> map_orderbys env x
    | None -> []
  in
  Select { from = v1; where = v2; select = v4; sel_orderbys = v5 }

let map_charpred (env : env) ((v1, v2, v3, v4, v5, v6) : CST.charpred) =
  let v1 = map_classname env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = (* ")" *) token env v3 in
  let v4 = (* "{" *) token env v4 in
  let v5 = map_exprorterm env v5 in
  let v6 = (* "}" *) token env v6 in
  PredicateDef (None, v1, [], PredicateExpr (Some v5))

let map_body (env : env) ((v1, v2, v3) : CST.body) : expr =
  let v1 = (* "{" *) token env v1 in
  let v2 = map_exprorterm env v2 in
  let v3 = (* "}" *) token env v3 in
  v2

let map_optbody (env : env) (x : CST.optbody) : expr option =
  match x with
  | `Empty tok -> None
  | `Body x -> Some (map_body env x)
  | `High (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 = (* "=" *) token env v1 in
      let v2 = map_literalid env v2 in
      let v3 = (* "(" *) token env v3 in
      let v4 =
        match v4 with
        | Some (v1, v2) ->
            let v1 = map_predicateexpr env v1 in
            let v2 =
              List_.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = map_predicateexpr env v2 in
                  v2)
                v2
            in
            v1 :: v2
        | None -> []
      in
      let v5 = (* ")" *) token env v5 in
      let v6 = (* "(" *) token env v6 in
      let v7 =
        match v7 with
        | Some x -> map_anon_call_arg_rep_COMMA_call_arg_25882ee env x
        | None -> []
      in
      let v8 = (* ")" *) token env v8 in
      Some
        (Call
           ( N (Id v2),
             List_.map (fun (x, y) -> (IdQualified x, Some y)) v4,
             (v6, v7, v8) ))

let map_datatypebranch (env : env)
    ((v1, v2, v3, v4, v5, v6, v7) : CST.datatypebranch) :
    type_definition_element =
  (* qldoc *)
  let _v1 =
    match v1 with
    | Some tok ->
        Some ((* pattern \/\*\*[^*]*\*+([^/*][^*]*\*+)*\/ *) token env tok)
    | None -> None
  in
  let _v2 =
    match v2 with
    | Some x -> Some (map_annotation env x)
    | None -> None
  in
  let v3 = map_classname env v3 in
  let v4 = (* "(" *) token env v4 in
  let v5 =
    match v5 with
    | Some x -> map_anon_vard_rep_COMMA_vard_76ab5f3 env x
    | None -> []
  in
  let v6 = (* ")" *) token env v6 in
  let v7 =
    match v7 with
    | Some x -> Some (map_body env x)
    | None -> None
  in
  (v3, v5, v7)

let map_memberpredicate (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.memberpredicate) : stmt =
  let v1 = map_anon_choice_pred_44fe1b2 env v1 in
  let v2 = map_predicatename env v2 in
  let _v3 = (* "(" *) token env v3 in
  let v4 =
    match v4 with
    | Some x -> map_anon_vard_rep_COMMA_vard_76ab5f3 env x
    | None -> []
  in
  let _v5 = (* ")" *) token env v5 in
  let v6 = map_optbody env v6 in
  PredicateDef (v1, v2, v4, PredicateExpr v6)

let map_classlesspredicate (env : env) ((v1, v2, v3) : CST.classlesspredicate) :
    stmt =
  let v1 = map_anon_choice_pred_44fe1b2 env v1 in
  let v2 = map_predicatename env v2 in
  let v3, v4 =
    match v3 with
    | `LPAR_opt_vard_rep_COMMA_vard_RPAR_optb (v1, v2, v3, v4) ->
        let v1 = (* "(" *) token env v1 in
        let v2 =
          match v2 with
          | Some x -> map_anon_vard_rep_COMMA_vard_76ab5f3 env x
          | None -> []
        in
        let v3 = (* ")" *) token env v3 in
        let v4 = map_optbody env v4 in
        (v2, PredicateExpr v4)
    | `Pred x ->
        let v1, v2 = map_predicatealiasbody env x in
        ([], PredicateAlias (v1, v2))
  in
  PredicateDef (v1, v2, v3, v4)

let map_datatypebranches (env : env) ((v1, v2) : CST.datatypebranches) =
  let v1 = map_datatypebranch env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let v1 = (* "or" *) token env v1 in
        let v2 = map_datatypebranch env v2 in
        v2)
      v2
  in
  v1 :: v2

let map_classmember (env : env) (x : CST.classmember) : stmt list =
  match x with
  | `Semg_ellips tok -> [ ExprStmt (Ellipsis (token env tok)) ]
  | `Rep_anno_choice_char (v1, v2) ->
      let v1 = List_.map (map_annotation env) v1 in
      let v2 =
        match v2 with
        | `Char x -> map_charpred env x
        | `Memb x -> map_memberpredicate env x
        | `Field x -> VarDecl (map_field env x)
      in
      [ v2 ]
  | `Qldoc tok -> []

let map_datatype (env : env) ((v1, v2, v3, v4) : CST.datatype) =
  let v1 = (* "newtype" *) token env v1 in
  let v2 = map_classname env v2 in
  let v3 = (* "=" *) token env v3 in
  let v4 = map_datatypebranches env v4 in
  NewType (v1, v2, v4)

let map_dataclass (env : env) ((v1, v2, v3) : CST.dataclass) : stmt =
  let tok = (* "class" *) token env v1 in
  let name = map_classname env v2 in
  match v3 with
  | `Opt_extends_type_rep_COMMA_type_opt_inst_type_rep_COMMA_type_choice_LCURL_rep_clas_RCURL
      (v1, v2, v3) ->
      let v1 =
        match v1 with
        | Some (v1, v2, v3) ->
            let v1 = (* "extends" *) token env v1 in
            let v2 = map_typeexpr env v2 in
            let v3 =
              List_.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = map_typeexpr env v2 in
                  v2)
                v3
            in
            v2 :: v3
        | None -> []
      in
      let v2 =
        match v2 with
        | Some (v1, v2, v3) ->
            let v1 = (* "instanceof" *) token env v1 in
            let v2 = map_typeexpr env v2 in
            let v3 =
              List_.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = map_typeexpr env v2 in
                  v2)
                v3
            in
            v2 :: v3
        | None -> []
      in
      let v3 =
        match v3 with
        | `LCURL_rep_clas_RCURL (v1, v2, v3) ->
            let v1 = (* "{" *) token env v1 in
            let v2 = List.concat_map (map_classmember env) v2 in
            let v3 = (* "}" *) token env v3 in
            (v1, v2, v3)
        | `SEMI tok -> Tok.unsafe_fake_bracket []
      in
      ClassDef (tok, name, ClassBody (v1, v2, v3))
  | `Type_e6aca0c x ->
      let type_ = map_typealiasbody env x in
      ClassDef (tok, name, ClassAlias type_)
  | `Type_9cc1977 x -> map_typeunionbody ~tok ~name env x

let rec map_module_ (env : env) ((v1, v2, v3, v4, v5) : CST.module_) =
  let v1 = (* "module" *) token env v1 in
  let v2 = map_modulename env v2 in
  let v3 =
    match v3 with
    | Some (v1, v2, v3, v4) ->
        let v1 = (* "<" *) token env v1 in
        let v2 = map_moduleparam env v2 in
        let v3 =
          List_.map
            (fun (v1, v2) ->
              let _v1 = (* "," *) token env v1 in
              let v2 = map_moduleparam env v2 in
              v2)
            v3
        in
        let v4 = (* ">" *) token env v4 in
        v2 :: v3
    | None -> []
  in
  (* TODO: *)
  let _v4 =
    match v4 with
    | Some (v1, v2, v3) ->
        Some
          (let v1 = (* "implements" *) token env v1 in
           let v2 = map_signatureexpr env v2 in
           let v3 =
             List_.map
               (fun (v1, v2) ->
                 let _v1 = (* "," *) token env v1 in
                 let v2 = map_signatureexpr env v2 in
                 v2)
               v3
           in
           ())
    | None -> None
  in
  let v5 =
    match v5 with
    | `LCURL_rep_modu_RCURL (v1, v2, v3) ->
        let _v1 = (* "{" *) token env v1 in
        let v2 = List.concat_map (map_modulemember env) v2 in
        let _v3 = (* "}" *) token env v3 in
        ModuleBody v2
    | `Modu x -> ModuleAlias (map_modulealiasbody env x)
  in
  ModuleDef (v1, v2, v3, v5)

and map_modulemember (env : env) (x : CST.modulemember) : stmt list =
  match x with
  | `Semg_ellips tok -> [ ExprStmt (Ellipsis (token env tok)) ]
  | `Rep_anno_choice_impo (v1, v2) ->
      (* TODO *)
      let _v1 = List_.map (map_annotation env) v1 in
      let v2 =
        match v2 with
        | `Impo x -> map_importdirective env x
        | `Clas x -> map_classlesspredicate env x
        | `Data_cb44f91 x -> map_dataclass env x
        | `Data_3931108 x -> map_datatype env x
        | `Select x -> map_select env x
        | `Module x -> map_module_ env x
      in
      [ v2 ]
  | `Qldoc _tok -> []

let map_ql (env : env) (x : CST.ql) : any =
  match x with
  | `Semg_exp (v1, v2) ->
      let _v1 = (* "__SEMGREP_EXPRESSION" *) token env v1 in
      let v2 = map_exprorterm env v2 in
      E v2
  | `Rep_modu xs -> Pr (List.concat_map (map_modulemember env) xs)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_ql.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      match map_ql env cst with
      | Pr xs -> xs
      | _ -> failwith "not a program")

let parse_string ~file ~contents =
  H.wrap_parser
    (fun () -> Tree_sitter_ql.Parse.string ~src_file:file contents)
    (fun cst _extras ->
      let env =
        {
          H.file = Fpath.v file;
          conv = (Pos.full_converters_str contents).linecol_to_bytepos_fun;
          extra = ();
        }
      in
      match map_ql env cst with
      | Pr xs -> xs
      | _ -> failwith "not a program")

let parse_expression_or_source_file str =
  let res = Tree_sitter_ql.Parse.string str in
  match res.errors with
  | [] -> res
  | _ ->
      let expr_str = "__SEMGREP_EXPRESSION " ^ str in
      Tree_sitter_ql.Parse.string expr_str

(* Need return type to be "any"*)
let parse_pattern str =
  H.wrap_parser
    (fun () -> parse_expression_or_source_file str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env = { H.file; conv = H.line_col_to_pos_pattern str; extra = () } in
      map_ql env cst)
