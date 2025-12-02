(**
   Boilerplate to be used as a template when mapping the fga CST
   to another type of tree.
*)

open Common
open Fpath_.Operators
module CST = Tree_sitter_fga.CST
module H = Parse_tree_sitter_helpers
open AST_generic
module G = AST_generic


type context = Program | Pattern
type env = context H.env

let in_pattern env =
  match env.H.extra with
  | Program -> false
  | Pattern -> true


(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let token = H.token
let str = H.str
let fb = Tok.unsafe_fake_bracket


(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)


let map_boolean_literal (env : env) (x : CST.boolean_literal) =
  match x with
  | `True tok -> Bool (true, token env tok)
  | `False tok -> Bool (false, token env tok)

let map_number_literal (env : env) (x : CST.number_literal) =
  match x with
  | `Float_lit tok ->
      let s, t = str env tok in
      Float (float_of_string_opt s, t)
  | `Int_lit tok ->
      let s, t = str env tok in
      Int (Parsed_int.parse (s, t))
  | `Uint_lit tok ->
      let s, t = str env tok in
      Int (Parsed_int.parse (s, t))

let map_operator (env : env) (x : CST.operator) =
  match x with
  | `Or tok -> (Or, token env tok)
  | `And tok -> (And, token env tok)
  | `ButS tok -> 
      let t = token env tok in
      (G.Other (G.Op_AlignOf), t) (* "but not" - custom operator *)

let map_comparative_operator (env : env) (x : CST.comparative_operator) =
  match x with
  | `EQEQ tok -> (Eq, token env tok)
  | `BANGEQ tok -> (NotEq, token env tok)
  | `LT tok -> (Lt, token env tok)
  | `LTEQ tok -> (LtE, token env tok)
  | `GT tok -> (Gt, token env tok)
  | `GTEQ tok -> (GtE, token env tok)

let rec map_expression (env : env) (x : CST.expression) : G.expr =
  match x with
  | `Num_lit x -> L (map_number_literal env x) |> G.e
  | `Bool_lit x -> L (map_boolean_literal env x) |> G.e
  | `Str_lit tok ->
      let s, t = str env tok in
      L (String (fb (s, t))) |> G.e
  | `Null_lit tok ->
      let t = token env tok in
      L (Null t) |> G.e
  | `Bin_exp x -> map_binary_expression env x
  | `Sele_exp (v1, _v2, v3) ->
      let id1 = str env v1 in
      let id2 = str env v3 in
      (* Selector like "context.user" *)
      let name = H2.name_of_ids [id1; id2] in
      N name |> G.e
  | `Call_exp (v1, v2) ->
      let func = 
        match v1 with
        | `Sele_exp (v1, _v2, v3) ->
            let id1 = str env v1 in
            let id2 = str env v3 in
            let name = H2.name_of_ids [id1; id2] in
            N name |> G.e
        | `Id tok ->
            let id = str env tok in
            N (H2.name_of_id id) |> G.e
      in
      let args = map_argument_list env v2 in
      Call (func, args) |> G.e
  | `Id tok ->
      let id = str env tok in
      N (H2.name_of_id id) |> G.e
  | `Semg_ellips tok ->
      let t = token env tok in
      Ellipsis t |> G.e
  | `Semg_meta tok ->
      let id = str env tok in
      N (H2.name_of_id id) |> G.e

and map_binary_expression (env : env) (x : CST.binary_expression) : G.expr =
  match x with
  | `Exp_choice_STAR_exp (v1, v2, v3) ->
      let e1 = map_expression env v1 in
      let op, tok = 
        match v2 with
        | `STAR tok -> (Mult, token env tok)
        | `SLASH tok -> (Div, token env tok)
        | `PERC tok -> (Mod, token env tok)
        | `LTLT tok -> (LSL, token env tok)
        | `GTGT tok -> (LSR, token env tok)
        | `AMP tok -> (BitAnd, token env tok)
        | `AMPHAT tok -> 
            let t = token env tok in
            (G.Other G.Op_AlignOf, t) (* &^ operator *)
      in
      let e3 = map_expression env v3 in
      G.opcall (op, tok) [e1; e3]
  | `Exp_choice_PLUS_exp (v1, v2, v3) ->
      let e1 = map_expression env v1 in
      let op, tok =
        match v2 with
        | `PLUS tok -> (Plus, token env tok)
        | `DASH tok -> (Minus, token env tok)
        | `BAR tok -> (BitOr, token env tok)
        | `HAT tok -> (BitXor, token env tok)
      in
      let e3 = map_expression env v3 in
      G.opcall (op, tok) [e1; e3]
  | `Exp_choice_EQEQ_exp (v1, v2, v3) ->
      let e1 = map_expression env v1 in
      let op, tok = map_comparative_operator env v2 in
      let e3 = map_expression env v3 in
      G.opcall (op, tok) [e1; e3]
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let e1 = map_expression env v1 in
      let tok = token env v2 in
      let e3 = map_expression env v3 in
      G.opcall (And, tok) [e1; e3]
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let e1 = map_expression env v1 in
      let tok = token env v2 in
      let e3 = map_expression env v3 in
      G.opcall (Or, tok) [e1; e3]

and map_argument_list (env : env) ((v1, v2, v3) : CST.argument_list) : G.arguments =
  let l = token env v1 in
  let args =
    match v2 with
    | Some (v1, v2) ->
        let arg1 = Arg (map_expression env v1) in
        let rest = 
          match v2 with
          | Some xs ->
              List.map (fun (_comma, expr) ->
                Arg (map_expression env expr)
              ) xs
          | None -> []
        in
        arg1 :: rest
    | None -> []
  in
  let r = token env v3 in
  (l, args, r)


let map_anon_choice_id_684e964 (env : env) (x : CST.anon_choice_id_684e964) : G.ident =
  match x with
  | `Id tok -> str env tok
  | `Semg_meta tok -> str env tok

let map_anon_choice_id_6cee6b4 (env : env) (x : CST.anon_choice_id_6cee6b4) : string * G.tok =
  match x with
  | `Id tok -> str env tok
  | `Rela_ref (v1, _hash, v3) ->
      let id1, t1 = str env v1 in
      let id2, t2 = str env v3 in
      (id1 ^ "#" ^ id2, Tok.combine_toks t1 [t2])
  | `All (v1, _colonstar) ->
      let id, t = str env v1 in
      (id ^ ":*", t)
  | `Semg_meta tok -> str env tok

let map_direct_relationship (env : env) ((v1, v2, v3) : CST.direct_relationship) : G.expr =
  let _lbracket = token env v1 in
  let content_str =
    match v2 with
    | `Choice_id_opt_cond_opt_rep_COMMA_choice_id_opt_cond (v1, _cond_opt, _rest) ->
        let id, _ = map_anon_choice_id_6cee6b4 env v1 in
        id
    | `Semg_ellips _tok -> "..."
  in
  let _rbracket = token env v3 in
  (* Represent as a string for now *)
  L (String (fb (content_str, _lbracket))) |> G.e

let map_relation_def (env : env) (x : CST.relation_def) : G.expr =
  match x with
  | `Direct_rela x -> map_direct_relationship env x
  | `Opt_direct_rela_op_choice_id_opt_rep_op_choice_id (direct_opt, v2, rest_opt) ->
      let base_expr =
        match direct_opt with
        (* TODO: incomplete implementation *)
        | Some (direct, op) ->
            let e1 = map_direct_relationship env direct in
            let _op, _tok = map_operator env op in
            e1 (* Will combine with next part *)
        | None ->
            let id_str = 
              match v2 with
              | `Id tok -> fst (str env tok)
              | `Indi_rela (v1, _from, v3) ->
                  let id1 = fst (str env v1) in
                  let id2 = fst (str env v3) in
                  id1 ^ " from " ^ id2
            in
            L (String (fb (id_str, fake "relation"))) |> G.e
      in
      let _rest = 
        match rest_opt with
        | Some xs -> 
            List.map (fun (_op, _id) -> ()) xs
        | None -> []
      in
      base_expr

let map_definition (env : env) ((v1, v2, v3, v4) : CST.definition) : G.stmt =
  let _define_tok = token env v1 in
  let name_id = map_anon_choice_id_684e964 env v2 in
  let _colon = token env v3 in
  let relation_expr = map_relation_def env v4 in
  (* Map to VarDef: define viewer: [user] becomes a variable definition *)
  let entity = G.basic_entity name_id in
  let vdef = { vinit = Some relation_expr; vtype = None; vtok = None } in
  DefStmt (entity, VarDef vdef) |> G.s

let map_condition_declaration (env : env) ((v1, v2, v3, v4, v5, v6) : CST.condition_declaration) : G.stmt =
  let _condition_tok = token env v1 in
  let name_id = str env v2 in
  let lparen = token env v3 in
  let params =
    match v4 with
    | Some (p1, rest_opt) ->
        let param1 = 
          let (param_name, _colon, param_type) = p1 in
          let pname = map_anon_choice_id_684e964 env param_name in
          let ptype = 
            match param_type with
            | `Type_id _x -> Some (G.ty_builtin "string") (* simplified *)
            | `Semg_meta _tok -> None
          in
          G.param_of_id pname ?ptype
        in
        let rest_params =
          match rest_opt with
          | Some xs ->
              List.map (fun (_comma, (param_name, _colon, param_type)) ->
                let pname = map_anon_choice_id_684e964 env param_name in
                let ptype = 
                  match param_type with
                  | `Type_id _x -> Some (G.ty_builtin "string")
                  | `Semg_meta _tok -> None
                in
                G.param_of_id pname ?ptype
              ) xs
          | None -> []
        in
        param1 :: rest_params
    | None -> []
  in
  let rparen = token env v5 in
  let (_lbrace, _body_expr, _rbrace) = v6 in
  (* Map to FuncDef: condition becomes a function *)
  let entity = G.basic_entity name_id in
  let fdef = {
    fkind = (G.Function, _condition_tok);
    fparams = (lparen, List.map (fun p -> G.Param p) params, rparen);
    frettype = None;
    fbody = G.FBDecl G.sc; (* Empty body for now *)
  } in
  DefStmt (entity, FuncDef fdef) |> G.s

let map_type_declaration (env : env) ((v1, v2, v3, v4, v5) : CST.type_declaration) : G.stmt =
  let _extend_opt = 
    match v1 with
    | Some tok -> Some (token env tok)
    | None -> None
  in
  let type_tok = token env v2 in
  let type_name = map_anon_choice_id_684e964 env v3 in
  let _newline = token env v4 in
  let relations =
    match v5 with
    | Some (_relations_tok, defs) ->
        List.map (fun x ->
          match x with
          | `Defi def -> 
              let stmt = map_definition env def in
              G.FieldStmt stmt
          | `Semg_ellips tok ->
              let t = token env tok in
              G.fieldEllipsis t
        ) defs
    | None -> []
  in
  (* Map to ClassDef: type document becomes a class with relations as fields *)
  let entity = G.basic_entity type_name in
  let cdef = {
    ckind = (G.Class, type_tok);
    cextends = [];
    cimplements = [];
    cmixins = [];
    cparams = fb [];
    cbody = fb relations;
  } in
  DefStmt (entity, ClassDef cdef) |> G.s

let map_source_file (env : env) (x : CST.source_file) : G.any =
  match x with
  | `Proj_file (_schema, _contents) ->
      (* Project files (.openfga.yaml) - return empty program *)
      Pr []
  | `Module_file (header, decls) ->
      let _header = 
        match header with
        | `Model (_model_tok, _newline, _schema) -> ()
        | `Module (_module_tok, _name) -> ()
      in
      let stmts = 
        List.map (fun x ->
          match x with
          | `Type_decl td -> map_type_declaration env td
          | `Cond_decl cd -> map_condition_declaration env cd
          | `Semg_ellips tok ->
              let t = token env tok in
              ExprStmt (Ellipsis t |> G.e, t) |> G.s
        ) decls
      in
      Pr stmts

let map_any (env : env) (x : CST.source_file) : G.any =
  map_source_file env x

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse_expression_or_source_file str =
  let res = Tree_sitter_fga.Parse.string str in
  match res.errors with
  | [] -> res
  | _ ->
      let expr_str = "__SEMGREP_EXPRESSION " ^ str in
      Tree_sitter_fga.Parse.string expr_str

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_fga.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = Program } in
      match map_source_file env cst with
      | G.Pr xs -> xs
      | _ -> failwith "not a program")

let parse_pattern str =
  H.wrap_parser
    (fun () -> parse_expression_or_source_file str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env = { H.file; conv = H.line_col_to_pos_pattern str; extra = Pattern } in
      map_source_file env cst)

(* let map_boolean_literal (env : env) (x : CST.boolean_literal) =
  (match x with
  | `True tok -> R.Case ("True",
      (* "true" *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* "false" *) token env tok
    )
  ) *)



(* Disable warnings against unused variables
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit *)

(* let token (env : env) (tok : Tree_sitter_run.Token.t) =
  R.Token tok

let blank (env : env) () =
  R.Tuple []

let map_imm_tok_prec_p1_hash (env : env) (tok : CST.imm_tok_prec_p1_hash) =
  (* "#" *) token env tok

let map_file (env : env) (tok : CST.file) =
  (* pattern .+\..+ *) token env tok

let map_float_literal (env : env) (tok : CST.float_literal) =
  (* float_literal *) token env tok

let map_identifier (env : env) (tok : CST.identifier) =
  (* pattern [a-zA-Z_-]+ *) token env tok *)

(* let map_operator (env : env) (x : CST.operator) =
  (match x with
  | `Or tok -> R.Case ("Or",
      (* "or" *) token env tok
    )
  | `And tok -> R.Case ("And",
      (* "and" *) token env tok
    )
  | `ButS tok -> R.Case ("ButS",
      (* "but not" *) token env tok
    )
  ) *)

(* let map_imm_tok_colonstar (env : env) (tok : CST.imm_tok_colonstar) =
  (* ":*" *) token env tok

let map_version (env : env) (tok : CST.version) =
  (* pattern [0-9]+\.[0-9]+ *) token env tok

let map_semgrep_metavariable (env : env) (tok : CST.semgrep_metavariable) =
  (* pattern \$[A-Z_][A-Z_0-9]* *) token env tok *)

(* let map_type_identifier (env : env) (x : CST.type_identifier) =
  (match x with
  | `Str tok -> R.Case ("Str",
      (* "string" *) token env tok
    )
  | `Int tok -> R.Case ("Int",
      (* "int" *) token env tok
    )
  | `Map tok -> R.Case ("Map",
      (* "map" *) token env tok
    )
  | `Uint tok -> R.Case ("Uint",
      (* "uint" *) token env tok
    )
  | `List tok -> R.Case ("List",
      (* "list" *) token env tok
    )
  | `Time tok -> R.Case ("Time",
      (* "timestamp" *) token env tok
    )
  | `Bool tok -> R.Case ("Bool",
      (* "bool" *) token env tok
    )
  | `Dura tok -> R.Case ("Dura",
      (* "duration" *) token env tok
    )
  | `Double tok -> R.Case ("Double",
      (* "double" *) token env tok
    )
  | `Ipad tok -> R.Case ("Ipad",
      (* "ipaddress" *) token env tok
    )
  ) *)

(* let map_uint_literal (env : env) (tok : CST.uint_literal) =
  (* uint_literal *) token env tok

let map_string_literal (env : env) (tok : CST.string_literal) =
  (* string_literal *) token env tok *)

(* let map_comparative_operator (env : env) (x : CST.comparative_operator) =
  (match x with
  | `EQEQ tok -> R.Case ("EQEQ",
      (* "==" *) token env tok
    )
  | `BANGEQ tok -> R.Case ("BANGEQ",
      (* "!=" *) token env tok
    )
  | `LT tok -> R.Case ("LT",
      (* "<" *) token env tok
    )
  | `LTEQ tok -> R.Case ("LTEQ",
      (* "<=" *) token env tok
    )
  | `GT tok -> R.Case ("GT",
      (* ">" *) token env tok
    )
  | `GTEQ tok -> R.Case ("GTEQ",
      (* ">=" *) token env tok
    )
  ) *)

(* let map_int_literal (env : env) (tok : CST.int_literal) =
  (* int_literal *) token env tok

let map_contents (env : env) ((v1, v2) : CST.contents) =
  let v1 = (* "contents:" *) token env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "-" *) token env v1 in
      let v2 = (* pattern .+\..+ *) token env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_module_ (env : env) ((v1, v2) : CST.module_) =
  let v1 = (* "module" *) token env v1 in
  let v2 = (* pattern [a-zA-Z_-]+ *) token env v2 in
  R.Tuple [v1; v2]

let map_conditional (env : env) ((v1, v2) : CST.conditional) =
  let v1 = (* "with" *) token env v1 in
  let v2 = (* pattern [a-zA-Z_-]+ *) token env v2 in
  R.Tuple [v1; v2]

let map_selector_expression (env : env) ((v1, v2, v3) : CST.selector_expression) =
  let v1 = (* pattern [a-zA-Z_-]+ *) token env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 = (* pattern [a-zA-Z_-]+ *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_quoted_version (env : env) ((v1, v2, v3) : CST.quoted_version) =
  let v1 = (* "'" *) token env v1 in
  let v2 = (* pattern [0-9]+\.[0-9]+ *) token env v2 in
  let v3 = (* "'" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_schema (env : env) ((v1, v2) : CST.schema) =
  let v1 = (* "schema" *) token env v1 in
  let v2 = (* pattern [0-9]+\.[0-9]+ *) token env v2 in
  R.Tuple [v1; v2] *)

(* let map_anon_choice_id_684e964 (env : env) (x : CST.anon_choice_id_684e964) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern [a-zA-Z_-]+ *) token env tok
    )
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* pattern \$[A-Z_][A-Z_0-9]* *) token env tok
    )
  ) *)

(* let map_number_literal (env : env) (x : CST.number_literal) =
  (match x with
  | `Float_lit tok -> R.Case ("Float_lit",
      (* float_literal *) token env tok
    )
  | `Int_lit tok -> R.Case ("Int_lit",
      (* int_literal *) token env tok
    )
  | `Uint_lit tok -> R.Case ("Uint_lit",
      (* uint_literal *) token env tok
    )
  ) *)

(* let map_anon_choice_id_096b091 (env : env) (x : CST.anon_choice_id_096b091) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern [a-zA-Z_-]+ *) token env tok
    )
  | `Indi_rela (v1, v2, v3) -> R.Case ("Indi_rela",
      let v1 = (* pattern [a-zA-Z_-]+ *) token env v1 in
      let v2 = (* "from" *) token env v2 in
      let v3 = (* pattern [a-zA-Z_-]+ *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  ) *)

(* let map_anon_choice_id_6cee6b4 (env : env) (x : CST.anon_choice_id_6cee6b4) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern [a-zA-Z_-]+ *) token env tok
    )
  | `Rela_ref (v1, v2, v3) -> R.Case ("Rela_ref",
      let v1 = (* pattern [a-zA-Z_-]+ *) token env v1 in
      let v2 = map_imm_tok_prec_p1_hash env v2 in
      let v3 = (* pattern [a-zA-Z_-]+ *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `All (v1, v2) -> R.Case ("All",
      let v1 = (* pattern [a-zA-Z_-]+ *) token env v1 in
      let v2 = map_imm_tok_colonstar env v2 in
      R.Tuple [v1; v2]
    )
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* pattern \$[A-Z_][A-Z_0-9]* *) token env tok
    )
  ) *)

(* let map_quoted_schema (env : env) ((v1, v2) : CST.quoted_schema) =
  let v1 = (* "schema:" *) token env v1 in
  let v2 = map_quoted_version env v2 in
  R.Tuple [v1; v2]

let map_model (env : env) ((v1, v2, v3) : CST.model) =
  let v1 = (* "model" *) token env v1 in
  let v2 = (* "\n" *) token env v2 in
  let v3 = map_schema env v3 in
  R.Tuple [v1; v2; v3]

let map_param (env : env) ((v1, v2, v3) : CST.param) =
  let v1 = map_anon_choice_id_684e964 env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 =
    (match v3 with
    | `Type_id x -> R.Case ("Type_id",
        map_type_identifier env x
      )
    | `Semg_meta tok -> R.Case ("Semg_meta",
        (* pattern \$[A-Z_][A-Z_0-9]* *) token env tok
      )
    )
  in
  R.Tuple [v1; v2; v3] *)

(* let rec map_argument_list (env : env) ((v1, v2, v3) : CST.argument_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_expression env v1 in
        let v2 =
          (match v2 with
          | Some xs -> R.Option (Some (
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_expression env v2 in
                R.Tuple [v1; v2]
              ) xs)
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Exp_choice_STAR_exp (v1, v2, v3) -> R.Case ("Exp_choice_STAR_exp",
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `STAR tok -> R.Case ("STAR",
            (* "*" *) token env tok
          )
        | `SLASH tok -> R.Case ("SLASH",
            (* "/" *) token env tok
          )
        | `PERC tok -> R.Case ("PERC",
            (* "%" *) token env tok
          )
        | `LTLT tok -> R.Case ("LTLT",
            (* "<<" *) token env tok
          )
        | `GTGT tok -> R.Case ("GTGT",
            (* ">>" *) token env tok
          )
        | `AMP tok -> R.Case ("AMP",
            (* "&" *) token env tok
          )
        | `AMPHAT tok -> R.Case ("AMPHAT",
            (* "&^" *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_choice_PLUS_exp (v1, v2, v3) -> R.Case ("Exp_choice_PLUS_exp",
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `PLUS tok -> R.Case ("PLUS",
            (* "+" *) token env tok
          )
        | `DASH tok -> R.Case ("DASH",
            (* "-" *) token env tok
          )
        | `BAR tok -> R.Case ("BAR",
            (* "|" *) token env tok
          )
        | `HAT tok -> R.Case ("HAT",
            (* "^" *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_choice_EQEQ_exp (v1, v2, v3) -> R.Case ("Exp_choice_EQEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = map_comparative_operator env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_AMPAMP_exp (v1, v2, v3) -> R.Case ("Exp_AMPAMP_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BARBAR_exp (v1, v2, v3) -> R.Case ("Exp_BARBAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Num_lit x -> R.Case ("Num_lit",
      map_number_literal env x
    )
  | `Bool_lit x -> R.Case ("Bool_lit",
      map_boolean_literal env x
    )
  | `Str_lit tok -> R.Case ("Str_lit",
      (* string_literal *) token env tok
    )
  | `Null_lit tok -> R.Case ("Null_lit",
      (* "null" *) token env tok
    )
  | `Bin_exp x -> R.Case ("Bin_exp",
      map_binary_expression env x
    )
  | `Sele_exp x -> R.Case ("Sele_exp",
      map_selector_expression env x
    )
  | `Call_exp (v1, v2) -> R.Case ("Call_exp",
      let v1 =
        (match v1 with
        | `Sele_exp x -> R.Case ("Sele_exp",
            map_selector_expression env x
          )
        | `Id tok -> R.Case ("Id",
            (* pattern [a-zA-Z_-]+ *) token env tok
          )
        )
      in
      let v2 = map_argument_list env v2 in
      R.Tuple [v1; v2]
    )
  | `Id tok -> R.Case ("Id",
      (* pattern [a-zA-Z_-]+ *) token env tok
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* pattern \$[A-Z_][A-Z_0-9]* *) token env tok
    )
  ) *)

(* let map_direct_relationship (env : env) ((v1, v2, v3) : CST.direct_relationship) =
  let v1 = (* "[" *) token env v1 in
  let v2 =
    (match v2 with
    | `Choice_id_opt_cond_opt_rep_COMMA_choice_id_opt_cond (v1, v2, v3) -> R.Case ("Choice_id_opt_cond_opt_rep_COMMA_choice_id_opt_cond",
        let v1 = map_anon_choice_id_6cee6b4 env v1 in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_conditional env x
            ))
          | None -> R.Option None)
        in
        let v3 =
          (match v3 with
          | Some xs -> R.Option (Some (
              R.List (List.map (fun (v1, v2, v3) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_anon_choice_id_6cee6b4 env v2 in
                let v3 =
                  (match v3 with
                  | Some x -> R.Option (Some (
                      map_conditional env x
                    ))
                  | None -> R.Option None)
                in
                R.Tuple [v1; v2; v3]
              ) xs)
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      )
    | `Semg_ellips tok -> R.Case ("Semg_ellips",
        (* "..." *) token env tok
      )
    )
  in
  let v3 = (* "]" *) token env v3 in
  R.Tuple [v1; v2; v3] *)

(* let map_condition_body (env : env) ((v1, v2, v3) : CST.condition_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3] *)

(* let map_relation_def (env : env) (x : CST.relation_def) =
  (match x with
  | `Direct_rela x -> R.Case ("Direct_rela",
      map_direct_relationship env x
    )
  | `Opt_direct_rela_op_choice_id_opt_rep_op_choice_id (v1, v2, v3) -> R.Case ("Opt_direct_rela_op_choice_id_opt_rep_op_choice_id",
      let v1 =
        (match v1 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_direct_relationship env v1 in
            let v2 = map_operator env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v2 = map_anon_choice_id_096b091 env v2 in
      let v3 =
        (match v3 with
        | Some xs -> R.Option (Some (
            R.List (List.map (fun (v1, v2) ->
              let v1 = map_operator env v1 in
              let v2 = map_anon_choice_id_096b091 env v2 in
              R.Tuple [v1; v2]
            ) xs)
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  ) *)

(* let map_condition_declaration (env : env) ((v1, v2, v3, v4, v5, v6) : CST.condition_declaration) =
  let v1 = (* "condition" *) token env v1 in
  let v2 = (* pattern [a-zA-Z_-]+ *) token env v2 in
  let v3 = (* "(" *) token env v3 in
  let v4 =
    (match v4 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_param env v1 in
        let v2 =
          (match v2 with
          | Some xs -> R.Option (Some (
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_param env v2 in
                R.Tuple [v1; v2]
              ) xs)
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v5 = (* ")" *) token env v5 in
  let v6 = map_condition_body env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6] *)

(* let map_definition (env : env) ((v1, v2, v3, v4) : CST.definition) =
  let v1 = (* "define" *) token env v1 in
  let v2 = map_anon_choice_id_684e964 env v2 in
  let v3 = (* ":" *) token env v3 in
  let v4 = map_relation_def env v4 in
  R.Tuple [v1; v2; v3; v4] *)

(* let map_relations (env : env) ((v1, v2) : CST.relations) =
  let v1 = (* "relations" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Defi x -> R.Case ("Defi",
          map_definition env x
        )
      | `Semg_ellips tok -> R.Case ("Semg_ellips",
          (* "..." *) token env tok
        )
      )
    ) v2)
  in
  R.Tuple [v1; v2] *)

(* let map_type_declaration (env : env) ((v1, v2, v3, v4, v5) : CST.type_declaration) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "extend" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = (* "type" *) token env v2 in
  let v3 = map_anon_choice_id_684e964 env v3 in
  let v4 = (* "\n" *) token env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_relations env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5] *)

(* let map_source_file (env : env) (x : CST.source_file) =
  (match x with
  | `Proj_file (v1, v2) -> R.Case ("Proj_file",
      let v1 = map_quoted_schema env v1 in
      let v2 = map_contents env v2 in
      R.Tuple [v1; v2]
    )
  | `Module_file (v1, v2) -> R.Case ("Module_file",
      let v1 =
        (match v1 with
        | `Model x -> R.Case ("Model",
            map_model env x
          )
        | `Module x -> R.Case ("Module",
            map_module_ env x
          )
        )
      in
      let v2 =
        R.List (List.map (fun x ->
          (match x with
          | `Type_decl x -> R.Case ("Type_decl",
              map_type_declaration env x
            )
          | `Cond_decl x -> R.Case ("Cond_decl",
              map_condition_declaration env x
            )
          | `Semg_ellips tok -> R.Case ("Semg_ellips",
              (* "..." *) token env tok
            )
          )
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  ) *)

(* let map_comment (env : env) (tok : CST.comment) =
  (* comment *) token env tok

let dump_tree root =
  map_source_file () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout

let map_extra (env : env) (x : CST.extra) =
  match x with
  | `Comment (_loc, x) -> ("comment", "comment", map_comment env x)

let dump_extras (extras : CST.extras) =
  List.iter (fun extra ->
    let ts_rule_name, ocaml_type_name, raw_tree = map_extra () extra in
    let details =
      if ocaml_type_name <> ts_rule_name then
        Printf.sprintf " (OCaml type '%s')" ocaml_type_name
      else
        ""
    in
    Printf.printf "%s%s:\n" ts_rule_name details;
    Tree_sitter_run.Raw_tree.to_channel stdout raw_tree
  ) extras *)
