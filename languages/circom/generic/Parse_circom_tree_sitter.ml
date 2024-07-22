open Common
open Either_
open Fpath_.Operators
module CST = Tree_sitter_circom.CST
module H = Parse_tree_sitter_helpers
open AST_generic
module G = AST_generic
module H2 = AST_generic_helpers

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = unit H.env

let token = H.token
let str = H.str
let _fb = Tok.unsafe_fake_bracket

(* let fake s = Tok.unsafe_fake_tok s *)
let fake_id s = (s, G.fake s)

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying from semgrep-circom/lib/Boilerplate.ml *)

(* Disable warnings against unused variables *)
[@@@warning "-26-27-32"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

let map_signal_visability (env : env) (x : CST.signal_visability) =
  match x with
  | `Input tok -> (* "input" *) str env tok
  | `Output tok -> (* "output" *) str env tok

let map_identifier (env : env) (tok : CST.identifier) =
  (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env tok

let map_int_ (env : env) (tok : CST.int_) =
  (* pattern \d+ *) str env tok

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  (* escape_sequence *) token env tok

let map_string_immediate_elt_inside_double_quote (env : env) (tok : CST.string_immediate_elt_inside_double_quote) =
  (* pattern "[^\"\\\\\\n]+|\\\\\\r?\\n" *) token env tok

let map_string_immediate_elt_inside_quote (env : env) (tok : CST.string_immediate_elt_inside_quote) =
  (* pattern "[^'\\\\\\n]+|\\\\\\r?\\n" *) token env tok

let map_circom_version (env : env) (tok : CST.circom_version) =
  (* pattern "\"?\\.? ?(\\d|\\*\
  )+(\\. ?(\\d|\\*\
  )+ ?(\\.(\\d|\\*\
  )+)?)?\"?" *) str env tok

let map_circom_pragma_token (env : env) (x : CST.circom_pragma_token) =
  (match x with
  | `Circom_circom_vers (v1, v2) ->
      let v1 = (* "circom" *) str env v1 in
      let v2 =
        (* pattern "\"?\\.? ?(\\d|\\*\
  )+(\\. ?(\\d|\\*\
  )+ ?(\\.(\\d|\\*\
  )+)?)?\"?" *) token env v2
      in (v1, Some v2)
  | `Circom_id (v1, v2) ->
      let v1 = (* "circom" *) str env v1 in
      let v2 =
        (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2
      in (v1, Some v2)
  )

let map_string_ (env : env) (x : CST.string_) : string wrap bracket =
  match x with
  | `DQUOT_rep_choice_str_imme_elt_inside_double_quote_DQUOT (v1, v2, v3) ->
      let l = (* "\"" *) token env v1 in
      let xs =
        List_.map
          (fun x ->
            match x with
            | `Str_imme_elt_inside_double_quote tok ->
                (* pattern "[^\"\\\\\\n]+|\\\\\\r?\\n" *) str env tok
            | `Esc_seq tok -> (* escape_sequence *) str env tok)
          v2
      in
      let r = (* "\"" *) token env v3 in
      G.string_ (l, xs, r)
  | `SQUOT_rep_choice_str_imme_elt_inside_quote_SQUOT (v1, v2, v3) ->
      let l = (* "'" *) token env v1 in
      let xs =
        List_.map
          (fun x ->
            match x with
            | `Str_imme_elt_inside_quote tok ->
                (* pattern "[^'\\\\\\n]+|\\\\\\r?\\n" *) str env tok
            | `Esc_seq tok -> (* escape_sequence *) str env tok)
          v2
      in
      let r = (* "'" *) token env v3 in
      G.string_ (l, xs, r)


let map_directive (env : env) (x : CST.directive) =
  (match x with
  | `Pragma_dire (v1, v2, v3) ->
      let tpragma = (* "pragma" *) token env v1 in
      let tcircom, anys_opt =
        match v2 with
        | `Circom_pragma_tok x ->
            map_circom_pragma_token env x
        | `Circom_custom_templs_tok tok ->
            (* "custom_templates" *) (str env tok, None)
      in 
      let anys_tokens = 
        match anys_opt with
        | Some anys -> [Tk anys]
        | None -> []
      in
      let sc = (* ";" *) token env v3 in
      [ Pragma (tcircom, [ Tk tpragma] @ anys_tokens @ [Tk sc ]) |> G.d ]
  | `Incl_dire (v1, v2, v3) ->
      let timport = (* "include" *) token env v1 in
      let _, path, _ = map_string_ env v2 in
      let tsc = (* ";" *) token env v3 in
      
      [ ImportAll (timport, FileName path, fake "") |> G.d ]
  )

let map_parameter (env : env) (x : CST.parameter) =
  match x with
  | `Id v1 ->
      let name =  (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v1 in
      let param =
        {
          G.pname = Some name;
          G.ptype = None;
          G.pdefault = None;
          G.pattrs = [];
          G.pinfo = G.empty_id_info ();
        }
      in
      G.Param param
  | `Ellips tok ->
    ParamEllipsis ((* "..." *) token env tok)

let map_trailing_comma env v =
  match v with
  | Some tok -> (* "," *) token env tok |> ignore
  | None -> ()

let map_parameter_list (env : env) ((v1, v2, v3) : CST.parameter_list) :
  parameters =
  let lp = (* "(" *) token env v1 in
  let params =
  match v2 with
  | Some (v1, v2, v3) ->
      let v1 = map_parameter env v1 in
      let v2 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_parameter env v2 in
            v2)
          v2
      in
      let _v3 = map_trailing_comma env v3 in
      v1 :: v2
  | None -> []
  in
  let rp = (* ")" *) token env v3 in
  (lp, params, rp)

let rec map_anon_choice_id_3723479 (env : env) (x : CST.anon_choice_id_3723479) =
  match x with
  | `Id tok ->
      let id = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env tok in
      N (H2.name_of_id id) |> G.e
  | `Member_exp x ->
      map_member_expression env x
  | `Array_access_exp x ->
      map_array_access_expression env x

and map_member_expression (env : env) ((v1, v2, v3) : CST.member_expression) =
  let e =
    match e with
    | `Exp x ->
        map_expression env x
    | `Id tok ->
        let id = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env tok in
        N (H2.name_of_id id) |> G.e
  in
  let tdot = (* "." *) token env v2 in
  let fld = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v3 in
  DotAccess (e, tdot, FN (H2.name_of_id fld)) |> G.e

and map_array_access_expression (env : env) ((v1, v2, v3, v4) : CST.array_access_expression) =
  let expression = map_expression env v1 in
  let lbracket = (* "[" *) token env v2 in
  let index =
    match v3 with
    | Some x -> Some (
        map_expression env x
      )
    | None -> R.Option None
  in
  let rbracket = (* "]" *) token env v4 in
  G.ArrayAccess (expr, (lbracket, index, rbracket))


let map_expression_statement (env : env) (x : CST.expression_statement) =
  match x with
  | `Exp_semi (v1, v2) ->
      let e = map_expression env v1 in
      let sc = (* ";" *) token env v2 in
      (e, sc)
  | `Ellips_SEMI (v1, v2) ->
      let tdots = (* "..." *) token env v1 in
      let sc = (* ";" *) token env v2 in
      (Ellipsis tdots |> G.e, sc)
  | `Ellips tok ->
      let tdots = (* "..." *) token env tok in
      (Ellipsis tdots |> G.e, G.sc)

and map_unary_expression (env : env) (x : CST.unary_expression) : expr =
  match x with
  | `BANG_exp (v1, v2) ->
      let v1 = (* "!" *) token env v1 in
      let v2 = map_expression env v2 in
      G.opcall (Not, v1) [ v2 ]
  | `TILDE_exp (v1, v2) ->
      let v1 = (* "~" *) token env v1 in
      let v2 = map_expression env v2 in
      G.opcall (BitNot, v1) [ v2 ]
  | `DASH_exp (v1, v2) ->
      let v1 = (* "-" *) token env v1 in
      let v2 = map_expression env v2 in
      G.opcall (Minus, v1) [ v2 ]
  | `PLUS_exp (v1, v2) ->
      let v1 = (* "+" *) token env v1 in
      let v2 = map_expression env v2 in
      G.opcall (Plus, v1) [ v2 ]

and map_binary_expression (env : env) (x : CST.binary_expression) : expr =
  match x with
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (And, v2) [ v1; v3 ]
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (Or, v2) [ v1; v3 ]
  | `Exp_GTGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ">>" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (LSR, v2) [ v1; v3 ]
  | `Exp_GTGTGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ">>>" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (ASR, v2) [ v1; v3 ]
  | `Exp_LTLT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (LSL, v2) [ v1; v3 ]
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (BitAnd, v2) [ v1; v3 ]
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (BitXor, v2) [ v1; v3 ]
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (BitOr, v2) [ v1; v3 ]
  | `Exp_PLUS_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (Plus, v2) [ v1; v3 ]
  | `Exp_DASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "-" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (Minus, v2) [ v1; v3 ]
  | `Exp_STAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "*" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (Mult, v2) [ v1; v3 ]
  | `Exp_SLASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (Div, v2) [ v1; v3 ]
  | `Exp_PERC_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "%" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (Mod, v2) [ v1; v3 ]
  | `Exp_STARSTAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "**" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (Pow, v2) [ v1; v3 ]
  | `Exp_LT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (Lt, v2) [ v1; v3 ]
  | `Exp_LTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<=" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (LtE, v2) [ v1; v3 ]
  | `Exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "==" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (Eq, v2) [ v1; v3 ]
  | `Exp_BANGEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "!=" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (NotEq, v2) [ v1; v3 ]
  | `Exp_GTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ">=" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (GtE, v2) [ v1; v3 ]
  | `Exp_GT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ">" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (Gt, v2) [ v1; v3 ]
 | `Exp_BSLASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "\\" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (FloorDiv, v2) [ v1; v3 ]

and map_ternary_expression (env : env) ((v1, v2, v3, v4, v5) : CST.ternary_expression) =
  let cond = map_expression env v1 in
  let quest = (* "?" *) token env v2 in
  let ok = map_expression env v3 in
  let s = (* ":" *) token env v4 in
  let elseE = map_expression env v5 in
  Conditional (cond, ok, elseE) |> G.s

and map_expression (env : env) (x : CST.expression) =
  match x with
  | `Choice_int x -> (
      (match x with
      (* | `Int tok -> R.Case ("Int",
          (* pattern \d+ *) token env tok
      | `Id tok -> R.Case ("Id",
          (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env tok
        ) *)
      (* | `Array x -> R.Case ("Array",
          map_array_ env x
        )
      | `Tuple x -> R.Case ("Tuple",
          map_tuple env x
        ) *)
      | `Un_exp x -> R.Case ("Un_exp",
          map_unary_expression env x
        )
      | `Bin_exp x -> R.Case ("Bin_exp",
          map_binary_expression env x
        )
      | `Tern_exp x -> R.Case ("Tern_exp",
          map_ternary_expression env x
        )
      | `Paren_exp x -> R.Case ("Paren_exp",
          map_parenthesized_expression env x
        )
      | `Call_exp x -> R.Case ("Call_exp",
          map_call_expression env x
        )
      | `Incr_exp x -> R.Case ("Incr_exp",
          map_increment_expression env x
        )
      | `Decr_exp x -> R.Case ("Decr_exp",
          map_decrement_expression env x
        )
      | `Member_exp x -> R.Case ("Member_exp",
          map_member_expression env x
        )
      | `Array_access_exp x -> R.Case ("Array_access_exp",
          map_array_access_expression env x
        )
      | `Assign_exp x -> R.Case ("Assign_exp",
          map_assignment_expression env x
        )
      )
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  | `Deep_ellips (v1, v2, v3) -> R.Case ("Deep_ellips",
      let v1 = (* "<..." *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "...>" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )

let map_statement (env : env) (x : CST.statement) : stmt = 
  match x with
  | `Ret_stmt (v1, v2, v3) ->
      let tret = (* "return" *) token env v1 in
      let eopt =
        match v2 with
        | Some x -> Some (map_expression env x)
        | None -> None
      in
      let sc = (* ";" *) token env v3 in
      Return (tret, eopt, sc) |> G.s
  | `Blk_stmt (v1, v2, v3) ->
      let lb = (* "{" *) token env v1 in
      let xs = List_.map (map_statement env) v2 in
      let rb = (* "}" *) token env v3 in
      Block (lb, xs, rb) |> G.s
  | `For_stmt v1 -> map_for_statement env v1
  | `Exp_stmt x ->
      let e, sc = map_expression_statement env x in
      ExprStmt (e, sc) |> G.s
  | `Var_decl_stmt x ->
      let ent, vdef = map_variable_declaration_statement env x in
      DefStmt (ent, VarDef vdef) |> G.s
  | `While_stmt (v1, v2, v3, v4, v5) ->
      let twhile = (* "while" *) token env v1 in
      let _lp = (* "(" *) token env v2 in
      let cond = map_expression env v3 in
      let _rp = (* ")" *) token env v4 in
      let st = map_statement env v5 in
      While (twhile, Cond cond, st) |> G.s
  | `If_stmt (v1, v2, v3, v4, v5, v6) ->
      let tif = (* "if" *) token env v1 in
      let _lp = (* "(" *) token env v2 in
      let cond = map_expression env v3 in
      let _rp = (* ")" *) token env v4 in
      let then_ = map_statement env v5 in
      let else_opt =
        match v6 with
        | Some (v1, v2) ->
            let _telse = (* "else" *) token env v1 in
            let st = map_statement env v2 in
            Some st
        | None -> None
      in
      If (tif, Cond cond, then_, else_opt) |> G.s

let map_function_body (env : env) ((v1, v2, v3) : CST.function_body) :
    function_body =
  let lb = (* "{" *) token env v1 in
  let xs = List_.map (map_statement env) v2 in
  let rb = (* "}" *) token env v3 in
  FBStmt (Block (lb, xs, rb) |> G.s)

let map_template_body (env : env) ((v1, v2, v3) : CST.template_body):
    template_body =
  let lb = (* "{" *) token env v1 in
  let xs = List_.map (map_statement env) v2 in
  let rb = (* "}" *) token env v3 in
  FBStmt (Block (lb, xs, rb) |> G.s)

let map_definition (env : env) (x : CST.definition) =
  match x with
  | `Func_defi (v1, v2, v3, v4) ->
      let v1 = (* "function" *) token env v1 in
      let v2 =
        (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2
      in
      let v3 = map_parameter_list env v3 in
      let v4 = map_function_body env v4 in
      R.Tuple [v1; v2; v3; v4]
  (* | `Temp_defi (v1, v2, v3, v4, v5) -> R.Case ("Temp_defi",
      let v1 = (* "template" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_template_type env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v3
      in
      let v4 = map_parameter_list env v4 in
      let v5 = map_template_body env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Main_comp_defi (v1, v2, v3, v4, v5, v6) -> R.Case ("Main_comp_defi",
      let v1 = (* "component" *) token env v1 in
      let v2 = (* "main" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_main_component_public_signals env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* "=" *) token env v4 in
      let v5 = map_call_expression env v5 in
      let v6 = (* ";" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    ) *)

let map_source_unit (env : env) (x : CST.source_unit) : item list =
  match x with
  | `Dire x ->
      let xs = map_directive env x in
      xs |> List_.map (fun dir -> DirectiveStmt dir |> G.s)
  | `Defi x ->
      let def = map_definition env x in
      [ DefStmt def |> G.s ]

let map_source_file(env: env) (x : CST.source_file) = 
  match x with
  | `Rep_source_unit v1 -> 
      let xss = List_.map  (map_source_unit env) v1 in 
      Pr (List.flatten xss)
  | `Rep1_stmt xs ->
      let xs = List_.map (map_statement env) xs in
      Ss xs
  | `Exp x -> 
      let e = map_expression env x in
        E e 
  
(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_circom.Parse.file !!file)
    (fun cst ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      match map_source_file env cst with
      | G.Pr xs
      | G.Ss xs ->
          xs
      | _ -> failwith "not a program")
      
(* todo: special mode to convert Ellipsis in the right construct! *)
let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_solidity.Parse.string str)
    (fun cst ->
      let file = Fpath.v "<pattern>" in
      let env = { H.file; conv = H.line_col_to_pos_pattern str; extra = () } in
      map_source_file env cst)