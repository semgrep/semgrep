open Fpath_.Operators
module CST = Tree_sitter_circom.CST
module H = Parse_tree_sitter_helpers
open AST_generic
open Common
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

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying from semgrep-circom/lib/Boilerplate.ml *)

(* Disable warnings against unused variables *)
[@@@warning "-26-27-32"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

let left_strip_space (s, t) =
  if s =~ "^ +\\(.*\\)$" then (Common.matched1 s, t) else (s, t)

let map_signal_visability (env : env) (x : CST.signal_visability) =
  match x with
  | `Input tok -> (* "input" *) [ str env tok ]
  | `Output tok -> (* "output" *) [ str env tok ]
(* TODO
   | `Input tok -> (* "input" *) G.OtherAttribute (str env tok, [])
   | `Output tok -> (* "output" *) G.OtherAttribute (str env tok, [])
*)

let map_identifier (env : env) (tok : CST.identifier) =
  (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env tok

let map_int_ (env : env) (tok : CST.int_) = (* pattern \d+ *) str env tok

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  (* escape_sequence *) token env tok

let map_string_immediate_elt_inside_double_quote (env : env)
    (tok : CST.string_immediate_elt_inside_double_quote) =
  (* pattern "[^\"\\\\\\n]+|\\\\\\r?\\n" *) token env tok

let map_string_immediate_elt_inside_quote (env : env)
    (tok : CST.string_immediate_elt_inside_quote) =
  (* pattern "[^'\\\\\\n]+|\\\\\\r?\\n" *) token env tok

let map_circom_version (env : env) (tok : CST.circom_version) =
  (* pattern "\"?\\.? ?(\\d|\\*\
     )+(\\. ?(\\d|\\*\
     )+ ?(\\.(\\d|\\*\
     )+)?)?\"?" *)
  str env tok

let map_circom_pragma_token (env : env) (x : CST.circom_pragma_token) =
  match x with
  | `Circom_circom_vers (v1, v2) ->
      let v1 = (* "circom" *) str env v1 in
      let v2 =
        (* pattern "\"?\\.? ?(\\d|\\*\
           )+(\\. ?(\\d|\\*\
           )+ ?(\\.(\\d|\\*\
           )+)?)?\"?" *)
        token env v2
      in
      (v1, Some v2)
  | `Circom_id (v1, v2) ->
      let v1 = (* "circom" *) str env v1 in
      let v2 = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2 in
      (v1, Some v2)

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
  match x with
  | `Pragma_dire (v1, v2, v3) ->
      let tpragma = (* "pragma" *) token env v1 in
      let tcircom, anys_opt =
        match v2 with
        | `Circom_pragma_tok x -> map_circom_pragma_token env x
        | `Circom_custom_templs_tok tok ->
            (* "custom_templates" *) (str env tok, None)
      in
      let anys_tokens =
        match anys_opt with
        | Some anys -> [ Tk anys ]
        | None -> []
      in
      let sc = (* ";" *) token env v3 in
      [ Pragma (tcircom, [ Tk tpragma ] @ anys_tokens @ [ Tk sc ]) |> G.d ]
  | `Incl_dire (v1, v2, v3) ->
      let timport = (* "include" *) token env v1 in
      let _, path, _ = map_string_ env v2 in
      let tsc = (* ";" *) token env v3 in

      [ ImportAll (timport, FileName path, fake "") |> G.d ]

let map_parameter (env : env) (x : CST.parameter) =
  match x with
  | `Id v1 ->
      let name =
        (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v1 |> left_strip_space
      in
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
  | `Ellips tok -> ParamEllipsis ((* "..." *) token env tok)

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

let rec map_anon_choice_id_3723479 (env : env) (x : CST.anon_choice_id_3723479)
    =
  match x with
  | `Id tok ->
      let id = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env tok in
      N (H2.name_of_id id) |> G.e
  | `Member_exp x -> map_member_expression env x
  | `Array_access_exp x -> map_array_access_expression env x

and map_increment_expression (env : env) ((v1, v2) : CST.increment_expression) =
  let e = map_anon_choice_id_3723479 env v1 in
  let op = (* "++" *) token env v2 in
  G.special (IncrDecr (Incr, Postfix), op) [ e ]

and map_decrement_expression (env : env) ((v1, v2) : CST.decrement_expression) =
  let e = map_anon_choice_id_3723479 env v1 in
  let op = (* "--" *) token env v2 in
  G.special (IncrDecr (Decr, Postfix), op) [ e ]

and map_array_access_expression (env : env)
    ((v1, v2, v3, v4) : CST.array_access_expression) =
  let expression = map_expression env v1 in
  let lbracket = (* "[" *) token env v2 in
  let index =
    match v3 with
    | Some x -> map_expression env x
    | None -> G.L (Null lbracket) |> G.e
  in
  let rbracket = (* "]" *) token env v4 in
  G.ArrayAccess (expression, (lbracket, index, rbracket)) |> G.e

and map_expression_statement (env : env) (x : CST.expression_statement) =
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
      let op = (* "!" *) token env v1 in
      let expr = map_expression env v2 in
      G.opcall (Not, op) [ expr ]
  | `TILDE_exp (v1, v2) ->
      let op = (* "~" *) token env v1 in
      let expr = map_expression env v2 in
      G.opcall (BitNot, op) [ expr ]
  | `DASH_exp (v1, v2) ->
      let op = (* "-" *) token env v1 in
      let expr = map_expression env v2 in
      G.opcall (Minus, op) [ expr ]
  | `PLUS_exp (v1, v2) ->
      let op = (* "+" *) token env v1 in
      let expr = map_expression env v2 in
      G.opcall (Plus, op) [ expr ]

and map_binary_expression (env : env) (x : CST.binary_expression) : expr =
  match x with
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let expr = map_expression env v1 in
      let op = (* "&&" *) token env v2 in
      let expr2 = map_expression env v3 in
      G.opcall (And, op) [ expr; expr2 ]
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let expr = map_expression env v1 in
      let op = (* "||" *) token env v2 in
      let expr2 = map_expression env v3 in
      G.opcall (Or, op) [ expr; expr2 ]
  | `Exp_GTGT_exp (v1, v2, v3) ->
      let expr = map_expression env v1 in
      let op = (* ">>" *) token env v2 in
      let expr2 = map_expression env v3 in
      G.opcall (LSR, op) [ expr; expr2 ]
  | `Exp_LTLT_exp (v1, v2, v3) ->
      let expr = map_expression env v1 in
      let op = (* "<<" *) token env v2 in
      let expr2 = map_expression env v3 in
      G.opcall (LSL, op) [ expr; expr2 ]
  | `Exp_AMP_exp (v1, v2, v3) ->
      let expr = map_expression env v1 in
      let op = (* "&" *) token env v2 in
      let expr2 = map_expression env v3 in
      G.opcall (BitAnd, op) [ expr; expr2 ]
  | `Exp_HAT_exp (v1, v2, v3) ->
      let expr = map_expression env v1 in
      let op = (* "^" *) token env v2 in
      let expr2 = map_expression env v3 in
      G.opcall (BitXor, op) [ expr; expr2 ]
  | `Exp_BAR_exp (v1, v2, v3) ->
      let expr = map_expression env v1 in
      let op = (* "|" *) token env v2 in
      let expr2 = map_expression env v3 in
      G.opcall (BitOr, op) [ expr; expr2 ]
  | `Exp_PLUS_exp (v1, v2, v3) ->
      let expr = map_expression env v1 in
      let op = (* "+" *) token env v2 in
      let expr2 = map_expression env v3 in
      G.opcall (Plus, op) [ expr; expr2 ]
  | `Exp_DASH_exp (v1, v2, v3) ->
      let expr = map_expression env v1 in
      let op = (* "-" *) token env v2 in
      let expr2 = map_expression env v3 in
      G.opcall (Minus, op) [ expr; expr2 ]
  | `Exp_STAR_exp (v1, v2, v3) ->
      let expr = map_expression env v1 in
      let op = (* "*" *) token env v2 in
      let expr2 = map_expression env v3 in
      G.opcall (Mult, op) [ expr; expr2 ]
  | `Exp_SLASH_exp (v1, v2, v3) ->
      let expr = map_expression env v1 in
      let op = (* "/" *) token env v2 in
      let expr2 = map_expression env v3 in
      G.opcall (Div, op) [ expr; expr2 ]
  | `Exp_PERC_exp (v1, v2, v3) ->
      let expr = map_expression env v1 in
      let op = (* "%" *) token env v2 in
      let expr2 = map_expression env v3 in
      G.opcall (Mod, op) [ expr; expr2 ]
  | `Exp_STARSTAR_exp (v1, v2, v3) ->
      let expr = map_expression env v1 in
      let op = (* "**" *) token env v2 in
      let expr2 = map_expression env v3 in
      G.opcall (Pow, op) [ expr; expr2 ]
  | `Exp_LT_exp (v1, v2, v3) ->
      let expr = map_expression env v1 in
      let op = (* "<" *) token env v2 in
      let expr2 = map_expression env v3 in
      G.opcall (Lt, op) [ expr; expr2 ]
  | `Exp_LTEQ_exp (v1, v2, v3) ->
      let expr = map_expression env v1 in
      let op = (* "<=" *) token env v2 in
      let expr2 = map_expression env v3 in
      G.opcall (LtE, op) [ expr; expr2 ]
  | `Exp_EQEQ_exp (v1, v2, v3) ->
      let expr = map_expression env v1 in
      let op = (* "==" *) token env v2 in
      let expr2 = map_expression env v3 in
      G.opcall (Eq, op) [ expr; expr2 ]
  | `Exp_BANGEQ_exp (v1, v2, v3) ->
      let expr = map_expression env v1 in
      let op = (* "!=" *) token env v2 in
      let expr2 = map_expression env v3 in
      G.opcall (NotEq, op) [ expr; expr2 ]
  | `Exp_GTEQ_exp (v1, v2, v3) ->
      let expr = map_expression env v1 in
      let op = (* ">=" *) token env v2 in
      let expr2 = map_expression env v3 in
      G.opcall (GtE, op) [ expr; expr2 ]
  | `Exp_GT_exp (v1, v2, v3) ->
      let expr = map_expression env v1 in
      let op = (* ">" *) token env v2 in
      let expr2 = map_expression env v3 in
      G.opcall (Gt, op) [ expr; expr2 ]
  | `Exp_BSLASH_exp (v1, v2, v3) ->
      let expr = map_expression env v1 in
      let op = (* "\\" *) token env v2 in
      let expr2 = map_expression env v3 in
      G.opcall (FloorDiv, op) [ expr; expr2 ]

and map_ternary_expression (env : env)
    ((v1, v2, v3, v4, v5) : CST.ternary_expression) =
  let cond = map_expression env v1 in
  let quest = (* "?" *) token env v2 in
  let ok = map_expression env v3 in
  let s = (* ":" *) token env v4 in
  let elseE = map_expression env v5 in
  Conditional (cond, ok, elseE) |> G.e

and map_parenthesized_expression (env : env)
    ((v1, v2, v3) : CST.parenthesized_expression) : expr =
  let _lp = (* "(" *) token env v1 in
  let e = map_expression env v2 in
  let _rp = (* ")" *) token env v3 in
  (* alt: ParenExpr (lp, e, rp) |> G.e *)
  e

and map_tuple (env : env) ((v1, v2, v3, v4, v5) : CST.tuple) =
  let lb = (* "(" *) token env v1 in
  let expr = map_expression env v2 in
  let other_exprs =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let other_expr = map_expression env v2 in
        other_expr)
      v3
  in
  let _v4 =
    match v4 with
    | Some tok -> Some ((* "," *) token env tok)
    | None -> None
  in
  let rb = (* ")" *) token env v5 in
  Container (Tuple, (lb, expr :: other_exprs, rb)) |> G.e

and map_call_expression (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.call_expression) =
  let _TODOparallel =
    match v1 with
    | Some tok -> (* "parallel" *) token env tok
    | None -> fake ""
  in
  let call_id = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v2 in
  let lb = (* "(" *) token env v3 in
  let arg_list =
    match v4 with
    | Some x -> map_argument_list env x
    | None -> []
  in
  let rb = (* ")" *) token env v5 in
  let v6TODO =
    match v6 with
    | Some x -> map_anonymous_inputs env x
    | None -> []
  in
  let id = G.N (G.Id (call_id, G.empty_id_info ())) |> G.e in
  let args = (lb, arg_list, rb) in
  G.Call (id, args) |> G.e
(* G.OtherExpr (("Call", v2), [Tk v2; Tk v1; G.Args v4; G.Args v6 ]) |> G.e *)

and map_argument_list (env : env) ((v1, v2) : CST.argument_list) =
  let arg1 = map_expression env v1 |> G.arg in
  let other_args =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_expression env v2 in
        v2 |> G.arg)
      v2
  in
  arg1 :: other_args

and map_anonymous_inputs (env : env) ((v1, v2, v3) : CST.anonymous_inputs) =
  let _lb = (* "(" *) token env v1 in
  let args_list =
    match v2 with
    | Some x -> map_argument_list env x
    | None -> []
  in
  let _rb = (* ")" *) token env v3 in
  args_list

and map_array_ (env : env) ((v1, v2, v3, v4, v5) : CST.array_) =
  let lb = (* "[" *) token env v1 in
  let expr1 = map_expression env v2 in
  let items =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_expression env v2 in
        v2)
      v3
  in
  let _v4 =
    match v4 with
    | Some tok -> Some ((* "," *) token env tok)
    | None -> None
  in
  let rb = (* "]" *) token env v5 in
  G.Container (G.Array, (lb, expr1 :: items, rb)) |> G.e

and map_member_expression (env : env) ((v1, v2, v3) : CST.member_expression) =
  let e =
    match v1 with
    | `Exp x -> map_expression env x
    | `Id tok ->
        let id = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env tok in
        N (H2.name_of_id id) |> G.e
  in
  let tdot = (* "." *) token env v2 in
  let fld = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v3 in
  DotAccess (e, tdot, FN (H2.name_of_id fld)) |> G.e

and map_assignment_expression (env : env)
    ((v1, v2, v3) : CST.assignment_expression) =
  let le =
    match v1 with
    | `Exp x -> map_expression env x
  in
  let op =
    match v2 with
    | `LTEQEQ tok -> (* "<==" *) (LDA, token env tok)
    | `EQEQGT tok -> (* "==>" *) (RDA, token env tok)
    | `LTDASHDASH tok -> (* "<--" *) (LSA, token env tok)
    | `DASHDASHGT tok -> (* "-->" *) (RSA, token env tok)
    | `AMPEQ tok -> (* "&=" *) (BitAnd, token env tok)
    | `PLUSEQ tok -> (* "+=" *) (Plus, token env tok)
    | `DASHEQ tok -> (* "-=" *) (Minus, token env tok)
    | `STAREQ tok -> (* "*=" *) (Mult, token env tok)
    | `STARSTAREQ tok -> (* "**=" *) (Pow, token env tok)
    | `SLASHEQ tok -> (* "/=" *) (Div, token env tok)
    | `BSLASHEQ tok -> (* "\\=" *) (FloorDiv, token env tok)
    | `PERCEQ tok -> (* "%=" *) (Mod, token env tok)
    | `BAREQ tok -> (* "|=" *) (BitOr, token env tok)
    | `HATEQ tok -> (* "^=" *) (BitXor, token env tok)
    | `GTGTEQ tok -> (* ">>=" *) (LSR, token env tok)
    | `LTLTEQ tok -> (* "<<=" *) (LSL, token env tok)
    | `EQEQEQ tok -> (* "===" *) (PhysEq, token env tok)
    | `EQ tok -> (* "=" *) (Eq, token env tok)
  in
  let re = map_expression env v3 in
  G.AssignOp (le, op, re) |> G.e

and map_expression (env : env) (x : CST.expression) =
  match x with
  | `Choice_int x -> (
      match x with
      | `Int tok ->
          let s, t = str env tok in
          G.L (G.Int (Parsed_int.parse (s, t))) |> G.e
      | `Id tok ->
          let id = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env tok in
          N (H2.name_of_id id) |> G.e
      | `Array x -> map_array_ env x
      | `Tuple x -> map_tuple env x
      | `Un_exp x -> map_unary_expression env x
      | `Bin_exp x -> map_binary_expression env x
      | `Tern_exp x -> map_ternary_expression env x
      | `Paren_exp x -> map_parenthesized_expression env x
      | `Call_exp x -> map_call_expression env x
      | `Incr_exp x -> map_increment_expression env x
      | `Decr_exp x -> map_decrement_expression env x
      | `Member_exp x -> map_member_expression env x
      | `Array_access_exp x -> map_array_access_expression env x
      | `Assign_exp x -> map_assignment_expression env x)
  | `Ellips tok -> (* "..." *) G.Ellipsis (token env tok) |> G.e
  | `Deep_ellips (v1, v2, v3) ->
      let l = token env v1 in
      let e = map_expression env v2 in
      let r = token env v3 in
      DeepEllipsis (l, e, r) |> G.e

let map_signal_tags (env : env) ((v1, v2, v3, v4) : CST.signal_tags) =
  let lb = (* "{" *) token env v1 in
  let tag1 = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2 in
  let other_tags =
    List_.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2 in
        v2)
      v3
  in
  let rb = (* "}" *) token env v4 in
  (lb, tag1 :: other_tags, rb)

let map_type_ (env : env) (x : CST.type_) =
  match x with
  | `Signal (v1, v2, v3) ->
      let signal = (* "signal" *) str env v1 in
      let visability =
        match v2 with
        | Some x -> map_signal_visability env x
        | None -> []
      in
      let _v3TODO =
        match v3 with
        | Some x -> Some (map_signal_tags env x)
        | None -> None
      in
      let n = H2.name_of_ids (signal :: visability) in
      G.TyN n |> G.t
  | `Var tok ->
      let x = (* "var" *) str env tok in
      G.ty_builtin x
  | `Comp tok ->
      (* "component" *)
      let x = (* "var" *) str env tok in
      G.ty_builtin x

let map_array_definition (env : env) (xs : CST.array_definition) ty =
  List_.map
    (fun (v1, v2, v3) ->
      let lb = (* "[" *) token env v1 in
      let eopt = map_expression env v2 in
      let rb = (* "]" *) token env v3 in
      TyArray ((lb, Some eopt, rb), ty) |> G.t)
    xs

let map_variable_initialization (env : env)
    ((v1, v2, v3, v4, v5) : CST.variable_initialization) ty =
  let id =
    (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v1 |> left_strip_space
  in
  let v2TODO =
    List_.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2 in
        v2)
      v2
  in
  let _v3 =
    match v3 with
    | Some tok -> Some ((* "," *) token env tok)
    | None -> None
  in
  let _arrTODO =
    match v4 with
    | Some x -> Some (map_array_definition env x ty)
    | None -> None
  in
  let e =
    match v5 with
    | Some (v1, v2) ->
        Some
          (let _v1TODO =
             match v1 with
             | `EQ tok -> (* "=" *) token env tok
             | `LTEQEQ tok -> (* "<==" *) token env tok
             | `EQEQGT tok -> (* "==>" *) token env tok
             | `LTDASHDASH tok -> (* "<--" *) token env tok
             | `DASHDASHGT tok -> (* "-->" *) token env tok
           in
           let v2 = map_expression env v2 in
           v2)
    | None -> None
  in
  (id, e)

let rec map_for_statement env v =
  match v with
  | `For_LPAR_choice_var_decl_stmt_choice_exp_stmt_opt_exp_RPAR_stmt
      (v1, v2, v3, v4, v5, v6, v7) ->
      let tfor = (* "for" *) token env v1 in
      let _lp = (* "(" *) token env v2 in
      let init =
        match v3 with
        | `Var_decl_stmt x ->
            let ent, vdef = map_variable_declaration_statement env x in
            [ ForInitVar (ent, vdef) ]
        | `Exp_stmt x ->
            let e, _sc = map_expression_statement env x in
            [ ForInitExpr e ]
        | `Semi tok ->
            (* ";" *)
            let _sc = token env tok in
            []
      in
      let cond =
        match v4 with
        | `Exp_stmt x ->
            let e, _sc = map_expression_statement env x in
            Some e
        | `Semi tok ->
            let _sc = (* ";" *) token env tok in
            None
      in
      let post =
        match v5 with
        | Some x -> Some (map_expression env x)
        | None -> None
      in
      let _rp = (* ")" *) token env v6 in
      let st = map_statement env v7 in
      For (tfor, ForClassic (init, cond, post), st) |> G.s
  | `For_LPAR_ellips_RPAR_stmt (v1, v2, v3, v4, v5) ->
      let tfor = (* "for" *) token env v1 in
      let _lp = (* "(" *) token env v2 in
      let tellipsis = (* "..." *) token env v3 in
      let _rp = (* ")" *) token env v4 in
      let st = map_statement env v5 in
      For (tfor, ForEllipsis tellipsis, st) |> G.s

and map_variable_declaration_statement (env : env)
    ((v1, v2, v3, v4, v5) : CST.variable_declaration_statement) =
  let ty = map_type_ env v1 in
  let id, e = map_variable_initialization env v2 ty in
  let v3TODO =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_variable_initialization env v2 ty in
        v2)
      v3
  in
  let _v4 =
    match v4 with
    | Some tok -> Some ((* "," *) token env tok)
    | None -> None
  in
  let sc = (* ";" *) token env v5 in
  let ent = G.basic_entity id in
  let def = { vtype = Some ty; vinit = e; vtok = Some sc } in
  (ent, def)

and map_statement (env : env) (x : CST.statement) =
  match x with
  | `Ret_stmt (v1, v2, v3) ->
      let v1 = (* "return" *) token env v1 in
      let v2 = Some (map_expression env v2) in
      let v3 = (* ";" *) token env v3 in
      Return (v1, v2, v3) |> G.s
  | `Blk_stmt (v1, v2, v3) ->
      let v1 = (* "{" *) token env v1 in
      let v2 = List_.map (map_statement env) v2 in
      let v3 = (* "}" *) token env v3 in
      Block (v1, v2, v3) |> G.s
  | `If_stmt (v1, v2, v3, v4, v5, v6) ->
      let tif = (* "if" *) token env v1 in
      let _lp = (* "(" *) token env v2 in
      let cond = map_expression env v3 in
      let _rp = (* ")" *) token env v4 in
      let then_ = map_statement env v5 in
      let else_opt =
        match v6 with
        | Some (v1, v2) ->
            let v1 = (* "else" *) token env v1 in
            let st = map_statement env v2 in
            Some st
        | None -> None
      in
      If (tif, Cond cond, then_, else_opt) |> G.s
  | `For_stmt x -> map_for_statement env x
  | `While_stmt (v1, v2, v3, v4, v5) ->
      let twhile = (* "while" *) token env v1 in
      let _lp = (* "(" *) token env v2 in
      let cond = map_expression env v3 in
      let _rp = (* ")" *) token env v4 in
      let st = map_statement env v5 in
      While (twhile, Cond cond, st) |> G.s
  | `Var_decl_stmt x ->
      let ent, vdef = map_variable_declaration_statement env x in
      DefStmt (ent, VarDef vdef) |> G.s
  | `Exp_stmt x ->
      let e, sc = map_expression_statement env x in
      ExprStmt (e, sc) |> G.s

let map_function_body (env : env) ((v1, v2, v3) : CST.function_body) :
    function_body =
  let lb = (* "{" *) token env v1 in
  let xs = List_.map (map_statement env) v2 in
  let rb = (* "}" *) token env v3 in
  FBStmt (Block (lb, xs, rb) |> G.s)

let map_template_body (env : env) ((v1, v2, v3) : CST.template_body) :
    function_body =
  let lb = (* "{" *) token env v1 in
  let xs = List_.map (map_statement env) v2 in
  let rb = (* "}" *) token env v3 in
  FBStmt (Block (lb, xs, rb) |> G.s)

let map_template_type (env : env) (x : CST.template_type) =
  match x with
  | `Custom tok -> (* "custom" *) G.OtherAttribute (str env tok, [])
  | `Para tok -> (* "parallel" *) G.OtherAttribute (str env tok, [])

let map_main_component_public_signals (env : env)
    ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.main_component_public_signals) =
  let v1 = (* "{" *) token env v1 in
  let v2 = (* "public" *) token env v2 in
  let v3 = (* "[" *) token env v3 in
  let first_param = map_parameter env v4 in
  let params =
    first_param
    :: List_.map
         (fun (v1, v2) ->
           let v1 = (* "," *) token env v1 in
           let v2 = map_parameter env v2 in
           v2)
         v5
  in
  let _v6 =
    match v6 with
    | Some tok -> Some ((* "," *) token env tok)
    | None -> None
  in
  let v7 = (* "]" *) token env v7 in
  let v8 = (* "}" *) token env v8 in
  (v3, params, v7)

let map_definition (env : env) (x : CST.definition) =
  match x with
  | `Func_defi (v1, v2, v3, v4) ->
      let tfunc = (* "function" *) token env v1 in
      let id =
        (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v2 |> left_strip_space
      in
      let params = map_parameter_list env v3 in
      let fbody = map_function_body env v4 in
      let ent = G.basic_entity id in
      let def =
        { fkind = (Function, tfunc); fparams = params; frettype = None; fbody }
      in
      (ent, FuncDef def)
  | `Temp_defi (v1, v2, v3, v4, v5) ->
      let tfunc = (* "template" *) token env v1 in
      let attrs =
        match v2 with
        | Some x -> [ map_template_type env x ]
        | None -> []
      in
      let id =
        (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env v3 |> left_strip_space
      in
      let params = map_parameter_list env v4 in
      let fbody = map_template_body env v5 in
      let ent = G.basic_entity id ~attrs in
      let def =
        { fkind = (Function, tfunc); fparams = params; frettype = None; fbody }
      in
      (ent, FuncDef def)
  | `Main_comp_defi (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "component" *) str env v1 in
      let id = (* "main" *) str env v2 in
      let v4 = (* "=" *) token env v4 in
      let _paramsTODO =
        match v3 with
        | Some x -> map_main_component_public_signals env x
        | None -> (fake "", [], fake "")
      in
      let ty = G.ty_builtin v1 in
      let vinit = map_call_expression env v5 in
      let sc = (* ";" *) token env v6 in
      let ent = G.basic_entity id in
      let def = { vinit = Some vinit; vtype = Some ty; vtok = Some sc } in
      (ent, VarDef def)

let map_source_unit (env : env) (x : CST.source_unit) : item list =
  match x with
  | `Dire x ->
      let xs = map_directive env x in
      xs |> List_.map (fun dir -> DirectiveStmt dir |> G.s)
  | `Defi x ->
      let def = map_definition env x in
      [ DefStmt def |> G.s ]

let map_source_file (env : env) (x : CST.source_file) =
  match x with
  | `Rep_source_unit v1 ->
      let xss = List_.map (map_source_unit env) v1 in
      Pr (List_.flatten xss)
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
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      match map_source_file env cst with
      | G.Pr xs
      | G.Ss xs ->
          xs
      | _ -> failwith "not a program")

(* todo: special mode to convert Ellipsis in the right construct! *)
let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_circom.Parse.string str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env = { H.file; conv = H.line_col_to_pos_pattern str; extra = () } in
      map_source_file env cst)
