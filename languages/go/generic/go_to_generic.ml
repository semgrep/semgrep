(* Yoann Padioleau
 *
 * Copyright (C) 2020-2022 r2c
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
open Either_
open Ast_go
module G = AST_generic
module H = AST_generic_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Ast_go to AST_generic.
 *
 * See AST_generic.ml for more information.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let id x = x
let string = id
let list = List_.map
let option = Option.map
let either = OCaml.map_of_either
let arithmetic_operator = id
let incr_decr = id
let prefix_postfix x = x
let error = AST_generic.error

let name_of_qualified_ident x =
  let xs =
    match x with
    | Left id -> [ id ]
    | Right (xs, id) -> xs @ [ id ]
  in
  H.name_of_ids xs

let fake tok s = Tok.fake_tok tok s
let unsafe_fake s = Tok.unsafe_fake_tok s
let _fake_id tok s = (s, fake tok s)
let unsafe_fake_id s = (s, unsafe_fake s)
let fb = Tok.unsafe_fake_bracket
let mk_name s tok = G.Id ((s, tok), G.empty_id_info ())

(* TODO? do results "parameters" can have names? *)
let return_type_of_results results =
  match results with
  | []
  | [ G.Param { G.ptype = None; _ } ] ->
      None
  | [ G.Param { G.ptype = Some t; _ } ] -> Some t
  | xs ->
      Some
        (G.TyTuple
           (xs
           |> List_.map (function
                | G.Param { G.ptype = Some t; _ } -> t
                | G.Param { G.ptype = None; _ } -> raise Impossible
                | G.ParamEllipsis t -> G.TyEllipsis t |> G.t
                | _ -> raise Impossible)
           |> fb)
        |> G.t)

let list_to_tuple_or_expr xs =
  match xs with
  | [] -> raise Impossible
  | [ x ] -> x
  | xs -> G.Container (G.Tuple, Tok.unsafe_fake_bracket xs) |> G.e

let mk_func_def fkind params ret st : G.function_definition =
  { G.fparams = params; frettype = ret; fbody = st; fkind }

(* TODO: use CondDecl *)
let wrap_init_in_block_maybe x v =
  match x with
  | None -> [ v ]
  | Some e -> [ G.s (G.Block (fb [ G.exprstmt e; v ])) ]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tok v = v

let wrap _of_a (v1, v2) =
  let v1 = _of_a v1 and v2 = tok v2 in
  (v1, v2)

let bracket of_a (t1, x, t2) = (tok t1, of_a x, tok t2)
let ident v = wrap string v

let qualified_ident v =
  match list ident v with
  | [ x ] -> Left x
  | [ x; y ] -> Right ([ x ], y)
  | _ -> raise Impossible

let top_func () =
  let rec type_ x = type_kind x |> G.t
  and type_kind = function
    | TName v1 ->
        let v1 = qualified_ident v1 in
        let name = name_of_qualified_ident v1 in
        G.TyN name
    | TGeneric (v1, v2) ->
        let id = qualified_ident v1 in
        let targs = type_arguments v2 in
        let name = name_of_qualified_ident id in
        G.TyApply (G.TyN name |> G.t, targs)
    | TPtr (t, v1) ->
        let v1 = type_ v1 in
        G.TyPointer (t, v1)
    | TArray (v1, v2) ->
        let v1 = bracket (option expr) v1 and v2 = type_ v2 in
        G.TyArray (v1, v2)
    | TArrayEllipsis (v1, v2) ->
        let t1, _v1, t2 = bracket tok v1 and v2 = type_ v2 in
        G.TyArray ((t1, None, t2), v2)
    | TFunc v1 ->
        let _ftok, (_l, params, r), ret = func_type v1 in
        let ret =
          match ret with
          | None -> G.ty_builtin ("void", r)
          | Some t -> t
        in
        G.TyFun (params, ret)
    | TMap (t, (lp, v1, rp), v2) ->
        let v1 = type_ v1 and v2 = type_ v2 in
        G.TyApply
          (G.TyN (mk_name "map" t) |> G.t, (lp, [ G.TA v1; G.TA v2 ], rp))
    | TChan (t, v1, v2) ->
        let v1 = chan_dir v1 and v2 = type_ v2 in
        G.TyApply (G.TyN (mk_name "chan" t) |> G.t, fb [ G.TA v1; G.TA v2 ])
    | TStruct (t, v1) ->
        let v1 = bracket (list struct_field) v1 in
        G.TyRecordAnon ((G.Class, t), v1)
    | TInterface (t, v1) ->
        let v1 = bracket (list interface_field) v1 in
        G.TyRecordAnon ((G.Interface, t), v1)
  and type_arguments v = bracket (list type_argument) v
  and type_argument v =
    let t = type_ v in
    G.TA t
  and chan_dir = function
    | TSend -> G.TyN (G.Id (unsafe_fake_id "send", G.empty_id_info ())) |> G.t
    | TRecv -> G.TyN (G.Id (unsafe_fake_id "recv", G.empty_id_info ())) |> G.t
    | TBidirectional ->
        G.TyN (G.Id (unsafe_fake_id "bidirectional", G.empty_id_info ())) |> G.t
  and func_type { ftok; fparams = l, params, r; fresults } :
      G.tok * G.parameters * G.type_ option =
    let fparams = list parameter_binding params in
    let fresults = list parameter_binding fresults in
    (ftok, (l, fparams, r), return_type_of_results fresults)
  and parameter_binding x =
    match x with
    | ParamClassic x -> parameter x
    | ParamEllipsis t -> G.ParamEllipsis t
    | ParamMetavarEllipsis id -> G.Param (G.param_of_id id)
  and parameter x =
    match x with
    | { pname; ptype; pdots } -> (
        let arg1 = option ident pname in
        let arg2 = type_ ptype in
        let arg3 = option tok pdots in
        let pclassic =
          {
            G.pname = arg1;
            ptype = Some arg2;
            pdefault = None;
            pattrs = [];
            pinfo = G.empty_id_info ();
          }
        in
        match arg3 with
        | None -> G.Param pclassic
        | Some tok -> G.ParamRest (tok, pclassic))
  and struct_field (v1, v2) =
    let attrs = option tag v2 |> Option.value ~default:[] in
    struct_field_kind attrs v1
  and struct_field_kind attrs = function
    | Field (v1, v2) ->
        let id = ident v1 and vtype = Some (type_ v2) in
        let entity =
          G.{ name = EN (Id (id, empty_id_info ())); attrs; tparams = None }
        in
        G.(fld (entity, VarDef { vinit = None; vtype; vtok = no_sc }))
    | EmbeddedField (v1, v2) ->
        let _v1TODO = option tok v1 and v2 = qualified_ident v2 in
        let name = name_of_qualified_ident v2 in
        let spec = (G.Spread, unsafe_fake "...") in
        let e = G.special spec [ G.N name |> G.e ] in
        let st = G.exprstmt e in
        G.F st
    | FieldEllipsis t -> G.field_ellipsis t
  and tag v =
    let attr = G.(E (e (L (String (fb v))))) in
    [ G.OtherAttribute (("GoTag", snd v), [ attr ]) ]
  and interface_field = function
    | Method (v1, v2) ->
        let v1 = ident v1 in
        let ftok, params, ret = func_type v2 in
        let ent = G.basic_entity v1 in
        let fdef =
          G.FuncDef (mk_func_def (G.Method, ftok) params ret (G.FBDecl G.sc))
        in
        G.fld (ent, fdef)
    | EmbeddedInterface v1 ->
        let v1 = qualified_ident v1 in
        let name = name_of_qualified_ident v1 in
        let spec = (G.Spread, unsafe_fake "...") in
        let e = G.special spec [ G.N name |> G.e ] in
        let st = G.exprstmt e in
        G.F st
    | FieldEllipsis2 t -> G.field_ellipsis t
    | Constraints xs -> (
        match xs with
        | [] -> raise Impossible
        | (_tilde_optTODO, ty) :: _xsTODO ->
            let ty = type_ ty in
            let st = G.OtherStmt (G.OS_Todo, [ G.T ty ]) |> G.s in
            G.F st)
  and expr_or_type v = either expr type_ v
  (* used for translating call make/call new *)
  and gen_new ty (l, args, r) t name =
    let return (ty, args) =
      G.New (fake t name, ty, G.empty_id_info (), (l, args, r))
    in
    let args = arguments args in
    (* this translation (esp for make) depends on the first argument,
     * where
     *)
    match ty with
    (* the first arg is indeed a type *)
    | ArgType ty -> return (type_ ty, args)
    (* metavar *)
    | Arg (Id v1) -> return (G.TyN (G.Id (v1, G.empty_id_info ())) |> G.t, args)
    (* ... *)
    | Arg (Ellipsis tok) ->
        return (G.TyEllipsis tok |> G.t, G.Arg (G.Ellipsis tok |> G.e) :: args)
    (* exp *)
    | Arg exp -> return (G.TyExpr (expr exp) |> G.t, args)
    | ArgDots _ -> raise Impossible
  and expr e =
    (match e with
    | DotAccessEllipsis (v1, v2) ->
        let v1 = expr v1 in
        G.DotAccessEllipsis (v1, v2)
    | BasicLit v1 ->
        let v1 = literal v1 in
        G.L v1
    | Id v1 ->
        let v1 = ident v1 in
        G.N (G.Id (v1, G.empty_id_info ()))
    | Selector (v1, v2, v3) ->
        let v1 = expr v1 and v2 = tok v2 and v3 = ident v3 in
        G.DotAccess (v1, v2, G.FN (Id (v3, G.empty_id_info ())))
    | Index (v1, v2) ->
        let v1 = expr v1 and v2 = bracket index v2 in
        G.ArrayAccess (v1, v2)
    (* It's much better to trans these calls to new/ref(new), as:
     * x : *tau =  new(tau)
     * x :  tau = make(tau)
     * and other sem(grep)antic information is useful for future analysis.
     *)
    | Call (Id ("new", t), None, (l, [ ty ], r)) ->
        G.Ref (fake t "new", gen_new ty (l, [], r) t "new" |> G.e)
    | Call (Id ("make", t), None, (l, ty :: args, r)) ->
        gen_new ty (l, args, r) t "make"
    | Call v1 ->
        let e, args = call_expr v1 in
        G.Call (e, args)
    | Cast (t, (l, e, r)) ->
        let t = type_ t and e = expr e in
        (* for semgrep and autofix to get the right range by including
         * 'r' in the range.
         * alt: change G.Cast to take a bracket
         *)
        AST_generic_helpers.set_e_range l r e;
        G.Cast (t, l, e)
    | Deref (v1, v2) ->
        let v1 = tok v1 and v2 = expr v2 in
        G.DeRef (v1, v2)
    | Ref (v1, v2) ->
        let v1 = tok v1 and v2 = expr v2 in
        G.Ref (v1, v2)
    | Unary (v1, v2) ->
        let v1, tok = wrap arithmetic_operator v1 and v2 = expr v2 in
        G.Call (G.IdSpecial (G.Op v1, tok) |> G.e, fb [ G.arg v2 ])
    | Binary (v1, v2, v3) ->
        let v1 = expr v1
        and v2, tok = wrap arithmetic_operator v2
        and v3 = expr v3 in
        G.Call
          (G.IdSpecial (G.Op v2, tok) |> G.e, fb ([ v1; v3 ] |> List_.map G.arg))
    | CompositeLit (v1, v2) ->
        let v1 = type_ v1
        and l, v2, r = bracket (list init_for_composite_lit) v2 in
        G.New (fake l "new", v1, G.empty_id_info (), (l, v2, r))
    | Slice (v1, (t1, v2, t2)) ->
        let e = expr v1 in
        let v1, v2, v3 = v2 in
        let v1 = option expr v1
        and v2 = option expr v2
        and v3 = option expr v3 in
        G.SliceAccess (e, (t1, (v1, v2, v3), t2))
    | TypeAssert (v1, (lp, v2, rp)) ->
        let v1 = expr v1 and v2 = type_ v2 in
        G.Call
          ( G.IdSpecial (G.Instanceof, fake lp "instanceof") |> G.e,
            (lp, [ G.Arg v1; G.ArgType v2 ], rp) )
    | Ellipsis v1 ->
        let v1 = tok v1 in
        G.Ellipsis v1
    | DeepEllipsis v1 ->
        let v1 = bracket expr v1 in
        G.DeepEllipsis v1
    | TypedMetavar (v1, v2, v3) ->
        let v1 = ident v1 in
        let v3 = type_ v3 in
        G.TypedMetavar (v1, v2, v3)
    | FuncLit (v1, v2) ->
        let ftok, params, ret = func_type v1 and v2 = stmt v2 in
        G.Lambda (mk_func_def (G.LambdaKind, ftok) params ret (G.FBStmt v2))
    | Receive (v1, v2) ->
        let v1 = tok v1 and v2 = expr v2 in
        G.OtherExpr (("Receive", v1), [ G.E v2 ])
    | Send (v1, v2, v3) ->
        let v1 = expr v1 and v2 = tok v2 and v3 = expr v3 in
        G.OtherExpr (("Send", v2), [ G.E v1; G.E v3 ])
    | TypeSwitchExpr (v1, v2) ->
        let _v1 = expr v1 and v2 = tok v2 in
        error v2 "TypeSwitchExpr should be handled in Switch statement"
    | ParenType v1 ->
        let v1 = type_ v1 in
        error
          (AST_generic_helpers.info_of_any (G.T v1))
          ("ParenType should disappear" ^ Dumper.dump v1))
    |> G.e
  and literal = function
    | Int v1 -> G.Int v1
    | Float v1 ->
        let v1 = wrap id v1 in
        G.Float v1
    | Imag v1 ->
        let v1 = wrap string v1 in
        G.Imag v1
    | Rune v1 ->
        let v1 = wrap string v1 in
        G.Char v1
    | String v1 ->
        let v1 = wrap string v1 in
        G.String (fb v1)
  and index v = expr v
  and arguments v = list argument v
  and argument = function
    | Arg v1 ->
        let v1 = expr v1 in
        G.Arg v1
    | ArgType v1 ->
        let v1 = type_ v1 in
        G.ArgType v1
    | ArgDots (v1, v2) ->
        let v1 = expr v1 in
        let v2 = tok v2 in
        let special =
          G.Call (G.IdSpecial (G.Spread, v2) |> G.e, fb [ G.arg v1 ]) |> G.e
        in
        G.Arg special
  and init = function
    | InitExpr v1 ->
        let v1 = expr v1 in
        v1
    | InitKeyValue (v1, v2, v3) ->
        let v1 = init v1 and _v2 = tok v2 and v3 = init v3 in
        G.Container (G.Tuple, Tok.unsafe_fake_bracket [ v1; v3 ]) |> G.e
    | InitBraces v1 ->
        let v1 = bracket (list init) v1 in
        (* Note that here we generate a List instead of a Dict
         * because not all elements are InitKeyValue (some are InitExpr).
         * TODO? we could detect if only has InitKeyValue and generate a Dict
         * instead?
         *)
        G.Container (G.List, v1) |> G.e
  (* Why generating ArgKwd instead of a Record with fields? Because in Go you
   * don't have to name every fields in a CompositeLit, so we need a case for
   * unamed fields, hence Arg and ArgKwd.
   * TODO? why do we generate Arg and ArgKwd for toplevel CompositeLit inits
   * and Container(List) for nested inits (see init() abobe)?
   *)
  and init_for_composite_lit = function
    | InitExpr v1 ->
        let v1 = expr v1 in
        G.Arg v1
    | InitKeyValue (v1, v2, v3) -> (
        let v2 = tok v2 and v3 = init v3 in
        match v1 with
        | InitExpr (Id id) -> G.ArgKwd (id, v3)
        | _ -> G.Arg (G.keyval (init v1) v2 v3))
    | InitBraces v1 ->
        let v1 = bracket (list init) v1 in
        G.Arg (G.Container (G.List, v1) |> G.e)
  and constant_expr v = expr v
  and simple = function
    | ExprStmt v1 ->
        let v1 = expr v1 in
        v1
    (* nice language! Assigns are at statement level! *)
    | Assign (v1, v2, v3) ->
        let v1 = list expr v1 and v2 = tok v2 and v3 = list expr v3 in
        G.Assign (list_to_tuple_or_expr v1, v2, list_to_tuple_or_expr v3) |> G.e
    | DShortVars (v1, v2, v3) ->
        let v1 = list expr v1 and v2 = tok v2 and v3 = list expr v3 in
        (* people don't want '=' assign pattern to match ':=' short var decls,
         * so better to not generate an Assign there too.
         * less: could define a ColonEq operator in AST_generic.ml
         *)
        G.AssignOp
          (list_to_tuple_or_expr v1, (G.Eq, v2), list_to_tuple_or_expr v3)
        |> G.e
    | AssignOp (v1, v2, v3) ->
        let v1 = expr v1
        and v2, tok = wrap arithmetic_operator v2
        and v3 = expr v3 in
        G.AssignOp (v1, (v2, tok), v3) |> G.e
    | IncDec (v1, v2, v3) ->
        let v1 = expr v1
        and v2, tok = wrap incr_decr v2
        and v3 = prefix_postfix v3 in
        G.Call (G.IdSpecial (G.IncrDecr (v2, v3), tok) |> G.e, fb [ G.Arg v1 ])
        |> G.e
  (* invariant: you should not use 'list stmt', but instead always
   * use list stmt_aux ... |> List_.flatten
   *)
  and stmt x = G.stmt1 (stmt_aux x)
  and stmt_aux = function
    | DeclStmts v1 ->
        (* bugfix:
         *  old: let v1 = list decl v1 in
         *       G.Block v1
         * but in sgrep with
         *   var $X = "...";
         *   ...
         *   var $Y = $X
         * we do no want the DeclStmts to be put under an extra Block
         * otherwise they will not match. This is why stmt_aux need
         * to return a list of stmts that may or may not be put inside
         * a Block depending on the context (Ss and Items will not).
         *)
        let v1 = list decl v1 in
        v1
    | Block (t1, v1, t2) ->
        let v1 = list stmt_aux v1 |> List_.flatten in
        [ G.Block (t1, v1, t2) |> G.s ]
    | Empty -> [ G.Block (fb []) |> G.s ]
    | SimpleStmt v1 ->
        let v1 = simple v1 in
        [ G.exprstmt v1 ]
    | If (t, v1, v2, v3, v4) ->
        let v1 = option simple v1
        and v2 = expr v2
        and v3 = stmt v3
        and v4 = option stmt v4 in
        (* TODO: use OtherCond and CondDecl! *)
        wrap_init_in_block_maybe v1 (G.If (t, G.Cond v2, v3, v4) |> G.s)
    | Switch (v0, v1, v2, v3) ->
        let v0 = tok v0 in
        let v1 = option simple v1
        and v2 =
          match v2 with
          | None -> None
          | Some s ->
              Some
                (G.Cond
                   (match s with
                   | ExprStmt (TypeSwitchExpr (e, tok1)) ->
                       let e = expr e in
                       G.Call
                         (G.IdSpecial (G.Typeof, tok1) |> G.e, fb [ G.Arg e ])
                       |> G.e
                   | DShortVars (xs, tok1, [ TypeSwitchExpr (e, tok2) ]) ->
                       let xs = list expr xs in
                       let e = expr e in
                       G.Assign
                         ( list_to_tuple_or_expr xs,
                           tok1,
                           G.Call
                             ( G.IdSpecial (G.Typeof, tok2) |> G.e,
                               fb [ G.Arg e ] )
                           |> G.e )
                       |> G.e
                   | s -> simple s))
        and v3 = list case_clause v3 in
        wrap_init_in_block_maybe v1 (G.Switch (v0, v2, v3) |> G.s)
    | Select (v1, v2) ->
        let v1 = tok v1 and v2 = list comm_clause v2 in
        [ G.Switch (v1, None, v2) |> G.s ]
    | For (t, vx, v4) ->
        let vx = for_header vx in
        let v4 = stmt v4 in
        [ G.For (t, vx, v4) |> G.s ]
    | Return (v1, v2) ->
        let v1 = tok v1 and v2 = option (list expr) v2 in
        [ G.Return (v1, v2 |> Option.map list_to_tuple_or_expr, G.sc) |> G.s ]
    | Break (v1, v2) ->
        let v1 = tok v1 and v2 = option ident v2 in
        [ G.Break (v1, H.opt_to_label_ident v2, G.sc) |> G.s ]
    | Continue (v1, v2) ->
        let v1 = tok v1 and v2 = option ident v2 in
        [ G.Continue (v1, H.opt_to_label_ident v2, G.sc) |> G.s ]
    | Goto (v1, v2) ->
        let v1 = tok v1 and v2 = ident v2 in
        [ G.Goto (v1, v2, G.sc) |> G.s ]
    | Fallthrough v1 ->
        let v1 = tok v1 in
        [ G.OtherStmt (G.OS_Fallthrough, [ G.Tk v1 ]) |> G.s ]
    | Label (v1, v2) ->
        let v1 = ident v1 and v2 = stmt v2 in
        [ G.Label (v1, v2) |> G.s ]
    | Go (v1, v2) ->
        let _v1 = tok v1 and e, args = call_expr v2 in
        [ G.OtherStmt (G.OS_Go, [ G.E (G.Call (e, args) |> G.e) ]) |> G.s ]
    | Defer (v1, v2) ->
        let _v1 = tok v1 and e, args = call_expr v2 in
        [ G.OtherStmt (G.OS_Defer, [ G.E (G.Call (e, args) |> G.e) ]) |> G.s ]
  and for_header = function
    | ForEllipsis t -> G.ForEllipsis t
    | ForClassic (v1, v2, v3) ->
        let v1 = option simple v1 in
        let v2 = option expr v2 in
        let v3 = option simple v3 in
        (* TODO: some of v1 are really ForInitVar *)
        let init =
          match v1 with
          | None -> []
          | Some e -> [ G.ForInitExpr e ]
        in
        G.ForClassic (init, v2, v3)
    | ForRange (v1, v2, v3) -> (
        let opt =
          option
            (fun (v1, v2) ->
              let v1 = list expr v1 and v2 = tok v2 in
              (v1, v2))
            v1
        and v2 = tok v2
        and v3 = expr v3 in
        match opt with
        | None ->
            let pattern = G.PatWildcard (fake v2 "_") in
            G.ForEach (pattern, v2, v3)
        | Some (xs, _tokEqOrColonEqTODO) ->
            let pattern =
              G.PatTuple
                (xs |> List_.map H.expr_to_pattern |> Tok.unsafe_fake_bracket)
            in
            G.ForEach (pattern, v2, v3))
  and case_clause = function
    | CaseClause (v1, v2) ->
        let v1 = case_kind v1 and v2 = stmt v2 in
        G.CasesAndBody (v1, v2)
    | CaseEllipsis (_v1, v2) -> G.CaseEllipsis v2
  and expr_or_type_to_pattern = function
    (* can't call expr_or_type because we want to intercept this one *)
    | Left (ParenType t) ->
        let t = type_ t in
        G.PatType t
    | x -> (
        match expr_or_type x with
        | Left e -> H.expr_to_pattern e
        | Right t -> G.PatType t)
  and case_kind = function
    | CaseExprs (tok, v1) ->
        v1 |> List_.map (fun x -> G.Case (tok, expr_or_type_to_pattern x))
    | CaseAssign (tok, v1, v2, v3) ->
        let v1 = list expr_or_type v1 and v3 = expr v3 in
        let v1 =
          v1
          |> List_.map (function
               | Left e -> e
               | Right _ -> error tok "TODO: Case Assign with Type?")
        in
        [
          G.CaseEqualExpr
            (tok, G.Assign (list_to_tuple_or_expr v1, v2, v3) |> G.e);
        ]
    | CaseDefault v1 ->
        let v1 = tok v1 in
        [ G.Default v1 ]
  and comm_clause v = case_clause v
  and call_expr (v1, v2, v3) =
    let e = expr v1 in
    let _toptTODO = option type_arguments v2 in
    let args = bracket arguments v3 in
    (e, args)
  and decl = function
    | DConst (v1, v2, v3) ->
        let v1 = ident v1
        and v2 = option type_ v2
        and v3 = option constant_expr v3 in
        let ent =
          G.basic_entity v1 ~attrs:[ G.attr G.Const (fake (snd v1) "const") ]
        in
        G.DefStmt (ent, G.VarDef { G.vinit = v3; vtype = v2; vtok = G.no_sc })
        |> G.s
    | DVar (v1, v2, v3) ->
        let v1 = ident v1 and v2 = option type_ v2 and v3 = option expr v3 in
        let ent =
          G.basic_entity v1 ~attrs:[ G.attr G.Var (fake (snd v1) "var") ]
        in
        G.DefStmt (ent, G.VarDef { G.vinit = v3; vtype = v2; vtok = G.no_sc })
        |> G.s
    | DTypeAlias (v1, v2, v3) ->
        let v1 = ident v1 and _v2 = tok v2 and v3 = type_ v3 in
        let ent = G.basic_entity v1 in
        G.DefStmt (ent, G.TypeDef { G.tbody = G.AliasType v3 }) |> G.s
    | DTypeDef (v1, v2, v3) ->
        let id = ident v1 in
        let tparams = option type_parameters v2 in
        let ty = type_ v3 in
        let ent = G.basic_entity id ?tparams in
        G.DefStmt (ent, G.TypeDef { G.tbody = G.NewType ty }) |> G.s
  and type_parameters v : G.type_parameters = bracket (list type_parameter) v
  and type_parameter v : G.type_parameter =
    let p = parameter_binding v in
    G.OtherTypeParam (("Param", G.fake ""), [ G.Pa p ])
  and top_decl = function
    | DFunc (v1, v2, (v3, v4)) ->
        let v1 = ident v1 in
        let tparams = option type_parameters v2 in
        let ftok, params, ret = func_type v3 in
        let body = stmt v4 in
        let ent = G.basic_entity v1 ?tparams in
        G.DefStmt
          ( ent,
            G.FuncDef
              (mk_func_def (G.Function, ftok) params ret (G.FBStmt body)) )
        |> G.s
    | DMethod (v1, v2, (v3, v4)) ->
        let v1 = ident v1
        and v2 = parameter v2
        and ftok, params, ret = func_type v3
        and v4 = stmt v4 in
        let ent = G.basic_entity v1 in
        let def = mk_func_def (G.Method, ftok) params ret (G.FBStmt v4) in
        let receiver =
          match v2 with
          | G.Param x -> G.ParamReceiver x
          (* Can this happen? Probably not, but the Go AST allows it *)
          | _ -> G.OtherParam (("Receiver", snd v1), [ G.Pa v2 ])
        in
        let l, params, r = def.G.fparams in
        let fparams = (l, receiver :: params, r) in
        G.DefStmt (ent, G.FuncDef { def with G.fparams }) |> G.s
    | DTop v1 ->
        let v1 = decl v1 in
        v1
    | STop v1 -> stmt v1
    | Package (t1, id) ->
        let id = ident id in
        G.DirectiveStmt (G.Package (t1, [ id ]) |> G.d) |> G.s
    | Import x ->
        let x = import x in
        G.DirectiveStmt (x |> G.d) |> G.s
  and import { i_path; i_kind; i_tok } =
    let module_name = G.FileName (wrap string i_path) in
    let s, tok = i_path in
    import_kind i_tok i_kind module_name (Filename.basename s, tok)
  and import_kind itok kind module_name _id_no_more_used =
    match kind with
    | ImportOrig ->
        (* old: in Go, import "a/b/c" is really equivalent to import c "a/b/c",
         * but we don't do return anymore G.ImportAs (itok, module_name, Some id)
         * otherwise sgrep can not know if an alias was explicitely given or not
         *)
        G.ImportAs (itok, module_name, None)
    | ImportNamed v1 ->
        let v1 = ident v1 in
        G.ImportAs (itok, module_name, Some (v1, G.empty_id_info ()))
    | ImportDot v1 ->
        let v1 = tok v1 in
        G.ImportAll (itok, module_name, v1)
  and program xs = list top_decl xs
  (* TODO? put after program and imports? *)
  and item_aux = function
    | ITop x -> [ top_decl x ]
    | IImport x ->
        let x = import x in
        [ G.DirectiveStmt (x |> G.d) |> G.s ]
    | IStmt x -> stmt_aux x
  and item x = G.stmt1 (item_aux x)
  and partial = function
    | PartialDecl x -> (
        let x = top_decl x in
        match x.G.s with
        | G.DefStmt def -> G.Partial (G.PartialDef def)
        | _ -> failwith "partial supported only for definitions")
    | PartialSingleField (v1, v2, v3) ->
        let v1 = ident v1 in
        let v3 = init v3 in
        G.Partial (G.PartialSingleField (v1, v2, v3))
    | PartialInitBraces v1 ->
        let v1 = bracket (list init) v1 in
        let e = G.Container (G.List, v1) |> G.e in
        G.E e
  and any x =
    let res =
      match x with
      | Partial v1 ->
          let v1 = partial v1 in
          v1
      | E v1 ->
          let v1 = expr v1 in
          G.E v1
      | S v1 ->
          let v1 = stmt v1 in
          G.S v1
      | T v1 ->
          let v1 = type_ v1 in
          G.T v1
      | Decl v1 ->
          let v1 = decl v1 in
          G.S v1
      | I v1 ->
          let v1 = import v1 in
          G.S (G.DirectiveStmt (v1 |> G.d) |> G.s)
      | P v1 ->
          let v1 = program v1 in
          G.Pr v1
      | Ident v1 ->
          let v1 = ident v1 in
          G.I v1
      (* not used anymore, Items is now used for sgrep *)
      | Ss v1 ->
          let v1 = list stmt_aux v1 in
          G.Ss (List_.flatten v1)
      | Item v1 ->
          let v1 = item v1 in
          G.S v1
      | Items v1 ->
          let v1 = list item_aux v1 in
          G.Ss (List_.flatten v1)
    in
    res
  in

  (program, any)

let program x =
  let p, _ = top_func () in
  p x

let any x =
  let _, a = top_func () in
  a x
