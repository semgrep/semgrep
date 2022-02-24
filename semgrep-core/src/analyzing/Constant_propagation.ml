(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open AST_generic
module H = AST_generic_helpers
module V = Visitor_AST

let logger = Logging.get_logger [ __MODULE__ ]

(* Deep Semgrep *)
let hook_constant_propagation_and_evaluate_literal = ref None

(* TODO: Remove duplication between the Generic-based and IL-based passes,
   ideally we should have a single pass, the IL-based. *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Two-pass constant propagation.
 *
 * 1. First pass is a basic flow-insensitive analysis but, crucially, it
 * considers global and class-level constants.
 * 2. Second pass is flow-sensitive, but intraprocedural and ignores the
 * global scope, it uses and refines the outcome of the first pass.
 *
 * This is mainly to provide advanced features to semgrep such as the
 * constant propagation of literals.
 *
 * Partial evaluation for Generic is provided by core/ast/Normalize_generic.ml,
 * we may not need it when we annotate every expression with constant/svalue info.
 *
 * Right now we just propagate constants when we are sure* that it is a constant
 * because:
 *  - the variable declaration use the 'const' keyword in Javascript/Go/...
 *  - the field declaration use the 'final' keyword in Java
 *  - we do a very basic const analysis where we check the variable
 *    is never assigned more than once.
 *
 * * We cannot be 100% sure e.g. due to aliasing.
 *
 * history:
 * - ver1: this used to be in Naming_AST.ml but better to split, even though
 * things will be slightly slower because we will visit the same file
 * twice.
 * - ver2: added second flow-sensitive constant propagation pass.
 * - ver3: do not assign constant values to labels; in x = E, x is just a label
 * (a "ref") and denotes a memory location rather than the value stored in it.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type var = string * AST_generic.sid

type env = {
  lang : Lang.t option;
  (* basic constant propagation of literals for semgrep *)
  constants : (var, svalue) Hashtbl.t;
  in_lvalue : bool ref;
}

let default_env lang =
  { lang; constants = Hashtbl.create 64; in_lvalue = ref false }

type lr_stats = {
  (* note that a VarDef defining the value will count as 1 *)
  lvalue : int ref;
  rvalue : int ref;
}

let default_lr_stats () = { lvalue = ref 0; rvalue = ref 0 }

type var_stats = (var, lr_stats) Hashtbl.t

(*****************************************************************************)
(* Environment Helpers *)
(*****************************************************************************)

let is_lang env l2 =
  match env.lang with
  | None -> false
  | Some l1 -> l1 = l2

let is_js env =
  match env.lang with
  | None -> false
  | Some lang -> Lang.is_js lang

let add_constant_env ident (sid, svalue) env =
  match svalue with
  | Lit _
  | Cst _ ->
      Hashtbl.add env.constants (H.str_of_ident ident, sid) svalue
  | Sym _
  | NotCst ->
      ()

let find_id env id id_info =
  match id_info with
  | { id_resolved = { contents = Some (_kind, sid) }; _ } ->
      let s = H.str_of_ident id in
      Hashtbl.find_opt env.constants (s, sid)
  | __else__ -> None

let find_name env name =
  match name with
  | Id (id, id_info) -> find_id env id id_info
  | IdQualified _ -> None

let deep_constant_propagation_and_evaluate_literal x =
  match !hook_constant_propagation_and_evaluate_literal with
  | None -> None
  | Some f -> f x

(*****************************************************************************)
(* Partial evaluation *)
(*****************************************************************************)
(* See also Dataflow_svalue.ml, for the IL-based version.... At some point
 * we should remove the code below and rely only on Dataflow_svalue.ml.
 * For that we may need to add `e_svalue` to AST_generic.expr and fill it in
 * during constant-propagation.
 *)

let ( let* ) = Option.bind

let fold_args1 f args =
  match args with
  | [] -> None
  | a1 :: args -> List.fold_left f a1 args

let find_type_args args =
  args
  |> List.find_map (function
       | Some (Lit (Bool _)) -> Some Cbool
       | Some (Lit (Int _)) -> Some Cint
       | Some (Lit (String _)) -> Some Cstr
       | Some (Cst ctype) -> Some ctype
       | _arg -> None)

let sign i = i asr (Sys.int_size - 1)

let int_add n m =
  let r = n + m in
  if sign n = sign m && sign r <> sign n then None (* overflow *) else Some r

let int_mult i1 i2 =
  let overflow =
    i1 <> 0 && i2 <> 0
    && ((i1 < 0 && i2 = min_int) (* >max_int *)
       || (i1 = min_int && i2 < 0) (* >max_int *)
       ||
       if sign i1 * sign i2 = 1 then abs i1 > abs (max_int / i2) (* >max_int *)
       else abs i1 > abs (min_int / i2) (* <min_int *))
  in
  if overflow then None else Some (i1 * i2)

let binop_int_cst op i1 i2 =
  match (i1, i2) with
  | Some (Lit (Int (Some n, t1))), Some (Lit (Int (Some m, _))) ->
      let* r = op n m in
      Some (Lit (Int (Some r, t1)))
  | Some (Lit (Int _)), Some (Cst Cint)
  | Some (Cst Cint), Some (Lit (Int _)) ->
      Some (Cst Cint)
  | _i1, _i2 -> None

let binop_bool_cst op b1 b2 =
  match (b1, b2) with
  | Some (Lit (Bool (b1, t1))), Some (Lit (Bool (b2, _))) ->
      Some (Lit (Bool (op b1 b2, t1)))
  | Some (Lit (Bool _)), Some (Cst Cbool)
  | Some (Cst Cbool), Some (Lit (Bool _)) ->
      Some (Cst Cbool)
  | _b1, _b2 -> None

let concat_string_cst s1 s2 =
  match (s1, s2) with
  | Some (Lit (String (s1, t1))), Some (Lit (String (s2, _))) ->
      Some (Lit (String (s1 ^ s2, t1)))
  | Some (Lit (String _)), Some (Cst Cstr)
  | Some (Cst Cstr), Some (Lit (String _))
  | Some (Cst Cstr), Some (Cst Cstr) ->
      Some (Cst Cstr)
  | _b1, _b2 -> None

let rec eval env x : svalue option =
  match x.e with
  | L x -> Some (Lit x)
  | N (Id (_, { id_svalue = { contents = Some x }; _ }))
  | DotAccess
      ( { e = IdSpecial (This, _); _ },
        _,
        FN (Id (_, { id_svalue = { contents = Some x }; _ })) ) ->
      Some x
  | Call
      ( { e = IdSpecial (EncodedString str_kind, _); _ },
        (_, [ Arg { e = L (String (str, str_tok) as str_lit); _ } ], _) ) -> (
      match str_kind with
      | "r" ->
          let str = String.escaped str in
          Some (Lit (String (str, str_tok)))
      | _else ->
          (* THINK: is this good enough for "b" and "u"? *)
          Some (Lit str_lit))
  | Call ({ e = IdSpecial (InterpolatedElement, _); _ }, (_, [ Arg e ], _)) ->
      eval env e
  | Call ({ e = IdSpecial special; _ }, args) -> eval_special env special args
  | Call ({ e = N name; _ }, args) -> eval_call env name args
  | N name -> (
      match find_name env name with
      | Some lit -> Some lit
      | None -> deep_constant_propagation_and_evaluate_literal x)
  | _ ->
      (* deep: *)
      deep_constant_propagation_and_evaluate_literal x

and eval_args env args =
  args |> unbracket
  |> List.map (function
       | Arg e -> eval env e
       | _ -> None)

and eval_special env (special, _) args =
  match (special, eval_args env args) with
  (* booleans *)
  | Op Not, [ Some (Lit (Bool (b, t))) ] -> Some (Lit (Bool (not b, t)))
  | Op Or, args -> fold_args1 (binop_bool_cst ( || )) args
  | Op And, args -> fold_args1 (binop_bool_cst ( && )) args
  (* integers *)
  | Op Plus, args when find_type_args args = Some Cint ->
      fold_args1 (binop_int_cst int_add) args
  | Op Mult, args when find_type_args args = Some Cint ->
      fold_args1 (binop_int_cst int_mult) args
  (* strings *)
  | (Op (Plus | Concat) | ConcatString _), args
    when find_type_args args = Some Cstr ->
      fold_args1 concat_string_cst args
  | __else__ -> None

and eval_call env name args =
  (* Built-in knowledge, we know these functions return constants when
   * given constants arguments. *)
  let args = eval_args env args in
  match (env.lang, name, args) with
  | ( Some Lang.Php,
      Id ((("escapeshellarg" | "htmlspecialchars_decode"), _), _),
      [ Some (Lit (String _) | Cst Cstr) ] ) ->
      Some (Cst Cstr)
  | _lang, _name, _args -> None

let constant_propagation_and_evaluate_literal ?lang =
  let env = default_env lang in
  eval env

let eval_expr env e = eval env e

(*****************************************************************************)
(* Poor's man const analysis *)
(*****************************************************************************)
(* This is mostly useful for languages without a const keyword (e.g., Python).
 *
 * Note that this may be incomplete and buggy. Worst case we do a
 * constant propagation on a variable containing a literal that may
 * have its content changed; not a big-deal in semgrep context actually.
 *
 * TODO: do it also for Java private fields; if they are not used in an
 * lvalue in this class, they can't be accessed from anywhere else so it's
 * safe to do the const propagation.
 *)
let var_stats prog : var_stats =
  let h = Hashtbl.create 101 in
  let get_stat_or_create var h =
    try Hashtbl.find h var
    with Not_found ->
      let stat = default_lr_stats () in
      Hashtbl.add h var stat;
      stat
  in

  let hooks =
    {
      V.default_visitor with
      V.kdef =
        (fun (k, _v) x ->
          match x with
          | ( {
                name =
                  EN
                    (Id
                      (id, { id_resolved = { contents = Some (_kind, sid) }; _ }));
                _;
              },
              VarDef { vinit = Some _e; _ } ) ->
              let var = (H.str_of_ident id, sid) in
              let stat = get_stat_or_create var h in
              incr stat.lvalue;
              k x
          | _ -> k x);
      V.kexpr =
        (fun (k, vout) x ->
          match x.e with
          (* TODO: very incomplete, what if Assign (Record?) *)
          | Assign
              ( {
                  e =
                    N
                      (Id
                        ( id,
                          { id_resolved = { contents = Some (_kind, sid) }; _ }
                        ));
                  _;
                },
                _,
                e2 )
          | AssignOp
              ( {
                  e =
                    N
                      (Id
                        ( id,
                          { id_resolved = { contents = Some (_kind, sid) }; _ }
                        ));
                  _;
                },
                _,
                e2 ) ->
              let var = (H.str_of_ident id, sid) in
              let stat = get_stat_or_create var h in
              incr stat.lvalue;
              (match x.e with
              | AssignOp _ -> incr stat.rvalue
              | _ -> ());
              vout (E e2)
          | Assign ({ e = Container ((Tuple | Array), (_, es, _)); _ }, _, e2)
            ->
              List.iter
                (function
                  | {
                      e =
                        N
                          (Id
                            ( id,
                              {
                                id_resolved = { contents = Some (_kind, sid) };
                                _;
                              } ));
                      _;
                    } ->
                      let var = (H.str_of_ident id, sid) in
                      let stat = get_stat_or_create var h in
                      incr stat.lvalue
                  | _ -> ())
                es;
              vout (E e2)
          | N (Id (id, { id_resolved = { contents = Some (_kind, sid) }; _ }))
            ->
              let var = (H.str_of_ident id, sid) in
              let stat = get_stat_or_create var h in
              incr stat.rvalue;
              k x
          | _ -> k x);
    }
  in
  let visitor = V.mk_visitor ~vardef_assign:false hooks in
  visitor (Pr prog);
  h

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(* !Note that this assumes Naming_AST.resolve has been called before! *)
let propagate_basic lang prog =
  logger#trace "Constant_propagation.propagate_basic program";
  let env = default_env (Some lang) in

  (* step1: first pass const analysis for languages without 'const/final' *)
  let stats = var_stats prog in

  (* step2: second pass where we actually propagate when we can *)
  let hooks =
    {
      V.default_visitor with
      (* the defs *)
      V.kdef =
        (fun (k, _v) x ->
          match x with
          | ( {
                name =
                  EN
                    (Id
                      (id, { id_resolved = { contents = Some (_kind, sid) }; _ }));
                attrs;
                _;
              },
              VarDef { vinit = Some { e = L literal; _ }; _ } )
          (* note that some languages such as Python do not have VarDef.
           * todo? should add those somewhere instead of in_lvalue detection?*)
            -> (
              match Hashtbl.find_opt stats (H.str_of_ident id, sid) with
              | Some stats ->
                  if
                    H.has_keyword_attr Const attrs
                    || H.has_keyword_attr Final attrs
                    || (!(stats.lvalue) = 1 && is_js env)
                  then add_constant_env id (sid, Lit literal) env;
                  k x
              | None ->
                  logger#debug "No stats for (%s,%d)" (H.str_of_ident id) sid;
                  k x)
          | _ -> k x);
      (* the uses (and also defs for Python Assign) *)
      V.kexpr =
        (fun (k, v) x ->
          match x.e with
          | N (Id (id, id_info)) when not !(env.in_lvalue) -> (
              match find_id env id id_info with
              | Some svalue -> id_info.id_svalue := Some svalue
              | _ -> ())
          | DotAccess ({ e = IdSpecial (This, _); _ }, _, FN (Id (id, id_info)))
            when not !(env.in_lvalue) -> (
              match find_id env id id_info with
              | Some svalue -> id_info.id_svalue := Some svalue
              | _ -> ())
          | ArrayAccess (e1, (_, e2, _)) ->
              v (E e1);
              Common.save_excursion env.in_lvalue false (fun () -> v (E e2))
              (* Assign that is really a hidden VarDef (e.g., in Python) *)
          | Assign
              ( {
                  e =
                    N
                      (Id
                        ( id,
                          { id_resolved = { contents = Some (kind, sid) }; _ }
                        ));
                  _;
                },
                _,
                rexp ) ->
              eval_expr env rexp
              |> Option.iter (fun svalue ->
                     match Hashtbl.find_opt stats (H.str_of_ident id, sid) with
                     | Some stats ->
                         if
                           !(stats.lvalue) = 1
                           (* restrict to Python/Ruby/PHP/JS/TS Globals for now *)
                           && (is_lang env Lang.Python || is_lang env Lang.Ruby
                             || is_lang env Lang.Php || is_js env)
                           && kind = Global
                         then add_constant_env id (sid, svalue) env
                     | None ->
                         logger#debug "No stats for (%s,%d)" (H.str_of_ident id)
                           sid;
                         ());
              v (E rexp)
          | Assign (e1, _, e2)
          | AssignOp (e1, _, e2) ->
              Common.save_excursion env.in_lvalue true (fun () -> v (E e1));
              v (E e2)
          | _ -> k x);
    }
  in
  let visitor = V.mk_visitor hooks in
  visitor (Pr prog);
  ()

let propagate_basic a b =
  Common.profile_code "Constant_propagation.xxx" (fun () -> propagate_basic a b)

let propagate_dataflow lang ast =
  logger#trace "Constant_propagation.propagate_dataflow program";
  let v =
    V.mk_visitor
      {
        V.default_visitor with
        V.kfunction_definition =
          (fun (_k, _) def ->
            let inputs, xs = AST_to_IL.function_definition lang def in
            let flow = CFG_build.cfg_of_stmts xs in
            let mapping = Dataflow_svalue.fixpoint lang inputs flow in
            Dataflow_svalue.update_svalue flow mapping);
      }
  in
  v (Pr ast)
