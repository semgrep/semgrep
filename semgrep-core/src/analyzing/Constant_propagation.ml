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
open Common
open AST_generic
module H = AST_generic_helpers
module V = Visitor_AST

let logger = Logging.get_logger [ __MODULE__ ]

(* Deep Semgrep *)
let hook_constant_propagation_and_evaluate_literal = ref None

(* TODO: Remove duplication between first and second pass, move towards
   making the first pass as light as possible. *)

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
 * See also semgrep/matching/Normalize_generic.ml which also performs
 * some expression evaluation. We should get rid of it though
 * once we have const analysis available on arguments.
 *
 * Right now we just propagate constants when we're sure it's a constant
 * because:
 *  - the variable declaration use the 'const' keyword in Javascript/Go/...
 *  - the field declaration use the 'final' keyword in Java
 *  - we do a very basic const analysis where we check the variable
 *    is never assigned more than once.
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
  (* basic constant propagation of literals for semgrep *)
  constants : (var, literal) assoc ref;
  in_lvalue : bool ref;
}

let default_env () = { constants = ref []; in_lvalue = ref false }

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

let add_constant_env ident (sid, literal) env =
  env.constants := ((H.str_of_ident ident, sid), literal) :: !(env.constants)

let find_id env id id_info =
  match id_info with
  | { id_resolved = { contents = Some (_kind, sid) }; _ } ->
      let s = H.str_of_ident id in
      List.assoc_opt (s, sid) !(env.constants)
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

let ( let* ) = Option.bind

let add_int_lits i1 i2 =
  let sign i = i asr (Sys.int_size - 1) in
  match (i1, i2) with
  | None, _
  | _, None ->
      None
  | Some i1, Some i2 ->
      let r = i1 + i2 in
      if sign i1 = sign i2 && sign r <> sign i1 then None (* overflow *)
      else Some r

let mult_int_lits i1 i2 =
  let sign i = i asr (Sys.int_size - 1) in
  match (i1, i2) with
  | None, _
  | _, None ->
      None
  | Some i1, Some i2 ->
      let overflow =
        i1 <> 0 && i2 <> 0
        && ((i1 < 0 && i2 = min_int) (* >max_int *)
           || (i1 = min_int && i2 < 0) (* >max_int *)
           ||
           if sign i1 * sign i2 = 1 then abs i1 > abs (max_int / i2)
             (* >max_int *)
           else abs i1 > abs (min_int / i2) (* <min_int *))
      in
      if overflow then None else Some (i1 * i2)

let filter_bool_literals lits =
  lits
  |> List.filter_map (function
       | Bool (b, _) -> Some b
       | _lit -> None)

let filter_int_literals lits =
  lits
  |> List.filter_map (function
       | Int (i, _) -> Some i
       | _lit -> None)

let filter_string_literals lits =
  lits
  |> List.filter_map (function
       | String (s, _) -> Some s
       | _lit -> None)

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
  | Call ({ e = IdSpecial (special, _); _ }, args) ->
      eval_special env special args
  | N name -> (
      match find_name env name with
      | Some lit -> Some (Lit lit)
      | None -> deep_constant_propagation_and_evaluate_literal x)
  | _ ->
      (* deep: *)
      deep_constant_propagation_and_evaluate_literal x

and eval_args_to_same_type_literals env args =
  args |> unbracket
  |> List.fold_left
       (fun acc arg ->
         let* lits = acc in
         match arg with
         | Arg e -> (
             let* e_sval = eval env e in
             match (lits, e_sval) with
             | [], Lit lit -> Some [ lit ]
             | Bool _ :: _, Lit (Bool _ as lit)
             | Int _ :: _, Lit (Int _ as lit)
             | String _ :: _, Lit (String _ as lit) ->
                 Some (lit :: lits)
             | _lits, _sval -> None)
         | _arg -> None)
       (Some [])
  |> Option.map List.rev

and eval_special env special args =
  let* lits = eval_args_to_same_type_literals env args in
  match (special, lits) with
  (* booleans *)
  | Op Not, [ Bool (b, t) ] -> Some (Lit (Bool (not b, t)))
  | Op Or, Bool (_, t1) :: _ ->
      let bools = filter_bool_literals lits in
      let disj = List.exists Fun.id bools in
      Some (Lit (Bool (disj, t1)))
  | Op And, Bool (_, t1) :: _ ->
      let bools = filter_bool_literals lits in
      let conj = List.for_all Fun.id bools in
      Some (Lit (Bool (conj, t1)))
  (* integers *)
  | Op Plus, Int (_, t1) :: _ ->
      let ints = filter_int_literals lits in
      let* sum = List.fold_left add_int_lits (Some 0) ints in
      Some (Lit (Int (Some sum, t1)))
  | Op Mult, Int (_, t1) :: _ ->
      let ints = filter_int_literals lits in
      let* prod = List.fold_left mult_int_lits (Some 1) ints in
      Some (Lit (Int (Some prod, t1)))
  (* strings *)
  | (Op (Plus | Concat) | ConcatString _), String (_, t1) :: _ ->
      let strs = filter_string_literals lits in
      let concated = String.concat "" strs in
      Some (Lit (String (concated, t1)))
  | __else__ -> None

let constant_propagation_and_evaluate_literal =
  let env = default_env () in
  eval env

let eval_expr env e =
  match eval env e with
  | Some (Lit lit) -> Some lit
  | Some _
  | None ->
      None

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
  let env = default_env () in

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
                    || !(stats.lvalue) = 1
                       && (lang = Lang.Js || lang = Lang.Ts)
                  then add_constant_env id (sid, literal) env;
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
              | Some literal -> id_info.id_svalue := Some (Lit literal)
              | _ -> ())
          | DotAccess ({ e = IdSpecial (This, _); _ }, _, FN (Id (id, id_info)))
            when not !(env.in_lvalue) -> (
              match find_id env id id_info with
              | Some literal -> id_info.id_svalue := Some (Lit literal)
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
              |> Option.iter (fun literal ->
                     match Hashtbl.find_opt stats (H.str_of_ident id, sid) with
                     | Some stats ->
                         if
                           !(stats.lvalue) = 1
                           (* restrict to Python/Ruby/PHP/JS/TS Globals for now *)
                           && (lang = Lang.Python || lang = Lang.Ruby
                             || lang = Lang.Php || Lang.is_js lang)
                           && kind = Global
                         then add_constant_env id (sid, literal) env
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
            let mapping = Dataflow_svalue.fixpoint inputs flow in
            Dataflow_svalue.update_svalue flow mapping);
      }
  in
  v (Pr ast)
