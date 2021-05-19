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
        (fun (k, v) x ->
          match x with
          | ( {
                name =
                  EN
                    (Id
                      (id, { id_resolved = { contents = Some (_kind, sid) }; _ }));
                _;
              },
              VarDef { vinit = Some e; _ } ) ->
              let var = (H.str_of_ident id, sid) in
              let stat = get_stat_or_create var h in
              incr stat.lvalue;
              (* We can't do `k x` otherwise the variable-declaration itself is
               * re-visited but now interpreted as an assignment (because of
               * v_vardef_as_assign_expr in Visitor_AST), and we would be wrongly
               * incrementing `stat.lvalue` a second time. Instead, we just need
               * to visit the defining expression. *)
              v (E e)
          | _ -> k x);
      V.kexpr =
        (fun (k, vout) x ->
          match x with
          (* TODO: very incomplete, what if Assign (Tuple?) *)
          | Assign
              ( N
                  (Id
                    (id, { id_resolved = { contents = Some (_kind, sid) }; _ })),
                _,
                e2 )
          | AssignOp
              ( N
                  (Id
                    (id, { id_resolved = { contents = Some (_kind, sid) }; _ })),
                _,
                e2 ) ->
              let var = (H.str_of_ident id, sid) in
              let stat = get_stat_or_create var h in
              incr stat.lvalue;
              (match x with AssignOp _ -> incr stat.rvalue | _ -> ());
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
  let visitor = V.mk_visitor hooks in
  visitor (Pr prog);
  h

(*****************************************************************************)
(* Partial evaluation *)
(*****************************************************************************)

let literal_of_bool b =
  let b_str = string_of_bool b in
  let tok = Parse_info.fake_info b_str in
  Bool (b, tok)

let bool_of_literal = function Bool (b, _) -> Some b | __else__ -> None

let eval_bop_bool op b1 b2 =
  match op with
  | Or -> Some (b1 || b2)
  | And -> Some (b1 && b2)
  | __else__ -> None

let literal_of_int i =
  let i_str = string_of_int i in
  let tok = Parse_info.fake_info i_str in
  Int (Some i, tok)

let int_of_literal = function Int (Some i, _) -> Some i | __else__ -> None

let eval_bop_int op i1 i2 =
  match op with
  | Plus -> Some (i1 + i2)
  | Mult -> Some (i1 * i2)
  | __else__ -> None

let literal_of_string s =
  let tok = Parse_info.fake_info s in
  String (s, tok)

let string_of_literal = function String (s, _) -> Some s | __else__ -> None

let eval_bop_string op s1 s2 =
  match op with Plus -> Some (s1 ^ s2) | __else__ -> None

let rec eval_expr env = function
  | L literal -> Some literal
  | N (Id (id, id_info)) -> find_id env id id_info
  (* TODO: do what we do in Normalize_generic.ml.
   * | Call(IdSpecial((Op(Plus | Concat) | ConcatString _), _), args)->
   *)
  | Call (IdSpecial (Op op, _), (_, args, _)) -> eval_op env op args
  | Call (IdSpecial (ConcatString _, _), (_, args, _)) ->
      eval_concat_string env args
  | Call (IdSpecial (InterpolatedElement, _), (_, [ Arg e ], _)) ->
      eval_expr env e
  | __else__ -> None

(* coupling: see also semgrep/matching/Normalize_generic.ml, even though
 * we should remove it because it's doing similar work.
 *)
and eval_op env op args =
  match args with
  | [ Arg e ] -> (
      match (op, eval_expr env e) with
      | Not, Some (Bool _ as l) ->
          bool_of_literal l >>= fun b -> Some (literal_of_bool (not b))
      | Plus, Some (Int _ as l) ->
          int_of_literal l >>= fun i -> Some (literal_of_int i)
      | Minus, Some (Int _ as l) ->
          int_of_literal l >>= fun i -> Some (literal_of_int (-i))
      | __else__ -> None )
  | [ Arg e1; Arg e2 ] -> (
      match (eval_expr env e1, eval_expr env e2) with
      | Some (Bool _ as l1), Some (Bool _ as l2) ->
          bool_of_literal l1 >>= fun b1 ->
          bool_of_literal l2 >>= fun b2 ->
          eval_bop_bool op b1 b2 >>= fun r -> Some (literal_of_bool r)
      | Some (Int _ as l1), Some (Int _ as l2) ->
          int_of_literal l1 >>= fun i1 ->
          int_of_literal l2 >>= fun i2 ->
          eval_bop_int op i1 i2 >>= fun r -> Some (literal_of_int r)
      | Some (String _ as l1), Some (String _ as l2) ->
          string_of_literal l1 >>= fun s1 ->
          string_of_literal l2 >>= fun s2 ->
          eval_bop_string op s1 s2 >>= fun r -> Some (literal_of_string r)
      | __else__ -> None )
  | __else__ -> None

and eval_concat_string env args : literal option =
  let go_concat : arguments -> string option =
    List.fold_left
      (fun res arg ->
        match arg with
        | Arg e ->
            let ( let* ) = ( >>= ) in
            let* r = res in
            let* lit = eval_expr env e in
            let* s = string_of_literal lit in
            Some (r ^ s)
        | _else -> None)
      (Some "")
  in
  args |> go_concat |> map_opt literal_of_string

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(* !Note that this assumes Naming_AST.resolve has been called before! *)
let propagate_basic lang prog =
  logger#info "Constant_propagation.propagate_basic progran";
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
              VarDef { vinit = Some (L literal); _ } )
          (* note that some languages such as Python do not have VarDef.
           * todo? should add those somewhere instead of in_lvalue detection?*)
            ->
              let stats =
                try Hashtbl.find stats (H.str_of_ident id, sid)
                with Not_found -> raise Impossible
              in
              if
                H.has_keyword_attr Const attrs
                || H.has_keyword_attr Final attrs
                || !(stats.lvalue) = 1
                   && (lang = Lang.Javascript || lang = Lang.Typescript)
              then add_constant_env id (sid, literal) env;
              k x
          | _ -> k x);
      (* the uses (and also defs for Python Assign) *)
      V.kexpr =
        (fun (k, v) x ->
          match x with
          | N (Id (id, id_info)) when not !(env.in_lvalue) -> (
              match find_id env id id_info with
              | Some literal -> id_info.id_constness := Some (Lit literal)
              | _ -> () )
          | DotAccess (IdSpecial (This, _), _, EN (Id (id, id_info)))
            when not !(env.in_lvalue) -> (
              match find_id env id id_info with
              | Some literal -> id_info.id_constness := Some (Lit literal)
              | _ -> () )
          | ArrayAccess (e1, (_, e2, _)) ->
              v (E e1);
              Common.save_excursion env.in_lvalue false (fun () -> v (E e2))
              (* Assign that is really a hidden VarDef (e.g., in Python) *)
          | Assign
              ( N (Id (id, { id_resolved = { contents = Some (kind, sid) }; _ })),
                _,
                rexp ) ->
              eval_expr env rexp
              |> do_option (fun literal ->
                     let stats =
                       try Hashtbl.find stats (H.str_of_ident id, sid)
                       with Not_found -> raise Impossible
                     in
                     if
                       !(stats.lvalue) = 1
                       (* restrict to Python/Ruby/PHP/JS/TS Globals for now *)
                       && ( lang = Lang.Python || lang = Lang.Ruby
                          || lang = Lang.PHP || Lang.is_js lang )
                       && kind = Global
                     then add_constant_env id (sid, literal) env);
              v (E rexp)
          | Assign (e1, _, e2) | AssignOp (e1, _, e2) ->
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

let propagate_dataflow ast =
  logger#info "Constant_propagation.propagate_dataflow progran";
  let v =
    V.mk_visitor
      {
        V.default_visitor with
        V.kfunction_definition =
          (fun (_k, _) def ->
            let inputs, xs = AST_to_IL.function_definition def in
            let flow = CFG_build.cfg_of_stmts xs in
            let mapping = Dataflow_constness.fixpoint inputs flow in
            Dataflow_constness.update_constness flow mapping);
      }
  in
  v (Pr ast)
