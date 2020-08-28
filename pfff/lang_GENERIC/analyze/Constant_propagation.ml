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
module Ast = AST_generic
module V = Visitor_AST

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Very basic constant propagation (no dataflow analysis involved).
 *
 * This is mainly to provide advanced features to semgrep such as the 
 * constant propagation of literals.
 *
 * Right now we just propagate constants when we're sure it's a constant
 * because:
 *  - the variable declaration use the 'const' keyword in Javascript/Go/...
 *  - the field declaration use the 'final' keyword in Java
 *  - we do a very basic const analysis where we check the variable
 *    is never assigned more than once.
 *
 * history: this used to be in Naming_AST.ml but better to split, even though
 * things will be slightly slower because we will visit the same file
 * twice.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type var = string * AST_generic.sid

type env = {
  (* basic constant propagation of literals for semgrep *)
  constants: (var, literal) assoc ref;
}

let default_env () = { constants = ref []; }

type lr_stats = {
    (* note that a VarDef defining the value will count as 1 *)
    lvalue: int ref;
    rvalue: int ref;
}
let default_lr_stats () = {lvalue = ref 0; rvalue = ref 0 }

type var_stats = (var, lr_stats) Hashtbl.t

(*****************************************************************************)
(* Environment Helpers *)
(*****************************************************************************)

let add_constant_env ident (sid, literal) env =
  env.constants := 
  ((Ast.str_of_ident ident, sid), literal)::!(env.constants)

(*****************************************************************************)
(* Poor's man const analysis *)
(*****************************************************************************)
(* This is mostly useful for languages without a const keyword (e.g., Python).
 *
 * Note that this may be incomplete and buggy. Worst case we do a
 * constant propagation on a variable containing a literal that may
 * have its content changed; not a big-deal in semgrep context actually.
 *)
let var_stats prog : var_stats =
  let h = Hashtbl.create 101 in
  let get_stat_or_create var h =
    try
      Hashtbl.find h var
    with Not_found ->
        let stat = default_lr_stats () in
        Hashtbl.add h var stat;
        stat
  in

  let visitor = V.mk_visitor { V.default_visitor with
    V.kdef = (fun (k, _v) x ->
      match x with
      | { name=id; info={ id_resolved = {contents = Some(_kind, sid)}; _}; _}, 
        VarDef ({ vinit = Some _; _ }) ->
          let var = (Ast.str_of_ident id, sid) in
          let stat = get_stat_or_create var h in
          incr stat.lvalue;
          k x
       | _ -> k x
    );
    V.kexpr = (fun (k, vout) x ->
       match x with
       (* TODO: very incomplete, what if Assign (Tuple?) *)
       | Assign (
          Id (id, ({ id_resolved = {contents = Some (_kind, sid)}; _ })),
          _,
          e2) ->
          let var = (Ast.str_of_ident id, sid) in
          let stat = get_stat_or_create var h in
          incr stat.lvalue;
          vout (E e2)

       | Id (id, ({ id_resolved = {contents = Some (_kind, sid)}; _ }))->
          let var = (Ast.str_of_ident id, sid) in
          let stat = get_stat_or_create var h in
          incr stat.rvalue;
          k x
       | _ -> k x
   );
  } in
  visitor (Pr prog);
  h

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(* !Note that this assumes Naming_AST.resolve has been called before! *)
let propagate2 lang prog =
  let env = default_env () in

  (* step1: first pass const analysis for languages without 'const/final' *)
  let stats = var_stats prog in

  (* step2: second pass where we actually propagate when we can *)
  let visitor = V.mk_visitor { V.default_visitor with
    (* the defs *)

    V.kdef = (fun (k, _v) x ->
      match x with
      | { name = id; 
          info = { id_resolved = {contents = Some (_kind, sid)}; _} as id_info;
          attrs = attrs; _}, 
        (* note that some languages such as Python do not have VarDef.
         * todo? should add those somewhere instead of in_lvalue detection? *)
        VarDef ({ vinit = Some (L literal); _ }) ->
          let _stats = 
             try Hashtbl.find stats (Ast.str_of_ident id, sid)
             with Not_found -> raise Impossible
           in
          if Ast.has_keyword_attr Const attrs ||
             Ast.has_keyword_attr Final attrs
             (* TODO later? (!(stats.rvalue) = 1) *)
          then begin
              id_info.id_const_literal := Some literal;
              add_constant_env id (sid, literal) env;
          end;
          k x

      | _ -> k x
    );

    (* the uses (and also defs for Python Assign) *)

    V.kexpr = (fun (k, _) x ->

       (match x with
       | Id (id, ({ id_resolved = {contents = Some (_kind, sid)}; _ }
                    as id_info))->
             let s = Ast.str_of_ident id in
             (match List.assoc_opt (s, sid) !(env.constants) with
             | Some (literal) ->
                 id_info.id_const_literal := Some literal
             | _ -> ()
             );

       (* Assign that is really a hidden VarDef (e.g., in Python) *)
       | Assign (
          Id (id, ({ id_resolved = {contents = Some (kind, sid)}; _ }
                    as id_info)),
          _,
          (L literal)) ->
          let stats = 
             try Hashtbl.find stats (Ast.str_of_ident id, sid)
             with Not_found -> raise Impossible
          in
          if (!(stats.rvalue) = 1) &&
             (* restrict to Python Globals for now, but could be extended *)
             lang = Lang.Python &&
             kind = Global
          then begin
              id_info.id_const_literal := Some literal;
              add_constant_env id (sid, literal) env;
          end;
          k x

       | _ -> ()
       );
       k x
   );
  }
  in
  visitor (Pr prog);
  ()

let propagate a b = Common.profile_code "Constant_propagation.xxx" (fun () ->
      propagate2 a b)
