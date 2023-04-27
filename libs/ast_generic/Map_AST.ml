(* Yoann Padioleau
 *
 * Copyright (C) 2019 r2c
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
open AST_generic
module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* hooks *)

type visitor_in = {
  kexpr : (expr -> expr) * visitor_out -> expr -> expr;
  kstmt : (stmt -> stmt) * visitor_out -> stmt -> stmt;
  kinfo : (tok -> tok) * visitor_out -> tok -> tok;
  kidinfo : (id_info -> id_info) * visitor_out -> id_info -> id_info;
  klit : (literal -> literal) * visitor_out -> literal -> literal;
  kargs :
    (argument list -> argument list) * visitor_out ->
    argument list ->
    argument list;
  kname : (name -> name) * visitor_out -> name -> name;
}

and visitor_out = {
  vitem : item -> item;
  vprogram : program -> program;
  vexpr : expr -> expr;
  vany : any -> any;
}

type env = {
  vin : visitor_in;
  (* Mutable to break a circular dependency *)
  mutable vout : visitor_out;
}

let noop_vout =
  {
    vitem = (fun x -> x);
    vprogram = (fun x -> x);
    vexpr = (fun x -> x);
    vany = (fun x -> x);
  }

let default_visitor =
  {
    kexpr = (fun (k, _) x -> k x);
    kstmt = (fun (k, _) x -> k x);
    kinfo = (fun (k, _) x -> k x);
    kidinfo = (fun (k, _) x -> k x);
    klit = (fun (k, _) x -> k x);
    kargs = (fun (k, _) x -> k x);
    kname = (fun (k, _) x -> k x);
  }

class ['self] mapper =
  object (_self : 'self)
    inherit [_] AST_generic.map_legacy as super

    method! visit_tok env v =
      let k = super#visit_tok env in
      env.vin.kinfo (k, env.vout) v

    method! visit_id_info env v =
      let k = super#visit_id_info env in
      env.vin.kidinfo (k, env.vout) v

    method! visit_name env name =
      let k = super#visit_name env in
      env.vin.kname (k, env.vout) name

    method! visit_expr env x =
      let k = super#visit_expr env in
      env.vin.kexpr (k, env.vout) x

    method! visit_literal env lit =
      let k = super#visit_literal env in
      env.vin.klit (k, env.vout) lit

    method! visit_argument_list env v =
      let k = super#visit_argument_list env in
      env.vin.kargs (k, env.vout) v

    method! visit_stmt env x =
      let k = super#visit_stmt env in
      env.vin.kstmt (k, env.vout) x
  end

(* Just pay the (minimal) cost of instantiating the mapper once, the first time
 * it's needed. *)
let v = lazy (new mapper)

let (mk_visitor : visitor_in -> visitor_out) =
 fun vin ->
  let (lazy v) = v in
  let env = { vin; vout = noop_vout } in
  let vout =
    {
      vitem = v#visit_stmt env;
      vprogram = v#visit_program env;
      vexpr = v#visit_expr env;
      vany = v#visit_any env;
    }
  in
  env.vout <- vout;
  vout

(*****************************************************************************)
(* Fix token locations *)
(*****************************************************************************)

let fix_token_locations_visitor =
  object (_self : 'self)
    inherit [_] AST_generic.map_legacy as super

    method! visit_id_info _fix ii =
      (* The id_info contains locations that should not be modified, and they
       * are likely outside the sub-AST of interest anyways. *)
      ii

    method! visit_expr fix e =
      super#visit_expr fix
        { e with e_range = Option.map (fun (x, y) -> (fix x, fix y)) e.e_range }

    method! visit_stmt fix s =
      super#visit_stmt fix
        { s with s_range = Option.map (fun (x, y) -> (fix x, fix y)) s.s_range }

    method! visit_tok fix t = Tok.fix_location fix t
  end
