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
open OCaml
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
  object (self : 'self)
    inherit [_] AST_generic.map as super

    method! visit_tok env v =
      let k x =
        match x with
        | { Tok.token = v_pinfo; transfo = v_transfo } ->
            let v_pinfo =
              (* todo? map_pinfo v_pinfo *)
              v_pinfo
            in
            (* not recurse in transfo ? *)
            {
              Tok.token = v_pinfo;
              (* generete a fresh field *)
              transfo = v_transfo;
            }
      in
      env.vin.kinfo (k, env.vout) v

    method! visit_id_info env v =
      let k x =
        match x with
        | {
         id_resolved = v_id_resolved;
         id_type = v_id_type;
         id_svalue = v3;
         id_hidden;
         id_info_id;
        } ->
            let v3 = map_of_ref (map_of_option (self#visit_svalue env)) v3 in
            let v_id_type =
              map_of_ref (map_of_option (self#visit_type_ env)) v_id_type
            in
            let v_id_resolved =
              map_of_ref
                (map_of_option (self#visit_resolved_name env))
                v_id_resolved
            in
            let id_hidden = map_of_bool id_hidden in
            {
              id_resolved = v_id_resolved;
              id_type = v_id_type;
              id_svalue = v3;
              id_hidden;
              id_info_id;
            }
      in
      env.vin.kidinfo (k, env.vout) v

    method! visit_name env name =
      let k = super#visit_name env in
      env.vin.kname (k, env.vout) name

    method! visit_expr env x =
      let k x =
        let ekind = super#visit_expr_kind env x.e in
        (* TODO? reuse the e_id or create a new one? *)
        G.e ekind
      in
      env.vin.kexpr (k, env.vout) x

    method! visit_literal env lit =
      let k = super#visit_literal env in
      env.vin.klit (k, env.vout) lit

    method visit_argument_list env v =
      let k = self#visit_list self#visit_argument env in
      env.vin.kargs (k, env.vout) v

    method! visit_arguments env v =
      self#visit_bracket self#visit_argument_list env v

    method! visit_stmt env x =
      let k x =
        let skind = self#visit_stmt_kind env x.s in
        { x with s = skind }
      in
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

(* Fix token locations to "relocate" a sub-AST. *)
let mk_fix_token_locations fix =
  mk_visitor
    {
      default_visitor with
      kidinfo =
        (fun (_k, _vout) ii ->
          (* The id_info contains locations that should not be modified, and they
           * are likely outside the sub-AST of interest anyways. *)
          ii);
      kexpr =
        (fun (k, _) e ->
          k
            {
              e with
              e_range = Option.map (fun (x, y) -> (fix x, fix y)) e.e_range;
            });
      kstmt =
        (fun (k, _) s ->
          k
            {
              s with
              s_range = Option.map (fun (x, y) -> (fix x, fix y)) s.s_range;
            });
      kinfo = (fun (_, _) t -> Tok.fix_location fix t);
    }
