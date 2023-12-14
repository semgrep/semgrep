(* Emma Jin
 *
 * Copyright (C) 2023 Semgrep, Inc
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
open Ppxlib
open Ast_helper

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let name_of_func_pat (pat : Parsetree.pattern) =
  match pat.ppat_desc with
  | Ppat_var { txt; _ } -> txt
  | _ -> "<no func name>"

let is_tracing_attribute (attr : Parsetree.attribute) =
  attr.attr_name.txt = "trace"

(* borrowed from module_ml.ml *)
let module_name_of_loc loc =
  let pos = loc.Location.loc_start in
  let file = pos.Lexing.pos_fname in
  let _d, b, _e = Filename_.dbe_of_filename file in
  String.capitalize_ascii b

(* To produce arguments like `~__FILE__`*)
let make_label loc l =
  ( Labelled l,
    {
      pexp_desc = Pexp_ident { txt = Lident l; loc };
      pexp_loc_stack = [];
      pexp_loc = loc;
      pexp_attributes = [];
    } )

(*****************************************************************************)
(* Mapper *)
(*****************************************************************************)

(* Turn `let f args = body` into
   `let f args = Trace_core.with_span ~__FILE__ ~__LINE__ "<action_name>" @@
                 fun _sp -> body`*)
let map_expr_add_tracing pat e =
  match e.pexp_desc with
  | Pexp_fun (arg_label, exp_opt, pattern, e) ->
      let loc = e.pexp_loc in
      let name = module_name_of_loc loc ^ "." ^ name_of_func_pat pat in
      let body_with_tracing =
        Exp.apply
          (Exp.ident { txt = Lident "@@"; loc })
          [
            ( Nolabel,
              Exp.apply
                (Exp.ident
                   { txt = Ldot (Lident "Trace_core", "with_span"); loc })
                [
                  make_label loc "__FILE__";
                  make_label loc "__LINE__";
                  (Nolabel, Exp.constant (Pconst_string (name, loc, None)));
                ] );
            ( Nolabel,
              Exp.fun_ Nolabel None
                {
                  ppat_desc = Ppat_var { txt = "_sp"; loc };
                  ppat_loc = loc;
                  ppat_attributes = [];
                  ppat_loc_stack = [];
                }
                e );
          ]
      in
      {
        e with
        pexp_desc = Pexp_fun (arg_label, exp_opt, pattern, body_with_tracing);
      }
  | _ -> e

(*****************************************************************************)
(* Main driver *)
(*****************************************************************************)

let impl (xs : structure) : structure =
  let map_trace_exprs =
    object
      inherit Ast_traverse.map as super

      method! value_binding vb =
        let vb = super#value_binding vb in
        let { pvb_expr; pvb_attributes; pvb_pat; _ } = vb in
        match List.find_opt is_tracing_attribute pvb_attributes with
        | Some _ ->
            let e' = map_expr_add_tracing pvb_pat pvb_expr in
            { vb with pvb_expr = e' }
        | None -> vb
    end
  in
  let mapped_exprs = map_trace_exprs#structure xs in
  mapped_exprs

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let () = Driver.register_transformation ~impl "ppx_tracing"
