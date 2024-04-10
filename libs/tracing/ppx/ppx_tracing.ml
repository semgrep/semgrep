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
(* Prelude *)
(*****************************************************************************)
(* A ppx rewriter to automatically transform
 *  let foo frm = body [@@trace]
 * into
 *  let foo frm = Tracing.with_span ~__FILE__ ~__LINE__ "X.foo" @@
                 fun _sp -> body
 *
 * This is mostly copied from `ppx_profiling`. The main differences:
 *   - The ppx is called "trace" rather than "tracing" to be in line with other
 *     ppxes (show, compare)
 *   - The implementation uses Ast_traverse.map, so instead of redefining foo
 *     underneath to use tracing, it rewrites foo. Using Ast_traverse.maps means
 *     the annotation works for nested functions
 *
 * > Why does this exist when the maintainer of trace already has a ppx?
 * We might want to add things specific to our codebase like a `trace_debug`
 * option, which is much easier to do without having to go through the
 * maintainer. Also, we have legacy ways of adding ppxses (`[@@profiling/trace]`)
 * that we don't want to ask the maintainer to support because it's non-standard.
 * Ultimately, supporting a ppx is not that hard.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let location_errorf ~loc fmt =
  Format.kasprintf
    (fun err ->
      raise (Ocaml_common.Location.Error (Ocaml_common.Location.error ~loc err)))
    fmt

let name_of_func_pat (pat : Parsetree.pattern) =
  match pat.ppat_desc with
  | Ppat_var { txt; _ } -> txt
  | _ -> "<no func name>"

let tracing_attr (attr : Parsetree.attribute) =
  if attr.attr_name.txt = "trace" then
    let payload =
      match attr.attr_payload with
      | PStr
          [
            {
              pstr_desc =
                Pstr_eval
                  ( { pexp_desc = Pexp_constant (Pconst_string (str, _, _)); _ },
                    _ );
              _;
            };
          ] ->
          Some str
      | _ -> None
    in
    Some payload
  else None

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

let make_traced_expr loc action_name var_pat e =
  Exp.apply
    (Exp.ident { txt = Lident "@@"; loc })
    [
      ( Nolabel,
        Exp.apply
          (Exp.ident { txt = Ldot (Lident "Tracing", "with_span"); loc })
          [
            make_label loc "__FILE__";
            make_label loc "__LINE__";
            (Nolabel, Exp.constant (Pconst_string (action_name, loc, None)));
          ] );
      (Nolabel, Exp.fun_ Nolabel None var_pat e);
    ]

(*****************************************************************************)
(* Mapper *)
(*****************************************************************************)

(* Turn `let f args = body` into
   `let f args = Trace_core.with_span ~__FILE__ ~__LINE__ "<action_name>" @@
                 fun _sp -> body`*)
let map_expr_add_tracing attr_payload pat e =
  match e.pexp_desc with
  | Pexp_fun (arg_label, exp_opt, pattern, e) ->
      let loc = e.pexp_loc in
      let action_name =
        match attr_payload with
        | Some s -> s
        | None ->
            (* Just use the function name *)
            module_name_of_loc loc ^ "." ^ name_of_func_pat pat
      in
      let var_pat = Ast_builder.Default.ppat_var ~loc { txt = "_sp"; loc } in
      let body_with_tracing = make_traced_expr loc action_name var_pat e in
      {
        e with
        pexp_desc = Pexp_fun (arg_label, exp_opt, pattern, body_with_tracing);
      }
  | _ -> e

(*****************************************************************************)
(* Main expander *)
(*****************************************************************************)

(* Implements `let%trace = "<name>"`. This code is mostly copied from
 * https://github.com/c-cube/ocaml-trace/blob/main/src/ppx/ppx_trace.ml
 * I only copied `rule_let` because for top level annotations we're still
 * using [@@trace] as discussed later *)

let expand_let ~ctxt var (action_name : string) e =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  Ast_builder.Default.(
    let var_pat =
      match var with
      | `Var v -> ppat_var ~loc:v.loc v
      | `Unit -> ppat_var ~loc { loc; txt = "_sp" }
    in
    make_traced_expr loc action_name var_pat e)

let extension_let =
  Extension.V3.declare "trace" Extension.Context.expression
    (let open! Ast_pattern in
     single_expr_payload
       (pexp_let nonrecursive
          (value_binding
             ~pat:
               (let pat_var = ppat_var __' |> map ~f:(fun f v -> f (`Var v)) in
                let pat_unit =
                  as__ @@ ppat_construct (lident (string "()")) none
                  |> map ~f:(fun f _ -> f `Unit)
                in
                alt pat_var pat_unit)
             ~expr:(estring __)
          ^:: nil)
          __))
    expand_let

let rule_let = Ppxlib.Context_free.Rule.extension extension_let

(*****************************************************************************)
(* Main driver *)
(*****************************************************************************)

(* Implements [@@trace]. This is not the standard way to add a ppx;
   for our use case you would usually use an expander. To keep with
   the existing syntax in our codebase and to maintain compatibility
   with our tooling, I'm using `[@@trace]` for top level annotations,
   but we should migrate to the standard syntax to simplify our code. *)

let impl (xs : structure) : structure =
  let map_trace_exprs =
    object
      inherit Ast_traverse.map as super

      method! value_binding vb =
        let vb = super#value_binding vb in
        let { pvb_expr; pvb_attributes; pvb_pat; _ } = vb in
        match List.find_map tracing_attr pvb_attributes with
        | Some attr_payload ->
            let e' = map_expr_add_tracing attr_payload pvb_pat pvb_expr in
            { vb with pvb_expr = e' }
        | None -> vb
    end
  in
  let mapped_exprs = map_trace_exprs#structure xs in
  mapped_exprs

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* TODO: add ~extensions so that `let%trace = ` is a possible transformation.
   Copy https://github.com/c-cube/ocaml-trace/blob/main/src/ppx/ppx_trace.ml *)
let () = Driver.register_transformation ~rules:[ rule_let ] ~impl "ppx_tracing"
