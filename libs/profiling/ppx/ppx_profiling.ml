(* Yoann Padioleau
 *
 * Copyright (C) 2020, 2021, 2023 Semgrep, Inc
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
 *  let foo frm = ... [@@profiling]
 * into
 *  let foo frm = ... let foo a = Profiling.profile_code "X.foo" (fun () -> foo a)
 *
 * News:
 *  - handle now @@[profiling] on functions using labels!
 *
 * TODO:
 *  - handle wrapped modules (maybe using __FUNCTION__ solves the issue?)
 *  - transform also functions in nested modules
 *
 * Usage to test:
 *   $ ocamlfind ppx_tools/rewriter ./ppx_profiling tests/test_profiling.ml
 *   UPDATE: does not work anymore
 *
 * To get familiar with the OCaml AST you can use:
 *   $ ocamlfind ppx_tools/dumpast tests/test_profiling.ml
 *  (this still works).
 *
 * Here is its output on tests/test_profiling.ml:
 *   ==>
 *   [{pstr_desc =
 *      Pstr_value (Nonrecursive,
 *       [{pvb_pat = {ppat_desc = Ppat_var {txt = "foo"}};
 *         pvb_expr =
 *          {pexp_desc =
 *            Pexp_fun ("", None, {ppat_desc = Ppat_var {txt = "frame"}},
 *             {pexp_desc =
 *               Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "failwith"}},
 *                [("", {pexp_desc = Pexp_constant (Const_string ("TODO", None))})])})};
 *         pvb_attributes = [({txt = "profiling"}, PStr [])]}])}]
 *   =========
 * (I wish I could use pfff -dump_ml, but my AST is different).
 *
 * update: if you use the dune build system, you can also use
 *   $ ocamlc -dsource _build/default/src/foo.pp.ml
 * to display the preprocessed code of src/foo.ml
 *
 * history and documentation:
 *  - first version based on original tutorial blog post for ppx_getenv:
 *    https://whitequark.org/blog/2014/04/16/a-guide-to-extension-points-in-ocaml/
 *  - use ocaml-migrate-parsetree ppx driver, so portable ppx rewriter
 *    http://ocamllabs.io/projects/2017/02/15/ocaml-migrate-parsetree.html
 *  - update of ppx_getenv using the latest ppxlib
 *    http://rgrinberg.com/posts/extension-points-3-years-later/
 *    but in my opinion it was not worth the complexity
 *  - deprecation of ocaml-migrate-parsetree ppx driver, so had to
 *    switch to ppxlib. Read some documentation like
 *    https://tarides.com/blog/2019-05-09-an-introduction-to-ocaml-ppx-ecosystem
 *    or the ppxlib manual, but they provide helpers for [@@deriving] like
 *    extensions that can not be apply to function definitions
 *    (see my issues for [@@deriving like for function definitions
 *    https://github.com/ocaml-ppx/ppxlib/issues/168#issuecomment-688748491).
 *    I followed nathan advice in the issue above and implemented
 *    ppx_profiling via Driver.register_transformation, which was
 *    pretty close to what I had before with ocaml-migrate-parsetree.
 *
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let rec parameters body =
  match body with
  | { pexp_desc = Pexp_fun (Nolabel, _, _, body); _ } ->
      Nolabel :: parameters body
  | { pexp_desc = Pexp_fun (Labelled name, _, _, body); _ } ->
      Labelled name :: parameters body
  | { pexp_desc = Pexp_fun (Optional name, _, _, body); _ } ->
      Optional name :: parameters body
  | _else_ -> []

let name_of_lbl_opt n lbl_opt =
  match lbl_opt with
  | Nolabel -> "a" ^ string_of_int n
  | Labelled s
  | Optional s ->
      s

let mk_params loc params e =
  let rec aux xs n =
    match xs with
    | [] -> e
    | x :: xs ->
        let param = name_of_lbl_opt n x in
        Exp.fun_ x None (Pat.var { txt = param; loc }) (aux xs (n + 1))
  in
  aux params 0

let mk_args loc params =
  let rec aux xs n =
    match xs with
    | [] -> []
    | x :: xs ->
        let arg = name_of_lbl_opt n x in
        (x, Exp.ident { txt = Lident arg; loc }) :: aux xs (n + 1)
  in
  aux params 0

(* copy paste of module_ml.ml *)
let module_name_of_filename s =
  let _d, b, _e = Filename_.dbe_of_filename s in
  String.capitalize_ascii b

(*****************************************************************************)
(* Mapper *)
(*****************************************************************************)

(* TODO: use Ast_traverse to visit and map, so we do not transform
 * only toplevel function definitions but also annotations on functions in
 * nested modules.
 *)
let impl xs =
  xs
  |> List_.map (fun item ->
         match item with
         (* let <fname> ... = ... [@@profiling <args_opt> *)
         | {
          pstr_desc =
            Pstr_value
              ( _,
                [
                  {
                    pvb_pat = { ppat_desc = Ppat_var { txt = fname; _ }; _ };
                    pvb_expr = body;
                    pvb_attributes =
                      [
                        {
                          attr_name = { txt = "profiling"; loc };
                          attr_payload = PStr args;
                          attr_loc = _;
                        };
                      ];
                    pvb_loc = _;
                  };
                ] );
          _;
         } ->
             (* Common.pr2 (Common.spf "profiling %s" fname); *)
             let params = parameters body in
             (* you can change the action name by specifying an explicit name
              * with [@@profiling "<explicit_name>"]
              *)
             let action_name =
               match args with
               | [] ->
                   let pos = loc.Location.loc_start in
                   let file = pos.Lexing.pos_fname in
                   (* alt: generate simply __FUNCTION__ instead? *)
                   let m = module_name_of_filename file in
                   m ^ "." ^ fname
               | [
                {
                  pstr_desc =
                    Pstr_eval
                      ( {
                          pexp_desc =
                            Pexp_constant (Pconst_string (name, _loc, None));
                          _;
                        },
                        _ );
                  _;
                };
               ] ->
                   name
               | _else_ ->
                   Location.raise_errorf ~loc
                     "@@profiling accepts nothing or a string"
             in

             (* let <fname> a b = Profiling.profile_code <action_name> (fun () ->
              *         <fname> a b)
              *)
             let item2 =
               Str.value Nonrecursive
                 [
                   Vb.mk
                     (Pat.var { txt = fname; loc })
                     (mk_params loc params
                        (Exp.apply
                           (Exp.ident
                              {
                                txt = Ldot (Lident "Profiling", "profile_code");
                                loc;
                              })
                           [
                             ( Nolabel,
                               Exp.constant
                                 (Pconst_string (action_name, loc, None)) );
                             ( Nolabel,
                               Exp.fun_ Nolabel None (Pat.any ())
                                 (Exp.apply
                                    (Exp.ident { txt = Lident fname; loc })
                                    (mk_args loc params)) );
                           ]));
                 ]
             in
             [ item; item2 ]
         | x -> [ x ])
  |> List_.flatten

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let () = Driver.register_transformation ~impl "ppx_profiling"
