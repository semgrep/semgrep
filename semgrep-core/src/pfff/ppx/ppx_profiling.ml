(* Yoann Padioleau
 *
 * Copyright (C) 2020, 2021 r2c
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
 *  let foo frm = ... let foo a = Common.profile_code "X.foo" (fun () -> foo a)
 *
 * Usage to test:
 *   $ ocamlfind ppx_tools/rewriter ./ppx_profiling tests/test_profiling.ml
 *
 * To get familiar with the OCaml AST you can use:
 *   $ ocamlfind ppx_tools/dumpast tests/test_profiling.ml
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
 * (I wish I could use ~/pfff/pfff -dump_ml, but my AST is different).
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
*)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let rec nb_parameters body =
  match body with
  | {pexp_desc = Pexp_fun (_, _, _, body); _} -> 1 + nb_parameters body
  | _ -> 0

let rec mk_params loc n e =
  if n = 0
  then e
  else
    let param = "a" ^ (string_of_int n) in
    Exp.fun_ Nolabel None (Pat.var {txt = param; loc})
      (mk_params loc (n-1) e)

let rec mk_args loc n =
  if n = 0
  then []
  else
    let arg = "a" ^ string_of_int n in
    (Nolabel, (Exp.ident {txt = Lident arg; loc}))::mk_args loc (n-1)

(* copy paste of pfff/lang_ml/module_ml.ml *)
let module_name_of_filename s =
  let (_d, b, _e) = Common2.dbe_of_filename s in
  String.capitalize_ascii b
(*****************************************************************************)
(* Mapper *)
(*****************************************************************************)

(* TODO: use Ast_traverse to visit and map, so we do not transform
 * only toplevel function definitions but also annotations on functions in
 * nested modules.
*)
let impl xs =
  xs |> List.map (fun item ->
    match item with
    (* let <fname> = ... [@@profiling <args_opt> *)
    | { pstr_desc =
          Pstr_value (_,
                      [{pvb_pat = {ppat_desc = Ppat_var {txt = fname; _}; _};
                        pvb_expr = body;
                        pvb_attributes = [
                          { attr_name = {txt = "profiling"; loc};
                            attr_payload = PStr args; attr_loc = _;
                          }
                        ];
                        pvb_loc = _;
                       }
                      ])
      ; _} ->
        (* Common.pr2 (Common.spf "profiling %s" fname); *)
        let nbparams = nb_parameters body in
        (* you can change the action name by specifying an explicit name
         * with [@@profiling "<explicit_name>"]
        *)
        let action_name =
          match args with
          | [] ->
              let pos = loc.Location.loc_start in
              let file = pos.Lexing.pos_fname in
              let m = module_name_of_filename file in
              m ^ "." ^ fname
          | [{pstr_desc =
                Pstr_eval
                  ({pexp_desc = Pexp_constant (Pconst_string (name, _loc, None));_},
                   _); _}] -> name
          | _ ->
              Location.raise_errorf ~loc
                "@@profiling accepts nothing or a string"
        in
        (* let <fname> a b = Common.profile_code <action_name> (fun () ->
         *         <fname> a b)
        *)

        let item2 =
          Str.value Nonrecursive [
            Vb.mk (Pat.var {txt = fname; loc})
              (mk_params loc nbparams
                 (Exp.apply
                    (Exp.ident
                       {txt = Ldot (Lident "Common", "profile_code" ); loc})
                    [Nolabel, Exp.constant (Pconst_string (action_name, loc, None));
                     Nolabel, Exp.fun_ Nolabel None (Pat.any ())
                       (Exp.apply (Exp.ident {txt = Lident fname; loc})
                          (mk_args loc nbparams))
                    ]))
          ]
        in
        [ item; item2]
    | x -> [x]
  ) |> List.concat
(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let () =
  Driver.register_transformation ~impl "ppx_profiling"
