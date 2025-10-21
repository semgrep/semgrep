(* Austin Theriault
 *
 * Copyright (C) Semgrep, Inc.
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
open Ppxlib

let errorf (e : expression) = Location.raise_errorf ~loc:e.pexp_loc

let lazy_from_fun =
  Ast_builder.Default.pexp_ident ~loc:Location.none
    { txt = Longident.parse "Lazy_safe.from_fun"; loc = Location.none }

let expand_lazy_safe e =
  match e.pexp_desc with
  | Pexp_apply (_, [ (label, thunk) ]) ->
      let e =
        match label with
        | Asttypes.Nolabel ->
            let lambda =
              Ast_builder.Default.pexp_fun ~loc:Location.none Asttypes.Nolabel
                None
                Ast_builder.Default.(punit ~loc:Location.none)
                thunk
            in
            Ast_builder.Default.eapply ~loc:e.pexp_loc lazy_from_fun [ lambda ]
        | Labelled label_str
        | Optional label_str ->
            errorf thunk "Cannot use label arg \"%s\"" label_str
      in
      Some e
  | _ -> None

let () =
  (* coupling: Lazy_safe.ml. This rewrites [lazy_safe expr] to
     [Lazy_safe.from_fun expr] *)
  let rule = Context_free.Rule.special_function "lazy_safe" expand_lazy_safe in
  Driver.register_transformation ~rules:[ rule ] "lazy_safe_macro"
