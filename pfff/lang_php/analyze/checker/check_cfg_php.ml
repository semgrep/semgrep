(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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

open Cst_php
module V = Visitor_php
module E = Error_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Most of the hard work is done by Control_flow_build_php.ml.
 * 
 * TODO: check dead statements for toplevel blocks ?
 *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let check_program2 prog =
  let visitor = V.mk_visitor { V.default_visitor with
    (* also valid for methods *)
    V.kfunc_def = (fun (_k, _) def ->
      try
        let flow = Controlflow_build_php.cfg_of_func def in
        Controlflow_build_php.deadcode_detection flow;
        ()
      with Controlflow_build_php.Error (err, loc) ->
        E.fatal loc (E.CfgError err);
    );
  }
  in
  visitor (Program prog)

let check_program a = 
  Common.profile_code "Checker.cfg" (fun () -> check_program2 a)
