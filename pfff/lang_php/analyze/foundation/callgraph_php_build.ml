(* Yoann Padioleau
 *
 * Copyright (C) 2012 Facebook
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

module Env = Env_interpreter_php
module Interp = Abstract_interpreter_php.Interp (Tainting_fake_php.Taint)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Small wrapper around Abstract_interpreter_php.program with
 * a functional interface (it returns a callgraph).
 *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let create_graph ?(show_progress=false) ?(strict=false) files db =

  Common.save_excursion Abstract_interpreter_php.extract_paths true (fun()->
  Common.save_excursion Abstract_interpreter_php.strict strict (fun()->
    Abstract_interpreter_php.graph := Map_.empty;

    files |> Console.progress ~show:show_progress (fun k ->
     List.iter (fun file ->
       k();
       try 
         let env = 
           Env.empty_env db file in
         let ast = 
           Ast_php_build.program (Parse_php.parse_program file) in
         let _heap = 
           Common2.timeout_function_opt (Some 20) (fun () ->
             Interp.program env Env.empty_heap ast
           )
         in
         ()
       with 
       | Timeout ->
           pr2 (spf "PB with %s, exn = Timeout" file)
       | exn ->
           pr2 (spf "PB with %s, exn = %s" file (Common.exn_to_s exn))
     )
    )
  ));
  !Abstract_interpreter_php.graph
