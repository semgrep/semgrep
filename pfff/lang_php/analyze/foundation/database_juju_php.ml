(* Julien Verlaguet, Yoann Padioleau
 *
 * Copyright (C) 2011, 2012 Facebook
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

open Ast_php
module A = Ast_php
module Env = Env_interpreter_php
module SMap = Map.Make (String)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Yet another "code database" for php functions/classes/constants.
 * This one is used by the abstract interpreter.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type database = {
  funs_juju    : Ast_php.func_def Common2.cached SMap.t ref;
  classes_juju : Ast_php.class_def Common2.cached SMap.t ref;
  constants_juju: Ast_php.constant_def Common2.cached SMap.t ref;
}

(*****************************************************************************)
(* Code database *)
(*****************************************************************************)

(* todo: position_info flag *)
let juju_db_of_files ?(show_progress=false) xs =
  let db = {
    funs_juju = ref SMap.empty;
    classes_juju = ref SMap.empty;
    constants_juju = ref SMap.empty;
  }
  in
  xs |> Console.progress ~show:show_progress (fun k -> 
   List.iter (fun file ->
    k();
    try
      let cst = Parse_php.parse_program file in		
      let ast = Ast_php_build.program cst in
      List.iter (fun x ->
        (* print warning when duplicate class/func *)
        let add aref name c =
          let s = (A.unwrap name) in
          if SMap.mem s !aref
          then pr2 (spf "ERROR in %s, %s is already present" file s);
          aref := SMap.add s (Common2.serial c) !aref
        in
        match x with
        | ClassDef c -> add db.classes_juju c.c_name c
        | FuncDef fd -> add db.funs_juju fd.f_name fd
        | ConstantDef c -> add db.constants_juju c.cst_name c
        | TypeDef _t -> 
            failwith "no support for typedefs in juju database"
        | NamespaceDef _ | NamespaceUse _ ->
            failwith "no support for namespace yet"

        | (Global _|StaticVars _
          |Try _ |Throw _
          |Continue _|Break _|Return _
          |Foreach _ |For _|Do _|While _
          |Switch _ |If _
          |Block _|Expr _
          ) -> ()
      ) ast
    with e -> 
      pr2 (spf "ERROR in %s, exn = %s" file (Common.exn_to_s e))
  ));
  db

(* todo: what if multiple matches?
 * less: profiling information
 *)
let code_database_of_juju_db db = 
  let get s aref =
    let x = SMap.find s !aref in Common2.unserial x
  in
 { Env.
   funs      = (fun s -> get s db.funs_juju);
   classes   = (fun s -> get s db.classes_juju); 
   constants = (fun s -> get s db.constants_juju);
 }
