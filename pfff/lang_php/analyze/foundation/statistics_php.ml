(* Yoann Padioleau
 * 
 * Copyright (C) 2009, 2010, 2011, 2012 Facebook
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

open Cst_php
module Ast = Cst_php
module E = Entity_code
module V = Visitor_php
module CG = Callgraph_php2

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * Compute different statistics on PHP code.
 * 
 * history:
 *  - used it to infer if a PHP file was a script, endpoint, or library
 *    file
 *  - used it to try to evaluate the coverage of the abstract interpreter
 *    and its callgraph computation; how many method calls are not "resolved"?
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type stat = (string, int) Common2.hash_with_default

(* todo? move this in h_program-lang/ ? This is quite similar to
 * statistics_code.mli ? but want the kinds of the toplevel funcalls,
 * which is probably quite PHP specific.
 *)
type stat2 = {
  mutable functions: int;
  mutable classes: int;

  mutable toplevels_funcalls: int;
  mutable toplevels_assign: int;
  mutable toplevels_other: int;
  mutable toplevels_include_requires: int;

  mutable toplevels_funcalls_kinds: (string * int) list;
  (* toplevels_assign_kinds? *)
}

(*
 * PHP does not have the notion of a main(), and so some PHP files contain
 * only function definitions while other contain toplevel statements.
 * It can be useful to know which kind a PHP file is. For instance
 * in the endpoints and scripts reaper we want to identify the files
 * under certain directories which are the starting points. This is
 * usually files with many toplevel statements which are not just
 * "directives" (e.g. ini_set(...) or require_xxx() or xxx_init()).
 *)
type php_file_kind =
  | LibFile
  | IncluderFile
  | ScriptOrEndpointFile

let default_stat2 () = {
  functions = 0;
  classes = 0;

  toplevels_funcalls = 0;
  toplevels_assign = 0;
  toplevels_other = 0;
  toplevels_include_requires = 0;

  toplevels_funcalls_kinds = [];
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let string_of_stat stat =
spf "
  functions = %d;
  classes = %d;

  toplevels_funcalls = %d;
  toplevels_assign = %d;
  toplevels_other = %d;
  toplevels_include_requires = %d;

  toplevels_funcalls_kinds = 
%s
"
  stat.functions stat.classes 
  stat.toplevels_funcalls stat.toplevels_assign 
  stat.toplevels_other stat.toplevels_include_requires
  (stat.toplevels_funcalls_kinds |> List.map (fun (s, d) ->
    spf "   %s -> %d\n" s d) |> Common.join "")

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

type stat_hooks = {
  entity: (Entity_code.entity_kind * string) -> unit;
  call: (Callgraph_php2.node * Callgraph_php2.node) -> unit;
}
let default_hooks = {
  entity = (fun _ -> ());
  call = (fun _ -> ());
}
(* todo: we abuse CG.node to represent class and partial methods *)
let fake = "UGLY"

let stat_of_program ?(hooks=default_hooks) h file ast =
  let inc fld = h#update fld (fun old -> old + 1); () in

  let current_node = ref (CG.File file) in
  h#update "LOC" (fun old -> old + Common2.nblines_with_wc file);
  h#update "SOC" (fun old -> old + Common2.filesize file);
  

  (Program ast) |> V.mk_visitor { V.default_visitor with
    V.ktop = (fun (k, _) x ->
      (match x with
      | FuncDef def ->
          let s = Ast.str_of_ident def.f_name in
          inc "function";
          hooks.entity (E.Function, s);
          Common.save_excursion current_node (CG.Function s) (fun() ->
            k x
          )
      | ConstantDef def -> 
          inc "constant";
          hooks.entity (E.Constant, Ast.str_of_ident def.cst_name);
          (* there should be no call inside constant definitions so
           * don't care about current_node
           *)
          k x
      | ClassDef def ->
          let s = Ast.str_of_ident def.c_name in
          inc (Class_php.string_of_class_type def.c_type);
          (* less: let kind = Class_php.class_type_of_ctype def.c_type in *)
          hooks.entity (E.Class, s);
          Common.save_excursion current_node (CG.Method (s, fake))(fun()->
            k x
          )
      | TypeDef _def ->
          failwith "TODO: support for typedefs"
      | NamespaceDef _ | NamespaceBracketDef _ 
      | NamespaceUse _
        ->
          failwith "TODO: support for namespace"

      | StmtList _ -> 
          k x
      | FinalDef _|NotParsedCorrectly _ -> ()
      );
    );
    V.kmethod_def = (fun (k, _) def ->
      (match !current_node with
      | CG.Method (classname, _) ->
          let s = Ast.str_of_ident def.f_name in
          Common.save_excursion current_node (CG.Method (classname, s))
            (fun()-> k def)
      | _ -> raise Impossible
      )
    );
    V.kstmt = (fun (k,_) x ->
      (match x with
      | FuncDefNested def -> 
          let s = Ast.str_of_ident def.f_name in
          inc "function"; inc "Nested function";
          Common.save_excursion current_node (CG.Function s) (fun() ->
            k x
          )
      | ClassDefNested def -> 
          let str = Class_php.string_of_class_type def.c_type in
          inc str; 
          inc ("Nested " ^ str);
          let s = Ast.str_of_ident def.c_name in
          Common.save_excursion current_node (CG.Method (s, fake))(fun()->
            k x
          )
      | _ ->
          k x
      );
    );
    V.kname = (fun (_k,_) x ->
      (match x with
      | Self _ | Parent _ | XName _ -> ()
      | LateStatic _ -> inc "Late static"
      );
    );
    V.kexpr = (fun (k, _) x ->
      (match x with
      | Eval _ -> inc "Eval"
      | Lambda _ -> inc "lambda"

      | Include (_, _e) | IncludeOnce (_, _e)
      | Require (_, _e) | RequireOnce (_, _e)
          -> 
          inc "include/require"
          (* todo: resolve? *)

      | Yield _ | YieldBreak _ -> inc "yield"

      | ObjGet (lval, _, Id _name) ->
          (match lval with
          | This _ -> inc "obj access with $this"
          | _ -> inc "obj access not $this"
          )

      | ClassGet (_lval, _, _) ->
          inc "DynamicClassVar"

      | Call (ClassGet(_, _, Id _), _) -> 
          inc "static method call"


      | Call (ClassGet (_, _, _), _args) ->
          inc "static method call Dynamic"

      | Call (ObjGet(lval, _, Id name), _xs) ->
          (* look at lval if simple form *)
          (match lval with
          | This _ -> 
              inc "method call with $this"
          | _ -> 
              inc "method call not $this";
              hooks.call (!current_node, 
                         CG.Method (fake, Ast.str_of_name name))
          )

      | Call (Id _, _args) ->
          inc "fun call"

      | Call (_, _args) -> 
          inc "fun call Dynamic"

      | Deref _ -> inc "Deref"

      
      | _ -> ()
      );
      k x
    );
  }


(*****************************************************************************)
(* Old entry point *)
(*****************************************************************************)

let stat2_of_program ast =
  let (funcs, classes, _topstmts) = 
    Lib_parsing_php.functions_methods_or_topstms_of_program ast in

  let _stat = { (default_stat2 ()) with
    functions = List.length funcs;
    classes = List.length classes;
  }
  in
  raise Todo


let kind_of_file_using_stat _stat =
  raise Todo
