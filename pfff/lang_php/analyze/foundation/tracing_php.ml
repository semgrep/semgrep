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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*
 * Zend PHP used to provide a very good dynamic tracer: Xdebug 
 * http://xdebug.org/. It was not only displaying the functions called
 * but also the concrete values of the arguments (when the argument
 * was not a too big data structure).
 * Note that HPHP provides only a small subset of Xdebug features 
 * (line coverage).
 * 
 * This module tries to implement a tracer for PHP using the abstract
 * interpreter. It is thus less precise than a real dynamic tracer
 * but it can cover the whole code! This tracer can be useful
 * to reverse engineer code, to know what is called.
 * 
 * The advantage of the abstract interpreter over a tracer based
 * on database_php.ml is that the abstract interpreter has a more
 * precise callgraph.
 *)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

let tracing = ref false

(*****************************************************************************)
(* Main entry points *)
(*****************************************************************************)

let process_entity s =
  if !tracing then pr ("Processing " ^ s)

(* indent based on depth until a certain point and then just count indicator *)
let call s path =
  if !tracing 
  then begin
    let depth = List.length path - 1 in
    let prefix = 
      if depth > 6
      then spf "      <%d>" depth
      else String.make depth ' '
    in
    pr (prefix ^ ">" ^ s)
  end
