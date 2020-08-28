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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* update: see checker/check_variables_php now
*)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* 
 * Analyse global decl. Analyze parameters. Add in internal env.
 * We don't need external env I think.
 * 
 * coupling: if modify this file, you probably want to update 
 * source_php.ml for the gui. C-s for scope.
 *)
