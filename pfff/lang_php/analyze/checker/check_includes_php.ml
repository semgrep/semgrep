(* Yoann Padioleau
 *
 * Copyright (C) 2011 Facebook
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

module Ast = Cst_php
module IR = Include_require_php
module E = Error_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * Most of the hard work is done by include_require_php.ml. Here
 * we just call this module and check if the resolved path exists.
 *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, _pr2_once = Common2.mk_pr2_wrappers Flag_analyze_php.verbose_checking

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let check env file ast = 

  let increqs = IR.all_increq_of_any (Ast.Program ast) in
  increqs |> List.iter (fun (_inckind, tok, incexpr) ->
    try (
      let path_opt = IR.resolve_path (env, Filename.dirname file) incexpr in
      match path_opt with
      | Some path ->
          if not (Sys.file_exists path)
          then E.fatal tok (E.FileNotFound path)
            
      | None ->
          E.warning tok (E.IncludeUnresolved)
    ) 
    with 
    | (Timeout | Common2.UnixExit _) as exn -> raise exn
    | exn -> 
      pr2 (spf "PB: treating a include/require: %s" 
             (Parse_info.string_of_info tok));
      raise exn
  );
  ()
