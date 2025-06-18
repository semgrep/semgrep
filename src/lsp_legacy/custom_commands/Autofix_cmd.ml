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

(* Commentary *)
(* The client actually handles applying the fix. This command is run afterwards *)
(* just to inform the language server that it was applied *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
open Lsp
open Types

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
let command = "semgrep/autofix"
let create () = Command.create ~title:"Apply Fix" ~command ()

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

let command_handler (session : Session.t) _arg_list =
  (* the actual edit is handled by the client. We run this command just so
     we can record it *)
  let metrics =
    { session.metrics with autofix_count = session.metrics.autofix_count + 1 }
  in
  ({ session with metrics }, None)
