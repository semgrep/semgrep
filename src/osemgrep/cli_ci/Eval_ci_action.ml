(*
   Copyright (c) 2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
module OutJ = Semgrep_output_v1_j

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Actions are sent by the backend to the CLI to customize dynamically its
 * behavior.
 *
 * This can be used for example to force people to update Semgrep.
 *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let eval (action : OutJ.action) : unit =
  match action with
  | `Delay f -> Unix.sleepf f
  | `Message str -> Logs.app (fun m -> m "%s" str)
  | `Exit code ->
      Error.exit_code_exn
        (Exit_code.of_int ~__LOC__ ~code
           ~description:"exit action from semgrep.dev")
