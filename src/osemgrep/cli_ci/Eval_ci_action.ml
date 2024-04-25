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
