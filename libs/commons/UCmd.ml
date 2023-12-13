open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Small wrapper around Bos.OS.Cmd
 *
 * A few functions contain a 'nosemgrep: forbid-exec' because anyway
 * those functions will/are also blacklisted in forbid-exec.jsonnet.
 *)

(*****************************************************************************)
(* Old Common.cmd_to_list *)
(*****************************************************************************)

exception CmdError of Unix.process_status * string

let process_output_to_list ?(verbose = false) command =
  (* alt: use Cmd.with_open_process_in *)
  (* nosemgrep: forbid-exec *)
  let chan = UUnix.open_process_in command in
  let res = ref ([] : string list) in
  let rec process_otl_aux () =
    let e = input_line chan in
    res := e :: !res;
    if verbose then UCommon.pr2 e;
    process_otl_aux ()
  in
  try process_otl_aux () with
  | End_of_file ->
      let stat = Unix.close_process_in chan in
      (List.rev !res, stat)

let cmd_to_list ?verbose command =
  let l, exit_status = process_output_to_list ?verbose command in
  match exit_status with
  | Unix.WEXITED 0 -> l
  | _ ->
      raise
        (CmdError
           ( exit_status,
             spf "CMD = %s, RESULT = %s" command (String.concat "\n" l) ))

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let string_of_run ~trim cmd =
  (* nosemgrep: forbid-exec *)
  let out = Cmd.bos_apply Bos.OS.Cmd.run_out cmd in
  (* nosemgrep: forbid-exec *)
  Bos.OS.Cmd.out_string ~trim out

let lines_of_run ~trim cmd =
  (* nosemgrep: forbid-exec *)
  let out = Cmd.bos_apply Bos.OS.Cmd.run_out cmd in
  (* nosemgrep: forbid-exec *)
  Bos.OS.Cmd.out_lines ~trim out

(* nosemgrep: forbid-exec *)
let status_of_run ?quiet = Cmd.bos_apply (Bos.OS.Cmd.run_status ?quiet)

(* TODO: switch to type Cmd.t for cmd *)
let with_open_process_in (cmd : string) f =
  (* nosemgrep: forbid-exec *)
  let chan = UUnix.open_process_in cmd in
  Common.protect ~finally:(fun () -> close_in chan) (fun () -> f chan)
