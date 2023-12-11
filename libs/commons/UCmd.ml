(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Small wrapper around Bos.OS.Cmd *)

(* old: was in Common.ml

   exception CmdError of Unix.process_status * string

   let process_output_to_list2 ?(verbose = false) command =
     (* alt: use Cmd.with_open_process_in *)
     let chan = UUnix.open_process_in command in
     let res = ref ([] : string list) in
     let rec process_otl_aux () =
       let e = input_line chan in
       res := e :: !res;
       if verbose then pr2 e;
       process_otl_aux ()
     in
     try process_otl_aux () with
     | End_of_file ->
         let stat = Unix.close_process_in chan in
         (List.rev !res, stat)

   let cmd_to_list ?verbose command =
     let l, exit_status = process_output_to_list2 ?verbose command in
     match exit_status with
     | Unix.WEXITED 0 -> l
     | _ ->
         raise
           (CmdError
              ( exit_status,
                spf "CMD = %s, RESULT = %s" command (String.concat "\n" l) ))

   let cmd_to_list_and_status = process_output_to_list2
*)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let string_of_run ~trim cmd =
  let out = Cmd.bos_apply Bos.OS.Cmd.run_out cmd in
  Bos.OS.Cmd.out_string ~trim out

let lines_of_run ~trim cmd =
  let out = Cmd.bos_apply Bos.OS.Cmd.run_out cmd in
  Bos.OS.Cmd.out_lines ~trim out

let status_of_run ?quiet = Cmd.bos_apply (Bos.OS.Cmd.run_status ?quiet)

(* TODO: switch to type Cmd.t for cmd *)
let with_open_process_in (cmd : string) f =
  let chan = UUnix.open_process_in cmd in
  Common.protect ~finally:(fun () -> close_in chan) (fun () -> f chan)
