(* Yoann Padioleau
 *
 * Copyright (C) 1998-2023 Yoann Padioleau
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
(* Stdout *)
(*****************************************************************************)

let pr s =
  UStdlib.print_string s;
  UStdlib.print_string "\n";
  flush UStdlib.stdout

let pr_time name f =
  let t1 = UUnix.gettimeofday () in
  Common.protect f ~finally:(fun () ->
      let t2 = UUnix.gettimeofday () in
      pr (spf "%s: %.6f s" name (t2 -. t1)))

(*****************************************************************************)
(* Stderr *)
(*****************************************************************************)
let pr2 s =
  UStdlib.prerr_string s;
  UStdlib.prerr_string "\n";
  flush UStdlib.stderr

let _already_printed = Hashtbl.create 101
let disable_pr2_once = ref false

let xxx_once f s =
  if !disable_pr2_once then pr2 s
  else if not (Hashtbl.mem _already_printed s) then (
    Hashtbl.add _already_printed s true;
    f ("(ONCE) " ^ s))

let pr2_once s = xxx_once pr2 s
let pr2_gen x = pr2 (Dumper.dump x)

let pr2_time name f =
  let t1 = UUnix.gettimeofday () in
  protect f ~finally:(fun () ->
      let t2 = UUnix.gettimeofday () in
      pr2 (spf "%s: %.6f s" name (t2 -. t1)))

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

(* now in prelude: exception UnixExit of int *)
let exn_to_real_unixexit f =
  try f () with
  | UnixExit x ->
      (* nosemgrep: forbid-exit *)
      UStdlib.exit x

let pp_do_in_zero_box f =
  UFormat.open_box 0;
  f ();
  UFormat.close_box ()

let before_exit = ref []

let main_boilerplate f =
  if not !Sys.interactive then
    exn_to_real_unixexit (fun () ->
        let default_handler signal =
          Sys.Signal_handle
            (fun _ ->
              let linux_signal = Sys_.ocaml_signal_to_signal signal in
              let handled_name = Sys_.signal_to_string linux_signal in
              (* Feel free to match on signal here :D *)
              pr2
                (handled_name
               ^ " signal intercepted, will do some cleaning before exiting");
              USys.set_signal signal Sys.Signal_default;
              (* But if do some try ... with e -> and if do not reraise the exn,
               * the bubble never goes at top and so I cant really C-c.
               *
               * A solution would be to not raise, but do the erase_temp_file in the
               * syshandler, here, and then exit.
               * The current solution is to not do some wild  try ... with e
               * by having in the exn handler a case: UnixExit x -> raise ... | e ->
               *)
              (* Convert to a "standard" unix exit code from a unix signal *)
              raise (UnixExit Sys_.(signal_to_linux_exit_code linux_signal)))
        in

        (* ref: https://faculty.cs.niu.edu/~hutchins/csci480/signals.htm *)
        (* all signals that want TERM i.e. graceful shutdown *)
        let default_signals = [ Sys.sigint ] in
        let unix_signals = [ Sys.sighup; Sys.sigpipe; Sys.sigterm ] in
        let signals =
          (* Sigint works on windows, rest don't. Not sure why :/*)
          if USys.os_type <> "Win32" then
            List.append default_signals unix_signals
          else default_signals
        in
        List.iter
          (fun (s : int) -> USys.set_signal s (default_handler s))
          signals;

        (* The finalize() below makes it tedious to go back from exns when we use
         * 'back' in ocamldebug. Hence the special code in finalize() to
         * run differently when in "debugger mode". However the
         * Common.debugger global will be set in main(), so too late, so
         * we have to be quicker here and set it for the finalize() below.
         *)
        if
          USys.argv |> Array.to_list
          |> List.exists (fun x -> x = "-debugger" || x = "--debugger")
        then debugger := true;

        finalize
          (fun () ->
            pp_do_in_zero_box (fun () ->
                try f () with
                (* <---- here it is *)
                | UUnix.Unix_error (e, fm, argm) ->
                    pr2
                      (spf "exn Unix_error: %s %s %s\n" (Unix.error_message e)
                         fm argm);
                    raise (UUnix.Unix_error (e, fm, argm))))
          (fun () ->
            !before_exit |> List.iter (fun f -> f ());
            (* nosemgrep: forbid-tmp *)
            UTmp.erase_temp_files ()))
(* let _ = if not !Sys.interactive then (main ()) *)
