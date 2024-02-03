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

let tags = Logs_.create_tags [ __MODULE__ ]

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
(* Files *)
(*****************************************************************************)

let cat file =
  let acc = ref [] in
  let chan = UStdlib.open_in_bin file in
  try
    while true do
      acc := input_text_line chan :: !acc
    done;
    assert false
  with
  | End_of_file ->
      close_in chan;
      List.rev !acc

(*
   This implementation works even with Linux files like /dev/fd/63
   created by bash's process substitution e.g.

     my-ocaml-program <(echo contents)

   See https://www.gnu.org/software/bash/manual/html_node/Process-Substitution.html

   In bash, '<(echo contents)' is replaced by something like
   '/dev/fd/63' which is a special file of apparent size 0 (as
   reported by `Unix.stat`) but contains data (here,
   "contents\n"). So we can't use 'Unix.stat' or 'in_channel_length'
   to obtain the length of the file contents. Instead, we read the file
   chunk by chunk until there's nothing left to read.

   Why such a function is not provided by the ocaml standard library is
   unclear.
*)
let read_file ?(max_len = max_int) path =
  if !jsoo then (
    let ic = UStdlib.open_in_bin path in
    let s = really_input_string ic (in_channel_length ic) in
    close_in ic;
    s)
  else
    let buf_len = 4096 in
    let extbuf = Buffer.create 4096 in
    let buf = Bytes.create buf_len in
    let rec loop fd =
      match Unix.read fd buf 0 buf_len with
      | 0 -> Buffer.contents extbuf
      | num_bytes ->
          assert (num_bytes > 0);
          assert (num_bytes <= buf_len);
          Buffer.add_subbytes extbuf buf 0 num_bytes;
          if Buffer.length extbuf >= max_len then Buffer.sub extbuf 0 max_len
          else loop fd
    in
    let fd = UUnix.openfile path [ Unix.O_RDONLY ] 0 in
    Common.protect ~finally:(fun () -> Unix.close fd) (fun () -> loop fd)

let write_file ~file s =
  let chan = UStdlib.open_out_bin file in
  output_string chan s;
  close_out chan

(* could be in control section too *)

let fullpath file =
  if not (USys.file_exists file) then
    failwith (spf "fullpath: file (or directory) %s does not exist" file);
  let dir, base =
    if USys.is_directory file then (file, None)
    else (Filename.dirname file, Some (Filename.basename file))
  in
  (* save *)
  let old = USys.getcwd () in

  USys.chdir dir;
  let here = USys.getcwd () in

  (* restore *)
  USys.chdir old;

  match base with
  | None -> here
  | Some x -> Filename.concat here x

(* emacs/lisp inspiration (eric cooper and yaron minsky use that too) *)
let (with_open_outfile :
      string (* filename *) -> ((string -> unit) * out_channel -> 'a) -> 'a) =
 fun file f ->
  let chan = UStdlib.open_out_bin file in
  let xpr s = output_string chan s in
  unwind_protect
    (fun () ->
      let res = f (xpr, chan) in
      close_out chan;
      res)
    (fun _e -> close_out chan)

let (with_open_infile : string (* filename *) -> (in_channel -> 'a) -> 'a) =
 fun file f ->
  let chan = UStdlib.open_in_bin file in
  unwind_protect
    (fun () ->
      let res = f chan in
      close_in chan;
      res)
    (fun _e ->
      (* TODO? use close_in_noerr? *)
      close_in chan)

(*****************************************************************************)
(* Tmp Files *)
(*****************************************************************************)

(* creation of tmp files, a la gcc *)

let _temp_files_created = Hashtbl.create 101

(* ex: new_temp_file "cocci" ".c" will give "/tmp/cocci-3252-434465.c" *)
let new_temp_file prefix suffix =
  let pid = if !jsoo then 42 else UUnix.getpid () in
  let processid = i_to_s pid in
  let tmp_file = UFilename.temp_file (prefix ^ "-" ^ processid ^ "-") suffix in
  Hashtbl.add _temp_files_created tmp_file ();
  tmp_file

let save_tmp_files = ref false

let erase_temp_files () =
  if not !save_tmp_files then (
    _temp_files_created
    |> Hashtbl.iter (fun s () ->
           Logs.debug (fun m -> m ~tags "erasing: %s" s);
           USys.remove s);
    Hashtbl.clear _temp_files_created)

let erase_this_temp_file f =
  if not !save_tmp_files then (
    Hashtbl.remove _temp_files_created f;
    Logs.debug (fun m -> m ~tags "erasing: %s" f);
    USys.remove f)

(*****************************************************************************)
(* Directories *)
(*****************************************************************************)

(** [dir_contents] returns the paths of all regular files that are
 * contained in [dir]. Each file is a path starting with [dir].
  *)
let dir_contents dir =
  let rec loop result = function
    | f :: fs -> (
        match f with
        | f when not (USys.file_exists f) ->
            Logs.err (fun m -> m ~tags "%s does not exist anymore" f);
            loop result fs
        | f when USys.is_directory f ->
            USys.readdir f |> Array.to_list
            |> List_.map (Filename.concat f)
            |> List.append fs |> loop result
        | f -> loop (f :: result) fs)
    | [] -> result
  in
  loop [] [ dir ]

let follow_symlinks = ref false

let vcs_re =
  "(^((\\.hg)|(CVS)|(\\.git)|(_darcs)|(\\.svn))$)|(.*\\.git_annot$)|(.*\\.marshall$)"
  |> Re.Posix.re |> Re.compile

let files_of_dir_or_files_no_vcs_nofilter xs =
  xs
  |> List_.map (fun x ->
         if USys.is_directory x then
           let files = dir_contents x in
           List.filter (fun x -> not (Re.execp vcs_re x)) files
         else [ x ])
  |> List_.flatten

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
        USys.set_signal Sys.sigint
          (Sys.Signal_handle
             (fun _ ->
               pr2 "C-c intercepted, will do some cleaning before exiting";
               (* But if do some try ... with e -> and if do not reraise the exn,
                * the bubble never goes at top and so I cant really C-c.
                *
                * A solution would be to not raise, but do the erase_temp_file in the
                * syshandler, here, and then exit.
                * The current solution is to not do some wild  try ... with e
                * by having in the exn handler a case: UnixExit x -> raise ... | e ->
                *)
               USys.set_signal Sys.sigint Sys.Signal_default;
               raise (UnixExit (-1))));

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
            erase_temp_files ()))
(* let _ = if not !Sys.interactive then (main ()) *)
