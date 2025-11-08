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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Pad's poor's man profiler. See pfff's Main.ml for example of use
 * and the -profile command-line flag.
 *
 * You should probably rely on ocamlprof, perf, memprof, and the
 * many other OCaml profiling tools.
 *)

open Printf

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type prof = ProfAll | ProfNone | ProfSome of string list

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

(* nosemgrep: no-ref-declarations-at-top-scope *)
let profile = ref ProfNone

(* nosemgrep: no-ref-declarations-at-top-scope *)
let report_at_exit = ref false

(* nosemgrep: no-ref-declarations-at-top-scope *)
let report_gc_stats = ref false

(* nosemgrep: no-ref-declarations-at-top-scope *)
let show_trace_profile = ref false

let check_profile category =
  match !profile with
  | ProfAll -> true
  | ProfNone -> false
  | ProfSome l -> List.mem category l

let table_lock = Mutex.create ()
let profile_table = Hashtbl.create 100

let adjust_profile_entry category difftime =
  Mutex.protect table_lock @@ fun () ->
  let xtime, xcount =
    try Hashtbl.find profile_table category with
    | Not_found ->
        let xtime = ref 0.0 in
        let xcount = ref 0 in
        Hashtbl.add profile_table category (xtime, xcount);
        (xtime, xcount)
  in
  xtime := !xtime +. difftime;
  incr xcount;
  ()

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* subtle: don't forget to give all argumens to f, otherwise partial app
 * and will profile nothing.
 *
 * todo: try also detect when complexity augment each time, so can
 * detect the situation for a function gets worse and worse ?
 *)
let measure category f =
  if not (check_profile category) then f ()
  else (
    if !show_trace_profile then Logs.debug (fun m -> m "> %s" category);
    let t = Unix.gettimeofday () in
    let res, prefix =
      try (Ok (f ()), "") with
      (*TODO: Timeout _ as*)
      | exn ->
          let e = Exception.catch exn in
          (Error e, "*")
    in
    let category = prefix ^ category in
    (* add a '*' to indicate timeout func *)
    let t' = Unix.gettimeofday () in

    if !show_trace_profile then Logs.debug (fun m -> m "< %s" category);

    adjust_profile_entry category (t' -. t);
    match res with
    | Ok res -> res
    | Error e -> Exception.reraise e)

(*****************************************************************************)
(* Reports *)
(*****************************************************************************)

type entry = { name : string; total_time : float; count : int }

let export () : entry list =
  Mutex.protect table_lock @@ fun () ->
  Hashtbl.fold
    (fun name (total_time, count) acc ->
      { name; total_time = !total_time; count = !count } :: acc)
    profile_table []
  |> List.sort (fun a b -> Float.compare b.total_time a.total_time)

(* todo: also put  % ? also add % to see if coherent numbers *)
let report () : string =
  let entries = export () in
  Buffer_.with_buffer_to_string (fun buf ->
      let prf fmt = Printf.bprintf buf fmt in
      prf "\n";
      prf "%s\n" (String.make 78 '-');
      prf "Simple profiling results\n";
      prf
        "                                                       total time \
         /      count\n";
      prf "%s\n" (String.make 78 '-');
      entries
      |> List.iter (fun x ->
             prf "%-50s : %10.3f s / %10d\n" x.name x.total_time x.count))

(*****************************************************************************)
(* Init *)
(*****************************************************************************)
let flags () =
  [
    ( "-profile",
      Arg.Unit
        (fun () ->
          profile := ProfAll;
          report_gc_stats := true;
          report_at_exit := true),
      " record and output profiling information to stderr" );
    ( "-simple_profiling",
      Arg.Unit (fun () -> profile := ProfAll),
      " record profiling information for inclusion in JSON results" );
    ("-show_trace_profile", Arg.Set show_trace_profile, " show trace");
  ]

let log_diagnostics_and_gc_stats () =
  eprintf "%s%!" (report ());
  if !report_gc_stats then Gc.print_stat stderr

(* ugly *)
let () =
  UCommon.before_exit :=
    (fun () -> if !report_at_exit then log_diagnostics_and_gc_stats ())
    :: !UCommon.before_exit
