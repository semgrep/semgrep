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
(* Prelude *)
(*****************************************************************************)
(* Pad's poor's man profiler. See pfff's Main.ml for example of use
 * and the -profile command-line flag.
 *
 * You should probably rely on ocamlprof, perf, memprof, and the
 * many other OCaml profiling tools.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type prof = ProfAll | ProfNone | ProfSome of string list

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

let profile = ref ProfNone
let show_trace_profile = ref false

let check_profile category =
  match !profile with
  | ProfAll -> true
  | ProfNone -> false
  | ProfSome l -> List.mem category l

let _profile_table = ref (Hashtbl.create 100)

let adjust_profile_entry category difftime =
  let xtime, xcount =
    try Hashtbl.find !_profile_table category with
    | Not_found ->
        let xtime = ref 0.0 in
        let xcount = ref 0 in
        Hashtbl.add !_profile_table category (xtime, xcount);
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
let profile_code category f =
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

let _is_in_exclusif = ref (None : string option)

let profile_code_exclusif category f =
  if not (check_profile category) then f ()
  else
    match !_is_in_exclusif with
    | Some s ->
        failwith (spf "profile_code_exclusif: %s but already in %s " category s)
    | None ->
        _is_in_exclusif := Some category;
        protect
          (fun () -> profile_code category f)
          ~finally:(fun () -> _is_in_exclusif := None)

let profile_code_inside_exclusif_ok _category _f = failwith "Todo"

(*****************************************************************************)
(* Diagnostic *)
(*****************************************************************************)

(* todo: also put  % ? also add % to see if coherent numbers *)
let profile_diagnostic () : string =
  if !profile =*= ProfNone then ""
  else
    let xs =
      Hashtbl.fold (fun k v acc -> (k, v) :: acc) !_profile_table []
      |> List.sort (fun (_k1, (t1, _n1)) (_k2, (t2, _n2)) -> compare t2 t1)
    in
    Buffer_.with_buffer_to_string (fun buf ->
        let prf fmt = Printf.bprintf buf fmt in
        prf "\n";
        prf "---------------------\n";
        prf "profiling result\n";
        prf "---------------------\n";
        xs
        |> List.iter (fun (k, (t, n)) ->
               prf "%-40s : %10.3f sec %10d count\n" k !t !n))

let warn_if_take_time timethreshold s f =
  let t = Unix.gettimeofday () in
  let res = f () in
  let t' = Unix.gettimeofday () in
  if t' -. t > float_of_int timethreshold then
    Logs.warn (fun m -> m "processing took %7.1fs: %s" (t' -. t) s);
  res

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let profile_code2 category f =
  profile_code category (fun () ->
      if !profile =*= ProfAll then
        Logs.info (fun m -> m "starting: %s" category);
      let t = Unix.gettimeofday () in
      let res = f () in
      let t' = Unix.gettimeofday () in
      if !profile =*= ProfAll then
        Logs.info (fun m -> m "ending: %s, %fs" category (t' -. t));
      res)

(*****************************************************************************)
(* Init *)
(*****************************************************************************)
let flags () =
  [
    ( "-profile",
      Arg.Unit (fun () -> profile := ProfAll),
      " output profiling information" );
    ("-show_trace_profile", Arg.Set show_trace_profile, " show trace");
  ]

let log_diagnostics_and_gc_stats () =
  Logs.warn (fun m -> m "%s" (profile_diagnostic ()));
  Gc.print_stat stderr

(* ugly *)
let _ =
  UCommon.before_exit :=
    (fun () -> if !profile <> ProfNone then log_diagnostics_and_gc_stats ())
    :: !UCommon.before_exit
