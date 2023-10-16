(* Austin Theriault
 *
 * Copyright (C) 2019-2023 Semgrep, Inc.
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
(*  *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

open Js_of_ocaml
open Semgrep_js_shared

let read_line_ref = ref (fun () -> Lwt.return None)
let read_exactly_ref = ref (fun _ -> Lwt.return None)

module Io = RPC_server.MakeLSIO (struct
  type input = unit
  type output = unit

  let read_line _ = !read_line_ref ()
  let stdin = ()
  let stdout = ()
  let flush _ = Lwt.return ()
  let atomic f oc = f oc

  let write _ str =
    (* nosem *)
    print_string str;
    Lwt.return ()

  let read_exactly _ n = !read_exactly_ref n
end)

(*****************************************************************************)
(* Code *)
(*****************************************************************************)
let _Promise = Js.Unsafe.global##._Promise
let ( let+ ) = Lwt.( >|= )

let promise_of_lwt lwt =
  new%js _Promise
    (Js.wrap_callback (fun resolve reject ->
         try%lwt
           let+ res = lwt () in
           Js.Unsafe.fun_call resolve [| Js.Unsafe.inject res |]
         with
         | e ->
             let msg = Printexc.to_string e in
             Firebug.console##error (Js.string msg);
             Js.Unsafe.fun_call reject
               [| Js.Unsafe.inject (new%js Js.error_constr (Js.string msg)) |]))

(* Stolen from Logs' logs_browser.ml *)
let ppf, flush =
  let b = Buffer.create 255 in
  let flush () =
    let s = Buffer.contents b in
    Buffer.clear b;
    s
  in
  (Format.formatter_of_buffer b, flush)

let console_report _src _level ~over k msgf =
  let k _ =
    Firebug.console##error (Js.string (flush ()));
    over ();
    k ()
  in
  msgf @@ fun ?header ?tags fmt ->
  ignore tags;
  match header with
  | None -> Format.kfprintf k ppf ("@[" ^^ fmt ^^ "@]@.")
  | Some h -> Format.kfprintf k ppf ("[%s] @[" ^^ fmt ^^ "@]@.") h

let _ =
  RPC_server.io_ref := (module Io);
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter { Logs.report = console_report };
  Js.export_all
    (object%js
       method init = init_jsoo
       method start = promise_of_lwt LS.start
       method getMountpoints = get_jsoo_mountpoint ()
       method setParsePattern = setParsePattern
       method setJustParseWithLang = setJustParseWithLang

       method setReadLine f =
         read_line_ref := fun () -> f () |> Js.to_string |> Lwt.return_some

       method setReadExactly f =
         read_exactly_ref := fun n -> f n |> Js.to_string |> Lwt.return_some
    end)
