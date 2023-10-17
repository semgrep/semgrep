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

module Io = RPC_server.MakeLSIO (struct
  type input = in_channel
  type output = out_channel

  let read_line ic = input_line ic |> Lwt.return_some
  let stdin = stdin
  let stdout = stdout
  let flush () = flush stdout |> Lwt.return
  let atomic f oc = f oc

  let write _ str =
    (* nosem *)
    print_string str;
    Lwt.return ()

  let read_exactly ic n =
    let rec read_exactly acc = function
      | 0 -> String.of_seq (acc |> List.rev |> List.to_seq)
      | n -> read_exactly (input_char ic :: acc) (n - 1)
    in
    let exact = read_exactly [] n in
    Lwt.return (Some exact)
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
  Http_helpers.client_ref := Some (module Cohttp_lwt_jsoo.Client);
  Js.export_all
    (object%js
       method init = init_jsoo
       method start = promise_of_lwt LS.start
       method getMountpoints = get_jsoo_mountpoint ()
       method setParsePattern = setParsePattern
       method setJustParseWithLang = setJustParseWithLang
    end)
