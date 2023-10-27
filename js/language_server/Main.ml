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
(* This is strongly coupled to [src/osemgrep/language_server] *)
(* This file serves as an interface for the above, that is easily *)
(* consumable by the javascript/node. *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

open Js_of_ocaml
open Semgrep_js_shared

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

let _ =
  RPC_server.io_ref := (module Semgrep_node_js_shared.Io);
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter { Logs.report = Semgrep_node_js_shared.console_report };
  Http_helpers.client_ref := Some (module Cohttp_lwt_jsoo.Client);
  Js.export_all
    (object%js
       method init = init_jsoo
       method start = Semgrep_node_js_shared.promise_of_lwt LS.start
       method getMountpoints = get_jsoo_mountpoint ()
       method setParsePattern = setParsePattern
       method setJustParseWithLang = setJustParseWithLang
    end)
