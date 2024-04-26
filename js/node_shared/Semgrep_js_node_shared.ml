(* Austin Theriault
 *
 * Copyright (C) Semgrep, Inc.
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
(* Callv isn't supported via cohttp jsoo for some reason!  But we use it for proxying *)
(* So here we patch it so it works, and then in XMLHttpRequest we *)
(* support proxies *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

module Patched_cohttp_lwt_jsoo : Cohttp_lwt.S.Client = struct
  include Cohttp_lwt_jsoo.Client

  (* Callv isn't currently supported on the JS side so we have to provide it ourselves :( *)
  let callv ?ctx (uri : Uri.t) stream =
    (* Differs from request uri if HTTP(S)_PROXY is set. But we handle this in
       the node_shared XHR*)
    ignore uri;
    Lwt.return
      (Lwt_stream.map_s
         (fun ((req, body) : Cohttp.Request.t * Cohttp_lwt.Body.t) ->
           call ?ctx ~headers:req.headers ~body ~chunked:false req.meth
             Cohttp.Request.(uri req))
         stream)
end

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

let init_cohttp () =
  Http_helpers.client_ref :=
    Some (module Patched_cohttp_lwt_jsoo : Cohttp_lwt.S.Client)
