(* Cooper Pierce and Yosef Alsuhaibani
 *
 * Copyright (C) Semgrep, Inc. All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see
 * <https://www.gnu.org/licenses/>.
 *)

exception DomainRaised of (exn * Printexc.raw_backtrace)
(* captures an exception that was raised on a remote domain, with a given
 * backtrace.  Since backtraces are lost when crossing domain boundaries
 * (see https://github.com/ocaml/ocaml/issues/11074 for discussion), we
 * explicitly capture and store the backtrace rather than re-raising.*)

let map ~(conf : Parallelism_config.eio_state) ~domain_count f l =
  Eio.Switch.run @@ fun sw ->
  let domain_mgr = Eio.Stdenv.domain_mgr conf.env in
  let pool = Executor_pool.create ~sw ~domain_count domain_mgr in

  (* nosemgrep: no-logs-in-library *)
  Logs.debug (fun m ->
      m "Mapping %d elements across %d domains" (List.length l) domain_count);

  Eio.Fiber.List.map ~max_fibers:domain_count
    (fun elem ->
      (* Please see the comment block in [Hook.ml] concerning safe values of
       * [weight], if you are intending on changing it! *)
      match
        Executor_pool.submit pool ~weight:1.0 (fun () ->
            try f elem with
            | e ->
                (* If an exception is propagated all the way up to the executor pool,
                 * we need to make sure we capture the backtrace _before_ we return;
                 * otherwise, the original exn will be re-raised when the domain joins
                 * but the stack trace will only contain the parent domain's frames. *)
                let bt = Printexc.get_raw_backtrace () in
                raise (DomainRaised (e, bt)))
      with
      | Ok res -> Ok res
      | Error err -> Error (elem, err))
    l
[@@tracing]

let () =
  let open Printexc in
  register_printer (function
    | DomainRaised (exn, bt) ->
        let exn_desc =
          match Printexc.use_printers exn with
          | None -> "with no registered printer"
          | Some s -> s
        in
        let str_of_bt = Printexc.raw_backtrace_to_string bt in
        Some
          (Printf.sprintf "Exception %s\nraised on child domain at\n %s"
             exn_desc str_of_bt)
    | _ -> None)
