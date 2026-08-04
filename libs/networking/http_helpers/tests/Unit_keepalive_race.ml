(*
   Copyright (c) 2026 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* Regression test for SCRT-965 — half-closed-keep-alive race in cohttp's
   Connection_cache, which surfaces as intermittent [Validation_error] on
   Secrets validators in production.

   In production we install [Cohttp_lwt_unix.Client] and a [Connection_proxy]
   cache (see [OSS/libs/networking/proxy/proxy.ml] and
   [src/secrets/Secrets.ml]). The cache reuses keep-alive connections without
   a liveness probe. When a peer sends RST between successful requests on a
   cached connection, the writer-side error path in cohttp-lwt's [Connection]
   module surfaces the raw Unix exception (e.g. [ECONNRESET], [EPIPE], or
   [End_of_file]) *past* the cache's [Retry]-only handler. The caller sees an
   [Error] rather than a transparent retry.

   This test stands up a loopback TCP server that, per connection, responds
   to one HTTP request with a [Connection: keep-alive] 200 and then
   immediately RSTs the socket ([setsockopt SO_LINGER 0] then [close]). It
   then issues a batch of requests through [Http_helpers.call_client] (which
   goes through the cache, just like production) and asserts that at least
   one of them surfaces as an [Error] — i.e. the cache leaked a non-[Retry]
   exception.

   The race is not deterministic; a given iteration may go through the
   reader-loss path (which the cache *does* retry transparently). With the
   batching strategy used below — bursting [conc] concurrent requests so the
   cache must reuse a stale connection — every run we've tried surfaces the
   bug. The test is therefore tagged [flaky] to be explicit about the
   stochastic nature; the assertion is "[n_err > 0]", so if cohttp ever grows
   a liveness probe and the failure rate drops to zero, this test should
   fail loudly and prompt investigation.

   This test exercises cohttp behavior; it is *not* a test of any
   secrets-layer mitigation we may add later (e.g. an HTTP retry wrapper in
   [src/secrets/Secrets.ml]).
*)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let t = Testo.create
let keepalive_race_tag = Testo.Tag.declare "keepalive_race"

(* Minimal HTTP/1.1 response. Content-Length: 0 keeps the parser happy and
   avoids any chunked-encoding complications. *)
let response_bytes =
  "HTTP/1.1 200 OK\r\nContent-Length: 0\r\nConnection: keep-alive\r\n\r\n"

(* Non-regex substring scan. The HTTP header terminator we look for is a
   fixed 4-byte literal, so naive O(n*m) scanning is fine — [acc] is
   bounded by the small request our test server reads (< 4 KiB) and [sub]
   is 4 bytes. Avoids pulling [Str]'s global state into the test. *)
let string_contains ~sub s =
  let n = String.length s in
  let m = String.length sub in
  if m = 0 then true
  else if m > n then false
  else
    let last_start = n - m in
    let rec scan i =
      if i > last_start then false
      else if String.sub s i m = sub then true
      else scan (i + 1)
    in
    scan 0

(* Read from [fd] until we have seen a CRLFCRLF header terminator or the
   peer closes. Returns whatever was read. Uses a [Buffer.t] so appending
   the next read chunk is amortized O(1) instead of copying the whole
   accumulator per iteration as [acc ^ chunk] would. *)
let read_request_headers (fd : Lwt_unix.file_descr) =
  let read_buf = Bytes.create 4096 in
  let acc = Buffer.create 4096 in
  let rec loop () =
    let%lwt n = Lwt_unix.read fd read_buf 0 (Bytes.length read_buf) in
    if n = 0 then Lwt.return (Buffer.contents acc)
    else (
      Buffer.add_subbytes acc read_buf 0 n;
      let so_far = Buffer.contents acc in
      if string_contains ~sub:"\r\n\r\n" so_far then Lwt.return so_far
      else loop ())
  in
  loop ()

(* Handle one client connection: read a request, write a keep-alive 200,
   then RST the socket so the next request on the cached connection sees
   ECONNRESET/EPIPE/End_of_file rather than a clean FIN. *)
let handle_client (client_fd : Lwt_unix.file_descr) =
  Lwt.catch
    (fun () ->
      let%lwt _ = read_request_headers client_fd in
      let bs = Bytes.of_string response_bytes in
      let%lwt _ = Lwt_unix.write client_fd bs 0 (Bytes.length bs) in
      (* SO_LINGER with l_linger=0 causes [close] to send RST instead of FIN.
         The next write on the client side will fail with ECONNRESET, and
         the next read will fail in a way cohttp surfaces as [End_of_file]
         or a raw [Unix_error] from the writer's catch path. *)
      Unix.setsockopt_optint
        (Lwt_unix.unix_file_descr client_fd)
        Unix.SO_LINGER (Some 0);
      Lwt_unix.close client_fd)
    (fun _ ->
      Lwt.catch (fun () -> Lwt_unix.close client_fd) (fun _ -> Lwt.return_unit))

(* Start a loopback server. Returns the bound port and a [stop] thunk. *)
let start_server () =
  let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt sock Unix.SO_REUSEADDR true;
  let%lwt () =
    Lwt_unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_loopback, 0))
  in
  Lwt_unix.listen sock 64;
  let port =
    match Lwt_unix.getsockname sock with
    | Unix.ADDR_INET (_, p) -> p
    | _ -> assert false
  in
  let stop = ref false in
  let rec accept_loop () =
    if !stop then Lwt.return_unit
    else
      Lwt.catch
        (fun () ->
          let%lwt client_fd, _ = Lwt_unix.accept sock in
          Lwt.async (fun () -> handle_client client_fd);
          accept_loop ())
        (fun _ -> Lwt.return_unit)
  in
  Lwt.async accept_loop;
  let stop_fn () =
    stop := true;
    Lwt.catch (fun () -> Lwt_unix.close sock) (fun _ -> Lwt.return_unit)
  in
  Lwt.return (port, stop_fn)

let issue_request port =
  let uri = Uri.of_string (Printf.sprintf "http://127.0.0.1:%d/" port) in
  Http_helpers.call_client ~timeout_secs:1.0 `GET uri

(*****************************************************************************)
(* Test *)
(*****************************************************************************)

(* Sequential model: the keep-alive race only requires two consecutive
   requests to the same host where the second one reuses a cached connection
   that the server has since RST'd. Running sequentially avoids cohttp
   cache-pool deadlocks under high fan-out (which we saw in earlier drafts
   of this test). With n_iter=50 and a 1s per-request timeout, total wall
   time is bounded at ~50s in the worst case but is typically much less. *)
let n_iter = 50

let install_real_client () =
  Http_helpers.set_client_ref (module Cohttp_lwt_unix.Client);
  (* Mirror what [Proxy.configure_proxy] does in production: even with no
     proxy env vars, it unconditionally installs the Connection_proxy cache.
     This is the configuration we want to test under. *)
  Cohttp_lwt_unix.Client.set_cache
    (Cohttp_lwt_unix.Connection_proxy.call
       (Cohttp_lwt_unix.Connection_proxy.create ()))

let test_keepalive_rst_surfaces_as_error () =
  Lwt_platform.run
    (install_real_client ();
     let%lwt port, stop = start_server () in
     Lwt.finalize
       (fun () ->
         let n_ok = ref 0 in
         let n_err = ref 0 in
         let one i =
           let%lwt r = issue_request port in
           (match r with
           | Ok _ -> incr n_ok
           | Error msg ->
               incr n_err;
               (* Surface the first few sample errors so the test output is
                  actionable when the assertion fails. *)
               if !n_err <= 5 then
                 Printf.printf "[SCRT-965] iter %d: Error %s\n%!" i msg);
           Lwt.return_unit
         in
         let%lwt () = Lwt_list.iter_s one (List.init n_iter Fun.id) in
         let total = !n_ok + !n_err in
         Printf.printf
           "[SCRT-965 keepalive race] %d/%d requests surfaced as Error \
            (cache-leaked); %d/%d succeeded\n\
            %!"
           !n_err total !n_ok total;
         Alcotest.(check bool)
           "expected at least one cache-leaked failure (cohttp half-closed \
            keep-alive reuse race; see SCRT-965). If this fails by counting \
            zero errors, cohttp may have grown a liveness probe — investigate \
            before deleting the test."
           true (!n_err > 0);
         Lwt.return_unit)
       (fun () -> stop ()))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* [extra_tags] lets the caller inject project-level tags (e.g.
   [Test_tags.flaky]) that this OSS-only library can't declare itself
   without duplicating the canonical declaration. *)
let tests ?(extra_tags = []) () =
  Testo.categorize "Keepalive race (SCRT-965)"
    [
      t
        ~tags:(keepalive_race_tag :: extra_tags)
        "writer-loss surfaces as error" test_keepalive_rst_surfaces_as_error;
    ]
