(*
   Copyright (c) 2026 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation, with the
   special exception on linking described in file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details.
*)
module Out = Semgrep_output_v1_t

(****************************************************************************)
(* Prelude *)
(****************************************************************************)

let t = Testo.create

(****************************************************************************)
(* Helpers *)
(****************************************************************************)

let pos line col = Out.{ line; col; offset = 0 }
let fpath_of_string_opt s = Fpath.of_string s |> Result.to_option

let fpath_of_string_exn s =
  match Fpath.of_string s with
  | Ok s -> s
  | Error (`Msg s) -> raise (Failure s)

let loc path line col_start col_end =
  Out.
    {
      path = fpath_of_string_exn path;
      start = pos line col_start;
      end_ = pos line col_end;
    }

(** Extract the artifact location as an [Fpath.t] from a thread flow location. *)
let uri_of_tfl (tfl : Sarif.Sarif_v_2_1_0_t.thread_flow_location) =
  let open Sarif.Sarif_v_2_1_0_t in
  let open Common in
  (* Some unpacking (ha!) *)
  let* l = tfl.location in
  let* phys = l.physical_location in
  let* artifact_location = phys.artifact_location in
  let* uri = artifact_location.uri in
  fpath_of_string_opt uri

let fpath = Alcotest.testable Fpath.pp Fpath.equal

let nesting_of_tfl (tfl : Sarif.Sarif_v_2_1_0_t.thread_flow_location) =
  Option.value tfl.nesting_level ~default:(-1L)

(****************************************************************************)
(* Tests *)
(****************************************************************************)

let test_cliloc_one_location () =
  let call_trace = Out.CliLoc (loc "src/foo.py" 4 13 17, "event") in
  let tfls = Sarif_output.call_trace_to_locations 0 call_trace in
  match tfls with
  | tfl :: [] -> begin
      Alcotest.(check (option fpath))
        __LOC__
        (fpath_of_string_opt "src/foo.py")
        (uri_of_tfl tfl);
      Alcotest.(check int64) __LOC__ 0L (nesting_of_tfl tfl)
    end
  | []
  | _ :: _ ->
      Alcotest.fail "Expected a thread flow traces w/length 1"

let test_clicall_no_intermediates () =
  let call_site = (loc "src/caller.py" 11 25 50, "callee_func()") in
  let sink = Out.CliLoc (loc "src/callee.py" 13 26 39, "template_file") in
  let call_trace = Out.CliCall (call_site, [], sink) in
  let tfls = Sarif_output.call_trace_to_locations 1 call_trace in
  match tfls with
  | [ call_tfl; sink_tfl ] -> begin
      Alcotest.(check (option fpath))
        __LOC__
        (fpath_of_string_opt "src/caller.py")
        (uri_of_tfl call_tfl);
      Alcotest.(check int64) __LOC__ 1L (nesting_of_tfl call_tfl);
      Alcotest.(check (option fpath))
        __LOC__
        (fpath_of_string_opt "src/callee.py")
        (uri_of_tfl sink_tfl);
      Alcotest.(check int64) __LOC__ 2L (nesting_of_tfl sink_tfl)
    end
  | _ -> Alcotest.fail "Expected a thread flow trace w/length 2"

let test_clicall_with_intermediates () =
  let call_site = (loc "src/caller.py" 11 25 50, "callee_func()") in
  let intermediate =
    Out.{ location = loc "src/callee.py" 10 67 75; content = "customer" }
  in
  let sink = Out.CliLoc (loc "src/callee.py" 13 26 39, "template_file") in
  let call_trace = Out.CliCall (call_site, [ intermediate ], sink) in
  let tfls = Sarif_output.call_trace_to_locations 1 call_trace in
  (* call + 1 intermediate + sink = 3 *)
  match tfls with
  | [ call_tfl; inter_tfl; sink_tfl ] -> begin
      Alcotest.(check (option fpath))
        __LOC__
        (fpath_of_string_opt "src/caller.py")
        (uri_of_tfl call_tfl);
      Alcotest.(check int64) __LOC__ 1L (nesting_of_tfl call_tfl);
      Alcotest.(check (option fpath))
        __LOC__
        (fpath_of_string_opt "src/callee.py")
        (uri_of_tfl inter_tfl);
      Alcotest.(check int64) __LOC__ 2L (nesting_of_tfl inter_tfl);
      Alcotest.(check (option fpath))
        __LOC__
        (fpath_of_string_opt "src/callee.py")
        (uri_of_tfl sink_tfl);
      Alcotest.(check int64) __LOC__ 2L (nesting_of_tfl sink_tfl)
    end
  | _ -> Alcotest.fail "Expected a thread flow trace w/length 3"

let test_call_trace_to_locations () =
  Testo.categorize "call_trace_to_locations"
    [
      t "CliLoc: one location, correct URI and nesting level"
        test_cliloc_one_location;
      t
        "CliCall with no intermediates: call at nesting N, sink at N+1, with \
         correct per-location URIs"
        test_clicall_no_intermediates;
      t
        "CliCall with intermediates: call at N, intermediates at N+1, sink at \
         N+1"
        test_clicall_with_intermediates;
    ]

let test_truncate_rule_id_short () =
  let id = "gitlab.flawfinder.drand48" in
  Alcotest.(check string) __LOC__ id (Sarif_output.truncate_rule_id id)

let test_truncate_rule_id_at_limit () =
  let id = String.make 255 'x' in
  Alcotest.(check string) __LOC__ id (Sarif_output.truncate_rule_id id)

let test_truncate_rule_id_over_limit () =
  (* Reproduces https://github.com/semgrep/semgrep/issues/10941:
     gitlab.flawfinder.drand48 has a 327-char rule ID *)
  let id = String.make 327 'x' in
  let result = Sarif_output.truncate_rule_id id in
  Alcotest.(check int) __LOC__ 255 (String.length result);
  (* First 247 chars are the original prefix *)
  Alcotest.(check string) __LOC__ (String.make 247 'x') (String.sub result 0 247);
  (* Last 8 chars are the MD5 hash suffix, not the naive truncation *)
  Alcotest.(check string) __LOC__ "ade735f7" (String.sub result 247 8);
  Alcotest.(check bool) __LOC__ false (result = String.make 255 'x')

let test_truncate_rule_id_no_collision () =
  (* Two IDs sharing a long common prefix must produce distinct truncated IDs.
     This is the case that plain truncation fails on. *)
  let id1 = String.make 300 'x' ^ "1" in
  let id2 = String.make 300 'x' ^ "2" in
  Alcotest.(check bool) __LOC__ true
    (Sarif_output.truncate_rule_id id1 <> Sarif_output.truncate_rule_id id2)

let test_truncate_rule_id () =
  Testo.categorize "truncate_rule_id"
    [
      t "short ID passes through unchanged" test_truncate_rule_id_short;
      t "ID of exactly 255 chars passes through unchanged"
        test_truncate_rule_id_at_limit;
      t "ID over 255 chars: length 255, correct prefix and hash suffix"
        test_truncate_rule_id_over_limit;
      t "two long IDs with common prefix produce distinct truncated IDs"
        test_truncate_rule_id_no_collision;
    ]

(****************************************************************************)
(* Entry point *)
(****************************************************************************)

let tests =
  Testo.categorize_suites "Sarif output"
    [ test_call_trace_to_locations (); test_truncate_rule_id () ]
