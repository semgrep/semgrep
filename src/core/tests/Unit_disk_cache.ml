(* Copyright 2026 Semgrep Inc. *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Unit tests for Disk_cache. *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

module Int_cache = Disk_cache.Make (struct
  type t = int

  let ext = "int_test"
  let has_closures = false
end)

module Fn_cache = Disk_cache.Make (struct
  type t = int -> int

  let ext = "fn_test"
  let has_closures = true
end)

let with_cache f =
  match Disk_cache.setup () with
  | Error msg -> Alcotest.fail ("setup failed: " ^ msg)
  | Ok cache ->
      Common.protect
        ~finally:(fun () -> Disk_cache.cleanup cache)
        (fun () -> f cache)

let write_ok cache key value =
  match Int_cache.write cache key value with
  | Error (IO { path; reason }) ->
      Alcotest.failf "write IO error at %s: %s" (Fpath.to_string path) reason
  | Error (Serde msg) -> Alcotest.failf "write serde error: %s" msg
  | Ok handle -> handle

let read_ok handle =
  match Int_cache.read handle with
  | Error (IO { path; reason }) ->
      Alcotest.failf "read IO error at %s: %s" (Fpath.to_string path) reason
  | Error (Serde msg) -> Alcotest.failf "read serde error: %s" msg
  | Ok v -> v

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

let test_round_trip () =
  with_cache (fun cache ->
      let handle = write_ok cache "key1" 42 in
      let v = read_ok handle in
      Alcotest.(check int) "round-trip value" 42 v;
      Int_cache.rm handle |> ignore)

let test_closure_round_trip () =
  with_cache (fun cache ->
      let f x = x + 1 in
      match Fn_cache.write cache "fn_key" f with
      | Error _ -> Alcotest.fail "write closure failed"
      | Ok handle -> (
          match Fn_cache.read handle with
          | Error _ -> Alcotest.fail "read closure failed"
          | Ok f' ->
              Alcotest.(check int) "closure result" 42 (f' 41);
              Fn_cache.rm handle |> ignore))

let test_rm_deletes_file () =
  with_cache (fun cache ->
      let handle = write_ok cache "key_rm" 99 in
      (* verify we can read it *)
      let _ = read_ok handle in
      Int_cache.rm handle |> ignore;
      (* reading after rm should fail *)
      match Int_cache.read handle with
      | Error _ -> ()
      | Ok _ -> Alcotest.fail "expected read to fail after rm")

let test_different_keys_different_files () =
  with_cache (fun cache ->
      let h1 = write_ok cache "key_a" 1 in
      let h2 = write_ok cache "key_b" 2 in
      let v1 = read_ok h1 in
      let v2 = read_ok h2 in
      Alcotest.(check int) "first value" 1 v1;
      Alcotest.(check int) "second value" 2 v2;
      Int_cache.rm h1 |> ignore;
      Int_cache.rm h2 |> ignore)

let test_same_key_overwrites () =
  with_cache (fun cache ->
      let _h1 = write_ok cache "same_key" 1 in
      let h2 = write_ok cache "same_key" 2 in
      let v = read_ok h2 in
      Alcotest.(check int) "overwritten value" 2 v;
      Int_cache.rm h2 |> ignore)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests =
  Testo.categorize "Disk_cache"
    [
      Testo.create "round-trip" test_round_trip;
      Testo.create "closure round-trip" test_closure_round_trip;
      Testo.create "rm deletes file" test_rm_deletes_file;
      Testo.create "different keys, different files"
        test_different_keys_different_files;
      Testo.create "same key overwrites" test_same_key_overwrites;
    ]
