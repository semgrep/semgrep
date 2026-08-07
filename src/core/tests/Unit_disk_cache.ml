(* Copyright 2026 Semgrep Inc. *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Unit tests for Disk_cache. Most tests run against both backends
   ([On_disk] and [In_memory]) to confirm behavioral parity; a few extra
   tests pin down the in-memory-specific contract (rm is a claim-release,
   equal_handle is physical identity). *)

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

let with_cache ?mode f =
  match Disk_cache.setup ?mode () with
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
(* Tests run against both backends *)
(*****************************************************************************)

let test_round_trip mode =
  with_cache ~mode (fun cache ->
      let handle = write_ok cache "key1" 42 in
      let v = read_ok handle in
      Alcotest.(check int) "round-trip value" 42 v;
      Int_cache.rm handle |> ignore)

let test_closure_round_trip mode =
  with_cache ~mode (fun cache ->
      let f x = x + 1 in
      match Fn_cache.write cache "fn_key" f with
      | Error _ -> Alcotest.fail "write closure failed"
      | Ok handle -> (
          match Fn_cache.read handle with
          | Error _ -> Alcotest.fail "read closure failed"
          | Ok f' ->
              Alcotest.(check int) "closure result" 42 (f' 41);
              Fn_cache.rm handle |> ignore))

(* On disk, [rm] deletes the file and subsequent reads fail. In memory, [rm]
   is a claim-release: the value stays readable through the handle we hold. *)
let test_rm mode =
  with_cache ~mode (fun cache ->
      let handle = write_ok cache "key_rm" 99 in
      let _ = read_ok handle in
      Int_cache.rm handle |> ignore;
      match (mode, Int_cache.read handle) with
      | Disk_cache.On_disk, Error _ -> ()
      | Disk_cache.On_disk, Ok _ ->
          Alcotest.fail "on-disk: expected read to fail after rm"
      | Disk_cache.In_memory, Ok v ->
          Alcotest.(check int) "in-memory: value survives rm" 99 v
      | Disk_cache.In_memory, Error _ ->
          Alcotest.fail "in-memory: read after rm should still succeed")

let test_different_keys mode =
  with_cache ~mode (fun cache ->
      let h1 = write_ok cache "key_a" 1 in
      let h2 = write_ok cache "key_b" 2 in
      let v1 = read_ok h1 in
      let v2 = read_ok h2 in
      Alcotest.(check int) "first value" 1 v1;
      Alcotest.(check int) "second value" 2 v2;
      Int_cache.rm h1 |> ignore;
      Int_cache.rm h2 |> ignore)

(* On disk the same key overwrites a single file; in memory each write yields
   an independent handle. Either way, the handle from the second write reads
   back the second value. *)
let test_same_key_latest_value mode =
  with_cache ~mode (fun cache ->
      let _h1 = write_ok cache "same_key" 1 in
      let h2 = write_ok cache "same_key" 2 in
      let v = read_ok h2 in
      Alcotest.(check int) "latest value" 2 v;
      Int_cache.rm h2 |> ignore)

(*****************************************************************************)
(* In-memory-specific tests *)
(*****************************************************************************)

(* Each write allocates a fresh blob, so equal_handle is physical identity:
   the same handle equals itself, but two writes (even of the same value) are
   distinct handles. *)
let test_in_memory_equal_handle_identity () =
  with_cache ~mode:Disk_cache.In_memory (fun cache ->
      let h1 = write_ok cache "k" 7 in
      let h2 = write_ok cache "k" 7 in
      Alcotest.(check bool)
        "same handle is equal" true
        (Int_cache.equal_handle h1 h1);
      Alcotest.(check bool)
        "distinct writes are not equal" false
        (Int_cache.equal_handle h1 h2);
      Int_cache.rm h1 |> ignore;
      Int_cache.rm h2 |> ignore)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let both name f =
  [
    Testo.create (name ^ " (on-disk)") (fun () -> f Disk_cache.On_disk);
    Testo.create (name ^ " (in-memory)") (fun () -> f Disk_cache.In_memory);
  ]

let tests =
  Testo.categorize "Disk_cache"
    (both "round-trip" test_round_trip
    @ both "closure round-trip" test_closure_round_trip
    @ both "rm" test_rm
    @ both "different keys" test_different_keys
    @ both "same key latest value" test_same_key_latest_value
    @ [
        Testo.create "in-memory equal_handle identity"
          test_in_memory_equal_handle_identity;
      ])
