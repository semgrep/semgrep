(*
   Copyright (c) 2026 Semgrep Inc., All rights reserved.
*)
open Common
open Fpath_.Operators

let ( let* ) = Result.bind

(* A cache either spills values to files under a temporary directory
 * ([Disk_backed]) or keeps them as marshaled byte blobs in the heap
 * ([Memory_backed]). The mode is chosen once at [setup]; every value cached
 * through a given [t] uses the same backend.
 *
 * The two backends are behaviorally identical from a caller's perspective:
 * both marshal on write and unmarshal on read, so every [read] returns a
 * fresh, independent copy of the value (copy-on-read). The only differences
 * are where the marshaled bytes live and, consequently, whether the
 * filesystem is touched. *)
type t = Disk_backed of Fpath.t | Memory_backed
type storage_mode = On_disk | In_memory

type error = IO of { path : Fpath.t; reason : string } | Serde of string
[@@deriving show]

let setup ?(mode = On_disk) () =
  match mode with
  | In_memory -> Ok Memory_backed
  | On_disk -> (
      try
        let dir = Filename.temp_dir "semgrep-" "" |> Fpath.v in
        Ok (Disk_backed dir)
      with
      | Sys_error msg -> Result.error ("Could not create disk cache: " ^ msg))

let cleanup (t : t) : unit =
  match t with
  | Memory_backed -> ()
  | Disk_backed dir -> (
      (* Remove remaining files, then the directory itself. *)
      (try
         (* nosemgrep: forbid-fs *)
         Sys.readdir !!dir
         |> Array.iter (fun name ->
             let path = !!(dir / name) in
             try Sys.remove path with
             | Sys_error err ->
                 (* nosemgrep: no-logs-in-library *)
                 Logs.warn (fun m ->
                     m "Disk_cache: can't remove %s: %s" path err))
       with
      | Sys_error err ->
          (* nosemgrep: no-logs-in-library *)
          Logs.warn (fun m -> m "Disk_cache: can't readdir %s: %s" !!dir err));
      try Sys.rmdir !!dir with
      | Sys_error err ->
          (* nosemgrep: no-logs-in-library *)
          Logs.warn (fun m -> m "Disk_cache: can't rmdir %s: %s" !!dir err))

let to_exn : error -> exn = function
  | IO { path; reason } ->
      Failure (spf "Failed to read/write %s: %s" !!path reason)
  | Serde msg -> Failure (spf "Failed to unmarshal cached value: %s" msg)

let unwrap : ('a, error) result -> 'a = function
  | Ok a -> a
  | Error e -> raise (to_exn e)

let s_to_ns s = int_of_float (s *. 1e9)

(*****************************************************************************)
(* Per-type cache interface *)
(*****************************************************************************)

module type DISK_CACHEABLE = sig
  type t

  val ext : string
  val has_closures : bool
end

type io_stats = {
  reads : int;
  writes : int;
  errors : int;
  cache_size_bytes : int;
  read_time_s : float;
  write_time_s : float;
}

module type S = sig
  type value
  type handle

  val write : t -> string -> value -> (handle, error) result
  val read : handle -> (value, error) result
  val rm : handle -> (unit, error) result
  val equal_handle : handle -> handle -> bool
  val record_stats_on_span : Opentelemetry.Span.t -> unit
end

module Make (V : DISK_CACHEABLE) : S with type value = V.t = struct
  type value = V.t

  (* A handle is either a path to a marshaled file (on-disk mode) or the
   * marshaled bytes of the value held in the heap (in-memory mode). In both
   * cases [read] unmarshals a fresh copy; the in-memory blob is simply the
   * exact bytes that would otherwise have been written to a file. *)
  type handle =
    | On_disk_handle of Fpath.t
    | In_memory_handle of value Marshaled.InMemory.t

  (* Atomic counters for I/O stats. Times stored as nanosecond ints. In
   * in-memory mode there is no filesystem I/O; the time counters measure
   * marshal/unmarshal CPU instead, and [cache_size_bytes] tracks the total
   * size of the outstanding (not-yet-[rm]'d) blobs. *)
  let stat_reads = Atomic.make 0
  let stat_writes = Atomic.make 0
  let stat_errors = Atomic.make 0
  let stat_cache_size_bytes = Atomic.make 0
  let stat_read_ns = Atomic.make 0
  let stat_write_ns = Atomic.make 0

  let snapshot_and_reset_stats () =
    {
      reads = Atomic.exchange stat_reads 0;
      writes = Atomic.exchange stat_writes 0;
      errors = Atomic.exchange stat_errors 0;
      cache_size_bytes = Atomic.get stat_cache_size_bytes;
      read_time_s = Float.of_int (Atomic.exchange stat_read_ns 0) /. 1e9;
      write_time_s = Float.of_int (Atomic.exchange stat_write_ns 0) /. 1e9;
    }

  let stat_size path =
    UUnix.stat path
    |> Result.map (fun (stats : Unix.stats) -> stats.st_size)
    |> Result.map_error (fun (err, _, _) ->
        IO { path; reason = Unix.error_message err })

  let count_error = function
    | Error _ -> Atomic.fetch_and_add stat_errors 1 |> ignore
    | Ok _ -> ()

  (* The two backends. Each owns its own [write]/[read]/[rm] and produces/
     consumes its own [handle] constructor; the dispatch below routes to them
     by cache mode (for [write]) or by handle (for [read]/[rm]). They share the
     stat counters above. *)

  (* Marshals each value to a file under the cache's temp directory: lowest
     resident memory, but touches the filesystem on every read and write. This
     is the original Disk_cache behavior. *)
  module On_disk = struct
    let write dir k v : (handle, error) result =
      let hashname = Digest.string k |> Digest.to_hex in
      let path = dir / spf "%s.%s" hashname V.ext in
      (* Capture old size before overwriting so the counter stays accurate.
         ENOENT is expected for new files; other stat errors are propagated. *)
      let* old_size =
        match UUnix.stat path with
        | Ok stats -> Ok stats.st_size
        | Error (Unix.ENOENT, _, _) -> Ok 0
        | Error (err, _, _) ->
            Error (IO { path; reason = Unix.error_message err })
      in
      let marshal_result, elapsed_s =
        Common.with_time (fun () ->
            try
              if V.has_closures then UMarshal_.write_with_closures v path
              else UMarshal_.write_value v path;
              Ok path
            with
            | Failure marshal_err -> Error (Serde marshal_err)
            | Sys_error reason -> Error (IO { path; reason }))
      in
      Atomic.fetch_and_add stat_writes 1 |> ignore;
      Atomic.fetch_and_add stat_write_ns (s_to_ns elapsed_s) |> ignore;
      let* path = marshal_result in
      let* new_size = stat_size path in
      Atomic.fetch_and_add stat_cache_size_bytes (new_size - old_size) |> ignore;
      Ok (On_disk_handle path)

    let read path : (value, error) result =
      let res, elapsed_s =
        Common.with_time (fun () ->
            try Ok (UMarshal_.get_value path) with
            | Failure msg -> Error (Serde msg)
            | Sys_error reason -> Error (IO { path; reason }))
      in
      Atomic.fetch_and_add stat_reads 1 |> ignore;
      Atomic.fetch_and_add stat_read_ns (s_to_ns elapsed_s) |> ignore;
      res

    (* Delete the file; subsequent reads of the handle fail. *)
    let rm path : (unit, error) result =
      let* size = stat_size path in
      let* () =
        try Ok (Sys.remove !!path) with
        | Sys_error reason -> Error (IO { path; reason })
      in
      Atomic.fetch_and_add stat_cache_size_bytes (-size) |> ignore;
      Ok ()
  end

  (* Keeps each value as a marshaled byte blob in the heap: no filesystem
     access, higher resident memory. The blob is the exact bytes that would
     otherwise have been written to a file. *)
  module In_memory = struct
    let write v : (handle, error) result =
      let flags = if V.has_closures then [ Marshal.Closures ] else [] in
      let marshal_result, elapsed_s =
        Common.with_time (fun () ->
            try Ok (Marshaled.InMemory.marshal ~flags v) with
            | Invalid_argument msg -> Error (Serde msg))
      in
      Atomic.fetch_and_add stat_writes 1 |> ignore;
      Atomic.fetch_and_add stat_write_ns (s_to_ns elapsed_s) |> ignore;
      let* blob = marshal_result in
      Atomic.fetch_and_add stat_cache_size_bytes
        (Marshaled.InMemory.size_bytes blob)
      |> ignore;
      Ok (In_memory_handle blob)

    let read blob : (value, error) result =
      let res, elapsed_s =
        Common.with_time (fun () ->
            try Ok (Marshaled.InMemory.unmarshal blob) with
            | Failure msg -> Error (Serde msg))
      in
      Atomic.fetch_and_add stat_reads 1 |> ignore;
      Atomic.fetch_and_add stat_read_ns (s_to_ns elapsed_s) |> ignore;
      res

    (* Claim-release: drop the entry from the size accounting, but the blob
       stays readable through the handle until it (and the handle) are
       garbage-collected. No caller reads a handle after [rm]. *)
    let rm blob : (unit, error) result =
      Atomic.fetch_and_add stat_cache_size_bytes
        (-Marshaled.InMemory.size_bytes blob)
      |> ignore;
      Ok ()
  end

  let write t k v : (handle, error) result =
    let result =
      match t with
      | Disk_backed dir -> On_disk.write dir k v
      | Memory_backed -> In_memory.write v
    in
    count_error result;
    result

  let read handle : (value, error) result =
    let result =
      match handle with
      | On_disk_handle path -> On_disk.read path
      | In_memory_handle blob -> In_memory.read blob
    in
    count_error result;
    result

  let rm handle : (unit, error) result =
    let result =
      match handle with
      | On_disk_handle path -> On_disk.rm path
      | In_memory_handle blob -> In_memory.rm blob
    in
    count_error result;
    result

  let equal_handle a b =
    match (a, b) with
    | On_disk_handle p1, On_disk_handle p2 -> Fpath.equal p1 p2
    (* Physical identity: each [write] allocates a fresh blob, so this is true
       handle identity, matching the on-disk path-equality semantics and
       avoiding a structural compare of large blobs. *)
    | In_memory_handle b1, In_memory_handle b2 -> phys_equal b1 b2
    | On_disk_handle _, In_memory_handle _
    | In_memory_handle _, On_disk_handle _ ->
        false

  let record_stats_on_span span =
    let stats = snapshot_and_reset_stats () in
    if stats.reads > 0 || stats.writes > 0 then
      let p = spf "disk_cache.%s" V.ext in
      Tracing.add_data_to_span span
        [
          (spf "%s.reads" p, `Int stats.reads);
          (spf "%s.writes" p, `Int stats.writes);
          (spf "%s.errors" p, `Int stats.errors);
          (spf "%s.cache_size_bytes" p, `Int stats.cache_size_bytes);
          (spf "%s.read_time_s" p, `Float stats.read_time_s);
          (spf "%s.write_time_s" p, `Float stats.write_time_s);
        ]
end
