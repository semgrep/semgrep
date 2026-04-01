(*
   Copyright (c) 2026 Semgrep Inc., All rights reserved.
*)
open Common
open Fpath_.Operators

let ( let* ) = Result.bind

type t = Fpath.t

type error = IO of { path : Fpath.t; reason : string } | Serde of string
[@@deriving show]

let setup () =
  try Filename.temp_dir "semgrep-" "" |> Fpath.v |> Result.ok with
  | Sys_error msg -> Result.error ("Could not create disk cache: " ^ msg)

let cleanup (t : t) : unit =
  (* Remove remaining files, then the directory itself. *)
  (try
     (* nosemgrep: forbid-fs *)
     Sys.readdir !!t
     |> Array.iter (fun name ->
            let path = !!(t / name) in
            try Sys.remove path with
            | Sys_error err ->
                (* nosemgrep: no-logs-in-library *)
                Logs.warn (fun m ->
                    m "Disk_cache: can't remove %s: %s" path err))
   with
  | Sys_error err ->
      (* nosemgrep: no-logs-in-library *)
      Logs.warn (fun m -> m "Disk_cache: can't readdir %s: %s" !!t err));
  try Sys.rmdir !!t with
  | Sys_error err ->
      (* nosemgrep: no-logs-in-library *)
      Logs.warn (fun m -> m "Disk_cache: can't rmdir %s: %s" !!t err)

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
  val record_stats_on_span : Opentelemetry.Scope.t -> unit
end

module Make (V : DISK_CACHEABLE) : S with type value = V.t = struct
  type value = V.t
  type handle = Fpath.t

  (* Atomic counters for I/O stats. Times stored as nanosecond ints. *)
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

  let write t k v : (handle, error) result =
    let hashname = Digest.string k |> Digest.to_hex in
    let path = t / spf "%s.%s" hashname V.ext in
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
    let result =
      let* path = marshal_result in
      let* new_size = stat_size path in
      Atomic.fetch_and_add stat_cache_size_bytes (new_size - old_size) |> ignore;
      Ok path
    in
    (match result with
    | Error _ -> Atomic.fetch_and_add stat_errors 1 |> ignore
    | Ok _ -> ());
    result

  let read path =
    let res, elapsed_s =
      Common.with_time (fun () ->
          try Ok (UMarshal_.get_value path) with
          | Failure msg -> Error (Serde msg)
          | Sys_error reason -> Error (IO { path; reason }))
    in
    Atomic.fetch_and_add stat_reads 1 |> ignore;
    Atomic.fetch_and_add stat_read_ns (s_to_ns elapsed_s) |> ignore;
    (match res with
    | Error _ -> Atomic.fetch_and_add stat_errors 1 |> ignore
    | Ok _ -> ());
    res

  let equal_handle = Fpath.equal

  let rm (path : handle) : (unit, error) result =
    let result =
      let* size = stat_size path in
      let* () =
        try Ok (Sys.remove !!path) with
        | Sys_error reason -> Error (IO { path; reason })
      in
      Atomic.fetch_and_add stat_cache_size_bytes (-size) |> ignore;
      Ok ()
    in
    (match result with
    | Error _ -> Atomic.fetch_and_add stat_errors 1 |> ignore
    | Ok _ -> ());
    result

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
