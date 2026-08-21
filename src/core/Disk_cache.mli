(* Copyright 2026 Semgrep Inc. *)

(** Generic implementation for caching values via Marshal, either on disk or
    in memory.

    Provides a reusable pattern for marshalling values and reading them back on
    demand, reducing GC pressure (and, on disk, peak resident memory) by keeping
    infrequently-used data in a compact marshaled form between pipeline phases.

    Two backends are available, chosen at {!setup} (see {!storage_mode}):
    - [On_disk] marshals values to files under a temporary directory. Lowest
      resident memory, but touches the filesystem on every read and write.
    - [In_memory] keeps each value as a marshaled byte blob in the heap. No
      filesystem access at all; higher resident memory. Intended for
      environments where filesystem access is slow but RAM is plentiful.

    Both backends are copy-on-read: every [read] unmarshals a fresh, independent
    value, so callers can safely mutate what they read without affecting the
    cache or other readers.

    Historically, this functionality was provided by the Pro_disk_cache
    module, but its interface overfit to marshalling ASTs specifically,
    and adding new marshallable types proved difficult in the presence
    of cyclic dependencies between pro.core and the types' home module.
    *)

type t

type storage_mode =
  | On_disk
  | In_memory  (** Selects a cache's backend (see the module docstring). *)

type error = IO of { path : Fpath.t; reason : string } | Serde of string
[@@deriving show]

val setup : ?mode:storage_mode -> unit -> (t, string) result
(** Initialises the cache. [mode] defaults to [On_disk], which creates a
    temporary directory. In [In_memory] mode no directory is created and the
    filesystem is never touched. *)

val cleanup : t -> unit
(** For an on-disk cache, remove any remaining files in the cache directory and
    the directory itself; safe to call even if some files have already been
    removed. For an in-memory cache this is a no-op (blobs are reclaimed by the
    GC once their handles are dropped).

    Pre: all handles obtained from this cache are no longer in use.
    Reading from an on-disk handle after [cleanup] will return an [IO] error. *)

val to_exn : error -> exn
(* For legacy code paths that use exceptions for error handling, transform
   an [error] into a readable default exception. *)

val unwrap : ('a, error) result -> 'a
(** For legacy code paths, unsafely unwrap the result or raise. *)

(*****************************************************************************)
(* Per-type cache interface *)
(*****************************************************************************)

module type DISK_CACHEABLE = sig
  type t
  (** The value type to cache. *)

  val ext : string
  (** Short description of the cache type, used for the on-disk file extension
      (e.g. "taint_cfgs", "ast_cache"). *)

  val has_closures : bool
  (** If [true], uses [Marshal.Closures] to support function values.
      Only safe for within-a-single-run caching (same binary). *)
end

module type S = sig
  type value
  type handle

  val write : t -> string -> value -> (handle, error) result
  (** Marshal [value] under [key]. On disk the key is hashed to produce a
      unique filename in the cache directory; in memory the key is unused. *)

  val read : handle -> (value, error) result
  (** Unmarshal a fresh copy of the value from the handle. *)

  val rm : handle -> (unit, error) result
  (** Release a cached entry. For an on-disk cache this deletes the file, and
      subsequent [read]s of the handle fail. For an in-memory cache this only
      releases the cache's bookkeeping; the value remains readable through an
      already-held handle and is reclaimed by the GC once the handle is
      dropped. No caller is expected to read a handle after [rm]. *)

  val equal_handle : handle -> handle -> bool
  (** Handle identity: path equality on disk, physical (pointer) identity of
      the marshaled blob in memory. *)

  val record_stats_on_span : Opentelemetry.Span.t -> unit
  (** Snapshot I/O stats, reset counters, and record them as attributes
      on the given OpenTelemetry span under keys like
      ["disk_cache.{ext}.reads"], ["disk_cache.{ext}.cache_size_bytes"], etc.
      In in-memory mode the [read_time_s]/[write_time_s] figures measure
      marshal/unmarshal CPU rather than I/O. *)
end

module Make (V : DISK_CACHEABLE) : S with type value = V.t
