(* Copyright 2026 Semgrep Inc. *)

(** Generic implementation for caching values on disk via Marshal.

    Provides a reusable pattern for writing values to a temp directory
    and reading them back on demand, reducing peak memory by keeping
    data on disk between pipeline phases.

    Historically, this functionality was provided by the Pro_disk_cache
    module, but its interface overfit to marshalling ASTs specifically,
    and adding new marshallable types proved difficult in the presence
    of cyclic dependencies between pro.core and the types' home module.
    *)

type t

type error = IO of { path : Fpath.t; reason : string } | Serde of string
[@@deriving show]

val setup : unit -> (t, string) result
(* Initialises the cache's temporary directory. *)

val cleanup : t -> unit
(** Remove any remaining files in the cache directory and the directory
    itself. Safe to call even if some files have already been removed.

    Pre: all handles obtained from this cache are no longer in use.
    Reading from a handle after [cleanup] will return an [IO] error. *)

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
  (** The value type to cache on disk. *)

  val ext : string
  (** Short description of the cache type, used for file extension
      (e.g. "taint_cfgs", "ast_cache"). *)

  val has_closures : bool
  (** If [true], uses [Marshal.Closures] to support function values.
      Only safe for within-a-single-run caching (same binary). *)
end

module type S = sig
  type value
  type handle

  val write : t -> string -> value -> (handle, error) result
  (** Marshal [value] to disk under [key]. The key is hashed to produce
      a unique filename in the cache directory. *)

  val read : handle -> (value, error) result
  (** Unmarshal a value from disk. *)

  val rm : handle -> unit
  (** Delete the cache file. *)
end

module Make (V : DISK_CACHEABLE) : S with type value = V.t
