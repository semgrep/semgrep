(*
   Copyright (c) 2026 Semgrep Inc., All rights reserved.
*)
open Common
open Fpath_.Operators

type t = Fpath.t

type error = IO of { path : Fpath.t; reason : string } | Serde of string
[@@deriving show]

type 'a handle = Fpath.t

let setup () =
  try Filename.temp_dir "semgrep-" "" |> Fpath.v |> Result.ok with
  | Sys_error msg -> Result.error ("Could not create disk cache: " ^ msg)

let to_exn : error -> exn = function
  | IO { path; reason } ->
      Failure (spf "Failed to read/write %s: %s" !!path reason)
  | Serde msg -> Failure (spf "Failed to unmarshal cached value: %s" msg)

let unwrap : ('a, error) result -> 'a = function
  | Ok a -> a
  | Error e -> raise (to_exn e)

(*****************************************************************************)
(* Per-type cache interface *)
(*****************************************************************************)

module type DISK_CACHEABLE = sig
  type t

  val ext : string
  val has_closures : bool
end

module type S = sig
  type value

  val write : t -> string -> value -> (value handle, error) result
  val read : value handle -> (value, error) result
  val rm : value handle -> unit
end

module Make (V : DISK_CACHEABLE) : S with type value = V.t = struct
  type value = V.t

  let write t k v : (value handle, error) result =
    let hashname = Digest.string k |> Digest.to_hex in
    let path = t / spf "%s.%s" hashname V.ext in

    try
      if V.has_closures then UMarshal_.write_with_closures v path
      else UMarshal_.write_value v path;
      Ok path
    with
    | Failure marshal_err -> Error (Serde marshal_err)
    | Sys_error reason -> Error (IO { path; reason })

  let read path =
    try Ok (UMarshal_.get_value path) with
    | Failure msg -> Error (Serde msg)
    | Sys_error reason -> Error (IO { path; reason })

  let rm (path : value handle) : unit =
    if Sys_.file_exists !!path then Sys.remove !!path
    else
      (* nosemgrep: no-logs-in-library *)
      Logs.warn (fun m ->
          m "Failed to remove nonexistent cached file %s" !!path)
end
