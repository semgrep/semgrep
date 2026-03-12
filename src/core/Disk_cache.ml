(*
   Copyright (c) 2026 Semgrep Inc., All rights reserved.
*)
open Common
open Fpath_.Operators

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
            | Sys_error _ -> ())
   with
  | Sys_error _ -> ());
  try Sys.rmdir !!t with
  | Sys_error _ -> ()

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
  type handle

  val write : t -> string -> value -> (handle, error) result
  val read : handle -> (value, error) result
  val rm : handle -> unit
end

module Make (V : DISK_CACHEABLE) : S with type value = V.t = struct
  type value = V.t
  type handle = Fpath.t

  let write t k v : (handle, error) result =
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

  let rm (path : handle) : unit =
    if Sys_.file_exists !!path then Sys.remove !!path
    else
      (* nosemgrep: no-logs-in-library *)
      Logs.warn (fun m ->
          m "Failed to remove nonexistent cached file %s" !!path)
end
