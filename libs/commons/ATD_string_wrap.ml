(* This module is here to help using the 'string wrap <ocaml module=Xxx>'
 * feature of atdgen.
 * See https://atd.readthedocs.io/en/stable/atdgen-reference.html#using-a-custom-wrapper
 * for more information.
 *)
open Common

module Fpath = struct
  type t = Fpath.t [@@deriving show, eq]

  let unwrap = Fpath.to_string
  let wrap = Fpath.v
  let to_yojson = Fpath_.to_yojson
  let of_yojson = Fpath_.of_yojson
end

module Uri = struct
  type t = Uri.t [@@deriving show]

  let unwrap = Uri.to_string
  let wrap = Uri.of_string
end

module Uuidm = struct
  type t = Uuidm.t [@@deriving show]

  let unwrap = Uuidm.to_string

  let wrap x =
    match Uuidm.of_string x with
    | Some x -> x
    | None -> failwith (spf "Uuidm parse error on %s" x)
end

module Sha1 = struct
  type t = Digestif.SHA1.t

  let unwrap = Digestif.SHA1.to_hex
  let wrap = Digestif.SHA1.of_hex
end

module Sha256 = struct
  type t = Digestif.SHA256.t

  let unwrap = Digestif.SHA256.to_hex
  let wrap = Digestif.SHA256.of_hex
end

module Datetime = struct
  type t = Timedesc.Timestamp.t

  let unwrap tm : string = Timedesc.Timestamp.to_rfc3339 tm

  let wrap s =
    (* Note that RFC 3339 is a subset of ISO 8601, so this does align with
     * unwrap. *)
    match Timedesc.Timestamp.of_iso8601 s with
    | Ok dt -> dt
    | Error s -> failwith (spf "wrong datetime format: %s" s)

  let () =
    Testo.test "ATD_string_wrap.Datetime" (fun () ->
        let now = Timedesc.Timestamp.now () in
        let s : string = unwrap now in
        let now' = wrap s in
        if not (now =*= now') then
          failwith
            (spf
               "the date %s which was unwrapped as %s is not the same when \
                wrapped back as %s"
               (Dumper.dump now) s (Dumper.dump now')))
end
