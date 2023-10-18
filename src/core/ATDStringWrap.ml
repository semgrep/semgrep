(* This module is here to help using the 'string wrap <ocaml module=Xxx>'
 * feature of atdgen.
 * See https://atd.readthedocs.io/en/stable/atdgen-reference.html#using-a-custom-wrapper
 * for more information.
 *)
open Common

module Uri = struct
  type t = Uri.t [@@deriving show]

  let unwrap = Uri.to_string
  let wrap = Uri.of_string
end

module Fpath = struct
  type t = Fpath.t [@@deriving show]

  let unwrap = Fpath.to_string
  let wrap = Fpath.v
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
