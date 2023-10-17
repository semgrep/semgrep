(* This module is here to help using the 'string wrap <ocaml module=Xxx>'
 * feature of atdgen.
 * See https://atd.readthedocs.io/en/stable/atdgen-reference.html#using-a-custom-wrapper
 * for more information.
 *)

module Uri = struct
  type t = Uri.t [@@deriving show]

  let unwrap = Uri.to_string
  let wrap = Uri.of_string
end

module Fpath = struct
  type t = Fpath.t [@@deriving show]

  let unwrap = Fpath.to_string
  let wrap = Fpath.of_string
end
