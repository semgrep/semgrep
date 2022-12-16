(*
 * this module is here just so that people can write
 *   module ISet = Set.Make (Int)
 * just like they write
 *   module SMap = Map.Make (String)
 *)

type t = int
let compare = (-)
