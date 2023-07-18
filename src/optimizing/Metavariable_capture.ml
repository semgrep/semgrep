open Printf
open Metavariable

let debug = false

(*
   Environment that is carried along and modified while matching a
   pattern AST against a target AST. It holds the captured metavariables
   which are eventually returned if matching is successful.
*)

type t = {
  (* All metavariable captures *)
  full_env : bindings;
  (* Only the captures that are used in the rest of the pattern.
     Used in the cache key. *)
  min_env : bindings;
}

let empty = { full_env = []; min_env = [] }

(* Get the value bound to a metavariable or return None. *)
let get_capture k env = List.assoc_opt k env.full_env

(*
     To be called each time a new value is captured, i.e. bound to a
     metavariable.
  *)
let add_capture k v env =
  if debug then printf "add_capture %s\n" k;
  let kv = (k, v) in
  let full_env = kv :: env.full_env in
  let min_env = kv :: env.min_env in
  { full_env; min_env }
