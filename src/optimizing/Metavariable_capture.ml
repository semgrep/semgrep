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
  (* This is the set of metavariables referenced in the rest of the pattern.
     It's used to determine the subset of bindings that should be kept in
     min_env. It comes from the last stmt node encountered in the pattern. *)
  last_stmt_backrefs : AST_utils.String_set.t;
}

let empty = { full_env = []; min_env = []; last_stmt_backrefs = Set_.empty }

(* Get the value bound to a metavariable or return None. *)
let get_capture k env = List.assoc_opt k env.full_env

(*
     A pattern node provides the set of metavariables that are already bound
     and checked against in the rest of the pattern. This is e.g. the
     's_backrefs' field for a statement node.
  *)
let has_backref k backrefs = Set_.mem k backrefs

(*
     To be called each time a new value is captured, i.e. bound to a
     metavariable.
  *)
let add_capture k v env =
  if debug then printf "add_capture %s\n" k;
  let kv = (k, v) in
  let full_env = kv :: env.full_env in
  let min_env = kv :: env.min_env in
  { env with full_env; min_env }
