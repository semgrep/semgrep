open Printf
open Metavariable

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

(*
     This is used for tracking the span of a matched sequence of statements.
  *)
let replace_capture k v env =
  if debug then printf "replace_capture %s\n" k;
  let kv = (k, v) in
  let full_env = kv :: List.remove_assoc k env.full_env in
  let min_env = kv :: List.remove_assoc k env.min_env in
  { env with full_env; min_env }

let remove_capture k env =
  if debug then printf "remove_capture %s\n" k;
  {
    env with
    full_env = List.remove_assoc k env.full_env;
    min_env = List.remove_assoc k env.min_env;
  }

(*
     To be called as early as possible after passing a backreference
     which may no longer be needed when descending down the pattern.
     For now, we call this only when reaching a new stmt pattern node.

     For simplicity, we assume any member of 'min_env' may no longer be
     needed. It may be more efficient to accumulate the backreferences
     that were encountered since the last stmt and only consider removing
     their bindings, rather than considering all the bindings in min_env.

     If we don't call this, the cache keys will be overspecified, reducing
     or preventing reuse.
  *)
let update_min_env env (stmt_pat : G.stmt) =
  if debug then printf "update_min_env\n";
  let backrefs =
    match stmt_pat.s_backrefs with
    | None -> assert false (* missing initialization *)
    | Some x -> x
  in
  let min_env =
    List.filter
      (fun (k, _v) ->
        let keep = Set_.mem k backrefs in
        if debug then printf "keep %s in min env: %B\n" k keep;
        keep)
      env.min_env
  in
  { env with min_env; last_stmt_backrefs = backrefs }
