(*
   Decorate a pattern and target ASTs to make the suitable for memoization
   during matching.
*)

open Printf
open AST_generic

let debug = false

(* A set of metavariables. Access cost is O(log n). *)
module Names = AST_generic.String_set

(* Count the number of occurrences of each backreference of a metavariable. *)
module Name_counts = Map.Make (String)

let print_names oc names =
  List.iter (fun s -> fprintf oc "  %s\n" s) (Set_.elements names)

let print_name_counts oc name_counts =
  List.iter
    (fun (s, n) -> fprintf oc "  %s: %i\n" s n)
    (Name_counts.bindings name_counts)

let add_one k name_counts =
  match Name_counts.find_opt k name_counts with
  | None -> Name_counts.add k 1 name_counts
  | Some n -> Name_counts.add k (n + 1) name_counts

let get_count k name_counts =
  match Name_counts.find_opt k name_counts with
  | None -> 0
  | Some n -> n

let diff_count k ~new_counts ~old_counts =
  let n = get_count k new_counts - get_count k old_counts in
  assert (n >= 0);
  n

let diff_backrefs bound_metavars ~new_backref_counts ~old_backref_counts =
  let not_backrefs_in_rest_of_pattern =
    Set_.fold (fun k acc ->
      let added_backref_count =
        diff_count k
          ~new_counts:new_backref_counts
          ~old_counts:old_backref_counts
      in
      match added_backref_count with
      | 0 -> Set_.add k acc
      | _ -> acc
    ) bound_metavars Set_.empty
  in
  Set_.diff bound_metavars not_backrefs_in_rest_of_pattern

(*
   During matching a pattern node against a program node, we consult
   a cache to see if we already have run this before and return the
   cached result. This is memoization.

   A cache key is formed from the contents of the pattern node,
   the program node, and all the environment that is sufficient to determine
   the result of the computation. The environment includes the set
   of bound metavariables that are referenced in the rest of the pattern.
   For this, it is correct to use all the bound metavariables rather than the
   ones that really needed. However, this reduces caching efficiency by having
   irrelevant parts of the environment, the unused metavariables, in the cache
   key.

   This function decorates the pattern AST with the set of metavariables
   that are used in the rest of the pattern. This set is consulted at runtime
   to determine whether a captured metavariable should go into the cache key.
*)
let prepare_pattern any =
  let bound_metavars = ref Set_.empty in
  let backref_counts = ref Name_counts.empty in
  let add_metavar name =
    if Set_.mem name !bound_metavars then
      backref_counts := add_one name !backref_counts
    else
      bound_metavars := Set_.add name !bound_metavars
  in
  (*
     This is the list of actions to run in reverse order of the original
     traversal of statements.
  *)
  let stack = ref [] in
  let add_to_stack f = stack := f :: ! stack in

  let visitor = Visitor_AST.mk_visitor {
    Visitor_AST.default_visitor with
    kident = (fun (_k, _) (id, _tok) ->
      if debug then
        printf "kident %s\n" id;
      if Metavars_generic.is_metavar_name id then
        add_metavar id
    );

    kstmt = (fun (k, _) stmt ->
      let stmt_id = (stmt.s_id :> int) in
      if debug then
        printf "kstmt %i\n" stmt_id;

      (* compare the number of backreferences encountered before and after
         visiting the rest of the pattern, so as to determine the set
         of metavariables occurring in the rest of the pattern. *)
      let pre_bound_metavars = !bound_metavars in
      let pre_backref_counts = !backref_counts in
      add_to_stack (fun () ->
        let post_backref_counts = !backref_counts in
        let backrefs =
          diff_backrefs
            pre_bound_metavars
            ~new_backref_counts: post_backref_counts
            ~old_backref_counts: pre_backref_counts
        in
        (*
           It happens that the same stmt can be visited multiple times, to
           the following assertion doesn't hold:

             assert (stmt.s_backrefs = None);
        *)
        stmt.s_backrefs <- Some backrefs;

        if debug then (
          printf "stmt %i\n" stmt_id;
          printf "$A backrefs before %i, after %i\n"
            (get_count "$A" pre_backref_counts)
            (get_count "$A" post_backref_counts);
          printf "bound metavariables:\n%a" print_names pre_bound_metavars;
          printf "pre backref counts:\n%a"
            print_name_counts pre_backref_counts;
          printf "post backref counts:\n%a"
            print_name_counts post_backref_counts;
          printf "backrefs:\n%a" print_names backrefs;
          printf "\n"
        )
      );
      (* continue scanning the current subtree. *)
      k stmt
    );
  } in
  visitor any;
  List.iter (fun f -> f ()) !stack;
  if debug then
    printf "pattern AST:\n%s\n" (AST_generic.show_any any)

module Cache_key = struct
  type env = Metavars_generic.metavars_binding [@@deriving show]

  (*
     key = (min_env, pattern_node_id, target_node_id)
     min_env: captured values referenced by metavariables in the rest of the
              pattern
  *)
  type t = env * Node_ID.t * Node_ID.t [@@deriving show]

  let equal : t -> t -> bool =
    fun ((env1, pat_id1, target_id1) as key1)
      ((env2, pat_id2, target_id2) as key2) ->
      let res =
        pat_id1 = pat_id2
        && target_id1 = target_id2
        && Metavars_generic.Referential.equal_metavars_binding env1 env2
      in
      if debug then
        printf "equal %s %s = %B\n"
          (show key1) (show key2) res;
      res

  let hash_combine a b =
    a + 97 * b

  let hash ((env, pat_id, target_id) as key : t) =
    let res =
      hash_combine (pat_id :> int)
        (hash_combine (target_id :> int)
           (Metavars_generic.Referential.hash_metavars_binding env))
    in
    if debug then
      printf "hash %s = %i\n" (show key) res;
    res
end

module Cache = struct
  module Tbl = Hashtbl.Make (Cache_key)
  type 'a t = 'a Tbl.t

  type pattern = AST_generic.stmt list
  type target = AST_generic.stmt list

  let create () = Tbl.create 1000

  (* debugging *)
  let cache_hits = ref 0
  let cache_misses = ref 0

  let match_stmt_list
      get_env cache compute
      (pattern : pattern) (target : target) full_env =
    match pattern, target with
    | [], _ | _, [] ->
        compute pattern target full_env
    | a :: _, b :: _ ->
        let env : Metavars_generic.Env.t = get_env full_env in
        let key = (env.min_env, a.s_id, b.s_id) in
        if debug then
          printf "match_stmt_list\n";
        match Tbl.find_opt cache key with
        | Some res ->
            incr cache_hits;
            if debug then
              printf "found cached result!\n";
            res
        | None ->
            incr cache_misses;
            let res = compute pattern target full_env in
            Tbl.replace cache key res;
            res

  let print_stats () =
    printf "cache hits: %i, cache misses: %i\n"
      !cache_hits !cache_misses
end
