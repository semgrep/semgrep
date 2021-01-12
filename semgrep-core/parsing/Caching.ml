(*
   Decorate a pattern and target ASTs to make the suitable for memoization
   during matching.
*)

open Printf
open AST_generic

(* A set of metavariables. Access cost is O(log n). *)
module Names = AST_generic.String_set

(* Count the number of occurrences of each backreference of a metavariable. *)
module Name_counts = Map.Make (String)

let print_names oc names =
  List.iter (fun s -> fprintf oc "  %s\n" s) (Names.elements names)

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
    Names.fold (fun k acc ->
      let added_backref_count =
        diff_count k
          ~new_counts:new_backref_counts
          ~old_counts:old_backref_counts
      in
      match added_backref_count with
      | 0 -> Names.add k acc
      | _ -> acc
    ) bound_metavars Names.empty
  in
  Names.diff bound_metavars not_backrefs_in_rest_of_pattern

let create_create_id () =
  let n = ref 0 in
  fun () ->
    let id = !n in
    assert (id >= 0);
    incr n;
    id

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
let prepare_pattern ?(debug = false) any =
  let bound_metavars = ref Names.empty in
  let backref_counts = ref Name_counts.empty in
  let add_metavar name =
    if Names.mem name !bound_metavars then
      backref_counts := add_one name !backref_counts
    else
      bound_metavars := Names.add name !bound_metavars
  in
  let create_id = create_create_id () in
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
      (* assign a node ID *)
      let stmt_id = create_id () in
      assert (stmt.s_id < 0);
      stmt.s_id <- stmt_id;
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
        assert (stmt.s_backrefs = None);
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

(* Assign node IDs to the statements in the target. *)
let prepare_target stmt_list =
  let create_id = create_create_id () in
  let visitor = Visitor_AST.mk_visitor {
    Visitor_AST.default_visitor with
    kstmt = (fun (k, _) stmt ->
      assert (stmt.s_id < 0);
      stmt.s_id <- create_id ();
      k stmt
    );
  } in
  visitor (Ss stmt_list)
