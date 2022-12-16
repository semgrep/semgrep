(* Yoann Padioleau
 *
 * Copyright (C) 2012, 2014 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
*)
open Common

module E = Entity_code
module G = Graph_code
module G2 = Graph_code_opti

open Dependencies_matrix_code
module DM = Dependencies_matrix_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Module to create a Dependency Structure Matrix (DSM) based on
 * a code graph.
 * See http://en.wikipedia.org/wiki/Design_structure_matrix
 * See also main_codegraph.ml
 *
 * history:
 *  - naive version
 *  - projection cache, memoize the projection of a node: given a deep node,
 *    what is the node present in the matrix that "represents" this deep node
 *  - full matrix pre-computation optimisation
 *  - compute lazily deep rows using only a subset of the edges
 *  - graph code opti, because using arrays is far more efficient than
 *    hashtbl and/or memoized hashtbl (hmmm)
 *  - remove full matrix, not anymore needed
 *  - better layout algorithm, minimize more backward dependencies
 *  - packing in "..." intermediate directories
 *  - TODO even better layout algorithm, hill climbing
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* Phantom types for safer array access between the graph_opti, dm *)
type 'a idx = int

type idm
type igopti
(* old: type ifull, but full matrix concept is not needed anymore *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let hashtbl_find_node h n =
  try Hashtbl.find h n
  with Not_found ->
    (* pr2 (spf "PB: %s" (G.string_of_node n));*)
    (* raise Not_found *)
    failwith (spf "Not_found: %s" (G.string_of_node n))

let hashtbl_find h n =
  try Hashtbl.find h n
  with Not_found ->
    pr2_gen ("PB:", n);
    raise Not_found

(*****************************************************************************)
(* Building a matrix (given a specific config) *)
(*****************************************************************************)

let build_with_tree2 tree gopti =

  (* todo? when we expand do we create a line for the expanded? if no
   * then it will have no projection so the test below is not enough.
   * but may make sense to create a line for it which corresponds to
   * the difference with the children so for all edges that link
   * directly to this one?
   *
  *)
  let nodes = final_nodes_of_tree tree in
  let n = List.length nodes in
  let n_nodes = G2.nb_nodes gopti in

  let name_to_idm = Hashtbl.create (n / 2) in
  let idm_to_name = Array.make n ("", E.Dir) in
  let igopti_to_idm = Array.make n_nodes (-1) in

  let (i: idm idx ref) = ref 0 in
  nodes |> List.iter (fun node ->
    Hashtbl.add name_to_idm node !i;
    idm_to_name.(!i) <- node;
    igopti_to_idm.(hashtbl_find_node gopti.G2.name_to_i node) <- !i;
    incr i;
  );

  let dm = {
    matrix = Common2.make_matrix_init ~nrow:n ~ncolumn:n (fun _i _j -> 0);
    name_to_i = name_to_idm;
    i_to_name = idm_to_name;
    config = tree;
  }
  in
  let (projected_parent_of_igopti: idm idx array) = Array.make n_nodes (-1) in
  let (iroot: igopti idx) = hashtbl_find_node gopti.G2.name_to_i G.root in
  let rec depth parent igopti =
    let children = gopti.G2.has_children.(igopti) in
    let idm = igopti_to_idm.(igopti) in
    let project =
      if idm = -1
      then parent
      else idm
    in
    projected_parent_of_igopti.(igopti) <- project;
    children |> List.iter (depth project);
  in
  depth (-1) iroot;

  gopti.G2.use |> Array.iteri (fun i xs ->
    let parent_i = projected_parent_of_igopti.(i) in
    xs |> List.iter (fun j ->
      let parent_j = projected_parent_of_igopti.(j) in
      (* It's possible we operate on a slice of the original dsm,
       * for instance when we focus on a node, in which case
       * the projection of an edge can not project on anything
       * in the current matrix.
      *)
      if parent_i <> -1 && parent_j <> -1
      then
        dm.matrix.(parent_i).(parent_j) <-
          dm.matrix.(parent_i).(parent_j) + 1
    )
  );
  dm

let build_with_tree a b =
  Common.profile_code "DM.build_with_tree" (fun () -> build_with_tree2 a b)


(*****************************************************************************)
(* Ordering the rows/columns helpers *)
(*****************************************************************************)

let formula x =
  assert(x > 0);
  (* 1 + (int_of_float (log10 (float_of_int x))) *)
  x

let count_column j m =
  let n = Array.length m in
  let cnt = ref 0 in
  for i = 0 to n - 1 do
    if m.(i).(j) > 0 && i <> j
    then cnt := !cnt + formula (m.(i).(j))
  done;
  !cnt

let is_empty_column n m dm =
  count_column (hashtbl_find_node dm.name_to_i n) m = 0

let count_row i m =
  let n = Array.length m in
  let cnt = ref 0 in
  for j = 0 to n - 1 do
    if m.(i).(j) > 0 && i <> j
    then cnt := !cnt + formula (m.(i).(j))
  done;
  !cnt

let is_empty_row n m dm =
  count_row (hashtbl_find_node dm.name_to_i n) m = 0

let empty_all_cells_relevant_to_node m dm n =
  let i = hashtbl_find_node dm.name_to_i n in
  let n = Array.length m in
  for x = 0 to n - 1 do
    m.(i).(x) <- 0;
    m.(x).(i) <- 0;
  done

(*****************************************************************************)
(* Hill climbing! *)
(*****************************************************************************)

let reduced_matrix nodes dm =
  let n = List.length nodes in
  let m = Common2.make_matrix_init ~nrow:n ~ncolumn:n (fun _i _j -> 0) in

  let a = Array.of_list nodes in

  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let ni = a.(i) in
      let nj = a.(j) in
      let xi = hashtbl_find_node dm.name_to_i ni in
      let xj = hashtbl_find_node dm.name_to_i nj in
      if i <> j then begin
        m.(i).(j) <- dm.matrix.(xi).(xj);
      end
    done
  done;
  a, m

let score_upper_triangle m dm =
  DM.score_upper_triangle { dm with matrix = m } []

(* less: there has to be a more efficient way ... *)
let switch k1 k2 (a,m) =
  let a' = Array.copy a in
  let m' = Array.map Array.copy m in
  let n = Array.length a in

  let f idx =
    match () with
    | _ when idx = k1 -> k2
    | _ when idx = k2 -> k1
    | _ -> idx
  in
  for i = 0 to n - 1 do
    a'.(i) <- a.(f i)
  done;

  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      m'.(i).(j) <- m.(f i).(f j)
    done
  done;
  a', m'


(* less: simulated anneahiling? *)
let hill_climbing nodes dm =
  let a, m = reduced_matrix nodes dm in
  let n = Array.length a in
  let current_score = score_upper_triangle m dm in
  pr2 (spf "current score = %d" current_score);

  let rec aux (a, m) current_score i ~jump =
    let j = i + jump in
    if j >= n
    then
      if jump = (Array.length m - 1)
      then (a, m)
      else aux (a, m) current_score 0 ~jump:(jump + 1)
    else
      let (a1,m1) = switch i j (a,m) in
      let new_score = score_upper_triangle m1 dm in
      if new_score < current_score
      then begin
        pr2 (spf " %s <-> %s, before = %d, after = %d (jmp=%d)"
               (G.string_of_node a.(i))
               (G.string_of_node a.(j))
               current_score new_score
               jump);
        aux (a1, m1) new_score 0 ~jump:1
      end
      else aux (a, m) current_score (i+1) ~jump
  in
  let (a, _m) = aux (a, m) current_score 0 ~jump:1 in
  Array.to_list a

(*****************************************************************************)
(* Ordering the rows/columns heuristics *)
(*****************************************************************************)

let sort_by_count_rows_low_first xs m dm =
  xs |> List.map (fun n -> n, count_row (hashtbl_find_node dm.name_to_i n) m)
  |> Common.sort_by_val_lowfirst
  |> List.map fst

(*
let sort_by_count_columns_high_first xs m dm =
  xs +> List.map (fun n -> n, count_column (hashtbl_find_node dm.name_to_i n) m)
     +> Common.sort_by_val_highfirst
     +> List.map fst
*)

(* todo: alternatives while discussing with matthieu
 * - find the first row, which should be the lines with the smallest
 *   sum (as it will be part of the big sum of the upper triangular),
 *   remove then this line, and iterate
 * - find the first row by doing the sum of (cell line / sum cell column)
 *   which is a bit equivalent to normalize, to do sum of percentage.
*)
let sort_by_count_rows_low_columns_high_first xs m dm =
  xs |> List.map (fun n ->
    let idx = hashtbl_find_node dm.name_to_i n in
    let h =
      float_of_int (count_row idx m)
      /.
      (1. +. float_of_int (count_column idx m))
    in
    n, h
  ) |> Common.sort_by_val_lowfirst
  |> List.map fst

(*
 * See http://dsmweb.org/dsmweb/en/understand-dsm/technical-dsm-tutorial/partitioning.html
 *
 * note: sorting by the number of cells which you depend on is not
 * enough. For instance let's say X depends on 3 cells, A, B, Y and Y
 * depends on 4: A, B, C, D. One could consider to put X
 * upper in the matrix, but because X depends on Y, it's better to put
 * Y upper.
 *
 * todo: optimize? we redo some computations ... should memoize
 * more? or just invert rows/columns in the original matrix?
 *)
let partition_matrix nodes dm =

  let m = dm.matrix |> Array.map Array.copy in
  let n = Array.length m in
  for i = 0 to n - 1 do
    m.(i).(i) <- 0
  done;

  let left = ref [] in
  let right = ref [] in

  let rec step1 nodes =
    (* "1. Identify system elements (or tasks) that can be determined (or
     * executed) without input from the rest of the elements in the matrix.
     * Those elements can easily be identified by observing an empty column
     * in the DSM. Place those elements to the left of the DSM. Once an
     * element is rearranged, it is removed from the DSM (with all its
     * corresponding marks) and step 1 is repeated on the remaining
     * elements."
    *)
    let elts_with_empty_columns, rest =
      nodes |> List.partition (fun node -> is_empty_column node m dm) in
    let xs =
      sort_by_count_rows_low_first elts_with_empty_columns dm.matrix dm in
    xs |> List.iter (empty_all_cells_relevant_to_node m dm);
    right := xs @ !right;
    (* pr2 (spf "step1: %s" (Common2.dump xs)); *)
    if null xs
    then rest
    else step1 rest

  and step2 nodes =
    (* "2.Identify system elements (or tasks) that deliver no
     * information to other elements in the matrix. Those elements can
     * easily be identified by observing an empty row in the DSM. Place
     * those elements to the right of the DSM. Once an element is
     * rearranged, it is removed from the DSM (with all its corresponding
     * marks) and step 2 is repeated on the remaining elements."
    *)
    let elts_with_empty_lines, rest =
      nodes |> List.partition (fun node -> is_empty_row node m dm) in
    (* I use dm.matrix here and not the current matrix m because I want
     * to sort by looking globally at whether this item uses very few things.
     * todo: maybe don't need this 'm dm' to pass around to all those sort_xxx
     * functions. Just pass dm.
    *)
    let xs = sort_by_count_rows_low_first elts_with_empty_lines dm.matrix dm in
    xs|> List.iter (empty_all_cells_relevant_to_node m dm);
    (* pr2 (spf "step2: %s" (Common2.dump xs)); *)
    left := !left @ xs;
    if null xs
    then step1 rest
    else step2 rest
  in

  let rest = step2 nodes in
  if null rest
  then !left @ !right
  else begin
(*
    pr2 "CYCLE";
    pr2_gen rest;
*)
    let rest = sort_by_count_rows_low_columns_high_first rest m dm in
    let rest = hill_climbing rest dm in
    !left @ rest @ !right
  end

(* to debug the heuristics *)
let info_orders dm =
  dm.matrix |> Array.mapi  (fun i _ ->
    let nrow = (count_row i dm.matrix) in
    let ncol = (count_column i dm.matrix) in
    let h = float_of_int nrow /. (1. +. float_of_int ncol) in
    h,
    (spf "%-20s: count lines = %d, count columns = %d, H = %.2f"
       (fst (dm.i_to_name.(i)))
       nrow
       ncol
       h)
  ) |> Array.to_list
  |> Common.sort_by_key_lowfirst
  |> List.iter (fun (_, s) ->
    pr2 s
  )

(*****************************************************************************)
(* Manual ordering *)
(*****************************************************************************)

let optional_manual_reordering (s, _node_kind) nodes constraints_opt =
  match constraints_opt with
  | None -> nodes
  | Some h ->
      if Hashtbl.mem h s
      then begin
        let xs = hashtbl_find h s in
        let horder = xs |> Common.index_list_1 |> Common.hash_of_list in
        let current = ref 0 in
        let nodes_with_order =
          nodes |> List.map (fun (s, node_kind) ->
            match Common2.hfind_option s horder with
            | None ->
                pr2 (spf "INFO_TXT: could not find %s in constraint set" s);
                (s, node_kind), !current
            | Some n ->
                current := n;
                (s, node_kind), n
          )
        in
        Common.sort_by_val_lowfirst nodes_with_order |> List.map fst
      end
      else begin
        pr2 (spf "didn't find entry in constraints for %s" s);
        nodes
      end

(*****************************************************************************)
(* Create fake "a/b/..." directories *)
(*****************************************************************************)

(* design decisions, when should we pack?
 *  - in an adhoc manner in adjust_graph.txt
 *  - in the graph lazily while building the final config
 *  - in the graph lazily as a preprocessing phase on a full config
 *  - in an offline phase that packs everything?
 *  - in the UI?
 *
 * Packing lazily is good but it does not necessaraly work well with
 * the Focus because depending on our focus, we may have want different
 * packings. Also it makes it a bit hard to use cg from the command line
 * in a subdirectory. A solution could be to restart from a fresh gopti for
 * for each new focus.
 *
 * Packing in the UI would be more flexible for the Focus,
 * but we need lots of extra logic whereas just abusing the Has and
 * reorganize the graph makes things (at first) easier.
 *
 * There are some issues also on how packing and layering impact each other.
 * We may not want to pack things that affect a lot the number of
 * backward references once packed.
*)

let threshold_pack = ref 30


(* Optionaly put less "relevant" entries under an extra "..." fake dir.
 *
 * We used to do this phase in build() at the same time we were building
 * the new config. But doing too many things at the same time was complicated
 * so better to separate a bit things in a preprocessing phase
 * where here we just adjust gopti.
 *
 * Moreover we wanted the heuristics to order and to pack to use different
 * schemes. For packing we want to put under "..." entries considered
 * irrelevant when looked globally, that is not look only at the
 * column count in the submatrix but in the whole matrix.
 * For instance if a/b/c/d is used a lot from e/, but not used
 * that much internally inside a/b/c, we still don't want
 * to put it under a extra "..." because this directory is globally
 * very important.
 *
 * Moreover when in Focused mode, the children of a node
 * are actually not the full set of children, and so we could pack
 * things under a "..." that are incomplete? Hmmm at the same time
 * it can be good to do some specialized packing based on a Focus.
*)
let adjust_gopti_if_needed_lazily tree gopti =
  let gopti = ref gopti in

  let rec aux (tree: tree) (brothers: Graph_code.node list) =
    match tree with
    | Node (n, xs) ->
        if null xs
        then Node (n, [])
        else
          (* less: use the full list of children of n? xs can be a subset
           * because in a focused generated config
          *)
        if List.length xs <= !threshold_pack
        then
          Node (n, xs |> List.map (fun (Node (n1, xs1)) ->
            let more_brothers =
              xs |> Common.map_filter (fun (Node (n2, _)) ->
                if n1 <> n2 then Some n2 else None
              )
            in
            aux (Node (n1, xs1)) (brothers @ more_brothers)
          ))
        else begin
          let children_nodes = xs |> List.map (fun (Node (n,_)) -> n) in
          let config = (Node (n,
                              (xs |> List.map (fun (Node (n, _)) -> Node (n, []))) @
                              (brothers |> List.map (fun n -> Node (n, [])))))
          in
          let dm = build_with_tree config !gopti in

          let score = children_nodes |> List.map (fun n ->
            let idx = hashtbl_find_node dm.name_to_i n in
            let m = dm.matrix in
            n, count_column idx m + count_row idx m
            (* + m.(idx).(idx) / 3 *)
          ) |> Common.sort_by_val_highfirst
                      |> List.map fst
          in
          (* minus one because after the packing we will have
           * threshold_pack - 1 + the new entry = threshold_pack
           * and so we will not loop again and again.
          *)
          let (ok, to_pack) = Common2.splitAt (!threshold_pack - 1) score in
          (* pr2 (spf "REPACKING: TO_PACK = %s, TO_KEEP = %s"
                 (Common.dump to_pack) (Common.dump ok)); *)
          let new_gopti, dotdotdot_entry =
            Graph_code_opti.adjust_graph_pack_some_children_under_dotdotdot
              n to_pack !gopti in
          gopti := new_gopti;
          Node (n,
                (ok @ [dotdotdot_entry]) |> List.map (fun n ->
                  (* todo: grab the children of n in the original config? *)
                  Node (n, [])
                )
               )
        end
  in
  let adjusted_tree = aux tree [] in
  !gopti, adjusted_tree


(*****************************************************************************)
(* Building the matrix *)
(*****************************************************************************)

(* The tree passed is a configuration one would like to explore. Note
 * that after a focus, the children of a node in this tree may not contain
 * all the original children of this node.
*)
let build tree constraints_opt gopti =

  let gopti, tree = adjust_gopti_if_needed_lazily tree gopti in

  (* let's compute a better reordered tree *)
  let rec aux tree =
    match tree with
    | Node (n, xs) ->
        if null xs
        then Node (n, [])
        else begin
          let config_depth1 =
            Node (n,xs |> List.map (function (Node (n2,_)) -> (Node (n2, []))))
          in
          let children_nodes =
            xs |> List.map (function (Node (n2, _)) -> n2) in
          let h_children_of_children_nodes =
            xs |> List.map (function (Node (n2, xs)) -> n2, xs) |>
            Common.hash_of_list
          in

          (* first draft *)
          let dm = build_with_tree config_depth1 gopti in

          (* Now we need to reorder to minimize the number of dependencies in
           * the top right corner of the matrix (of the submatrix actually)
          *)
          let nodes_reordered =
            partition_matrix children_nodes dm in
          let nodes_reordered =
            optional_manual_reordering n nodes_reordered constraints_opt in

          Node (n,
                nodes_reordered |> List.map (fun n2 ->
                  let xs =
                    try Hashtbl.find h_children_of_children_nodes n2
                    (* probably one of the newly created "..." child *)
                    with Not_found -> []
                  in
                  (* recurse *)
                  aux (Node (n2, xs))
                ))
        end
  in
  let ordered_config = aux tree in
  build_with_tree ordered_config gopti, gopti

(*****************************************************************************)
(* Path manipulation *)
(*****************************************************************************)

(* less: could be put in dependencies_matrix_code.ml *)
let put_expand_just_before_last_focus_if_not_children n xs g =
  let rec aux xs =
    match xs with
    | [] -> [Expand n]
    | x::xs ->
        (match x with
         | Expand _ -> x::aux xs
         | Focus (n2, _style) ->
             let children = Graph_code_opti.all_children n2 g in
             if not (List.mem n children)
             then (Expand n)::x::xs
             else x::aux xs
        )
  in
  aux xs

let fix_path path g =
  let rec aux acc xs =
    match xs with
    | [] -> acc
    | x::xs ->
        (match x with
         | Focus _ ->
             aux (acc @ [x]) xs
         | Expand n ->
             aux (put_expand_just_before_last_focus_if_not_children n acc g) xs
        )
  in
  aux [] path


let config_of_path (path: config_path) gopti =
  let path = fix_path path gopti in
  let initial_config = basic_config_opti gopti in
  (* pr2_gen path; *)
  path |> List.fold_left (fun (config, gopti) e ->
    match e with
    | Expand node ->
        expand_node_opti node config gopti, gopti
    | Focus (node, kind) ->
        let dm, gopti = build config None gopti in
        focus_on_node node kind config dm, gopti
  ) (initial_config, gopti)
