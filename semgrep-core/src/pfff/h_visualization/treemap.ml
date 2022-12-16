(*s: treemap.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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
(*e: Facebook copyright *)
open Common2

open Figures

module Color = Simple_color

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type ('dir, 'file) tree = ('dir, 'file) Common2.tree

(*s: type treemap *)
type ('dir, 'file) treemap =
  (treemap_data * 'dir, treemap_data * 'file) tree
and treemap_data = {
  size : int;
  color : Simple_color.color;
  label: string;
}
(*e: type treemap *)
(* with tarzan *)

(*s: type algorithm *)
type algorithm =
  | Classic
  | Squarified
  | SquarifiedNoSort
  | Ordered of pivot

and pivot =
  | PivotBySize
  | PivotByMiddle
  (*e: type algorithm *)

(*s: variable algos *)
let algos = [Classic; Squarified; SquarifiedNoSort;
             Ordered PivotBySize; Ordered PivotByMiddle]
(*e: variable algos *)

(*s: type screen_dim *)
type screen_dim = {
  (* total width/height *)
  w: int;
  h: int;
  (* the viewport *)
  w_view: int;
  h_view: int;
  (* extra information *)
  h_status: int;
  w_legend: int;
}
(*e: type screen_dim *)


(*s: type rectangle1 *)
(* The array has 2 elements, for x, y. I use an array because that's how
 * the seminal algorithm on treemap was written. It allows to pass
 * as an int the current split and do x.(axis_split) and do a 1-axis_split
 * in recursive calls to go from a x-split to a y-split.
 *
 * A rectangle is represented by 2 variables called P and Q in the seminal
 * algorithm.
*)
(*
type rectangle1 =
  float array (* lower left  coord, P *) *
  float array (* upper right coord, Q *)
*)

(*e: type rectangle1 *)

(* A cleaner rectangle type, not tied to the seminal paper design decisions *)

(* Now that my treemap visualizer uses a minimap, it does not completely
 * use the full width.
 * 16/9 = 1.777777
 * 21/9 = 2.33
 * I use 2510x1580 for the full codemap window, so it could be 1.58, but
 * then there is a menu up and a status bar down so it should be
 * higher than 1.58.
*)
let xy_ratio = ref 1.71

(* The dimentions are in a  [0.0-1.0] range for y and [0.0-xyratio] for x,
 * where xyratio is used to cope with most 16/9 screens.
*)
let rect_ortho () =
  { p = {x = 0.0; y = 0.0; }; q = { x = !xy_ratio; y = 1.0} }

(* the dimentions are in a  [0.0-1.0] range
 * opti? have a quad tree instead of a list, can improve search time
*)
type treemap_rendering = treemap_rectangle list
and treemap_rectangle = {
  tr_rect: rectangle;
  tr_color: int (* Simple_color.color *);
  tr_label: string;
  tr_depth: int;
  tr_is_node: bool;
}
(* with tarzan *)

(*s: type layout_func *)
type ('a, 'b) layout_func =
  (float * ('a, 'b) treemap) list ->
  int ->
  rectangle ->
  (float * ('a, 'b) treemap * rectangle) list
(*e: type layout_func *)


(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

(*s: function treemap accessors *)
let color_of_treemap_node x =
  match x with
  | Node (({color = c; _}, _), _) -> c
  | Leaf ({color = c; _}, _) -> c

let size_of_treemap_node x =
  match x with
  | Node (({size = s; _}, _), _) -> s
  | Leaf ({size = s; _}, _) -> s
(*e: function treemap accessors *)

(*s: function algorithm accessors *)
let algo_of_s algo =
  match algo with
  | "classic" -> Classic
  | "squarified" -> Squarified
  | "squarified_no_sort" -> SquarifiedNoSort
  | "ordered" -> Ordered PivotBySize
  | "ordered_by_size" -> Ordered PivotBySize
  | "ordered_by_middle" -> Ordered PivotByMiddle


  | "default" -> Ordered PivotByMiddle
  | _ -> failwith "not a valid algorithm"

let s_of_algo algo =
  match algo with
  | Classic -> "classic"
  | Squarified -> "squarified"
  | SquarifiedNoSort -> "squarified_no_sort"
  | Ordered PivotBySize -> "ordered_by_size"
  | Ordered PivotByMiddle -> "ordered_by_middle"
(*e: function algorithm accessors *)

(*****************************************************************************)
(* Treemap Helpers *)
(*****************************************************************************)

(*s: function treemap_of_tree *)
let treemap_of_tree2
    ~size_of_leaf
    ~color_of_leaf
    ?(label_of_file=(fun _ -> ""))
    ?(label_of_dir=(fun _ -> ""))
    tree =
  let rec aux tree =
    match tree with
    | Node (nodeinfo, xs) ->
        let sizeme = ref 0 in

        let child = List.map (fun x ->
          let (res, size) = aux x in
          sizeme := !sizeme + size;
          res
        ) xs
        in
        (* old:
         * let children = xs +> List.map aux in
         * let child = children +> List.map fst in
         * let sizes = children +> List.map snd in
         * let sizeme = Common.sum sizes in
        *)
        let sizeme = !sizeme in
        Node((
          {
            size = sizeme;
            color = Color.black; (* TODO ? nodes have colors ? *)
            label = label_of_dir nodeinfo;
          }, nodeinfo),
          child), sizeme
    | Leaf leaf ->
        let sizeme = size_of_leaf leaf in
        let nodeinfo = leaf in
        Leaf((
          {
            size = sizeme;
            color = color_of_leaf leaf;
            label = label_of_file leaf;
          }, nodeinfo)
        ), sizeme
  in
  let (tree, _size) = aux tree in
  tree
(*e: function treemap_of_tree *)

let treemap_of_tree ~size_of_leaf  ~color_of_leaf
    ?label_of_file ?label_of_dir tree =
  Common.profile_code "Treemap.treemap_of_tree" (fun () ->
    treemap_of_tree2 ~size_of_leaf  ~color_of_leaf
      ?label_of_file ?label_of_dir tree)

(*****************************************************************************)
(* Treemap algorithms *)
(*****************************************************************************)

(*---------------------------------------------------------------------------*)
(* basic algorithm *)
(*---------------------------------------------------------------------------*)

(* display_treemap and display_treemap_generic are now in
 * in treemap_graphics.ml, because of Graphics dependency.
*)

(*---------------------------------------------------------------------------*)
(* slice and dice algorithm layout *)
(*---------------------------------------------------------------------------*)

(*s: layout slice and dice *)
let (slice_and_dicing_layout: ('a, 'b) layout_func) =
  fun children depth rect ->

  let p = [| rect.p.x; rect.p.y |] in
  let q = [| rect.q.x; rect.q.y |] in

  let axis_split = (depth + 1) mod 2 in

  let stotal = children |> List.map fst |> Common2.sum_float in

  let width = q.(axis_split) -. p.(axis_split) in

  children |> List.map (fun (size, child) ->

    q.(axis_split) <-
      p.(axis_split) +.
      ((size) /. stotal) *. width;

    let rect_here = {
      p = {  x = p.(0); y = p.(1); };
      q = {  x = q.(0); y = q.(1); }
    }
    in
    p.(axis_split) <- q.(axis_split);
    size, child, rect_here
  )
(*e: layout slice and dice *)

(*---------------------------------------------------------------------------*)
(* squarified algorithm *)
(*---------------------------------------------------------------------------*)

(*s: squarified examples *)
(* ref: www.win.tue.nl/~vanwijk/stm.pdf
 *
 * In the following I use some of the examples in the paper so you'll need
 * the paper to follow what I say.
*)


(*
 * A few examples.
 *
 * the total sum in squarified_list_area_ex is 24, just like the area
 * of rect_orig below. This simplifies discussions.
 *
 * I've added the string later as we want squarify to also return
 * information related to the node with its size (that is the full treemap
 * node, with its descendant)
*)
let squarified_list_area_ex =
  [6; 6; 4; 3; 2; 2; 1] |> List.map (fun x -> float_of_int x, spf "info: %d" x)

(* normally our algorithm should do things proportionnally to the size
 * of the aready. It should not matter that the total sum of area is
 * equal to the size of the rectangle. Indeed later we will always do
 * things in an ortho plan, that is with a rectangle 0x0 to 1x1.
*)
let squarified_list_area_ex2 =
  squarified_list_area_ex |> List.map (fun (x, info) -> x *. 2.0, info)
let dim_rect_orig =
  { p = {x = 0.0; y = 0.0; }; q = { x = 6.0; y = 4.0} }
(*e: squarified examples *)

(*s: type split *)
type split =
  (* Spread one next to the other, e.g. | | | | | |
   * The split lines will be vertical, but the rectangles
   * would be spreaded horizontally. In the paper they call that horizontal
   * Split but I prefer Spread, because the split lines are actually verticals.
  *)
  | SpreadHorizontally

  (* Spread one on top of the other eg _
   *                                   _
   *                                   _
  *)
  | SpreadVertically
  (*e: type split *)

(*s: function ratio_rect_dim *)
(* we want the ratio to be a close to 1 as possible (that is to be a square) *)
let ratio_rect_dim (w,h) =
  let res = max (w /. h) (h /. w) in
  (* assert (res >= 1.0); *)
  res

let _ = assert (ratio_rect_dim (6.0, 4.0) = 1.5)
let _ = assert (ratio_rect_dim (4.0, 6.0) = 1.5)
(*e: function ratio_rect_dim *)

(*s: function worst *)
(* On the running example, at the first step we want to add the rect of
 * size 6 on the left, alone, and its aspect ratio will be 8/3.
 * Indeed its height is fixed (4) and so his width is
 * whatever that must lead to an area of 6, that is 6/4 (1.5)
 * which leads then to an aspect ratio of 4 vs 1.5 = 4 / 1.5 = 8/3.
 * If we add 2 rect of size 6, then their aspect ratio is 1.5 which is
 * better
*)

let worst elems_in_row  size_side_row =
  let s = Common2.sum_float elems_in_row in
  let rplus = Common2.maximum elems_in_row in
  let rminus = Common2.minimum elems_in_row in

  (* cf formula in paper *)
  max ((Common2.square size_side_row *. rplus) /. Common2.square s)
    (Common2.square s /.  (Common2.square size_side_row *. rminus))

let _ = assert
  (worst [6.0] 4.0 = 8.0 /. 3.0) (* 2.66667 *)
let _ = assert
  (worst [6.0;6.0] 4.0 = 3.0 /. 2.0) (* 1.5, which is close to 1 so better *)
let _ = assert
  (worst [6.0;6.0;4.0] 4.0 = 4.0) (* 4.0, we regress *)
(*e: function worst *)

(*s: function layout *)
(* We are given a fixed row which contains a set of elems that we have
 * to spread unoformly, just like in the original algorithm.
*)
let layout row rect =

  let p = [| rect.p.x; rect.p.y |] in
  let q = [| rect.q.x; rect.q.y |] in

  let children = row in

  let stotal = children |> List.map fst |> Common2.sum_float in
  let children = children |> List.map (fun (size, info) ->
    size /. stotal (* percentage *),
    size,
    info
  )
  in

  let res = ref [] in
  let spread =
    if rect_width rect >= rect_height rect
    then SpreadHorizontally
    else SpreadVertically
  in
  let axis_split =
    match spread with
    | SpreadHorizontally -> 0
    | SpreadVertically -> 1
  in
  let width = q.(axis_split) -. p.(axis_split) in

  children |> List.iter (fun (percent_child, size_child, info) ->

    q.(axis_split) <-
      p.(axis_split) +.
      percent_child *. width;
    let rect_here = {
      p = {  x = p.(0); y = p.(1); };
      q = {  x = q.(0); y = q.(1); }
    }
    in
    Common.push (size_child, info, rect_here) res;
    p.(axis_split) <- q.(axis_split);
  );
  !res
(*e: function layout *)

(* the main algorithmic part of squarifying *)
(*s: function squarify_orig *)
let rec (squarify_orig:
           ?verbose:bool ->
         (float * 'a) list -> (float * 'a) list -> rectangle ->
         (float * 'a * rectangle) list
        ) =
  fun ?(verbose=false) children current_row rect ->
  (* does not work well because of float approximation.
   * assert(Common.sum_float (children ++ current_row) = rect_area rect);
  *)
  let (p, q) = rect.p, rect.q in

  let floats xs = List.map fst xs in

  (* First heuristic in the squarified paper *)
  let spread =
    if rect_width rect >= rect_height rect (* e.g. 6 x 4 rectangle *)
    then SpreadHorizontally
    else SpreadVertically
  in

  (* We now know what kind of row we want. If spread horizontally then
   * we will have a row on the left to fill and the size of the side of
   * this row is known and is the height of the rectangle (in our ex 4).
   * In the paper they call this variable 'width' but it's misleading.
   * Note that because we are in Horizontal mode, inside this left row,
   * things will be spreaded this time vertically.
  *)
  let size_side_row =
    match spread with
    | SpreadHorizontally -> rect_height rect
    | SpreadVertically -> rect_width rect
  in
  match children with
  | c::cs ->
      if null current_row ||
         (worst (floats (current_row @ [c])) size_side_row)
         <=
         (worst (floats current_row)         size_side_row)
      then
        (* not yet optimal row, let's recurse *)
        squarify_orig cs (current_row @ [c]) rect
      else begin
        (* optimal layout for the left row. We can fix it. *)
        let srow = Common2.sum_float (floats current_row) in
        let stotal = Common2.sum_float (floats (current_row @ children)) in
        let portion_for_row = srow /. stotal in

        let row_rect, remaining_rect =
          match spread with
          | SpreadHorizontally ->
              let middle_x =
                (q.x -. p.x) *. portion_for_row
                +. p.x
              in
              {
                p = p;
                q = { x = middle_x; y = q.y };
              },
              {
                p = { x = middle_x; y = p.y};
                q = q;
              }

          | SpreadVertically ->
              let middle_y =
                (q.y -. p.y) *. portion_for_row
                +. p.y in
              {
                p = p;
                q = { x = q.x; y = middle_y;};
              },
              {
                p = { x = p.x; y = middle_y};
                q = q;
              }


        in
        if verbose then begin
          pr2 "layoutrow:";
          pr2_gen current_row;
          pr2 "row rect";
          pr2 (s_of_rectangle row_rect);
        end;

        let rects_row = layout current_row row_rect in
        let rects_remain = squarify_orig children [] remaining_rect in
        rects_row @ rects_remain
      end
  | [] ->
      if verbose then begin
        pr2 "layoutrow:";
        pr2_gen current_row;
        pr2 "row rect";
        pr2 (s_of_rectangle rect);
      end;

      layout current_row rect
(*e: function squarify_orig *)

(*s: function squarify *)
let squarify children rect =
  (* squarify_orig assume the sum of children = area rect *)
  let area = rect_area rect in
  let total = Common2.sum_float (List.map fst children) in
  let children' = children |> List.map (fun (x, info) ->
    (x /. total) *. area,
    info
  )
  in
  squarify_orig children' [] rect
(*e: function squarify *)



(*s: function test_squarify *)
let test_squarify () =
  pr2_gen (worst [6.0] 4.0);
  pr2_gen (worst [6.0;6.0] 4.0);
  pr2_gen (worst [6.0;6.0;4.0] 4.0);
  pr2_xxxxxxxxxxxxxxxxx ();
  squarify squarified_list_area_ex dim_rect_orig |> ignore;
  pr2_xxxxxxxxxxxxxxxxx ();
  squarify squarified_list_area_ex2 (rect_ortho()) |> ignore;
  ()
(*e: function test_squarify *)


(*s: layout squarify *)
let (squarify_layout: ('a, 'b) layout_func) =
  fun children _depth rect ->
  let children' = children |> Common.sort_by_key_highfirst in
  squarify children' rect

let (squarify_layout_no_sort_size: ('a, 'b) layout_func) =
  fun children _depth rect ->
  squarify children rect
(*e: layout squarify *)


(*---------------------------------------------------------------------------*)
(* Ordered squarified algorithm *)
(*---------------------------------------------------------------------------*)

(*s: ordered examples *)
(* ref:
*)

let children_ex_ordered_2001 = [
  1; 5; 3; 4; 5; 1;
  10; 1; 1; 2; 7; 3;
  5; 2; 10; 1; 2; 1;
  1; 2;
]
(*e: ordered examples *)

(*s: type pivotized *)
type 'a pivotized = {
  left: 'a;
  right: 'a;
  pivot: 'a; (* this one should be singleton and the other a list *)
  above_pivot: 'a;
}
(*e: type pivotized *)

(*s: function compute_rects_pivotized *)
let compute_rects_pivotized childs_pivotized rect spread =
  let (p, q) = rect.p, rect.q in

  let x = childs_pivotized in
  let size = {
    left = Common2.sum_float (List.map fst x.left);
    right = Common2.sum_float (List.map fst x.right);
    pivot = Common2.sum_float (List.map fst x.pivot);
    above_pivot = Common2.sum_float (List.map fst x.above_pivot);
  }
  in

  let total_size = size.left +. size.right +. size.pivot +. size.above_pivot in

  let portion_for_left = size.left /. total_size in
  let portion_for_right = size.right /. total_size in

  let portion_for_pivot_vs_above =
    (size.pivot ) /. (size.pivot +. size.above_pivot)
  in

  (* computing the rectangle of the left and right is easy as the
   * height is fixed (when we spread horizontally)
  *)
  match spread with
  | SpreadHorizontally ->
      (* TODO do something that adapt to rect ? lourd que rect
       * commence pas 0,0, ca fait faire des calculs en plus. *)
      let middle_x1 =
        p.x +. ((rect_width rect) *. portion_for_left)
      in
      let middle_x2 =
        q.x -.  ((rect_width rect) *. portion_for_right)
      in
      let middle_y =
        p.y +. ((rect_height rect) *. portion_for_pivot_vs_above)
      in
      { left = {
          p = p;
          q = { x = middle_x1; y = q.y } };
        right = {
          p = { x = middle_x2; y = p.y };
          q = q; };
        pivot = {
          p = { x = middle_x1; y = p.y};
          q = { x = middle_x2; y = middle_y}; };
        above_pivot = {
          p = { x = middle_x1; y = middle_y };
          q = { x = middle_x2; y = q.y; } };
      }

  | SpreadVertically ->
      (* just the reverse of previous code, x become y and vice versa *)
      let middle_y1 =
        p.y +. ((rect_height rect) *. portion_for_left)
      in
      let middle_y2 =
        q.y -. ((rect_height rect) *. portion_for_right)
      in

      let middle_x =
        p.x +. ((rect_width rect) *. portion_for_pivot_vs_above)
      in
      { left = {
          p = p;
          q = { x = q.x; y = middle_y1; } };
        right = {
          p = { x = p.x; y = middle_y2; };
          q = q; };
        pivot = {
          p = { x = p.x;  y = middle_y1; };
          q = { x = middle_x; y = middle_y2; } };
        above_pivot = {
          p = { x = middle_x; y = middle_y1; };
          q = { x = q.x; y = middle_y2; } }
      }
(*e: function compute_rects_pivotized *)

(*s: function balayer_right_wrong *)
(*
let rec balayer_right_wrong xs =
  match xs with
  | [] -> []
  | x::xs ->
      let first =
        [], x::xs
      in
      let last =
        x::xs, []
      in
      let rest = balayer_right_wrong xs in
      let rest' = rest +> List.map (fun (start, theend) -> x::start, theend) in
      [first] ++ rest' ++ [last]
*)

let balayer_right xs =
  let n = List.length xs in
  let res = ref [] in
  for i = 0 to n do
    Common.push (take i xs, drop i xs) res;
  done;
  List.rev !res
let _ = assert (balayer_right [1;2;3;2] =
                [
                  [], [1;2;3;2];
                  [1], [2;3;2];
                  [1;2], [3;2];
                  [1;2;3], [2];
                  [1;2;3;2], [];
                ])
(*e: function balayer_right_wrong *)

(*s: function orderify_children *)
let orderify_children ?(pivotf=PivotBySize) xs rect =

  let rec aux xs rect =
    match xs with
    | [] -> []
    | [size, x] ->
        [size, x, rect]

    | _x::_y::_ys ->

        let left, pivot, right =
          match pivotf with
          | PivotBySize ->
              let pivot_max = Common2.maximum (xs |> List.map fst) in
              Common2.split_when
                (fun x -> fst x = pivot_max) xs
          | PivotByMiddle ->
              let nmiddle = List.length xs / 2 in
              let start, thend = Common2.splitAt nmiddle xs in

              start, List.hd thend, List.tl thend
        in

        let spread =
          if rect_width rect >= rect_height rect (* e.g. 6 x 4 rectangle *)
          then SpreadHorizontally
          else SpreadVertically
        in

        let right_combinations = balayer_right right in

        let scores_and_rects =
          right_combinations |> List.map (fun (above_pivot, right) ->

            let childs_pivotized =
              { left = left;
                pivot = [pivot];
                right = right;
                above_pivot = above_pivot;
              }
            in
            let rects = compute_rects_pivotized childs_pivotized rect spread in
            ratio_rect_dim (rect_width rects.pivot, rect_height rects.pivot),
            (rects,
             childs_pivotized)
          )
        in
        let best = Common.sort_by_key_lowfirst scores_and_rects |> List.hd in
        let (_score, (rects, childs_pivotized)) = best in

        (* pr2_gen rects; *)
        aux childs_pivotized.left rects.left @
        aux childs_pivotized.pivot rects.pivot @
        aux childs_pivotized.above_pivot rects.above_pivot @
        aux childs_pivotized.right rects.right @
        []
  in
  aux xs rect
(*e: function orderify_children *)

(*s: function test_orderify *)
let test_orderify () =
  let xs = children_ex_ordered_2001 |> List.map float_of_int in
  let rect = rect_ortho () in

  let fake_treemap = () in
  let children = xs |> List.map (fun size -> size, fake_treemap) in

  let layout = orderify_children children rect in
  pr2_gen layout
(*e: function test_orderify *)


(*s: layout ordered *)
let (ordered_layout: ?pivotf:pivot -> ('a, 'b) layout_func) =
  fun ?pivotf children _depthTODOMAYBE rect ->
  orderify_children ?pivotf children rect
(*e: layout ordered *)

(*---------------------------------------------------------------------------*)
(* cushion algorithm *)
(*---------------------------------------------------------------------------*)

(* TODO *)

(*---------------------------------------------------------------------------*)
(* frontend *)
(*---------------------------------------------------------------------------*)

let layoutf_of_algo algo =
  match algo with
  | Classic -> slice_and_dicing_layout
  | Squarified -> squarify_layout
  | SquarifiedNoSort -> squarify_layout_no_sort_size
  | Ordered pivotf -> ordered_layout ~pivotf


let render_treemap_algo2 = fun ?(algo=Classic) ?(big_borders=false) treemap ->
  let flayout = layoutf_of_algo algo in

  let treemap_rects = ref [] in

  let rec aux_treemap root rect ~depth =
    let (p,q) = rect.p, rect.q in

    if not (valid_rect rect)
    then () (* TODO ? warning ? *)
    else

      (match root with
       | Leaf (tnode, _fileinfo) ->
           let color = color_of_treemap_node root in

           Common.push {
             tr_rect = rect;
             tr_color = color;
             tr_label = tnode.label;
             tr_depth = depth;
             tr_is_node = false;
           } treemap_rects;


       | Node (mode, children) ->

           (* let's draw some borders. Far better to see the structure. *)
           Common.push {
             tr_rect = rect;
             tr_color = Color.black;
             tr_label = (fst mode).label;
             tr_depth = depth;
             tr_is_node = true;
           } treemap_rects;

           (* does not work, weird *)
           let border =
             if not big_borders then
               match depth with
               | 1 -> 0.0
               | 2 -> 0.003
               | 3 -> 0.001
               | 4 -> 0.0005
               | 5 -> 0.0002
               | _ -> 0.0
             else
               match depth with
               | 1 -> 0.0
               | 2 -> 0.003
               | 3 -> 0.0015
               | 4 -> 0.0010
               | 5 -> 0.0008
               | 6 -> 0.0005
               | _ -> 0.0002
           in
           let p = {
             x = p.x +. border;
             y = p.y +. border;
           }
           in
           let q = {
             x = q.x -. border;
             y = q.y -. border;
           }
           in
           (* todo? can overflow ... check still inside previous rect *)
           let rect = { p = p; q = q } in

           let children' =
             children |> List.map (fun child ->
               float_of_int (size_of_treemap_node child),
               child
             )
           in

           let rects_with_info =
             (* generic call *)
             flayout children' depth rect
           in
           (* less: assert rects_with_info are inside rect ? *)

           rects_with_info |> List.iter (fun (_x, child, rect) ->
             aux_treemap child rect ~depth:(depth + 1)
           );


      )
  in
  aux_treemap treemap (rect_ortho()) ~depth:1;

  List.rev !treemap_rects

let render_treemap ?algo ?big_borders x =
  Common.profile_code "Treemap.render_treemap" (fun () ->
    render_treemap_algo2 ?algo ?big_borders x)

(*****************************************************************************)
(* Main display function  *)
(*****************************************************************************)

(* now in treemap_graphics.ml *)

(*****************************************************************************)
(* Source converters  *)
(*****************************************************************************)

type directory_sort =
  | NoSort
  | SortDirThenFiles
  | SortDirAndFiles
  | SortDirAndFilesCaseInsensitive

let follow_symlinks = ref false

(*s: function tree_of_dir *)
(*
let tree_of_dir2
  ?(filter_file=(fun _ -> true))
  ?(filter_dir=(fun _ -> true))
  ?(sort=SortDirAndFilesCaseInsensitive)
  ~file_hook
  dir
 =
  let rec aux dir =

    let subdirs =
      Common2.readdir_to_dir_list dir +> List.map (Filename.concat dir) in
    let files =
      Common2.readdir_to_file_list dir +> List.map (Filename.concat dir) in

    let subdirs =
      subdirs +> Common.map_filter (fun dir ->
        if filter_dir dir
        then Some (dir, aux dir)
        else None
      )
    in
    let files =
      files +> Common.map_filter (fun file ->
        if filter_file file
        then Some (file, (Leaf (file, file_hook file)))
        else None
      )
    in

    let agglomerated =
      match sort with
      | NoSort -> subdirs ++ files
      | SortDirThenFiles ->
          Common.sort_by_key_lowfirst subdirs ++
          Common.sort_by_key_lowfirst files
      | SortDirAndFiles ->
          Common.sort_by_key_lowfirst (subdirs ++ files)
      | SortDirAndFilesCaseInsensitive ->
          let xs = (subdirs ++ files) +> List.map (fun (s, x) ->
            lowercase s, x
          )
          in
          Common.sort_by_key_lowfirst xs
    in
    let children = List.map snd agglomerated in
    Node(dir, children)
  in
  aux dir
*)
(*e: function tree_of_dir *)


(* specialized version *)
let tree_of_dir3
    ?(filter_file=(fun _ -> true))
    ?(filter_dir=(fun _ -> true))
    ?(sort=SortDirAndFilesCaseInsensitive)
    ~file_hook
    dir
  =
  if sort <> SortDirAndFilesCaseInsensitive
  then failwith "Only SortDirAndFilesCaseInsensitive is handled";

  let rec aux dir =

    let children = Sys.readdir dir in
    let children = Array.map (fun x -> String.lowercase_ascii x, x) children in

    Array.fast_sort (fun (a1, _b1) (a2, _b2) -> compare a1 a2) children;

    let res = ref [] in

    children |> Array.iter (fun (_, f) ->
      let full = Filename.concat dir f in

      let stat = Common2.unix_lstat_eff full in

      match stat.Unix.st_kind with
      | Unix.S_REG ->
          if filter_file full
          then Common.push (Leaf (full, file_hook full)) res
      | Unix.S_DIR ->
          if filter_dir full
          then Common.push (aux full) res
      | Unix.S_LNK ->
          if !follow_symlinks then
            (try
               (match (Unix.stat full).Unix.st_kind with
                | Unix.S_REG ->
                    if filter_file full
                    then Common.push (Leaf (full, file_hook full)) res
                | Unix.S_DIR ->
                    if filter_dir full
                    then Common.push (aux full) res
                | _ -> ()
               )
             with Unix.Unix_error _ ->
               pr2 (spf "PB stat link at %s" full);
            )
          else ()
      | _ -> ()
    );
    Node(dir, List.rev !res)
  in
  aux dir


let tree_of_dir ?filter_file ?filter_dir ?sort ~file_hook a =
  Common.profile_code "Treemap.tree_of_dir" (fun () ->
    tree_of_dir3 ?filter_file ?filter_dir ?sort ~file_hook a)

let tree_of_dir_or_file ?filter_file ?filter_dir ?sort ~file_hook path =
  if Common2.is_directory path
  then tree_of_dir ?filter_file ?filter_dir ?sort ~file_hook path
  else Leaf (path, file_hook path)



(* Some nodes may have stuff in common that we should factor.
 * todo: factorize code with Common.tree_of_files
*)
let add_intermediate_nodes root_path nodes =
  let root = chop_dirsymbol root_path in
  if not (Common2.is_absolute root)
  then failwith ("must pass absolute path, not: " ^ root);

  let root = Common.split "/" root in

  (* extract dirs and file from file, e.g. ["home";"pad"], "__flib.php", path *)
  let xs = nodes |> List.map (fun x ->
    match x with
    | Leaf (file, _) -> Common2.dirs_and_base_of_file file, x
    | Node (dir, _) -> Common2.dirs_and_base_of_file dir, x
  )
  in
  (* remove the root part *)
  let xs = xs |> List.map (fun ((dirs, base), node) ->
    let n = List.length root in
    let (root', rest) =
      Common2.take n dirs,
      Common2.drop n dirs
    in
    assert(root' =*= root);
    (rest, base), node
  )
  in
  (* now ready to build the tree recursively *)
  let rec aux current_root xs =
    let files_here, rest =
      xs |> List.partition (fun ((dirs, _base), _) -> null dirs)
    in
    let groups =
      rest |> group_by_mapped_key (fun ((dirs, _base),_) ->
        (* would be a file if null dirs *)
        assert(not (null dirs));
        List.hd dirs
      ) in

    let nodes =
      groups |> List.map (fun (k, xs) ->
        let xs' = xs |> List.map (fun ((dirs, base), node) ->
          (List.tl dirs, base), node
        )
        in
        let dirname = Filename.concat current_root k in
        Node (dirname, aux dirname xs')
      )
    in
    let leaves = files_here |> List.map (fun ((_dir, _base), node) ->
      node
    ) in
    nodes @ leaves
  in
  aux root_path xs


let tree_of_dirs_or_files2 ?filter_file ?filter_dir ?sort ~file_hook paths =
  match paths with
  | [] -> failwith "tree_of_dirs_or_files: empty list"
  | [x] -> tree_of_dir_or_file ?filter_file ?filter_dir ?sort ~file_hook x
  | xs ->
      let nodes =
        xs |> List.map (fun x ->
          tree_of_dir_or_file ?filter_file ?filter_dir ?sort ~file_hook x
        )
      in
      let root = Common2.common_prefix_of_files_or_dirs xs in
      let nodes = add_intermediate_nodes root nodes in
      Node (root, nodes)

let tree_of_dirs_or_files ?filter_file ?filter_dir ?sort ~file_hook x =
  Common.profile_code "Treemap.tree_of_dirs_or_files" (fun () ->
    tree_of_dirs_or_files2 ?filter_file ?filter_dir ?sort ~file_hook x
  )

(* Some software, especially java have often a long chain
 * of single directory, like org/eclipse/...
 * which then introduce extra depth in the treemap which leads
 * to overlapping labels and very small labels for the actual
 * childrens. This function removes those intermediate singleton
 * sub directories.
*)
let rec remove_singleton_subdirs tree =
  match tree with
  | Leaf _x -> tree
  | Node (x, [Node (_y, ys)]) ->
      (* todo? merge x and y ? *)
      remove_singleton_subdirs (Node (x, ys))
  | Node (x, ys) ->
      Node (x, List.map remove_singleton_subdirs ys)

(*****************************************************************************)
(* Testing *)
(*****************************************************************************)

(*s: concrete rectangles example *)
(* src: python treemap.py
 * lower, upper, rgb
*)
let treemap_rectangles_ex = [
  [0.0, 0.0], [1.0, 1.0],                                                                 (0.17778372236496054, 0.75183542244426871, 0.77892130219255096);
  [0.0, 0.0], [0.27659574468085107, 1.0],                                                 (0.54757582213226441, 0.945582381819014, 0.26427761420055917);
  [0.0, 0.0], [0.27659574468085107, 0.38461538461538464],                                 (0.71931501307446211, 0.95905644995588246, 0.28633110533256656);
  [0.0, 0.38461538461538464], [0.27659574468085107, 1.0],                                 (0.29508972521695809, 0.35521829137775873, 0.46070336222733932);
  [0.0, 0.38461538461538464], [0.10372340425531915, 1.0],                                 (0.51529552034735771, 0.53725734991812635, 0.22430742368105949);
  [0.10372340425531915, 0.38461538461538464], [0.27659574468085107, 1.0],                 (0.43861905319415506, 0.16281118710897469, 0.60250203640050937);
  [0.27659574468085107, 0.0], [0.36170212765957449, 1.0],                                 (0.3743827201120038, 0.07170428778373239, 0.09006244270341246);
  [0.36170212765957449, 0.0], [0.8936170212765957, 1.0],                                  (0.39117531981521536, 0.16579633978705666, 0.63690597944460248);
  [0.36170212765957449, 0.0], [0.8936170212765957, 0.20000000000000001],                  (0.34982099039431447, 0.54618822154424429, 0.19282777912183513);
  [0.36170212765957449, 0.20000000000000001], [0.8936170212765957, 0.28000000000000003],  (0.14570785913376116, 0.88033416430670342, 0.51911403487550056);
  [0.36170212765957449, 0.28000000000000003], [0.8936170212765957, 0.76000000000000001],  (0.79691567717907263, 0.3307536109585284, 0.95607296382731199);
  [0.36170212765957449, 0.28000000000000003], [0.45035460992907805, 0.76000000000000001], (0.7038680786604008, 0.12714028216462059, 0.17131117338368551);
  [0.45035460992907805, 0.28000000000000003], [0.58333333333333337, 0.76000000000000001], (0.036414279679915174, 0.94100891978030599, 0.017007582879843386);
  [0.58333333333333337, 0.28000000000000003], [0.8936170212765957, 0.76000000000000001],  (0.63659306932350279, 0.25303150185397794, 0.81066700006123815);
  [0.58333333333333337, 0.28000000000000003], [0.8936170212765957, 0.48571428571428577],  (0.38368601825375115, 0.083946154840038423, 0.048274714595522017);
  [0.58333333333333337, 0.48571428571428577], [0.8936170212765957, 0.62285714285714289],  (0.70513207607633877, 0.95785105976069096, 0.87735329563400943);
  [0.58333333333333337, 0.62285714285714289], [0.8936170212765957, 0.76000000000000001],  (0.80565735169264896, 0.75578523763882166, 0.10757369310766951);
  [0.36170212765957449, 0.76000000000000001], [0.8936170212765957, 1.0],                  (0.57042872206220896, 0.9335301149492965, 0.86254084187238389);
  [0.36170212765957449, 0.76000000000000001], [0.62765957446808507, 1.0],                 (0.31530318311042171, 0.97066142447913661, 0.93180609525183578);
  [0.62765957446808507, 0.76000000000000001], [0.8936170212765957, 1.0],                  (0.18330061581424317, 0.82234170300788867, 0.38303955663618716);
  [0.8936170212765957, 0.0], [1.0, 1.0],                                                    (0.20641218447120302, 0.35715481613716149, 0.86620796882602547);
  [0.8936170212765957, 0.0], [1.0, 0.59999999999999998],                                    (0.7942020522649591, 0.27351921049542915, 0.86191731793444748);
  [0.8936170212765957, 0.59999999999999998], [1.0, 1.0],                                  (0.27214488578650742, 0.41635201268319189, 0.1301335726270938);
]
(*e: concrete rectangles example *)



(*s: variable tree_ex_shneiderman_1991 *)
let tree_ex_shneiderman_1991 =
  let ninfo = () in
  Node (ninfo,  [
    Leaf 12;
    Leaf 6;
    Node (ninfo,  [
      Leaf 2;
      Leaf 2;
      Leaf 2;
      Leaf 2;
      Leaf 2;
    ]);
    Node(ninfo,  [
      Node(ninfo,  [
        Leaf 5;
        Leaf 20;
      ]);
      Node(ninfo,  [
        Leaf 5;
      ]);
      Leaf 40;
    ]);
  ])
(*e: variable tree_ex_shneiderman_1991 *)

(*s: variable tree_ex_wijk_1999 *)
let tree_ex_wijk_1999 =
  let ninfo = () in
  Node (ninfo,  [
    Leaf 6;
    Leaf 6;
    Leaf 4;
    Leaf 3;
    Leaf 2;
    Leaf 2;
    Leaf 1;
  ])
(*e: variable tree_ex_wijk_1999 *)

(*s: variable treemap_ex_ordered_2001 *)
let (treemap_ex_ordered_2001: (unit, unit) treemap) =
  let children = children_ex_ordered_2001 in

  let children_treemap =
    children |> Common.index_list_1 |> List.map (fun (size, i) ->

      Leaf ({
        size = size;
        color = Color.color_of_string (spf "grey%d" (90 - (i * 3)));
        label = spf "size = %d" size;
      }, ())
    )
  in
  let total_size = Common2.sum children in
  Node (({
    size = total_size;
    color = Color.black;
    label = "";
  }, ()), children_treemap
  )
(*e: variable treemap_ex_ordered_2001 *)









(*****************************************************************************)
(* Actions *)
(*****************************************************************************)

let actions () = [
  (*s: treemap actions *)
  "-test_squarify", "<>",
  Common.mk_action_0_arg (test_squarify);
  "-test_orderify", "<>",
  Common.mk_action_0_arg (test_orderify);
  (*e: treemap actions *)
]
(*e: treemap.ml *)
