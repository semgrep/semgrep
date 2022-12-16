(*s: treemap_graphics.ml *)
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

open Common

open Treemap

module Color = Simple_color

module F = Figures

(*****************************************************************************)
(* Graphics Helpers *)
(*****************************************************************************)

(*s: function current_dim *)
let current_dim ~w_legend ~h_status =

  let w, h = Graphics.size_x (), Graphics.size_y () in

  let w_view, h_view =
    Graphics.size_x () - w_legend,
    Graphics.size_y () - h_status
  in

  {
    w = w;
    h = h;
    w_view = w_view;
    h_view = h_view;
    h_status = h_status;
    w_legend = w_legend;
  }
(*e: function current_dim *)

(*s: function draw_rect_treemap_float_ortho *)
(*
 * The treemap algorithms assume an ortho? space from 0,0 to 1.1 but
 * our current screen have pixels and goes from 0,0 to 1024,168 for
 * instance. Those functions are here to make the translation
 * (it can produce some aliasing effects).

 * TODO: pass a converter function from ortho space to regular ?
 * as in opengl?
 *)

let draw_rect_treemap_float_ortho ((x1, y1),(x2, y2)) color (w, h) =

  let w = float_of_int w in
  let h = float_of_int h in

  let x1, y1 = int_of_float (x1 *. w), int_of_float (y1 *. h) in
  let x2, y2 = int_of_float (x2 *. w), int_of_float (y2 *. h) in
  let w = (x2 - x1) in
  let h = (y2 - y1) in
  Graphics.set_color color;

  if w <= 0 || h <= 0
  then None
  else begin
    Graphics.fill_rect
      x1 y1 w h;
    Some ((x1,y1), (x2,y2))
  end
(*e: function draw_rect_treemap_float_ortho *)

(*s: graphic helpers *)
let draw_string_centered str =
  let (w, h) = Graphics.text_size str in
  Graphics.rmoveto (- w / 2) (- h / 2);
  Graphics.draw_string str

let draw_text_center_rect_float_ortho ((x1, y1),(x2, y2)) color (w, h) str =
  let w = float_of_int w in
  let h = float_of_int h in

  let x1, y1 = int_of_float (x1 *. w), int_of_float (y1 *. h) in
  let x2, y2 = int_of_float (x2 *. w), int_of_float (y2 *. h) in

  let w = (x2 - x1) in
  let h = (y2 - y1) in

  Graphics.set_color color;
  Graphics.moveto (x1 + w / 2 ) (y1 + h / 2);
  let (w2, h2) = Graphics.text_size str in
  if str <> "" && w2 < w && h2 < h
  then begin
    (* does not work :( Graphics.set_text_size 40; *)
    draw_string_centered str;
    (*
    pr2 str;
    pr2_gen (x1, y1);
    *)
  end;
  ()


let draw_label rect (w, h) depth label ~is_dir =
  let (p, q) = rect.p, rect.q in

  let font_label_opt =
    if is_dir then
      match depth with
      | 1 -> None
      | 2 -> Some  "-misc-*-*-*-*-20-*-*-*-*-*-*"
      | 3 -> Some  "-misc-*-*-*-*-10-*-*-*-*-*-*"
      | 4 -> Some "-misc-*-*-*-*7-*-*-*-*-*-*"
      | _ -> None
    else
      Some "-misc-*-*-*-*-6-*-*-*-*-*-*"
  in

  font_label_opt |> Option.iter (fun font ->
    Graphics.set_font font;

    draw_text_center_rect_float_ortho
      ((p.x, p.y),
       (q.x, q.y))
      (if is_dir then Graphics.black else Color.c "grey37")
      (w, h)
      label
  )
(*e: graphic helpers *)

(*****************************************************************************)
(* Treemap Helpers *)
(*****************************************************************************)

(*s: function update_mat_with_fileinfo *)
let update_mat_with_fileinfo fileinfo mat rect =

  let ((x1,y1), (x2,y2)) = rect in

  for i = x1 to x2 - 1 do
    for j = y1 to y2 - 1 do
      mat.(i).(j) <- Some fileinfo;
    done
  done
(*e: function update_mat_with_fileinfo *)

(*****************************************************************************)
(* Main display function  *)
(*****************************************************************************)

(*s: function display_treemap *)
(*
 * ref: http://hcil.cs.umd.edu/trs/91-03/91-03.html, page 6
 *
 * The algorithm is very simple. Look at the paper. I've just added
 * the depth argument.
 *
 * axis_split is 0 when split enclosing rectangle vertically, and 1
 * when doing it horizontally. We alternate hence the (1 - axis_split) below.
 *
 * still? look if python port look the same
 *)
let display_treemap (treemap: ('dir,'file) treemap) (w, h) =

  let mat = Array.make_matrix w h None in

  (* p and q are the coords of the current rectangle being laid out *)
  let rec aux_treemap root p q axis_split ~depth =

    (* todo? join the 2 match in a single one ? *)
    (match root with
     | Leaf (tnode, fileinfo) ->
         let color = color_of_treemap_node root in

         let rect_opt =
           draw_rect_treemap_float_ortho
             ((p.(0), p.(1)),
              (q.(0), q.(1)))
             color
             (w, h)
         in
         rect_opt |> Option.iter (update_mat_with_fileinfo fileinfo mat)

     | Node (tnode, dirinfo) ->
         ()
    );
    let size_root = size_of_treemap_node root in
    let width = q.(axis_split) -. p.(axis_split) in
    match root with
    | Node (mode, children) ->
        children |> List.iter (fun child ->
          (* if want margin, then maybe can increment slightly p and decrement
           * q ? like 1% of its width ?
          *)
          q.(axis_split) <-
            p.(axis_split) +.
            (float_of_int (size_of_treemap_node child) /.
             float_of_int (size_root)) *. width;
          aux_treemap child (Array.copy p) (Array.copy q) (1 - axis_split)
            ~depth:(depth + 1)
          ;
          p.(axis_split) <- q.(axis_split);
        )
    | Leaf _ -> ()
  in
  aux_treemap treemap [|0.0;0.0|] [|1.0;1.0|] 0  ~depth:1;
  mat
(*e: function display_treemap *)


(*---------------------------------------------------------------------------*)
(* generic frontend, taking layout-maker function as a parameter  *)
(*---------------------------------------------------------------------------*)

(*s: function display_treemap_generic *)
let display_treemap_generic
    ?(drawing_file_hook=(fun _rect _file _mat -> ()))
    (treemap: ('dir,'file) treemap)
    (w, h)
    flayout
  =

  let mat = Array.make_matrix w h None in

  let rec aux_treemap root rect ~depth =
    let (p,q) = rect.p, rect.q in

    if not (valid_rect rect)
    then () (* TODO ? warning ? *)
    else

      (match root with
       | Leaf (tnode, fileinfo) ->
           let color = color_of_treemap_node root in

           let rect_opt =
             draw_rect_treemap_float_ortho
               ((p.x, p.y),
                (q.x, q.y))
               color
               (w, h)
           in
           let info = fileinfo in

           (match rect_opt with
            | None -> ()
            | Some ((x1,y1), (x2,y2)) ->

                for i = x1 to x2 - 1 do
                  for j = y1 to y2 - 1 do
                    mat.(i).(j) <- Some info;
                  done
                done;

                drawing_file_hook {
                  F.lower_left =   { F.x = x1; F.y = y1 };
                  F.upper_right =  { F.x = x2; F.y = y2 };
                }
                  fileinfo
                  mat

           );
           draw_label rect  (w, h) depth (tnode).label ~is_dir:false


       | Node (mode, children) ->

           (* let's draw some borders. Far better to see the structure. *)
           let _rect_opt =
             draw_rect_treemap_float_ortho
               ((p.x, p.y),
                (q.x, q.y))
               Graphics.black
               (w, h)

           in
           (* does not work, weird *)
           let border =
             match depth with
             | 1 -> 0.0
             | 2 -> 0.002
             | 3 -> 0.001
             | 4 -> 0.0005
             | 5 -> 0.0002
             | _ -> 0.0
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

           rects_with_info |> List.iter (fun (x, child, rect) ->
             aux_treemap child rect ~depth:(depth + 1)
           );

           draw_label rect (w, h) depth (fst mode).label ~is_dir:true
      )
  in
  aux_treemap treemap rect_ortho ~depth:1;
  mat
(*e: function display_treemap_generic *)


(*s: function display_treemap_algo *)
let display_treemap_algo ?(algo=Classic) ?drawing_file_hook
    treemap (w, h) =

  (* old: display_treemap            treemap (w, h) *)
  let layoutf = layoutf_of_algo algo in
  display_treemap_generic ?drawing_file_hook
    treemap (w, h) layoutf
(*e: function display_treemap_algo *)


(*s: function display_treemap_interactive *)
let display_treemap_interactive
    ?algo
    ?drawing_file_hook
    ?(info_of_file_under_cursor=(fun _ _ -> ""))
    treemap
    dim
  =
  let dim = ref dim in
  let matrix_info = ref (
    display_treemap_algo
      ?algo
      ?drawing_file_hook
      treemap
      (!dim.w_view, !dim.h_view)
  )
  in
  while true do
    let status = Graphics.wait_next_event [
      Graphics.Mouse_motion;
      Graphics.Key_pressed;
      Graphics.Button_down;
      Graphics.Button_up;
    ]
    in
    let (x,y) = status.Graphics.mouse_x, status.Graphics.mouse_y in

    if x >= 0 && y >= 0 && x < !dim.w_view && y < !dim.h_view
    then begin

      (* clear the status area *)
      Graphics.set_color Graphics.white;
      Graphics.fill_rect 0 (!dim.h - !dim.h_status) !dim.w (!dim.h);

      Graphics.set_color Graphics.black;
      Graphics.moveto (0 + !dim.w / 2) (!dim.h - (!dim.h_status / 2));

      let info =
        try
          !matrix_info.(x).(y)
        with Invalid_argument(s) ->
          pr2 (spf "pb with coord (%d,%d).  %s" x y s);
          raise (Invalid_argument(s))

      in
      match info with
      | None -> pr2 "Impossible";
      | Some file ->
          let s = info_of_file_under_cursor status file in
          (* draw_string_centered (spf "x = %03d, y = %03d; info = %s" x y s); *)
          Graphics.set_font "-misc-*-*-*-*-12-*-*-*-*-*-*";
          draw_string_centered (spf "%s" s);
    end;

    (* a resize has taken place *)
    let w, h = Graphics.size_x (), Graphics.size_y () in
    if w <> !dim.w || h <> !dim.h
    then begin
      dim := current_dim ~w_legend:!dim.w_legend ~h_status:!dim.h_status;
      Graphics.clear_graph ();
      matrix_info :=
        display_treemap_algo
          ?algo
          ?drawing_file_hook
          treemap
          (!dim.w_view, !dim.h_view);
      (* draw_legend_hook !dim ? *)
    end
  done
(*e: function display_treemap_interactive *)

(*s: function info_of_file_under_cursor_default *)

let info_of_file_under_cursor_default = fun status (f, _) ->
  let s = f in
  if status.Graphics.button
  then begin
    pr2 (spf "%s" f);
    (* Sys.command (spf "/home/pad/packages/Linux/bin/emacsclient -n %s" f) +> ignore; *)
  end;
  if status.Graphics.keypressed (* Graphics.key_pressed () *)
  then raise (UnixExit 0);
  s
(*e: function info_of_file_under_cursor_default *)

(*****************************************************************************)
(* Testing *)
(*****************************************************************************)

(*s: function test_treemap_manual *)
(* test draw_rect_treemap_float_ortho *)
let test_treemap_manual () =
  Graphics.open_graph " 640x640";
  Graphics.set_color (Graphics.rgb 1 1 1);
  let w, h = Graphics.size_x (), Graphics.size_y () in

  treemap_rectangles_ex |> List.iter (fun (upper, lower, (r,g,b)) ->
    match upper, lower with
    | [x1, y1], [x2, y2] ->
        let maxc = float_of_int 256 in
        let (r,g,b) =
          int_of_float (r *. maxc),
          int_of_float (g *. maxc),
          int_of_float (b *. maxc)
        in
        let color = Graphics.rgb (r) (g) (b) in

        draw_rect_treemap_float_ortho ((x1, y1),(x2, y2)) color (w, h)
        |> ignore
    | _ -> failwith "wront format"
  );
  Common.pause();
  ()
(*e: function test_treemap_manual *)

(*s: function test_treemap *)
let test_treemap algorithm treemap =
  Graphics.open_graph " 640x640";
  Graphics.set_color (Graphics.rgb 1 1 1);
  let w, h = Graphics.size_x (), Graphics.size_y () in

  Graphics.set_line_width 2;

  display_treemap_algo ~algo:algorithm treemap (w, h) |> ignore;
  while true do
    let status = Graphics.wait_next_event [
      Graphics.Key_pressed;
    ]
    in
    if status.Graphics.keypressed (* Graphics.key_pressed () *)
    then raise (UnixExit 0);
  done;
  (* old: pause (); *)
  ()
(*e: function test_treemap *)

(* test tree_of_dir *)
(*s: function test_treemap_dir *)
let test_treemap_dir dir algo =

  let w_view_hint, h_view_hint = 640, 640 in
  let h_status = 30 in

  Graphics.open_graph (spf " %dx%d" w_view_hint (h_view_hint+ h_status));
  Graphics.set_color (Graphics.rgb 1 1 1);
  let w_view, h_view =
    Graphics.size_x (),
    Graphics.size_y () - h_status
  in
  let w, h = Graphics.size_x (), Graphics.size_y () in

  let maxc = 256 in
  let dim = {
    w = w;
    h = h;
    w_view = w_view;
    h_view = h_view;
    h_status = h_status;
    w_legend = 10;
  }
  in

  (* work ? Graphics.set_line_width 2; *)

  let tree =
    tree_of_dir ~file_hook:(fun file ->
      file, Common.filesize file
    )
      dir
  in

  let treemap = treemap_of_tree
      ~size_of_leaf:(fun (f, intleaf) -> intleaf)
      ~color_of_leaf:(fun (f, intleaf) ->
        Graphics.rgb (Random.int maxc) (Random.int maxc) (Random.int maxc)
      )
      ~label_of_dir:(fun dir -> basename dir)
      tree
  in

  display_treemap_interactive
    ~algo
    treemap
    dim
    ~info_of_file_under_cursor:(fun status (f, size) ->
      let s = f in
      if status.Graphics.button
      then begin
        pr2 (spf "%s" f);
        Sys.command (spf "/home/pad/packages/Linux/bin/emacsclient -n %s" f) |> ignore;
      end;

      if status.Graphics.keypressed (* Graphics.key_pressed () *)
      then raise (UnixExit 0);
      s
    );


  ()
(*e: function test_treemap_dir *)

(* test treemap_of_tree, and display_treemap *)
(*s: function test_treemap_tree *)
let test_treemap_tree algorithm ex =
  let maxc = 256 in

  let tree =
    match ex with
    | 1 -> tree_ex_shneiderman_1991
    | 2 -> tree_ex_wijk_1999
    | _ -> raise Impossible
  in

  let treemap = treemap_of_tree
      ~size_of_leaf:(fun intleaf -> intleaf)
      ~color_of_leaf:(fun intleaf ->
        Graphics.rgb (Random.int maxc) (Random.int maxc) (Random.int maxc)
      )
      ~label_of_file:(fun intleaf -> i_to_s intleaf)
      tree
  in
  test_treemap algorithm treemap
(*e: function test_treemap_tree *)

(*****************************************************************************)
(* Actions *)
(*****************************************************************************)

let actions () = [
  (*s: treemap_graphics actions *)
  "-test_treemap_manual", "<>",
  Common.mk_action_0_arg (test_treemap_manual);

  "-test_treemap", "<algorithm>",
  Common.mk_action_1_arg (fun s ->
    let treemap = treemap_ex_ordered_2001 in
    test_treemap (algo_of_s s) treemap

  );

  "-test_treemap_tree", "<algorithm> <ex>",
  Common.mk_action_2_arg (fun s i ->
    test_treemap_tree (algo_of_s s) (s_to_i i)
  );
  "-test_treemap_dir", "<dir> <algorithm>",
  Common.mk_action_2_arg (fun dir str ->
    test_treemap_dir dir (algo_of_s str)
  );

  (*e: treemap_graphics actions *)
]

(*e: treemap_graphics.ml *)
