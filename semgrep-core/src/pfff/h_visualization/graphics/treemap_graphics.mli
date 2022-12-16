(*s: treemap_graphics.mli *)

open Treemap

(* seminal code and algorithm *)
(*s: signature display_treemap *)
val display_treemap :
  ('dir, 'file) treemap -> int * int -> 'file option Common.matrix
(*e: signature display_treemap *)

(*s: signature display_treemap_algo *)
val display_treemap_algo :
  ?algo:algorithm ->
  ?drawing_file_hook:
    (Figures.rect_pixel -> 'file -> 'file option Common.matrix -> unit) ->
  ('dir, 'file) treemap ->
  int * int ->
  'file option Common.matrix
(*e: signature display_treemap_algo *)

(* main entry point *)
(*s: signature display_treemap_interactive *)
val display_treemap_interactive :
  ?algo:algorithm ->
  ?drawing_file_hook:
    (Figures.rect_pixel -> 'file -> 'file option Common.matrix -> unit) ->
  (* used to display file information in the status area *)
  ?info_of_file_under_cursor:(Graphics.status -> 'file -> string) ->
  ('dir, 'file) treemap ->
  screen_dim ->
  unit
(*e: signature display_treemap_interactive *)

(*s: signature graphic helpers *)
val draw_rect_treemap_float_ortho :
  (float * float) * (float * float) ->
  Graphics.color -> int * int -> ((int * int) * (int * int)) option
(*x: signature graphic helpers *)
val info_of_file_under_cursor_default :
  Graphics.status -> (Common.filename * 'a) -> string

val current_dim:
  w_legend:int -> h_status:int -> screen_dim
(*e: signature graphic helpers *)

(*s: signature test treemap functions *)
val test_treemap_manual : unit -> unit
val test_treemap_tree : algorithm -> int -> unit
val test_treemap_dir : string -> algorithm -> unit
(*e: signature test treemap functions *)

(*e: treemap_graphics.mli *)
