(*s: treemap.mli *)
open Figures

(* tree -> treemap -> treemap_rendering *)

type ('dir, 'file) tree =
  ('dir, 'file) Common2.tree

(*s: type treemap *)
type ('dir, 'file) treemap =
  (treemap_data * 'dir, treemap_data * 'file) tree
and treemap_data = {
  size : int;
  color : Simple_color.color;
  label: string;
}
(*e: type treemap *)

type treemap_rendering = treemap_rectangle list
and treemap_rectangle = {
  tr_rect: rectangle;
  tr_color: int (* Simple_color.color *);
  tr_label: string;
  tr_depth: int;
  tr_is_node: bool;
}



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

val xy_ratio : float ref

val rect_ortho: unit -> rectangle

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

(*s: type layout_func *)
type ('a, 'b) layout_func =
  (float * ('a, 'b) treemap) list ->
  int ->
  rectangle ->
  (float * ('a, 'b) treemap * rectangle) list
(*e: type layout_func *)

(*s: signature algos *)
val algos: algorithm list

val layoutf_of_algo: algorithm -> ('a, 'b) layout_func

(*e: signature algos *)

(* treemap -> treemap_rendering *)
val render_treemap:
  ?algo:algorithm ->
  ?big_borders:bool ->
  ('dir, 'file) treemap -> treemap_rendering

(*s: signature treemap_of_tree *)
(* tree -> treemap (see also treemap_json.ml) *)
val treemap_of_tree :
  size_of_leaf:('file -> int) ->
  color_of_leaf:('file -> Simple_color.color) ->
  ?label_of_file:('file -> string) ->
  ?label_of_dir:('dir -> string) ->
  ('dir, 'file) tree ->
  ('dir, 'file) treemap
(*e: signature treemap_of_tree *)

(*s: signature tree_of_dir *)
type directory_sort =
  | NoSort
  | SortDirThenFiles
  | SortDirAndFiles
  | SortDirAndFilesCaseInsensitive
val follow_symlinks: bool ref
(*e: signature tree_of_dir *)

(* paths -> tree (see also Common.tree2_of_files) *)
val tree_of_dirs_or_files:
  ?filter_file:(Common.filename -> bool) ->
  ?filter_dir:(Common.dirname -> bool) ->
  ?sort:directory_sort ->
  file_hook:(Common.filename -> 'a) ->
  Common2.path list ->
  (Common.dirname, Common.filename * 'a) tree



val remove_singleton_subdirs:
  (Common.dirname, Common.filename * 'a) tree ->
  (Common.dirname, Common.filename * 'a) tree


(* internal functions *)
(*s: signature treemap accessors *)
val color_of_treemap_node :
  ('a, 'b) treemap -> Simple_color.color
val size_of_treemap_node :
  ('a, 'b) treemap  -> int
(*e: signature treemap accessors *)

(*s: signature algorithm accessors *)
val s_of_algo: algorithm -> string
val algo_of_s: string -> algorithm
(*e: signature algorithm accessors *)

(* tests *)
(*s: signature tree and treemap examples *)
val treemap_rectangles_ex:
  ((float * float) list * (float * float) list * (float * float * float)) list

val tree_ex_shneiderman_1991 : (unit, int) tree
val tree_ex_wijk_1999: (unit, int) tree
val treemap_ex_ordered_2001: (unit, unit) treemap
(*e: signature tree and treemap examples *)

val actions : unit -> Common.cmdline_actions

(*e: treemap.mli *)
