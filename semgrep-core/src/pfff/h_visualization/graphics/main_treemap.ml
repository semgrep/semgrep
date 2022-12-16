(*s: main_treemap.ml *)
open Common

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

(*s: treemap_viewer flags *)
let algorithm = ref Treemap.Squarified
let big_screen = ref false

let verbose = ref false
(*e: treemap_viewer flags *)

(* action mode *)
let action = ref ""

let version = "0.1"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let init_graph big_screen =

  let w_view_hint, h_view_hint =
    if big_screen
    then
      2300, 1500
    else
      640, 640
  in
  let h_status = 30 in
  let w_legend = 200 in

  Graphics.open_graph
    (spf " %dx%d" (w_view_hint + w_legend) (h_view_hint+ h_status));
  Graphics.set_color (Graphics.rgb 1 1 1);
  let w_view, h_view =
    Graphics.size_x () - w_legend,
    Graphics.size_y () - h_status
  in
  let w, h = Graphics.size_x (), Graphics.size_y () in

  {
    Treemap.w = w;
    h = h;
    w_view = w_view;
    h_view = h_view;
    h_status = h_status;
    w_legend = w_legend;
  }


(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

(*s: function main_action *)
let main_action jsonfile =
  let json = Json_in.load_json jsonfile in
  let treemap = Treemap_json.treemap_of_json json in

  let rendering = Treemap.render_treemap_algo treemap in
  let json = Treemap_json.json_of_treemap_rendering rendering in
  let s = Json_out.string_of_json json in
  pr2 s;

  let dim = init_graph !big_screen in

  Treemap_graphics.display_treemap_interactive
    ~algo:!algorithm
    ~info_of_file_under_cursor:Treemap_graphics.info_of_file_under_cursor_default
    treemap dim
  ;
  ()
(*e: function main_action *)

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () =
  Treemap.actions () @
  Treemap_json.actions () @
  []

let options () =
  [
    (*s: treemap_viewer cmdline options *)
    "-algorithm", Arg.String (fun s ->
      algorithm := Treemap.algo_of_s s;
    ),
    (spf " <algo> (choices are: %s, default = %s"
       (Treemap.algos |> List.map Treemap.s_of_algo |> Common.join ", ")
       (Treemap.s_of_algo !algorithm));

    "-big_screen", Arg.Set big_screen,
    " ";
    "-verbose", Arg.Set verbose,
    " ";
    (*e: treemap_viewer cmdline options *)
  ] @
  Common.options_of_actions action (all_actions()) @
  Common.cmdline_flags_devel () @
  Common.cmdline_flags_verbose () @
  Common.cmdline_flags_other () @
  [
    "-version",   Arg.Unit (fun () ->
      pr2 (spf "ocamltreemap version: %s" version);
      exit 0;
    ),
    "  guess what";

    (* this can not be factorized in Common *)
    "-date",   Arg.Unit (fun () ->
      pr2 "version: $Date: 2008/10/26 00:44:57 $";
      raise (Common.UnixExit 0)
    ),
    "   guess what";
  ] @
  []

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () =
  let usage_msg =
    "Usage: " ^ Common.basename Sys.argv.(0) ^
    " [options] <json file> " ^ "\n" ^ "Options are:"
  in
  (* does side effect on many global flags *)
  let args = Common.parse_options (options()) usage_msg Sys.argv in

  (* must be done after Arg.parse, because Common.profile is set by it *)
  Common.profile_code "Main total" (fun () ->

    (match args with

     (* --------------------------------------------------------- *)
     (* actions, useful to debug subpart *)
     (* --------------------------------------------------------- *)
     | xs when List.mem !action (Common.action_list (all_actions())) ->
         Common.do_action !action xs (all_actions())

     | _ when not (Common.null_string !action) ->
         failwith ("unrecognized action or wrong params: " ^ !action)

     (* --------------------------------------------------------- *)
     (* main entry *)
     (* --------------------------------------------------------- *)
     | [x] ->
         main_action x

     (* --------------------------------------------------------- *)
     (* empty entry *)
     (* --------------------------------------------------------- *)
     | [] ->
         Common.usage usage_msg (options());
         failwith "too few arguments"

     | x::y::xs ->
         Common.usage usage_msg (options());
         failwith "too many arguments"
    )
  )

(*****************************************************************************)
let _ =
  Common.main_boilerplate (fun () ->
    main ();
  )

(*e: main_treemap.ml *)
