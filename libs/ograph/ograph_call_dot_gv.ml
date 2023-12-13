open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Call 'dot', 'gv', or 'open' to display a graph
 *)

(*****************************************************************************)
(* Dot generation *)
(*****************************************************************************)

let generate_ograph_generic g label fnode filename =
  UCommon.with_open_outfile filename (fun (xpr, _) ->
      xpr "digraph misc {\n";
      xpr "size = \"10,10\";\n";
      (match label with
      | None -> ()
      | Some x -> xpr (spf "label = \"%s\";\n" x));

      let nodes = g#nodes in
      nodes#iter (fun (k, node) ->
          let str, border_color, inner_color = fnode (k, node) in
          let color =
            match inner_color with
            | None -> (
                match border_color with
                | None -> ""
                | Some x -> spf ", style=\"setlinewidth(3)\", color = %s" x)
            | Some x -> (
                match border_color with
                | None ->
                    spf ", style=\"setlinewidth(3),filled\", fillcolor = %s" x
                | Some x' ->
                    spf
                      ", style=\"setlinewidth(3),filled\", fillcolor = %s, \
                       color = %s"
                      x x')
          in
          (* so can see if nodes without arcs were created *)
          xpr (spf "%d [label=\"%s   [%d]\"%s];\n" k str k color));

      nodes#iter (fun (k, _node) ->
          let succ = g#successors k in
          succ#iter (fun (j, _edge) -> xpr (spf "%d -> %d;\n" k j)));
      xpr "}\n");
  ()

let generate_ograph_xxx g filename =
  UCommon.with_open_outfile filename (fun (xpr, _) ->
      xpr "digraph misc {\n";
      xpr "size = \"10,10\";\n";

      let nodes = g#nodes in
      nodes#iter (fun (k, (_node, s)) ->
          (* so can see if nodes without arcs were created *)
          xpr (spf "%d [label=\"%s   [%d]\"];\n" k s k));

      nodes#iter (fun (k, _node) ->
          let succ = g#successors k in
          succ#iter (fun (j, _edge) -> xpr (spf "%d -> %d;\n" k j)));
      xpr "}\n");
  ()

(*****************************************************************************)
(* Visualization *)
(*****************************************************************************)

let get_os = lazy (Platform.kernel ())

(* TODO: switch from cmd_to_list to UCmd.status_of_run with
 * properly built Cmd, or even switch to CapExec!
 *)
let launch_png_cmd filename =
  UCmd.cmd_to_list (spf "dot -Tpng %s -o %s.png" filename filename) |> ignore;
  UCmd.cmd_to_list (spf "open %s.png" filename) |> ignore;
  ()

let launch_gv_cmd filename =
  UCmd.cmd_to_list ("dot " ^ filename ^ " -Tps  -o " ^ filename ^ ".ps;")
  |> ignore;
  UCmd.cmd_to_list ("gv " ^ filename ^ ".ps") |> ignore;
  (* weird: I needed this when I launch the program with '&' via eshell,
   * otherwise 'gv' did not get the chance to be launched
   * Unix.sleep 1;
   *)
  ()

let display_graph_cmd filename =
  match Lazy.force get_os with
  | Platform.Darwin -> launch_png_cmd filename
  | Platform.Linux -> launch_gv_cmd filename
  | Platform.OtherKernel _ -> ()

let print_ograph_extended g filename display_graph =
  generate_ograph_xxx g filename;
  if display_graph then display_graph_cmd filename

let print_ograph_mutable g filename display_graph =
  generate_ograph_xxx g filename;
  if display_graph then display_graph_cmd filename

let print_ograph_mutable_generic ?(title = None) ?(display_graph = true)
    ?(output_file = "/tmp/ograph.dot") ~s_of_node g =
  generate_ograph_generic g title s_of_node output_file;
  if display_graph then display_graph_cmd output_file
