module E = Entity_code
module G = Graph_code

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

(* ---------------------------------------------------------------------- *)
(* Code graph *)
(* ---------------------------------------------------------------------- *)
let test_graph_code () =
  let g = G.create () in
  g |> G.add_node G.root;

  g |> G.add_node ("a", E.Dir);
  g |> G.add_node ("c", E.Dir);
  g |> G.add_node ("a/b", E.Dir);
  g |> G.add_node ("a/b/foo.php", E.File);
  g |> G.add_node ("c/bar.php", E.File);

  g |> G.add_edge (("a", E.Dir), ("a/b", E.Dir)) G.Has;
  g |> G.add_edge (("a/b", E.Dir), ("a/b/foo.php", E.File)) G.Has;
  g |> G.add_edge (("c", E.Dir), ("c/bar.php", E.File)) G.Has;

  g |> G.add_edge (("a/b/foo.php", E.File), ("c/bar.php", E.File)) G.Use;
  G.display_with_gv g;
  ()

let test_dsm file =
  let g = Graph_code.load file in
  let config = Dependencies_matrix_code.basic_config g in
  let gopti = Graph_code_opti.convert g in
  let dm,_ = Dependencies_matrix_build.build config None gopti in
  Dependencies_matrix_code.display dm;
  ()

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-test_graph_code", " <>",
  Common.mk_action_0_arg test_graph_code;
  "-test_dsm", " <file>",
  Common.mk_action_1_arg test_dsm;
]
