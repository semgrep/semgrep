open Dependencies_matrix_code
module E = Entity_code
module G = Graph_code
module DM = Dependencies_matrix_code
module DMBuild = Dependencies_matrix_build

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Data *)
(*****************************************************************************)
let build_g_and_dm () =
  let g = G.create () in
  g |> G.add_node (".", E.Dir);
  g |> G.add_node ("foo.ml", E.File);
  g |> G.add_node ("a", E.Dir);
  g |> G.add_node ("a/x.ml", E.File);
  g |> G.add_node ("a/y.ml", E.File);
  g |> G.add_node ("bar.ml", E.File);
  g |> G.add_edge ((".", E.Dir), ("foo.ml", E.File)) G.Has;
  g |> G.add_edge ((".", E.Dir), ("bar.ml", E.File)) G.Has;
  g |> G.add_edge ((".", E.Dir), ("a", E.Dir)) G.Has;
  g |> G.add_edge (("a", E.Dir), ("a/x.ml", E.File)) G.Has;
  g |> G.add_edge (("a", E.Dir), ("a/y.ml", E.File)) G.Has;

  g |> G.add_edge (("a/x.ml", E.File), ("foo.ml", E.File)) G.Use;
  g |> G.add_edge (("a/y.ml", E.File), ("foo.ml", E.File)) G.Use;
  g |> G.add_edge (("bar.ml", E.File), ("foo.ml", E.File)) G.Use;
  g |> G.add_edge (("a/y.ml", E.File), ("a/x.ml", E.File)) G.Use;
  g |> G.add_edge (("bar.ml", E.File), ("a/y.ml", E.File)) G.Use;

  let dm = {
    matrix = [|
      [| 0; 0; 0; 0|];
      [| 1; 0; 0; 0|];
      [| 1; 2; 0; 0|];
      [| 1; 0; 3; 0|];
    |];
    name_to_i = Common.hash_of_list [
      ("foo.ml", E.File), 0;
      ("a/x.ml", E.File), 1;
      ("a/y.ml", E.File), 2;
      ("bar.ml", E.File), 3;
    ];
    i_to_name = [|
      ("foo.ml", E.File);
      ("a/x.ml", E.File);
      ("a/y.ml", E.File);
      ("bar.ml", E.File);
    |];
    config =
      Node ((".", E.Dir), [
        Node (("foo.ml", E.File), []);
        Node (("a", E.Dir), [
          Node (("a/x.ml", E.File), []);
          Node (("a/y.ml", E.File), []);
        ]);
        Node (("bar.ml", E.File), []);
      ]);
  } in
  g, dm

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let tests ~graph_of_string =
  Testutil.pack_suites "graph_code" [

    (*---------------------------------------------------------------------------*)
    (* The graph *)
    (*---------------------------------------------------------------------------*)
    Testutil.pack_tests "graph" [

      "scc", (fun () ->
        let g = G.create () in
        let (-->) f1 f2 =
          let f1 = f1, E.Function in
          let f2 = f2, E.Function in
          if not (G.has_node f1 g)
          then G.add_node f1 g;
          if not (G.has_node f2 g)
          then G.add_node f2 g;
          G.add_edge (f1, f2) G.Use g
        in
        (* foo -> bar <-> bar_mutual
         *          \
         *           -> bar_bis
        *)
        "foo" --> "bar";
        "bar" --> "bar_mutual";
        "bar_mutual" --> "bar";
        "bar" --> "bar_bis";

        let (scc, _hscc) = G.strongly_connected_components_use_graph g in
        Alcotest.(check bool)
          "it should find the right strongly connected components"
          true (scc = [|
            [("bar_bis", E.Function)];
            [("bar_mutual", E.Function); ("bar", E.Function)];
            [("foo", E.Function)]
          |]);

        let numbering = G.top_down_numbering g in
        let xs = Common.hash_to_list numbering |> Common.sort_by_val_lowfirst in
        Alcotest.(check bool)
          "it should find the right ordering of nodes"
          true
          (xs = [
             ("foo", E.Function), 0;
             ("bar", E.Function), 1;
             ("bar_mutual", E.Function), 1;
             ("bar_bis", E.Function), 2;
           ]);

        let numbering = G.bottom_up_numbering g in
        let xs = Common.hash_to_list numbering |> Common.sort_by_val_lowfirst in
        Alcotest.(check bool)
          "it should find the right ordering of nodes"
          true
          (xs = [
             ("bar_bis", E.Function), 0;
             ("bar", E.Function), 1;
             ("bar_mutual", E.Function), 1;
             ("foo", E.Function), 2;
           ]);
      );


      "adjust graph", (fun () ->
        let (g, _dm) = build_g_and_dm () in
        let adjust = [("a", "EXTRA_DIR")] in
        Graph_code.adjust_graph g adjust [];
        let gopti = Graph_code_opti.convert g in
        let config = DM.basic_config g in
        let _dm = DMBuild.build config None gopti in
        ()
      );

      "create fake dotdotdot entries", (fun () ->
        let (g, _dm) = build_g_and_dm () in
        let gopti = Graph_code_opti.convert g in
        Common.save_excursion DMBuild.threshold_pack 2 (fun () ->
          let config = DM.basic_config_opti gopti in
          let dm, gopti = DMBuild.build config None gopti in
          let config2 =
            DM.expand_node_opti ("./...", E.Dir) dm.config gopti in
          let dm, gopti = DMBuild.build config2 None gopti in
          (* pr2_gen dm; *)
          let _xs = DM.explain_cell_list_use_edges (1, 0) dm gopti in
          (* pr2_gen xs *)
          ()
        )
      );

(*
      "uses and users of file XXX", (fun () ->
        let g = G.create () in
        let nodeinfo f =
          { G.
            props = [];
            pos = { Parse_info.
              str = ""; charpos = -1; line = 1; column = 0;
              file = fst f ^ ".php";
            };
          }
        in
        let (-->) f1 f2 =
          let f1 = f1, E.Function in
          let f2 = f2, E.Function in
          if not (G.has_node f1 g)
          then begin
            G.add_node f1 g;
            G.add_nodeinfo f1 (nodeinfo f1) g;
          end;
          if not (G.has_node f2 g)
          then begin
            G.add_node f2 g;
            G.add_nodeinfo f2 (nodeinfo f2) g;
          end;
          G.add_edge (f1, f2) G.Use g
        in
        (* foo.php -> bar.php <-> bar_mutial.php
         *  \
         *   -> bar_bis.php
         *)
        "foo" --> "bar";
        "bar" --> "bar_mutual";
        "bar_mutual" --> "bar";
        "bar" --> "bar_bis";

        let uses_of_file, users_of_file =
          Graph_code_analysis.build_uses_and_users_of_file g in
        let uses = List.assoc "bar.php" uses_of_file in
        let users = List.assoc "bar.php" users_of_file in
        Alcotest.(check bool)
          "it should find all uses"
          ["bar_bis.php"; "bar_mutual.php"]
          uses;
        Alcotest.(check bool)
          "it should find all users"
          ["bar_mutual.php"; "foo.php"]
          users;
      );
*)

      "class analysis", (fun () ->
        let file_content = "
class A {
public function foo() { }
}
class B extends A {
public function foo() { }
}
class C {
public function foo() { }
}
" in
        let g = graph_of_string file_content in
        let dag = Graph_code_class_analysis.class_hierarchy g in

        let node = ("A", E.Class) in
        let children = Graphe.succ node dag in
        Alcotest.(check (list string))
          "it should find the direct children of a class"
          ["B"]
          (children |> List.map fst);

        let dag = Graph_code_class_analysis.class_hierarchy g in
        let hmethods = Graph_code_class_analysis.toplevel_methods g dag in
        let xs = Hashtbl.find_all hmethods "foo" in
        Alcotest.(check (list string))
          "it should find the toplevel methods"
          ["C.foo";"A.foo"]
          (xs |> List.map fst);

        let node = ("A.foo", E.Method) in
        let methods = Graph_code_class_analysis.dispatched_methods g dag node in
        Alcotest.(check (list string))
          "it should find the dispatched methods"
          ["B.foo"]
          (methods |> List.map fst);
      );
    ];

    (*---------------------------------------------------------------------------*)
    (* The matrix *)
    (*---------------------------------------------------------------------------*)

    Testutil.pack_tests "dm" [

      "dead columns", (fun () ->
        let (_, dm) = build_g_and_dm () in
        Alcotest.(check bool) "same bools" false (DM.is_dead_column 0 dm);
        Alcotest.(check bool) "same bools" true (DM.is_dead_column 3 dm);
        ()
      );
      "internal helpers", (fun () ->
        let (_, dm) = build_g_and_dm () in
        let arr = DM.parents_of_indexes dm in
        Alcotest.(check bool)
          "same values" true
          (arr =
           [| [(".", E.Dir)];
              [(".", E.Dir); ("a", E.Dir); ];
              [(".", E.Dir); ("a", E.Dir); ];
              [(".", E.Dir)];
           |]);
        Alcotest.(check int)
          "It should not find distance between foo.ml and a/x.ml"
          (DM.distance_entity (0, 1) arr) 0;
        Alcotest.(check int)
          "It should find distance between a/x.ml and foo.ml"
          (DM.distance_entity (1, 0) arr) 1;
        Alcotest.(check int)
          "It should not find distance between a/x.ml a/y.ml"
          (DM.distance_entity (1, 2) arr) 0;

        Alcotest.(check bool)
          "same values" false (DM.is_internal_helper 0 dm);
        Alcotest.(check bool)
          "same values" true (DM.is_internal_helper 1 dm);
        Alcotest.(check bool)
          "same values" false (DM.is_internal_helper 2 dm);
      );

      "explain cell", (fun () ->
        let (g, dm) = build_g_and_dm () in
        let gopti = Graph_code_opti.convert g in
        let xs = DM.explain_cell_list_use_edges (2, 1) dm gopti in
        Alcotest.(check bool) "same values" true
          (xs = [
             ("a/y.ml", E.File), ("a/x.ml", E.File);
           ]);
      );
    ]
  ]
