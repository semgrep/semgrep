(*
 * Please imagine a long and boring gnu-style copyright notice
 * appearing just here.
 *)
open Common

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* Extract the "core", the essential parts of a codebase, the signal versus
 * the noise!
 *
 * Using codemap is great to navigate a large codebase and codegraph is
 * great to get a first high level view of its software architecture,
 * but on very very large codebase, it is still very hard to find and
 * understand the "core" of a software. For instance on the Linux kernel,
 * the drivers take more than 50% of the codebase, but none of those
 * drivers are actually essential to understand the "core" of Linux.
 * Once you''ve seen one driver you got the main ideas and seeing
 * another driver will not improve significantly your comprehension
 * of the whole codebase. In some cases such as www, the whole codebase is so
 * messy that even codegraph has a hard time to convey the software
 * architecture as it''s difficult to find a meaningful layering of the code
 * because of the too many backward dependencies.
 *
 * Fortunately in most codebase a lots of things are actual plugins
 * or extensions of a core (for Linux it is the many device drivers,
 * file systems, internet protocols, etc). The goal of codeslicer is
 * to detect those less important extensions and to offer a view of
 * the codebase where only the essential things have been kept.
 * The resulting codebase will hopefully be far smaller and have
 * better layering properties. One can then use codegraph and codemap
 * on this subset.
 *
 * Note that the codemap/codegraph slicing feature offers in part
 * the "get a subset of a codebase" functionality described here. But codemap
 * requires the programmer to know where to start slicing from (usually
 * the main()) and the slice can actually contain many extensions.
 *
 * A nice side effect of the codeslicer is that because the resulting codebase
 * is far smaller it''s also faster to run expensive analysis that are
 * currently hard to scale to millions LOC (e.g. datalog, but even codegraph
 * and codemap which have troubles to scale to www).
 *
 * history:
 *  - I had a simple code slicer using graph_code that I used to get
 *    all the code relevant to arc build (on which I could run
 *    codegraph with class analysis ON)
 *  - I was doing lots of manual codeslicing when working on Kernel.tex.nw
 *    by removing many device drivers, internet protocols, file systems
 *  - I was doing even more manual codeslicing when LPizing the whole plan9
 *    by removing support for many architectures, hosts, less important
 *    programs, compatability with other operating systems, less important
 *    or obsolete features. Then came the idea of trying to automate this
 *    codeslicing, especially for www.
*)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

(* In addition to flags that can be tweaked via -xxx options (cf the
 * full list of options in the "the options" section below), this
 * program also depends on external files ?
*)

let output_dir = ref "CODESLICER"

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* Some debugging functions *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)
let main_action _xs =
  raise Todo

(*****************************************************************************)
(* Extra Actions *)
(*****************************************************************************)
module GC = Graph_code

let big_parent_branching_factor = 5


let find_big_branching_factor graph_file =
  let g = Graph_code.load graph_file in
  let hierarchy = Graph_code_class_analysis.class_hierarchy g in

  pr2 (spf "step0: number of nodes = %d" (Graph_code.nb_nodes g));

  (* step1: find the big parents and the children candidates to remove *)

  let big_parents =
    hierarchy |> Graphe.nodes |> List.filter (fun parent ->
      let children = Graphe.succ parent hierarchy in
      (* should modulate by the branching factor of the parent? *)
      List.length children > big_parent_branching_factor
    )
  in

  let hierarchy_transitive = Graphe.transitive_closure hierarchy in

  (* initial set *)
  let dead_candidates () =
    big_parents
    (* Inheritance transtive closure,
     * todo: could keep 1? the biggest one in terms of use?
    *)
    |> List.map (fun parent -> Graphe.succ parent hierarchy_transitive)
    |> List.flatten
    (* Has transitive closure, to also remove the fields, methods of a class *)
    |> List.map (fun node -> Graph_code.node_and_all_children node g)
    |> List.flatten
  in

  let hdead_candidates = Common.hashset_of_list (dead_candidates ()) in

  pr2 (spf "step1: big parents = %d, initial candidates for removal = %d"
         (List.length big_parents)
         (Hashtbl.length hdead_candidates));


  let progress = ref true in
  let pass = ref 0 in
  let last_cnt = ref 0 in

  let hlive_done = Hashtbl.create 101 in
  let hlivermakes_stats = Hashtbl.create 101 in

  while !progress do

    (* ok the previous phase may have discovered newly dead code, that
     * may have rendered live children of big parents originally,
     * but that should not anymore, so let''s reconsider all children!
    *)
    dead_candidates () |> List.iter (fun node ->
      Hashtbl.replace hdead_candidates node true
    );

    (* step2: make sure none of the candidate are used by live entities *)

    let live = ref (Graph_code.all_nodes g |> Common.exclude (fun node ->
      Hashtbl.mem hdead_candidates node
    ))
    in

    let make_live ~from node =
      if not (Hashtbl.mem hlive_done (from, node))
      then begin
        Hashtbl.add hlive_done (from, node) true;
        Hashtbl.replace hlivermakes_stats from
          (1+(try Hashtbl.find hlivermakes_stats from with Not_found -> 0));
      end;
      let xs = Graph_code.node_and_all_children node g in
      xs |> List.iter (fun node ->
        Hashtbl.remove hdead_candidates node;
        Common.push node live
      )
    in

    while !live <> [] do
      let this_round = !live in
      live := [];

      this_round |> List.iter (fun live_node ->
        let uses = Graph_code.succ live_node Graph_code.Use g in
        uses |> List.iter (fun use_of_live_node ->
          if Hashtbl.mem hdead_candidates use_of_live_node then begin
            make_live use_of_live_node ~from:live_node
          end
        )
      )
    done;

    pr2 (spf "step2(%d): candidates for removal = %d" !pass
           (Hashtbl.length hdead_candidates));

    (* step3: mark as live parent (in the Has sense) of live entities *)
    (* TODO *)

    (* step4: remove newly dead code (helpers of removed classes) *)

    let users_of_node = Graph_code.mk_eff_use_pred g in
    let dead = ref (Common.hashset_to_list hdead_candidates) in
    progress := false;

    let make_dead node =
      let xs = Graph_code.node_and_all_children node g in
      xs |> List.iter (fun node ->
        Hashtbl.replace hdead_candidates node true;
        Common.push node dead;
        (* a newly dead, should reconsider children of original
         * big parents!
        *)
        progress := true;
      )
    in

    while !dead <> [] do
      let this_round = !dead in
      dead := [];

      this_round |> List.iter (fun dead_node ->
        let uses = Graph_code.succ dead_node Graph_code.Use g in
        let live_uses_of_dead_code =
          uses |> Common.exclude (fun node -> Hashtbl.mem hdead_candidates node)
        in

        live_uses_of_dead_code |> List.iter (fun live_use_of_dead_node ->
          let xs =
            let node = live_use_of_dead_node in
            Graph_code.node_and_all_children node g
          in
          let hxs = Common.hashset_of_list xs in

          let users =
            xs |> List.map (fun node -> users_of_node node) |> List.flatten in

          (* maybe a newly dead! *)
          if users |> List.for_all (fun node ->
            Hashtbl.mem hdead_candidates node ||
            Hashtbl.mem hxs node
          )
          then make_dead live_use_of_dead_node
        )
      )
    done;

    pr2 (spf "step4(%d): candidates for removal = %d" !pass
           (Hashtbl.length hdead_candidates));

    (* debug
       hdead_candidates +> Common.hashset_to_list +> List.iter (fun node ->
        pr (spf "DEAD: %s" (Graph_code.string_of_node node))
       );
    *)
    incr pass;
    if Hashtbl.length hdead_candidates = !last_cnt
    then progress := false
    else last_cnt := Hashtbl.length hdead_candidates
  done;

  (* step5: slice the code! *)

  hlivermakes_stats |> Common.hash_to_list |> Common.sort_by_val_highfirst
  |> Common.take_safe 10 |> List.iter (fun (k, v) ->
    pr2 (spf "livemaker: %s (%d)"
           (Graph_code.string_of_node k) v)
  );

  (* First approximation *)

  (* later: do the actual slice of the code given a list of dead nodes
   * but will probably  need range info
  *)

  let files_to_remove =
    Graph_code.all_nodes g |> Common.map_filter (fun node ->
      match node with
      | filename, Entity_code.File ->
          (* ? should look for all children recursively? *)
          let children = Graph_code.children node g in

          (* debug
                  if filename =~ ".*AbstractDirectedGraphTestCase" then begin
                    children +> List.iter (fun child ->
                      if not (Hashtbl.mem hdead_candidates child)
                      then pr2_gen child
                    )
                  end;
          *)
          if children |> List.for_all
               (fun node -> Hashtbl.mem hdead_candidates node) &&
             (* for files like webroot/index.php with no entities *)
             List.length children >= 1
          then Some filename
          else None
      | _ -> None
    ) |> Common.sort
  in
  let dir = Filename.dirname graph_file in
  let file = Filename.concat dir "list_slicer" in
  pr2 (spf "generating data in %s" file);
  Common.with_open_outfile file (fun (pr_no_nl, _chan) ->
    let pr s = pr_no_nl (s ^ "\n") in
    files_to_remove |> List.iter pr
  );

  ()


module E = Entity_code

(* xs are the set of dirs or files we are interested in; the starting points
 * for the DFS.
*)
let extract_transitive_deps xs =
  let pwd = Sys.getcwd () in
  let graph_file = Filename.concat pwd Graph_code.default_filename in
  let g = Graph_code.load graph_file in

  let hdone = Hashtbl.create 101 in

  let max_depth = 4 in

  let start_nodes =
    xs |> List.map (fun path ->
      let node =
        if Sys.is_directory path
        then path, E.Dir
        else path, E.File
      in
      if not (GC.has_node node g)
      then failwith (spf "could not find %s" path);
      node
    )
  in
  let rec dfs depth xs =
    match xs with
    | [] -> ()
    (* www specific, do not include the transitive deps of flib_init() *)
    | ("flib_init", E.Function)::xs -> dfs depth xs
    | n::xs ->
        (if Hashtbl.mem hdone n || depth > max_depth
         then ()
         else begin
           Hashtbl.add hdone n true;
           let uses = GC.succ n GC.Use g in
           dfs (depth + 1) uses;
           let children = GC.children n g in
           (* we want all children, especially subdirectories *)
           dfs (depth + 0) children
         end);
        dfs depth xs

  in
  dfs 0 start_nodes;
  let files = hdone |> Common.hashset_to_list |> Common.map_filter (fun n ->
    try
      let file = GC.file_of_node n g in
      Some file
    with Not_found -> None
  ) |> Common.hashset_of_list |> Common.hashset_to_list in
  (*pr2 (spf "%d" (List.length files));*)
  files |> List.iter pr;
  let dir = !output_dir in
  Common.command2 (spf "mkdir -p %s" dir);
  files |> List.iter (fun file ->
    let subdir = Filename.dirname file in
    Common.command2 (spf "mkdir -p %s/%s" dir subdir);
    Common.command2 (spf "cp %s %s/%s" file dir subdir);
  )


let slice_dir_with_file file =
  let file = Common.fullpath file in
  let dir = Filename.dirname file in
  let dotdir = Filename.dirname dir in
  let dst = Filename.concat dotdir "CODESLICER" in
  Common2.command2_y_or_no_exit_if_no (spf "rm -rf %s" dst);
  let cmd = spf "cp -a %s %s" dir dst in
  pr2 cmd;
  Common.command2 cmd;
  Common.cat file |> List.iter (fun file ->
    Common.command2 (spf "rm -f %s/%s" dst file)
  )



(* ---------------------------------------------------------------------- *)
let pfff_extra_actions () = [
  "-extract_transitive_deps", " <files or dirs> (works with -o)",
  Common.mk_action_n_arg extract_transitive_deps;
  "-find_big_branching_factor", " <file>",
  Common.mk_action_1_arg find_big_branching_factor;
  "-slice_dir_with_file", " <list_slicer_file>",
  Common.mk_action_1_arg slice_dir_with_file;
]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () =
  pfff_extra_actions() @
  []

let options () = [
  "-o", Arg.Set_string output_dir,
  " <dir> generate codeslice in dir";

] @
  Flag_parsing.cmdline_flags_verbose () @
  Flag_parsing_cpp.cmdline_flags_debugging () @
  Flag_parsing_cpp.cmdline_flags_macrofile () @

  Common.options_of_actions action (all_actions()) @
  Common2.cmdline_flags_devel () @
  Common2.cmdline_flags_other () @
  [
    "-version",   Arg.Unit (fun () ->
      pr2 (spf "codeslicer version: %s" Config_pfff.version);
      exit 0;
    ), "  guess what";
  ]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () =

  Gc.set {(Gc.get ()) with Gc.stack_limit = 1000 * 1024 * 1024};
  (* Common_extra.set_link();
     let argv = Features.Distribution.mpi_adjust_argv Sys.argv in
  *)

  let usage_msg =
    "Usage: " ^ Common2.basename Sys.argv.(0) ^
    " [options] <file or dir> " ^ "\n" ^ "Options are:"
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
     | x::xs ->
         main_action (x::xs)

     (* --------------------------------------------------------- *)
     (* empty entry *)
     (* --------------------------------------------------------- *)
     | [] ->
         Common.usage usage_msg (options());
         failwith "too few arguments"
    )
  )

(*****************************************************************************)
let _ =
  Common.main_boilerplate (fun () ->
    main ();
  )
