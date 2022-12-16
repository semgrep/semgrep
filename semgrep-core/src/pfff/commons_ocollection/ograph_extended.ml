open Common

open Oassocb
open Osetb

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * An imperative directed polymorphic graph.
 *
 * todo?: prendre en parametre le type de finitemap et set?
 * todo?: add_arc doit ramer, car del la key, puis add. Better to
 *  have a ref to a set?
 *
 * opti: graph with pointers and a tag visited => need keep global value
 *  visited_counter.  check(that node is in, ...), display.
 * opti: when the graph structure is stable, have a method compact,  that
 *  transforms that in a matrix (assert that all number between 0 and
 *  free_index are used,  or do some defrag-like-move/renaming).
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type nodei = int

(*****************************************************************************)
(* Pure version *)
(*****************************************************************************)

class ['a,'b] ograph_extended =
  let build_assoc () = new oassocb [] in (* opti?: = oassoch *)
  let build_set ()   = new osetb Set_.empty in

  object(_o)
    (* inherit ['a] ograph *)

    val free_index = 0

    val succ = build_assoc()
    val pred = build_assoc()
    val nods = build_assoc()

    method add_node (e: 'a) =
      let i = free_index in
      ({<
        nods = nods#add (i, e);
        pred = pred#add (i, build_set() );
        succ = succ#add (i, build_set() );
        free_index = i + 1;
      >}, i)

    method add_nodei i (e: 'a) =
      ({<
        nods = nods#add (i, e);
        pred = pred#add (i, build_set() );
        succ = succ#add (i, build_set() );
        free_index = (max free_index i) + 1;
      >}, i)


    method del_node (i) =
      {<
        (* check: e is effectively the index associated with e,
           and check that already in *)

        (* todo: assert that have no pred and succ, otherwise
         * will have some dangling pointers
        *)
        nods = nods#delkey i;
        pred = pred#delkey i;
        succ = succ#delkey i;
      >}

    method replace_node (i, (e: 'a)) =
      assert (nods#haskey i);
      {<
        nods = nods#replkey (i, e);
      >}

    method add_arc ((a,b),(v: 'b)) =
      {<
        succ = succ#replkey (a, (succ#find a)#add (b, v));
        pred = pred#replkey (b, (pred#find b)#add (a, v));
      >}
    method del_arc ((a,b),v) =
      {<
        succ = succ#replkey (a, (succ#find a)#del (b,v));
        pred = pred#replkey (b, (pred#find b)#del (a,v));
      >}

    method successors   e = succ#find e
    method predecessors e = pred#find e

    method nodes = nods
    method allsuccessors = succ

(*
    method ancestors xs =
      let rec aux xs acc =
        match xs#view with (* could be done with an iter *)
        | Empty -> acc
        | Cons(x, xs) -> (acc#add x)
              +> (fun newacc -> aux (o#predecessors x) newacc)
              +> (fun newacc -> aux xs newacc)
      in aux xs (f2()) (* (new osetb []) *)

    method children  xs =
      let rec aux xs acc =
        match xs#view with (* could be done with an iter *)
        | Empty -> acc
        | Cons(x, xs) -> (acc#add x)
              +> (fun newacc -> aux (o#successors x) newacc)
              +> (fun newacc -> aux xs newacc)
      in aux xs (f2()) (* (new osetb []) *)

    method brothers  x =
      let parents = o#predecessors x in
      (parents#fold (fun acc e -> acc $++$ o#successors e) (f2()))#del x

*)

  end

(*****************************************************************************)
(* Mutable version *)
(*****************************************************************************)

class ['a,'b] ograph_mutable =
  let build_assoc () = new oassocb [] in
  let build_set ()   = new osetb Set_.empty in

  object(o)

    val mutable free_index = 0

    val mutable succ = build_assoc()
    val mutable pred = build_assoc()
    val mutable nods = build_assoc()

    method add_node (e: 'a) =
      let i = free_index in
      nods <- nods#add (i, e);
      pred <- pred#add (i, build_set() );
      succ <- succ#add (i, build_set() );
      free_index <- i + 1;
      i

    method add_nodei i (e: 'a) =
      nods <- nods#add (i, e);
      pred <- pred#add (i, build_set() );
      succ <- succ#add (i, build_set() );
      free_index <- (max free_index i) + 1;


    method del_node (i) =
      (* check: e is effectively the index associated with e,
         and check that already in *)

      (* todo: assert that have no pred and succ, otherwise
       * will have some dangling pointers
      *)
      nods <- nods#delkey i;
      pred <- pred#delkey i;
      succ <- succ#delkey i;

    method replace_node (i, (e: 'a)) =
      assert (nods#haskey i);
      nods <- nods#replkey (i, e);

    method add_arc ((a,b),(v: 'b)) =
      succ <- succ#replkey (a, (succ#find a)#add (b, v));
      pred <- pred#replkey (b, (pred#find b)#add (a, v));
    method del_arc ((a,b),v) =
      succ <- succ#replkey (a, (succ#find a)#del (b,v));
      pred <- pred#replkey (b, (pred#find b)#del (a,v));

    method successors   e = succ#find e
    method predecessors e = pred#find e

    method nodes = nods
    method allsuccessors = succ

    method nb_nodes =
      nods#length

    method nb_edges =
      nods#fold (fun acc (i, _e) ->
        let children = o#successors i in
        acc + children#cardinal
      ) 0

  end

(*****************************************************************************)
(* API *)
(*****************************************************************************)

(* depth first search *)
let dfs_iter xi f g =
  let already = Hashtbl.create 101 in
  let rec aux_dfs xs =
    xs |> List.iter (fun xi ->
      if Hashtbl.mem already xi then ()
      else begin
        Hashtbl.add already xi true;
        f xi;
        let succ = g#successors xi in
        aux_dfs (succ#tolist |> List.map fst);
      end
    ) in
  aux_dfs [xi]


let dfs_iter_with_path xi f g =
  let already = Hashtbl.create 101 in
  let rec aux_dfs path xi =
    if Hashtbl.mem already xi then ()
    else begin
      Hashtbl.add already xi true;
      f xi path;
      let succ = g#successors xi in
      let succ' = succ#tolist |> List.map fst in
      succ' |> List.iter (fun yi ->
        aux_dfs (xi::path) yi
      );
    end
  in
  aux_dfs [] xi



let generate_ograph_generic g label fnode filename =
  Common.with_open_outfile filename (fun (pr,_) ->
    pr "digraph misc {\n" ;
    pr "size = \"10,10\";\n" ;
    (match label with
       None -> ()
     | Some x -> pr (Printf.sprintf "label = \"%s\";\n" x));

    let nodes = g#nodes in
    nodes#iter (fun (k,node) ->
      let (str,border_color,inner_color) = fnode (k, node) in
      let color =
        match inner_color with
          None ->
            (match border_color with
               None -> ""
             | Some x -> Printf.sprintf ", style=\"setlinewidth(3)\", color = %s" x)
        | Some x ->
            (match border_color with
               None -> Printf.sprintf ", style=\"setlinewidth(3),filled\", fillcolor = %s" x
             | Some x' -> Printf.sprintf ", style=\"setlinewidth(3),filled\", fillcolor = %s, color = %s" x x') in
      (* so can see if nodes without arcs were created *)
      pr (spf "%d [label=\"%s   [%d]\"%s];\n" k str k color)
    );

    nodes#iter (fun (k,_node) ->
      let succ = g#successors k in
      succ#iter (fun (j,_edge) ->
        pr (spf "%d -> %d;\n" k j);
      );
    );
    pr "}\n" ;
  );
  ()


let generate_ograph_xxx g filename =
  with_open_outfile filename (fun (pr,_) ->
    pr "digraph misc {\n" ;
    pr "size = \"10,10\";\n" ;

    let nodes = g#nodes in
    nodes#iter (fun (k,(_node, s)) ->
      (* so can see if nodes without arcs were created *)
      pr (spf "%d [label=\"%s   [%d]\"];\n" k s k)
    );

    nodes#iter (fun (k,_node) ->
      let succ = g#successors k in
      succ#iter (fun (j,_edge) ->
        pr (spf "%d -> %d;\n" k j);
      );
    );
    pr "}\n" ;
  );
  ()


let get_os =
  let os = lazy (
    let ic = Unix.open_process_in "uname" in
    Fun.protect
      (fun () ->
         let uname = input_line ic in
         match uname with
         | "Darwin" -> `MacOs
         | "Linux" -> `Linux
         | _ -> `Unknown
      )
      ~finally:(fun () -> ignore (Unix.close_process_in ic))
  ) in
  fun () -> Lazy.force os

let launch_png_cmd filename =
  let _status =
    Unix.system (Printf.sprintf "dot -Tpng %s -o %s.png" filename filename) in
  let _status =
    Unix.system (Printf.sprintf "open %s.png" filename)
  in ()

let launch_gv_cmd filename =
  let _status =
    Unix.system ("dot " ^ filename ^ " -Tps  -o " ^ filename ^ ".ps;") in
  let _status =
    Unix.system ("gv " ^ filename ^ ".ps")
  in
  (* zarb: I needed this when I launch the program with '&' via eshell,
   * otherwise gv did not get the chance to be launched
   * Unix.sleep 1;
  *)
  ()

let display_graph_cmd filename =
  match get_os () with
  | `MacOs -> launch_png_cmd filename
  | `Linux -> launch_gv_cmd filename
  | `Unknown -> ()

let print_ograph_extended g filename display_graph =
  generate_ograph_xxx g filename;
  if display_graph then display_graph_cmd filename

let print_ograph_mutable g filename display_graph =
  generate_ograph_xxx g filename;
  if display_graph then display_graph_cmd filename

let print_ograph_mutable_generic
    ?(title=None)
    ?(display_graph = true)
    ?(output_file = "/tmp/ograph.dot")
    ~s_of_node
    g =
  generate_ograph_generic g title s_of_node output_file;
  if display_graph then display_graph_cmd output_file
