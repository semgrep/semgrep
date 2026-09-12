(*
   Topological sorting of the type definitions.
*)

open Printf
open CST_grammar

let debug = false

let rec collect_names acc x =
  match x with
  | Repeat x
  | Repeat1 x
  | Optional x -> collect_names acc x
  | Seq l -> List.fold_left (fun acc x -> collect_names acc x) acc l
  | Choice l -> List.fold_left (fun acc (_, x) -> collect_names acc x) acc l
  | Symbol name -> name :: acc
  | Token _
  | Blank -> acc
  | Alias (name, _) -> name :: acc

let extract_rule_deps (rule : rule) =
  let deps = collect_names [] rule.body in
  if debug then
    printf "%s -> %s\n%!" rule.name (String.concat " " deps);
  (rule.name, deps)

(*
   Generic function on top of Tsort.sort_strongly_connected_components.
   Fails with exception if there's no entry for a dependency.

   The input is presorted so as to make the output insensitive to
   the input order.
*)
let tsort ~show_id ~get_deps elts =
  let deps_data =
    List.map (fun elt ->
      let id, deps = get_deps elt in
      let deps = List.sort compare deps in
      let self_dep = List.mem id deps in
      (id, deps, self_dep, elt)
    ) elts
    |> List.sort (fun (a, _, _, _) (b, _, _, _) -> compare a b)
  in
  let tbl = Hashtbl.create 100 in
  List.iter (fun ((id, _, _, _) as x) -> Hashtbl.replace tbl id x) deps_data;
  let tsort_input = List.map (fun (id, deps, _, _) -> (id, deps)) deps_data in
  let tsort_output = Tsort.sort_strongly_connected_components tsort_input in
  List.map (fun group ->
    List.map (fun id ->
      let _id, _deps, self_dep, elt =
        try Hashtbl.find tbl id
        with Not_found ->
          invalid_arg (sprintf "tsort: found an unknown dependency: %s"
                         (show_id id))
      in
      (self_dep, elt)
    ) group
  ) tsort_output

(*
   Sort a list of objects so as to minimize mutual dependencies in the
   generated code. Tsort works on elements IDs (rule names = strings).
   Here we take care of:
   - extracting the names of the dependencies of each rule
   - substituting the sorted names with the rules
*)
let sort rules =
  tsort ~show_id:(fun s -> s) ~get_deps:extract_rule_deps rules
