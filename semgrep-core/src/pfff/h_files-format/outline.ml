open Common

(*****************************************************************************)
(* The data structure *)
(*****************************************************************************)

type outline_node = {
  stars: string;
  title: string;
  before_first_children: string list;
}
type outline = outline_node Common2.tree2

let outline_default_regexp = "^\\(\\*+\\)[ ]*\\(.*\\)"

let root_stars = ""
let root_title = "__ROOT__"

(*****************************************************************************)
(* Helpers, accessors *)
(*****************************************************************************)

let is_root_node node =
  String.length node.stars = 0 &&
  node.title = root_title


let extract_outline_line ?(outline_regexp=outline_default_regexp) s =
  if s =~ outline_regexp
  then matched2 s
  else failwith (spf "line does not match regexp: %s  vs %s" s outline_regexp)


(*****************************************************************************)
(* Loading, saving *)
(*****************************************************************************)

(* Similar to parenthesizd expression parsing, or ifdef parsing as
 * in parsing_hacks, but a little different cos don't have the
 * end delimiter in most cases. The end delimiter is in fact
 * the start of a new header or the end of the file.
*)
let parse_outline ?(outline_regexp=outline_default_regexp) file =
  let xs = Common.cat file in

  (* just differentiate outline lines from regular lines *)
  let headers_or_not =
    xs |> List.map (fun s ->
      if s =~ outline_regexp
      then
        let (stars, line) = extract_outline_line ~outline_regexp s in
        Left (String.length stars, stars, line)
      else
        Right s
    )
  in
  let root = (0, root_stars, root_title) in
  (* pack the Right with each appropriate Left *)
  let headers =
    let rec aux (acc_right, outline) xs =
      match xs with
      | [] -> [(outline, List.rev acc_right)]
      | x::xs ->
          (match x with
           | Right regular ->
               aux (regular::acc_right, outline) xs
           | Left outline2 ->
               (outline, List.rev acc_right)::aux ([], outline2) xs
          )
    in
    aux ([], root) headers_or_not
  in

  (* build the tree *)
  let trees =
    let rec aux_outline xs =
      match xs with
      | [] -> []
      | x::xs ->
          let ((lvl, stars, title), before_first_children) = x in

          let (children, rest) = xs |> Common2.span (fun x2 ->
            let ((lvl2, _, _), _) = x2 in
            lvl2 > lvl
          )
          in
          let node =
            { stars = stars;
              title = title;
              before_first_children = before_first_children;
            }
          in
          let children_trees = aux_outline children in

          (Common2.Tree (node, children_trees))::aux_outline rest
    in
    aux_outline headers
  in
  match trees with
  | [root] -> root
  | _ -> failwith "wierd, multiple roots"



let write_outline outline file =
  Common.with_open_outfile file (fun (pr_no_nl, _chan) ->
    let pr s = pr_no_nl (s ^ "\n") in

    outline |> Common2.tree2_iter (fun node ->
      if not (is_root_node node)
      then pr (node.stars ^ node.title);

      node.before_first_children |> List.iter pr;
    );
  )
