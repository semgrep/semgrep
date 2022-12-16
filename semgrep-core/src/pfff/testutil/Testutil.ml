(*
   Utilities for writing test suites for Alcotest.
*)

open Printf

type test = string * (unit -> unit)

(*
   We use '>' because '.' is common in files and we don't want to split
   'foo.py' into 'foo' and 'py'.

   Alternatively, we could keep paths as string lists and avoid the issue
   of choosing a separator.
*)
let path_sep = '>'

let path_sep_str = String.make 1 path_sep

let pretty_path_sep_str = " > "

let list_map f l = List.rev_map f l |> List.rev

let list_flatten ll =
  List.fold_left (fun acc l -> List.rev_append l acc) [] ll |> List.rev

let pack_tests suite_name (tests : test list) : test list =
  list_map (fun (path, func) -> (suite_name ^ path_sep_str ^ path, func)) tests

let pack_suites suite_name (tests : test list list) : test list =
  tests |> list_flatten |> pack_tests suite_name

(*
   Sort by path. For this, we split the paths on '>' and then take advantage
   of the polymorphic 'compare' which does the right thing.

   Compare:
     compare "a>b" "a b" = 1  (* wrong *)
   vs.
     compare ["a"; "b"] ["a b"] = -1  (* correct *)
*)
let sort (tests : test list) : test list =
  tests
  |> list_map (fun ((name, _func) as test) ->
    let k = String.split_on_char path_sep name in
    (k, test))
  |> List.stable_sort (fun (a, _) (b, _) -> compare a b)
  |> list_map snd

(*
   "Foo.Bar.hello" -> ("Foo.Bar", "hello")
   "hello" -> ("", "hello")
   "" -> ("", "")
*)
let split_path s =
  match String.rindex_opt s path_sep with
  | None -> ("", s)
  | Some dot_pos ->
      let left_len = dot_pos in
      let right_len = String.length s - left_len - 1 in
      (String.sub s 0 left_len, String.sub s (dot_pos + 1) right_len)

(*
   Group pairs by the first value of the pair, preserving the original
   order as much as possible.
*)
let group_by_key key_value_list =
  let tbl = Hashtbl.create 100 in
  key_value_list
  |> List.iteri (fun pos (k, v) ->
    let tbl_v =
      match Hashtbl.find_opt tbl k with
      | None -> (pos, [ v ])
      | Some (pos, vl) -> (pos, v :: vl)
    in
    Hashtbl.replace tbl k tbl_v);
  let clusters =
    Hashtbl.fold (fun k (pos, vl) acc -> (pos, (k, List.rev vl)) :: acc) tbl []
  in
  clusters
  |> List.sort (fun (pos1, _) (pos2, _) -> compare pos1 pos2)
  |> list_map snd

let use_pretty_path_separator path =
  path |> String.split_on_char path_sep |> String.concat pretty_path_sep_str

let to_alcotest ?(speed_level = `Quick) tests : unit Alcotest.test list =
  tests
  |> list_map (fun (path, func) ->
    let category, name = split_path path in
    let category =
      match category with
      | "" -> name
      | s -> s
    in
    let pretty_category = use_pretty_path_separator category in
    (pretty_category, (name, speed_level, func)))
  |> group_by_key

let make_pcre_filter pat =
  let re =
    try Re.Pcre.re pat |> Re.compile
    with e ->
      failwith (
        Printf.sprintf "Cannot parse PCRE pattern '%s': %s"
          pat
          (Printexc.to_string e)
      )
  in
  fun s -> Re.matches re s <> []

let filter ?substring ?pcre tests =
  let has_substring =
    match substring with
    | None -> (fun _ -> true)
    | Some sub ->
        let re = Re.str sub |> Re.compile in
        fun s -> Re.matches re s <> []
  in
  let matches_pcre =
    match pcre with
    | None -> (fun _ -> true)
    | Some pat -> make_pcre_filter pat
  in
  tests
  |> List.filter (fun (path, _test) ->
    let pretty_path = use_pretty_path_separator path in
    (has_substring path
     || has_substring pretty_path)
    && (matches_pcre path
        || matches_pcre pretty_path)
  )

let run what f =
  printf "running %s...\n%!" what;
  Fun.protect
    ~finally:(fun () ->
      printf "done with %s.\n%!" what
    )
    f
