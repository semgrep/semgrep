(*
   Utilities for writing test suites for Alcotest.

   Keep in mind that some of this may become its own library or move to
   Alcotest.
*)

open Printf

type output = Stdout | Stderr | Merged_stdout_stderr | Separate_stdout_stderr

type 'a t = {
  category : string list;
  name : string;
  func : unit -> 'a;
  (* Options *)
  speed_level : Alcotest.speed_level;
  check_output : output option;
  (* Automatically determined *)
  id : string;
}

type test = unit t
type lwt_test = unit Lwt.t t

(* Legacy type that doesn't support options *)
type simple_test = string * (unit -> unit)

(****************************************************************************)
(* Helpers *)
(****************************************************************************)
(*
   Dumb stuff for which
*)

let list_map f l = List.rev_map f l |> List.rev

let list_flatten ll =
  List.fold_left (fun acc l -> List.rev_append l acc) [] ll |> List.rev

(****************************************************************************)
(* Conversions *)
(****************************************************************************)

(* Create a short alphanumeric hash. With only 28 bits, there's a tiny
   chance that we run into collisions. *)
let update_id x =
  (* 30-bit hash *)
  let hash = Hashtbl.hash_param 1_000_000 1_000_000 (x.category, x.name) in
  let long_id = Printf.sprintf "%08x" hash in
  assert (String.length long_id = 8);
  (* 28 bits or 7 hexadecimal characters *)
  (* nosemgrep: ocamllint-str-first-chars *)
  let id = String.sub long_id 0 7 in
  { x with id }

let create_test ?check_output ?(speed_level = `Quick) name func =
  { category = []; name; func; speed_level; check_output; id = "" } |> update_id

let simple_test (name, func) = create_test name func
let simple_tests simple_tests = list_map simple_test simple_tests

let pack_tests_pro suite_name (tests : _ list) : _ list =
  list_map
    (fun x -> { x with category = suite_name :: x.category } |> update_id)
    tests

let pack_tests suite_name tests = pack_tests_pro suite_name (simple_tests tests)

let pack_suites suite_name (tests : _ t list list) : _ t list =
  tests |> list_flatten |> pack_tests_pro suite_name

(*
   Sort by category and test name.
*)
let sort (tests : _ t list) : _ t list =
  tests
  |> List.stable_sort (fun a b ->
         let c = compare a.category b.category in
         if c <> 0 then c else String.compare a.name b.name)

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

let to_alcotest tests : _ list =
  tests
  |> list_map (fun x ->
         let suite_name =
           match x.category with
           | [] -> x.name
           | path -> String.concat " > " path
         in
         let suite_name = sprintf "[%s] %s" x.id suite_name in
         (* This is the format expected by Alcotest: *)
         (suite_name, (x.name, x.speed_level, x.func)))
  |> group_by_key

let to_alcotest_lwt = to_alcotest
let registered_tests : test list ref = ref []
let registered_lwt_tests : lwt_test list ref = ref []
let register x = registered_tests := x :: !registered_tests
let register_lwt x = registered_lwt_tests := x :: !registered_lwt_tests

let test ?check_output ?speed_level name func =
  create_test ?check_output ?speed_level name func |> register

let test_lwt ?check_output ?speed_level name func =
  create_test ?check_output ?speed_level name func |> register_lwt

let get_registered_tests () = List.rev !registered_tests
let get_registered_lwt_tests () = List.rev !registered_lwt_tests
