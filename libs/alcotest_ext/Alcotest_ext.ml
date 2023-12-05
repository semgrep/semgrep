(*
   Utilities for writing test suites for Alcotest.

   Keep in mind that some of this may become its own library or move to
   Alcotest.
*)

open Printf

(****************************************************************************)
(* Helpers *)
(****************************************************************************)

(* safe version of List.map for ocaml < 5 *)
let list_map f l = List.rev_map f l |> List.rev

(* safe version of List.flatten *)
let list_flatten ll =
  List.fold_left (fun acc l -> List.rev_append l acc) [] ll |> List.rev

(****************************************************************************)
(* Main types *)
(****************************************************************************)

type expected_outcome = Alcotest_ext_types.expected_outcome =
  | Should_succeed
  | Should_fail of string

type outcome = Alcotest_ext_types.outcome = Succeeded | Failed

module Tag = Alcotest_ext_tag

type output_kind = Alcotest_ext_types.output_kind =
  | Ignore_output
  | Stdout
  | Stderr
  | Merged_stdout_stderr
  | Separate_stdout_stderr

type 'a t = 'a Alcotest_ext_types.test = {
  id : string;
  category : string list;
  name : string;
  func : unit -> 'a;
  expected_outcome : expected_outcome;
  tags : Tag.t list;
  speed_level : Alcotest.speed_level;
  output_kind : output_kind;
  skipped : bool;
}

type test = unit t
type lwt_test = unit Lwt.t t

(* Legacy type that doesn't support options *)
type simple_test = string * (unit -> unit)

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

let create ?(category = []) ?(expected_outcome = Should_succeed)
    ?(output_kind = Ignore_output) ?(skipped = false) ?(speed_level = `Quick)
    ?(tags = []) name func =
  {
    id = "";
    category;
    name;
    func;
    expected_outcome;
    tags;
    speed_level;
    output_kind;
    skipped;
  }
  |> update_id

let opt option default = Option.value option ~default

let update ?category ?expected_outcome ?func ?name ?output_kind ?skipped
    ?speed_level ?tags old =
  {
    id = "";
    category = opt category old.category;
    name = opt name old.name;
    func = opt func old.func;
    expected_outcome = opt expected_outcome old.expected_outcome;
    tags = opt tags old.tags;
    speed_level = opt speed_level old.speed_level;
    output_kind = opt output_kind old.output_kind;
    skipped = opt skipped old.skipped;
  }
  |> update_id

let has_tag tag test = List.mem tag test.tags
let simple_test (name, func) = create name func
let simple_tests simple_tests = list_map simple_test simple_tests

let pack_tests_pro suite_name (tests : _ list) : _ list =
  list_map (fun x -> update x ~category:(suite_name :: x.category)) tests

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
         let tags =
           match x.tags with
           | [] -> ""
           | tags ->
               let tags =
                 List.sort Tag.compare tags |> List.map Tag.to_string
               in
               sprintf " {%s}" (String.concat ", " tags)
         in
         let suite_name =
           match x.category with
           | [] -> x.name
           | path -> String.concat " > " path
         in
         let suite_name = sprintf "[%s]%s %s" x.id tags suite_name in
         let func = if x.skipped then Alcotest.skip else x.func in
         (* This is the format expected by Alcotest: *)
         (suite_name, (x.name, x.speed_level, func)))
  |> group_by_key

let to_alcotest_lwt = to_alcotest
let registered_tests : test list ref = ref []
let registered_lwt_tests : lwt_test list ref = ref []
let register x = registered_tests := x :: !registered_tests
let register_lwt x = registered_lwt_tests := x :: !registered_lwt_tests

let test ?category ?expected_outcome ?output_kind ?skipped ?speed_level name
    func =
  create ?category ?expected_outcome ?output_kind ?skipped ?speed_level name
    func
  |> register

let test_lwt ?category ?expected_outcome ?output_kind ?skipped ?speed_level name
    func =
  create ?category ?expected_outcome ?output_kind ?skipped ?speed_level name
    func
  |> register_lwt

let get_registered_tests () = List.rev !registered_tests
let get_registered_lwt_tests () = List.rev !registered_lwt_tests
