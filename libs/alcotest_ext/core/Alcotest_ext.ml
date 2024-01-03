(*
   Utilities for writing test suites for Alcotest.

   Keep in mind that some of this may become its own library or move to
   Alcotest.
*)

module T = Types

(****************************************************************************)
(* Main types *)
(****************************************************************************)

type expected_outcome = T.expected_outcome =
  | Should_succeed
  | Should_fail of string

type outcome = T.outcome = Succeeded | Failed

type captured_output = T.captured_output =
  | Ignored of string
  | Captured_stdout of string * string
  | Captured_stderr of string * string
  | Captured_stdout_stderr of string * string
  | Captured_merged of string

type expected_output = T.expected_output =
  | Ignored
  | Expected_stdout of string
  | Expected_stderr of string
  | Expected_stdout_stderr of string * string (* stdout, stderr *)
  | Expected_merged of string (* combined output *)

type result = T.result = {
  outcome : outcome;
  captured_output : captured_output;
}

type expectation = T.expectation = {
  expected_outcome : expected_outcome;
  expected_output : (expected_output, string list (* missing files *)) Result.t;
}

type status = T.status = {
  expectation : expectation;
  result : (result, string list) Result.t;
}

type status_class = T.status_class = PASS | FAIL | XFAIL | XPASS | MISS

type status_summary = T.status_summary = {
  status_class : status_class;
  has_expected_output : bool;
}

type 'unit_promise test_with_status =
  'unit_promise T.test * status * status_summary

type 'unit_promise subcommand_result = 'unit_promise Cmd.subcommand_result =
  | Run_result of 'unit_promise test_with_status list
  | Status_result of 'unit_promise test_with_status list
  | Approve_result

(* export *)
module Mona = Mona
module Tag = Tag

type output_kind = T.output_kind =
  | Ignore_output
  | Stdout
  | Stderr
  | Merged_stdout_stderr
  | Separate_stdout_stderr

type 'unit_promise t = 'unit_promise T.test = {
  id : string;
  internal_full_name : string;
  category : string list;
  name : string;
  func : unit -> 'unit_promise;
  expected_outcome : expected_outcome;
  tags : Tag.t list;
  mask_output : (string -> string) list;
  checked_output : output_kind;
  skipped : bool;
  tolerate_chdir : bool;
  m : 'unit_promise Mona.t;
}

type test = unit t

(* Legacy type that doesn't support options *)
type simple_test = string * (unit -> unit)

(* Polymorphic type alias for an Alcotest's 'test_case'. *)
type 'unit_promise alcotest_test_case =
  string * [ `Quick | `Slow ] * (unit -> 'unit_promise)

(* Polymorphic type alias for an Alcotest's 'test'. *)
type 'unit_promise alcotest_test =
  string * 'unit_promise alcotest_test_case list

(****************************************************************************)
(* Conversions *)
(****************************************************************************)

(*
   Create an hexadecimal hash that is just long enough to not suffer from
   collisions in realistic use cases defined as:
   "works fine up to 1 million tests".

   With 12 hexadecimal characters (48 bits), the probability of a collision
   within 1 million hashes is 0.0018. If anyone runs into collisions due
   to having more than a million tests, we can add an option to increase
   the number of bits.
*)
let update_id (test : _ t) =
  let internal_full_name = T.recompute_internal_full_name test in
  let md5_hex = internal_full_name |> Digest.string |> Digest.to_hex in
  assert (String.length md5_hex = 32);
  (* nosemgrep: ocamllint-str-first-chars *)
  let id = String.sub md5_hex 0 12 in
  { test with id; internal_full_name }

let create_gen ?(category = []) ?(checked_output = Ignore_output)
    ?(expected_outcome = Should_succeed) ?(mask_output = []) ?(skipped = false)
    ?(tags = []) ?(tolerate_chdir = false) mona name func =
  {
    id = "";
    internal_full_name = "";
    category;
    name;
    func;
    expected_outcome;
    tags;
    mask_output;
    checked_output;
    skipped;
    tolerate_chdir;
    m = mona;
  }
  |> update_id

let create ?category ?checked_output ?expected_outcome ?mask_output ?skipped
    ?tags ?tolerate_chdir name func =
  create_gen ?category ?checked_output ?expected_outcome ?mask_output ?skipped
    ?tags ?tolerate_chdir Mona.sync name func

let opt option default = Option.value option ~default

let update ?category ?checked_output ?expected_outcome ?func ?mask_output ?name
    ?skipped ?tags ?tolerate_chdir old =
  {
    id = "";
    internal_full_name = "";
    category = opt category old.category;
    name = opt name old.name;
    func = opt func old.func;
    (* requires same type for func and old.func *)
    expected_outcome = opt expected_outcome old.expected_outcome;
    tags = opt tags old.tags;
    mask_output = opt mask_output old.mask_output;
    checked_output = opt checked_output old.checked_output;
    skipped = opt skipped old.skipped;
    tolerate_chdir = opt tolerate_chdir old.tolerate_chdir;
    m = old.m;
  }
  |> update_id

let mask_line ?(mask = "<MASKED>") ?(after = "") ?(before = "") () =
  let pat =
    Printf.sprintf {|%s[^\n]*%s|} (Re.Pcre.quote after) (Re.Pcre.quote before)
  in
  let rex = Re.Pcre.regexp pat in
  let subst _matched = after ^ mask ^ before in
  fun subj -> Re.Pcre.substitute ~rex ~subst subj

let mask_pcre_pattern ?(mask = "<MASKED>") pat =
  let rex = Re.Pcre.regexp pat in
  let subst _matched = mask in
  fun subj -> Re.Pcre.substitute ~rex ~subst subj

let mask_temp_paths ?(mask = "<TEMPORARY FILE PATH>") () =
  let pat =
    Re.Pcre.quote (Filename.get_temp_dir_name ()) ^ {|[/\\A-Za-z0-9_.-]*|}
  in
  mask_pcre_pattern ~mask pat

(* Allow conversion from Lwt to synchronous function *)
let update_func (test : 'a t) mona2 func : 'b t = { test with func; m = mona2 }
let has_tag tag test = List.mem tag test.tags
let simple_test (name, func) = create name func
let simple_tests simple_tests = Helpers.list_map simple_test simple_tests

let pack_tests_pro suite_name (tests : _ list) : _ list =
  Helpers.list_map
    (fun x -> update x ~category:(suite_name :: x.category))
    tests

let pack_tests suite_name tests = pack_tests_pro suite_name (simple_tests tests)

let pack_suites suite_name (tests : _ t list list) : _ t list =
  tests |> Helpers.list_flatten |> pack_tests_pro suite_name

(*
   Sort by category and test name.
*)
let sort (tests : _ t list) : _ t list =
  tests
  |> List.stable_sort (fun a b ->
         let c = compare a.category b.category in
         if c <> 0 then c else String.compare a.name b.name)

let to_alcotest = Run.to_alcotest
let registered_tests : test list ref = ref []
let register x = registered_tests := x :: !registered_tests

let test ?category ?checked_output ?expected_outcome ?mask_output ?skipped ?tags
    ?tolerate_chdir name func =
  create ?category ?checked_output ?expected_outcome ?mask_output ?skipped ?tags
    ?tolerate_chdir name func
  |> register

let get_registered_tests () = List.rev !registered_tests
let interpret_argv_gen = Cmd.interpret_argv
let interpret_argv = interpret_argv_gen ~mona:Mona.sync
