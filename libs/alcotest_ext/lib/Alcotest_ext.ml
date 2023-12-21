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

type test_with_status = unit T.test * status * status_summary

type subcommand_result = Cmd.subcommand_result =
  | Run_result of test_with_status list
  | Status_result of test_with_status list
  | Approve_result

(* export *)
module Tag = Tag

type output_kind = T.output_kind =
  | Ignore_output
  | Stdout
  | Stderr
  | Merged_stdout_stderr
  | Separate_stdout_stderr

type 'a t = 'a T.test = {
  id : string;
  internal_full_name : string;
  category : string list;
  name : string;
  func : unit -> 'a;
  expected_outcome : expected_outcome;
  tags : Tag.t list;
  speed_level : Alcotest.speed_level;
  mask_output : (string -> string) list;
  output_kind : output_kind;
  skipped : bool;
  tolerate_chdir : bool;
}

type test = unit t
type lwt_test = unit Lwt.t t

(* Legacy type that doesn't support options *)
type simple_test = string * (unit -> unit)

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

let create ?(category = []) ?(expected_outcome = Should_succeed)
    ?(mask_output = []) ?(output_kind = Ignore_output) ?(skipped = false)
    ?(speed_level = `Quick) ?(tags = []) ?(tolerate_chdir = false) name func =
  {
    id = "";
    internal_full_name = "";
    category;
    name;
    func;
    expected_outcome;
    tags;
    speed_level;
    mask_output;
    output_kind;
    skipped;
    tolerate_chdir;
  }
  |> update_id

let opt option default = Option.value option ~default

let update ?category ?expected_outcome ?func ?mask_output ?name ?output_kind
    ?skipped ?speed_level ?tags ?tolerate_chdir old =
  {
    id = "";
    internal_full_name = "";
    category = opt category old.category;
    name = opt name old.name;
    func = opt func old.func;
    (* requires same type for func and old.func *)
    expected_outcome = opt expected_outcome old.expected_outcome;
    tags = opt tags old.tags;
    speed_level = opt speed_level old.speed_level;
    mask_output = opt mask_output old.mask_output;
    output_kind = opt output_kind old.output_kind;
    skipped = opt skipped old.skipped;
    tolerate_chdir = opt tolerate_chdir old.tolerate_chdir;
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

(* Allow conversion from Lwt to synchronous function *)
let update_func (test : 'a t) func : 'b t = { test with func }
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
let to_alcotest_lwt = Run.to_alcotest_lwt
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
let interpret_argv = Cmd.interpret_argv
