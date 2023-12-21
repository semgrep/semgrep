(*
   Utilities for writing test suites that are compatible with Alcotest.

   This is the only module exported by this library. Other modules are
   either hidden or included as submodules such as 'Alcotest_ext_tag'
   which is exposed as 'Alcotest_ext.Tag'. This allows us to:
   - use dedicated file-modules for data structures without cramming
     everything into this single file such as Tag module providing Tag.t
   - hide internal modules that shouldn't be accessed by users of the library

   Dune exposes only this module as long as its name matches the name of the
   library.

   Alcotest API:
   https://mirage.github.io/alcotest/alcotest/Alcotest/index.html
*)

(****************************************************************************)
(* A bunch of types for advanced uses and subject to frequent changes. *)
(****************************************************************************)

type expected_outcome =
  | Should_succeed
  | Should_fail of string (* explains why we expect this test to fail *)

type outcome = Succeeded | Failed

type captured_output =
  | Ignored of string (* unchecked combined output *)
  | Captured_stdout of string * string (* stdout, unchecked output *)
  | Captured_stderr of string * string (* stderr, unchecked output *)
  | Captured_stdout_stderr of string * string (* stdout, stderr *)
  | Captured_merged of string (* combined output *)

type expected_output =
  | Ignored
  | Expected_stdout of string
  | Expected_stderr of string
  | Expected_stdout_stderr of string * string (* stdout, stderr *)
  | Expected_merged of string (* combined output *)

type result = { outcome : outcome; captured_output : captured_output }

type expectation = {
  expected_outcome : expected_outcome;
  expected_output : (expected_output, string list (* missing files *)) Result.t;
}

type status = {
  expectation : expectation;
  result : (result, string list) Result.t;
}

type status_class = PASS | FAIL | XFAIL | XPASS | MISS

type status_summary = {
  status_class : status_class;
  has_expected_output : bool;
}

(****************************************************************************)
(* Main interface *)
(****************************************************************************)

type output_kind =
  | Ignore_output
  | Stdout
  | Stderr
  | Merged_stdout_stderr
  | Separate_stdout_stderr

module Tag : module type of Tag

(*
   A test is a name and a function that raises exceptions to signal
   test failure.

   There are two main recommended ways of writing a test:

   1. With 'assert false':

   Each test may use 'assert false' to indicate that the test doesn't pass.
   This is the simplest way of failing while also showing the location
   of the failure. When using 'assert false', you should print the expected
   value and actual value if applicable.

   2. With 'Alcotest.(check ...)':

   This is a little nicer because the error messages prints something like
   "Expecting 'foo', got 'bar'". However, this makes tests more complicated
   to write. If the test already prints the expected value and the actual
   value as its output, it's just easier to fail with 'assert false'.

   In any case, Alcotest will capture the output (stdout, stderr) of each
   test and put it in its own file so we can consult it later. Don't
   hesitate to log a lot during the execution of the test.
*)
type 'a t = private {
  (* Hash of the full name of the test, computed automatically. *)
  id : string;
  (* Full name of the test, derived automatically from category and name. *)
  internal_full_name : string;
  (* Categories are made for organizing tests as a tree which is useful
     for display and filtering. A new category is created typically when
     grouping multiple test suites into one with 'pack_suites'.
     e.g. ["food"; "fruit"; "kiwi"] *)
  category : string list;
  name : string;
  func : unit -> 'a;
  (***** Options *****)
  expected_outcome : expected_outcome;
  tags : Tag.t list; (* tags must be declared once using 'create_tag' *)
  (* special "tag" supported directly by Alcotest: *)
  speed_level : Alcotest.speed_level;
  (* An optional function to rewrite any output data so as to mask the
     variable parts. *)
  mask_output : (string -> string) list;
  output_kind : output_kind;
  (* The 'skipped' property causes a test to be skipped by Alcotest but still
     shown as "[SKIP]" rather than being omitted. *)
  skipped : bool;
  (* If the test function changes the current directory without restoring it,
     it's an error unless this flag is set. *)
  tolerate_chdir : bool;
}

type test = unit t
type test_with_status = test * status * status_summary

(*
   The return type of each subcommand.
   It allows custom code to do something with the test data e.g. export
   to the JUnit format via the optional 'handle_subcommand_result' argument
   of 'interpret_argv'.
*)
type subcommand_result =
  | Run_result of test_with_status list
  | Status_result of test_with_status list
  | Approve_result

(* Legacy type that doesn't support options *)
type simple_test = string * (unit -> unit)

(* Lwt.t promises transpiled to JS via jsoo must have their Lwt.t nature
   hoisted all the way to the top level, so we can run them properly on the
   JS runtime.
   When running such tests in JS, we need our tests to also return promises.
*)
type lwt_test = unit Lwt.t t

(*
   Create a test to appear in a test suite.
*)
val create :
  ?category:string list ->
  ?expected_outcome:expected_outcome ->
  ?mask_output:(string -> string) list ->
  ?output_kind:output_kind ->
  ?skipped:bool ->
  ?speed_level:Alcotest.speed_level ->
  ?tags:Tag.t list ->
  ?tolerate_chdir:bool ->
  string ->
  (unit -> 'a) ->
  'a t

(*
   Update some of the test's fields. This ensures that the 'id' is recomputed
   correctly. If specified, any of the optional property will replace
   the previous value.
*)
val update :
  ?category:string list ->
  ?expected_outcome:expected_outcome ->
  ?func:(unit -> 'a) ->
  ?mask_output:(string -> string) list ->
  ?name:string ->
  ?output_kind:output_kind ->
  ?skipped:bool ->
  ?speed_level:Alcotest.speed_level ->
  ?tags:Tag.t list ->
  ?tolerate_chdir:bool ->
  'a t ->
  'a t

(*
   String replacement utilities to be used for masking the variable parts
   of captured output. This is for the 'mask_output' option of 'create'.
*)
val mask_line :
  ?mask:string -> ?after:string -> ?before:string -> unit -> string -> string

val mask_pcre_pattern : ?mask:string -> string -> string -> string

(*
   Special case of the 'update' function that allows a different type
   for the new test function. This is useful for converting an Lwt test
   to a regular one.
*)
val update_func : 'a t -> (unit -> 'b) -> 'b t
val has_tag : Tag.t -> 'a t -> bool

(* Convert legacy optionless format to new format *)
val simple_test : string * (unit -> 'a) -> 'a t
val simple_tests : (string * (unit -> 'a)) list -> 'a t list

(* Legacy interface. It's fine to keep using it but it doesn't allow
   defining tests with special options such as capturing and checking stdout. *)
val pack_tests : string -> simple_test list -> test list

(* Register a test. The test gets added to the global list of tests.
   This is meant to declare inline tests as follows:

     let () = Alcotest_ext.test "foo" (fun () ->
       (* test body raising exceptions to signal failure *)
       ...
     )
*)
val test :
  ?category:string list ->
  ?expected_outcome:expected_outcome ->
  ?output_kind:output_kind ->
  ?skipped:bool ->
  ?speed_level:Alcotest.speed_level ->
  string ->
  (unit -> unit) ->
  unit

val test_lwt :
  ?category:string list ->
  ?expected_outcome:expected_outcome ->
  ?output_kind:output_kind ->
  ?skipped:bool ->
  ?speed_level:Alcotest.speed_level ->
  string ->
  (unit -> unit Lwt.t) ->
  unit

(* Get the list of registered tests. *)
val get_registered_tests : unit -> test list
val get_registered_lwt_tests : unit -> lwt_test list

(*
   Usage:

     pack_tests_pro "apples" [test_color; test_juiciness]
     pack_suites "fruit" [apple_tests; banana_tests; strawberry_tests]
*)
val pack_tests_pro : string -> 'a t list -> 'a t list
val pack_suites : string -> 'a t list list -> 'a t list

(*
   Sort tests by path, alphabetically:

     "a>b" comes before "a>c",
     "a>b" come before "a b".

   Non-ascii path components are sorted by byte order, possibly giving
   unexpected results.
*)
val sort : 'a t list -> 'a t list

(*
   Convert a test suite to be run with the Alcotest.run which provides
   a command-line interface with the 'test' and 'list' subcommands only.

   The outcome of tests that are expected to fail (Should_fail) will
   be flipped so as to produce OK iff the test function raises an exception
   as expected.
*)
val to_alcotest : test list -> unit Alcotest.test list

(*
   Same as 'to_alcotest' but with Lwt promises.

   TODO: Do we really need a special type for Lwt tests?
*)
val to_alcotest_lwt : lwt_test list -> unit Alcotest_lwt.test list

(*
   Launch the extended command-line interface with subcommands for running
   the tests but also for checking test statuses and for approving
   new output.

   Return value: exit code reflecting overall success or failure (0 or 1),
   and subcommand-specific data for export to JUnit or similar.

   argv: command line to parse. Defaults to Sys.argv.
   expectation_workspace_root: Storage path for expected output. The default
     is 'tests/snapshots'.
   status_workspace_root: Storage path for test results. The default is
     '_build/alcotest_ext/status'.
   project_name: name of the program as shown in the --help page and used
     as a folder name for storing test results.
*)
val interpret_argv :
  ?argv:string array ->
  ?expectation_workspace_root:string ->
  ?handle_subcommand_result:(int -> subcommand_result -> unit) ->
  ?status_workspace_root:string ->
  project_name:string ->
  (unit -> test list) ->
  unit
