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

module Mona : module type of Mona
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
type 'unit_promise t = private {
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
  func : unit -> 'unit_promise;
  (***** Options *****)
  expected_outcome : expected_outcome;
  tags : Tag.t list; (* tags must be declared once using 'create_tag' *)
  (* An optional function to rewrite any output data so as to mask the
     variable parts. *)
  mask_output : (string -> string) list;
  checked_output : output_kind;
  (* The 'skipped' property causes a test to be skipped by Alcotest but still
     shown as "[SKIP]" rather than being omitted. *)
  skipped : bool;
  (* If the test function changes the current directory without restoring it,
     it's an error unless this flag is set. *)
  tolerate_chdir : bool;
  (* All the tests in a test suite should share this field. *)
  m : 'unit_promise Mona.t;
}

(* The type of an ordinary test, i.e. one that returns when it's done rather
   than one that returns a deferred computation (Lwt, Async, etc.). *)
type test = unit t
type 'unit_promise test_with_status = 'unit_promise t * status * status_summary

(*
   The return type of each subcommand.
   It allows custom code to do something with the test data e.g. export
   to the JUnit format via the optional 'handle_subcommand_result' argument
   of 'interpret_argv'.
*)
type 'unit_promise subcommand_result =
  | Run_result of 'unit_promise test_with_status list
  | Status_result of 'unit_promise test_with_status list
  | Approve_result

(* Legacy type that doesn't support options *)
type simple_test = string * (unit -> unit)

(*
   Create a test to appear in a test suite.
*)
val create :
  ?category:string list ->
  ?checked_output:output_kind ->
  ?expected_outcome:expected_outcome ->
  ?mask_output:(string -> string) list ->
  ?skipped:bool ->
  ?tags:Tag.t list ->
  ?tolerate_chdir:bool ->
  string ->
  (unit -> unit) ->
  unit t

(*
   Generic version of 'create' provided for libraries whose test function
   returns a promise (Lwt, Async, ...).
*)
val create_gen :
  ?category:string list ->
  ?checked_output:output_kind ->
  ?expected_outcome:expected_outcome ->
  ?mask_output:(string -> string) list ->
  ?skipped:bool ->
  ?tags:Tag.t list ->
  ?tolerate_chdir:bool ->
  'unit_promise Mona.t ->
  string ->
  (unit -> 'unit_promise) ->
  'unit_promise t

(*
   Update some of the test's fields. This ensures that the 'id' is recomputed
   correctly. If specified, any of the optional property will replace
   the previous value.
*)
val update :
  ?category:string list ->
  ?checked_output:output_kind ->
  ?expected_outcome:expected_outcome ->
  ?func:(unit -> 'unit_promise) ->
  ?mask_output:(string -> string) list ->
  ?name:string ->
  ?skipped:bool ->
  ?tags:Tag.t list ->
  ?tolerate_chdir:bool ->
  'unit_promise t ->
  'unit_promise t

(*
   String replacement utilities to be used for masking the variable parts
   of captured output. This is for the 'mask_output' option of 'create'.
*)
val mask_line :
  ?mask:string -> ?after:string -> ?before:string -> unit -> string -> string

val mask_pcre_pattern : ?mask:string -> string -> string -> string

(*
   Mask strings that look like temporary file paths. This is useful in the
   following cases:
   - the temporary folder depends on the platform (Unix, Windows) or
     on the environment (TMPDIR environment variable or equivalent);
   - the files placed in the system's temporary folder are assigned
     random names.
*)
val mask_temp_paths : ?mask:string -> unit -> string -> string

(*
   Special case of the 'update' function that allows a different type
   for the new test function. This is useful for converting an Lwt test
   to a regular one.
*)
val update_func :
  'unit_promise t ->
  'unit_promise2 Mona.t ->
  (unit -> 'unit_promise2) ->
  'unit_promise2 t

val has_tag : Tag.t -> 'a t -> bool

(* Convert legacy optionless format to new format *)
val simple_test : string * (unit -> unit) -> unit t
val simple_tests : (string * (unit -> unit)) list -> unit t list

(* Legacy interface. It's fine to keep using it but it doesn't allow
   defining tests with special options such as capturing and checking stdout. *)
val pack_tests : string -> simple_test list -> test list

(* Register a test. This supports only synchronous tests.

   The test gets added to the global list of tests.
   This is meant to declare inline tests as follows:

     let () = Alcotest_ext.test "foo" (fun () ->
       (* test body raising exceptions to signal failure *)
       ...
     )
*)
val test :
  ?category:string list ->
  ?checked_output:output_kind ->
  ?expected_outcome:expected_outcome ->
  ?mask_output:(string -> string) list ->
  ?skipped:bool ->
  ?tags:Tag.t list ->
  ?tolerate_chdir:bool ->
  string ->
  (unit -> unit) ->
  unit

(* Get the list of registered tests. *)
val get_registered_tests : unit -> test list

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

(* Type alias for Alcotest test cases *)
type 'unit_promise alcotest_test_case =
  string * [ `Quick | `Slow ] * (unit -> 'unit_promise)

(* Type alias for an Alcotest 'test'. *)
type 'unit_promise alcotest_test =
  string * 'unit_promise alcotest_test_case list

(*
   Export our tests to a list of tests that can run in Alcotest.
   This removes the ability to store test outcomes or to check the test output
   against expectations. Tests that are expected to fail and indeed fail
   (XFAIL) will be treated as successful by Alcotest. Conversely, tests that
   fail to raise an exception (XPASS) will be shown as failed by Alcotest.
*)
val to_alcotest : 'unit_promise t list -> 'unit_promise alcotest_test list

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
  ?handle_subcommand_result:(int -> unit subcommand_result -> unit) ->
  ?status_workspace_root:string ->
  project_name:string ->
  (unit -> test list) ->
  unit

val interpret_argv_gen :
  ?argv:string array ->
  ?expectation_workspace_root:string ->
  ?handle_subcommand_result:(int -> 'unit_promise subcommand_result -> unit) ->
  ?status_workspace_root:string ->
  mona:'unit_promise Mona.t ->
  project_name:string ->
  (unit -> 'unit_promise t list) ->
  'unit_promise
