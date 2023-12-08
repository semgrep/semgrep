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

type expected_outcome = Should_succeed | Should_fail of string

type output_kind =
  | Ignore_output
  | Stdout
  | Stderr
  | Merged_stdout_stderr
  | Separate_stdout_stderr

module Tag : module type of Alcotest_ext_tag

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
  output_kind : output_kind;
  (* The 'skipped' property causes a test to be skipped by Alcotest but still
     shown as "[SKIP]" rather than being omitted. *)
  skipped : bool;
}

type test = unit t

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
  ?output_kind:output_kind ->
  ?skipped:bool ->
  ?speed_level:Alcotest.speed_level ->
  ?tags:Tag.t list ->
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
  ?name:string ->
  ?output_kind:output_kind ->
  ?skipped:bool ->
  ?speed_level:Alcotest.speed_level ->
  ?tags:Tag.t list ->
  'a t ->
  'a t

val has_tag : Tag.t -> 'a t -> bool

(* Convert legacy optionless format to new format *)
val simple_test : string * (unit -> 'a) -> 'a t
val simple_tests : (string * (unit -> 'a)) list -> 'a t list

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

(* Legacy interface. It's fine to keep using it but it doesn't allow
   defining tests with special options such as capturing and checking stdout. *)
val pack_tests : string -> (string * (unit -> 'a)) list -> 'a t list

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
*)
val to_alcotest : test list -> unit Alcotest.test list

(* Do we really need a special type for Lwt tests? *)
val to_alcotest_lwt : lwt_test list -> unit Alcotest_lwt.test list

(*
   Launch the extended command-line interface with subcommands for running
   the tests but also for checking test statuses and for approving
   new output.

   argv: command line to parse. Defaults to Sys.argv.
   name: name of the program as shown in the --help page. Defaults to "test".
*)
val interpret_argv :
  ?argv:string array ->
  ?expectation_workspace_root:string ->
  ?status_workspace_root:string ->
  project_name:string ->
  test list ->
  int
