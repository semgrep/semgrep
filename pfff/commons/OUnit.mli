(*s: pfff/commons/OUnit.mli *)
(***********************************************************************)
(* The OUnit library                                                   *)
(*                                                                     *)
(* Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008              *) 
(* Maas-Maarten Zeeman.                                                *)

(*
The package OUnit is copyright by Maas-Maarten Zeeman.

Permission is hereby granted, free of charge, to any person obtaining
a copy of this document and the OUnit software ("the Software"), to
deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute,
sublicense, and/or sell copies of the Software, and to permit persons
to whom the Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

The Software is provided ``as is'', without warranty of any kind,
express or implied, including but not limited to the warranties of
merchantability, fitness for a particular purpose and noninfringement.
In no event shall Maas-Maarten Zeeman be liable for any claim, damages
or other liability, whether in an action of contract, tort or
otherwise, arising from, out of or in connection with the Software or
the use or other dealings in the software.
*)
(***********************************************************************)

(** The OUnit library can be used to implement unittests

    To uses this library link with
      [ocamlc OUnit.cmo]
    or 
      [ocamlopt OUnit.cmx]
 
    @author Maas-Maarten Zeeman
*)

(** {5 Assertions} 

    Assertions are the basic building blocks of unittests. *)

(*s: signature [[OUnit.assert_failure]] *)
(** Signals a failure. This will raise an exception with the specified
    string. 

    @raise Failure to signal a failure *)
val assert_failure : string -> 'a
(*e: signature [[OUnit.assert_failure]] *)

(*s: signature [[OUnit.assert_bool]] *)
(** Signals a failure when bool is false. The string identifies the 
    failure.
    
    @raise Failure to signal a failure *)
val assert_bool : msg:string -> bool -> unit
(*e: signature [[OUnit.assert_bool]] *)

(*s: signature [[OUnit.TODOOPERATOR]] *)
(** Shorthand for assert_bool 

    @raise Failure to signal a failure *)
val ( @? ) : string -> bool -> unit
(*e: signature [[OUnit.TODOOPERATOR]] *)

(*s: signature [[OUnit.assert_string]] *)
(** Signals a failure when the string is non-empty. The string identifies the
    failure. 
    
    @raise Failure to signal a failure *) 
val assert_string : string -> unit
(*e: signature [[OUnit.assert_string]] *)


(*s: signature [[OUnit.assert_equal]] *)
(** Compares two values, when they are not equal a failure is signaled.
    The cmp parameter can be used to pass a different compare function. 
    This parameter defaults to ( = ). The optional printer can be used 
    to convert the value to string, so a nice error message can be 
    formatted. When msg is also set it can be used to identify the failure. 

    @raise Failure description *)
val assert_equal : ?cmp:('a -> 'a -> bool) ->  ?printer:('a -> string) -> 
                   ?msg:string -> 'a -> 'a -> unit
(*e: signature [[OUnit.assert_equal]] *)

(*s: signature [[OUnit.assert_raises]] *)
(** Asserts if the expected exception was raised. When msg is set it can 
    be used to identify the failure

    @raise Failure description *)
val assert_raises : ?msg:string -> exn -> (unit -> 'a) -> unit
(*e: signature [[OUnit.assert_raises]] *)

(** {5 Skipping tests } 
  
   In certain condition test can be written but there is no point running it, because they
   are not significant (missing OS features for example). In this case this is not a failure
   nor a success. Following function allow you to escape test, just as assertion but without
   the same error status.
  
   A test skipped is counted as success. A test todo is counted as failure.  *)

(*s: signature [[OUnit.skip_if]] *)
(** [skip cond msg] If [cond] is true, skip the test for the reason explain in [msg].
  * For example [skip_if (Sys.os_type = "Win32") "Test a doesn't run on windows"].
  *)
val skip_if : bool -> string -> unit
(*e: signature [[OUnit.skip_if]] *)

(*s: signature [[OUnit.todo]] *)
(** The associated test is still to be done, for the reason given.
  *)
val todo : string -> unit
(*e: signature [[OUnit.todo]] *)

(** {5 Compare Functions} *)

(*s: signature [[OUnit.cmp_float]] *)
(** Compare floats up to a given relative error. *)
val cmp_float : ?epsilon: float -> float -> float -> bool
(*e: signature [[OUnit.cmp_float]] *)

(** {5 Bracket}

    A bracket is a functional implementation of the commonly used
    setUp and tearDown feature in unittests. It can be used like this:

    "MyTestCase" >:: (bracket test_set_up test_fun test_tear_down) *)

(*s: signature [[OUnit.bracket]] *)
(** *)
val bracket : (unit -> 'a) -> ('a -> 'b) -> ('a -> 'c) -> unit -> 'c
(*e: signature [[OUnit.bracket]] *)

(** {5 Constructing Tests} *)

(*s: type [[OUnit.test_fun]] *)
(** The type of test function *)
type test_fun = unit -> unit
(*e: type [[OUnit.test_fun]] *)

(*s: type [[OUnit.test]] *)
(** The type of tests *)
type test =
    TestCase of test_fun
  | TestList of test list
  | TestLabel of string * test
(*e: type [[OUnit.test]] *)

(*s: signature [[OUnit.TODOOPERATOR (pfff/commons/OUnit.mli)]] *)
(** Create a TestLabel for a test *)
val (>:) : string -> test -> test
(*e: signature [[OUnit.TODOOPERATOR (pfff/commons/OUnit.mli)]] *)

(*s: signature [[OUnit.TODOOPERATOR (pfff/commons/OUnit.mli)2]] *)
(** Create a TestLabel for a TestCase *)
val (>::) : string -> test_fun -> test
(*e: signature [[OUnit.TODOOPERATOR (pfff/commons/OUnit.mli)2]] *)

(*s: signature [[OUnit.TODOOPERATOR (pfff/commons/OUnit.mli)3]] *)
(** Create a TestLabel for a TestList *)
val (>:::) : string -> test list -> test
(*e: signature [[OUnit.TODOOPERATOR (pfff/commons/OUnit.mli)3]] *)

(** Some shorthands which allows easy test construction.

   Examples:

   - ["test1" >: TestCase((fun _ -> ()))] =>  
   [TestLabel("test2", TestCase((fun _ -> ())))]
   - ["test2" >:: (fun _ -> ())] => 
   [TestLabel("test2", TestCase((fun _ -> ())))]

   - ["test-suite" >::: ["test2" >:: (fun _ -> ());]] =>
   [TestLabel("test-suite", TestSuite([TestLabel("test2", TestCase((fun _ -> ())))]))]
*)

(*s: signature [[OUnit.test_decorate]] *)
(** [test_decorate g tst] Apply [g] to test function contains in [tst] tree. *)
val test_decorate : (test_fun -> test_fun) -> test -> test
(*e: signature [[OUnit.test_decorate]] *)

(*s: signature [[OUnit.test_filter]] *)
(** [test_filter paths tst] Filter test based on their path string representation. *)
val test_filter : string list -> test -> test option
(*e: signature [[OUnit.test_filter]] *)

(** {5 Retrieve Information from Tests} *)

(*s: signature [[OUnit.test_case_count]] *)
(** Returns the number of available test cases *)
val test_case_count : test -> int
(*e: signature [[OUnit.test_case_count]] *)

(*s: type [[OUnit.node]] *)
(** Types which represent the path of a test *)
type node = ListItem of int | Label of string
(*e: type [[OUnit.node]] *)
(*s: type [[OUnit.path]] *)
type path = node list (** The path to the test (in reverse order). *)
(*e: type [[OUnit.path]] *)

(*s: signature [[OUnit.string_of_node]] *)
(** Make a string from a node *)
val string_of_node : node -> string
(*e: signature [[OUnit.string_of_node]] *)

(*s: signature [[OUnit.string_of_path]] *)
(** Make a string from a path. The path will be reversed before it is 
    tranlated into a string *)
val string_of_path : path -> string
(*e: signature [[OUnit.string_of_path]] *)

(*s: signature [[OUnit.test_case_paths]] *)
(** Returns a list with paths of the test *)
val test_case_paths : test -> path list
(*e: signature [[OUnit.test_case_paths]] *)

(** {5 Performing Tests} *)

(*s: type [[OUnit.test_result]] *)
(** The possible results of a test *)
type test_result =
    RSuccess of path
  | RFailure of path * string
  | RError of path * string
  | RSkip of path * string
  | RTodo of path * string
(*e: type [[OUnit.test_result]] *)

(*s: type [[OUnit.test_event]] *)
(** Events which occur during a test run *)   
type test_event =
    EStart of path 
  | EEnd of path
  | EResult of test_result
(*e: type [[OUnit.test_event]] *)

(*s: signature [[OUnit.perform_test]] *)
(** Perform the test, allows you to build your own test runner *)
val perform_test : (test_event -> 'a) -> test -> test_result list
(*e: signature [[OUnit.perform_test]] *)

(*s: signature [[OUnit.run_test_tt]] *)
(** A simple text based test runner. It prints out information
    during the Test. *)
val run_test_tt : ?verbose:bool -> test -> test_result list
(*e: signature [[OUnit.run_test_tt]] *)

(*s: signature [[OUnit.run_test_tt_main]] *)
(** Main version of the text based test runner. It reads the supplied command 
    line arguments to set the verbose level and limit the number of test to run
  *)
val run_test_tt_main : test -> test_result list
(*e: signature [[OUnit.run_test_tt_main]] *)
(*e: pfff/commons/OUnit.mli *)
