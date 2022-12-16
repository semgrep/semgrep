open Common

(*
 * src: Jane Street Core library.
 * update: Normally no more needed in OCaml 3.11 as part of the
 *  default runtime.
 *)
external print : unit -> unit = "print_exception_backtrace_stub" "noalloc"


(* ---------------------------------------------------------------------- *)
(* testing *)
(* ---------------------------------------------------------------------- *)

exception MyNot_Found

let foo1 () =
  if 1 = 2
  then raise MyNot_Found
  else 2

let foo2 () =
  foo1 () + 2

let test_backtrace () =
  (try ignore(foo2 ())
   with exn ->
     pr2 (Common.exn_to_s exn);
     print();
     failwith "other exn"
  );
  print_string "ok cool\n";
  ()

let actions () =
  [
    "-test_backtrace", "   ",
    Common.mk_action_0_arg test_backtrace;
  ]
