(* Fnptr_test_ml.ml *)
(* Exercise 'Fnptr' module, OCaml side. *)

open Printf

(* The C functions are defined in fnptr_test_c.c. *)

(* Function that adds 5 to its argument. *)
external get_add5 : unit -> (int, int) Fnptr.t = "get_add5"

(* Function that adds a number specified at creation time to its
 * argument. *)
external get_addN : int -> (int, int) Fnptr.t = "get_addN"

(* Function that marks the given Fnptr.t as invalid. *)
external dispose_addN : (int, int) Fnptr.t -> unit = "dispose_addN"

let main () : unit =
  printf "hello\n";

  let add5 : (int, int) Fnptr.t = get_add5 () in
  let eight = Fnptr.call add5 3 in
  printf "eight: %d\n" eight;

  let add6 : (int, int) Fnptr.t = get_addN 6 in
  let seven = Fnptr.call add6 1 in
  printf "seven: %d\n" seven;

  printf "ten: %d\n" (Fnptr.call add6 4);

  dispose_addN add6;
  try
    printf "calling an invalid fnptr...\n";
    flush stdout;
    ignore (Fnptr.call add6 1);
    printf "should have failed!\n";
    exit 2
  with
  | Invalid_argument _ as e ->
      printf "as expected: %s\n" (Printexc.to_string e);

      printf "eleven: %d\n" (Fnptr.call add5 6)

let () = main ()

(* EOF *)
