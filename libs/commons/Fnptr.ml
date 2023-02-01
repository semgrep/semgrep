(* Fnptr.ml *)
(* Module to allow invoking C function pointers. *)

(*
  The existing OCaml module "Callback" is for registering OCaml
  functions to be called from C.  OCaml "external" declarations allow
  named C functions to called from OCaml, but not arbitrary function
  pointers.

  This module allows function pointers to be packaged with their 'extra'
  data (on the C side, not visible here) so they can be used from OCaml
  to approximate closures.

  It is then possible in OCaml to wrap a call to 'call_fnptr' in order
  to make a true closure that can be passed around and used like any
  other closure.
*)

(* This type encapsulates a pointer to a function defined in C and
 * passed to the OCaml code as a callback.  That function accepts an
 * 'a and returns a 'b.
 *
 * Although the C code could raise an exception by calling
 * 'caml_raise_XXX', the expectation is that users of this interface
 * will encode unusual values in 'b, since conveying exceptions across
 * language boundaries is somewhat fraught. *)
type ('a, 'b) t

(* Given an fnptr and its argument, invoke it to get the result.
 *
 * Be aware that an fnptr can be invalidated from the C side, in which
 * case this function will throw an Invalid_argument exception.  An
 * interface that deals with fnptrs should document their valid
 * lifetime. *)
external call : ('a, 'b) t -> 'a -> 'b = "call_fnptr"

(* EOF *)
