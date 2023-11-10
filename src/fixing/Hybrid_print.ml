(* Nat Mote
 *
 * Copyright (C) 2019-2022 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

open Common
module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Implements hybrid printing of the generic AST. Augments any
 * Ugly_print_AST.printer_t class with the ability to first try a different
 * printing function for any given node.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

module type Printer = sig
  class printer : Ugly_print_AST.printer_t
end

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let with_fallback value fallback =
  match value with
  | Ok x -> Ok x
  | Error e1 -> (
      match fallback with
      | (lazy (Ok x)) -> Ok x
      | (lazy (Error e2)) ->
          Error
            (spf
               "Failed to print AST with hybrid printer. First error:\n\
                %s\n\n\
                Second error:\n\
                %s"
               e1 e2))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* Classes are not first-class. Functors are the only way to create a class
 * that inherits from some undetermined other class of a particular type.
 * There will be a small amount of boilerplate associated with this per
 * language. *)
module Make (Fallback : Printer) : sig
  class printer :
    (AST_generic.any -> (Immutable_buffer.t, string) result) ->
    Ugly_print_AST.printer_t
end = struct
  class printer
    (primary : AST_generic.any -> (Immutable_buffer.t, string) result) :
    Ugly_print_AST.printer_t =
    object
      inherit Fallback.printer as fallback

      method! print_argument arg =
        with_fallback (primary (G.Ar arg)) (lazy (fallback#print_argument arg))

      method! private print_expr_without_parens e =
        with_fallback (primary (G.E e))
          (lazy (fallback#print_expr_without_parens e))

      method! print_unbracketed_arguments args =
        with_fallback (primary (G.Args args))
          (lazy (fallback#print_unbracketed_arguments args))

      method! print_ident ident =
        with_fallback (primary (G.I ident)) (lazy (fallback#print_ident ident))

      (* TODO Fill in more cases as needed. *)
    end
end
