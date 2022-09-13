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

module G = AST_generic

(******************************************************************************)
(* Implements hybrid printing of the generic AST. Augments any
 * Ugly_print_AST.printer_t class with the ability to first try a different
 * printing function for any given node.
 *
 * Uses inheritance via a functor so that this is accomplished via dynamic
 * dispatch, so that printer authors do not have to concern themselves with this
 * logic as they fill in cases.
 *
 * Specifically designed for autofix, where we can print some nodes by lifting
 * their original text from the source target or pattern.
 *)
(******************************************************************************)

module type Printer = sig
  class printer : Ugly_print_AST.printer_t
end

(* Classes are not first-class. Functors are the only way to create a class that
 * inherits from some undetermined other class of a particular type. There will
 * be a small amount of boilerplate associated with this per language. *)
module Make (Fallback : Printer) : sig
  class printer :
    (AST_generic.any -> Immutable_buffer.t option) -> Ugly_print_AST.printer_t
end = struct
  class printer (primary : AST_generic.any -> Immutable_buffer.t option) :
    Ugly_print_AST.printer_t =
    object
      inherit Fallback.printer as fallback

      method! print_any any =
        match primary any with
        | Some x -> Some x
        | None -> fallback#print_any any

      method! print_argument arg =
        match primary (G.Ar arg) with
        | Some x -> Some x
        | None -> fallback#print_argument arg

      (* TODO Fill in more cases as needed. *)
    end
end
