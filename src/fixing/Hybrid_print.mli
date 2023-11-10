(* Implements hybrid printing of the generic AST. Augments any
 * Ugly_print_AST.printer_t class with the ability to first try a different
 * printing function for any given node.
 *
 * Uses inheritance via a functor so that this is accomplished via dynamic
 * dispatch, so that printer authors do not have to concern themselves with
 * this logic as they fill in cases.
 *
 * Specifically designed for autofix, where we can print some nodes by lifting
 * their original text from the source target or pattern.
 *)

module type Printer = sig
  class printer : Ugly_print_AST.printer_t
end

module Make (Fallback : Printer) : sig
  class printer :
    (AST_generic.any -> (Immutable_buffer.t, string) result) ->
    Ugly_print_AST.printer_t
end
