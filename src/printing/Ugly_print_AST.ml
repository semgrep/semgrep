(* Nat Mote
 *
 * Copyright (C) 2019-2022 Semgrep Inc.
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
module B = Immutable_buffer

let b = B.of_string
let combine = B.combine

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* UglyPrinter prints syntactically-correct code without attempting to format
 * it nicely (as opposed to a pretty printer such as those based on Wadler's "A
 * prettier printer" [1]). It would be difficult, if not impossible, to print
 * code that conforms to everyone's style guides, linters, and formatters, so
 * the prettifying of code is left to other tools.
 *
 * UglyPrinter can print code snippets, not just full programs. Any of the
 * `print_` methods can be used by consumers.
 *
 * If UglyPrinter encounters a construct that it is unable to handle, it returns
 * `Error`. It is crucial that maintainers preserve this property, since
 * otherwise it is possible that we could present syntactically-incorrect code
 * as part of, for example, an autofix.
 *
 * Classes are used here so that we can take advantage of inheritance for
 * implementation-sharing and dynamic dispatch. Dynamic dispatch allows for the
 * hybrid printer used for autofix, where we use the original text from the
 * target or pattern for nodes that have been lifted unchanged. This is
 * implemented by inheriting from the plain printer. With a different
 * architecture, printer authors would need to deal with the logic for this.
 * This way, printer authors need only focus on printing AST correctly.
 *
 * OCaml's powerful object system will also allow us to share implementation
 * between printers to whatever extent is appropriate. `Pretty_print_AST`, for
 * example, gates on language within each function. We could replicate that here
 * if we like.
 *
 * This module should replace `Pretty_print_AST`. The main motivator for
 * creating a new printer module is that the use of classes allows for the
 * hybrid printer in autofix, as described above. `Pretty_print_AST` also does
 * not consistently report when it is unable to print a node correctly. We can
 * address that as we migrate it here.
 *
 * [1] https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

class type printer_t = object
  method print_any : G.any -> (Immutable_buffer.t, string) result

  (* Prints an expression, inserting parentheses around it if needed. *)
  method print_expr : G.expr -> (Immutable_buffer.t, string) result
  method print_expr_kind : G.expr_kind -> (Immutable_buffer.t, string) result
  method print_argument : G.argument -> (Immutable_buffer.t, string) result
  method print_arguments : G.arguments -> (Immutable_buffer.t, string) result

  method print_unbracketed_arguments :
    G.argument list -> (Immutable_buffer.t, string) result

  method print_dot_access :
    G.expr -> G.tok -> G.field_name -> (Immutable_buffer.t, string) result

  method print_field_name : G.field_name -> (Immutable_buffer.t, string) result
  method print_name : G.name -> (Immutable_buffer.t, string) result
  method print_ident : G.ident -> (Immutable_buffer.t, string) result

  method print_call :
    G.expr -> G.arguments -> (Immutable_buffer.t, string) result

  (* Takes `expr` rather than `operator` to facilitate hybrid_print. The
   * hybrid_print primary printer takes an `AST_generic.any`, and there is no
   * `any` variant for `operator`.
   *
   * We could refactor to support this but unless we run into other similar
   * issues, it doesn't seem worth the additional boilerplate. This doesn't
   * make implementing a printer any more difficult, since `print_expr` should
   * handle `IdSpecial (Op ...)` anyway. *)
  method print_opcall :
    G.expr -> G.arguments -> (Immutable_buffer.t, string) result

  method print_ordinary_call :
    G.expr -> G.arguments -> (Immutable_buffer.t, string) result

  (* TODO Add more nodes as needed. *)

  (* Clients should not (and cannot) call the methods below, but subclasses of
   * base_printer should consider overriding them to control printing
   * behavior. *)

  (* Determines whether the node in question needs to be surrounded by
   * parentheses. For example, `2 + 3 * 5` has a different meaning than
   * `(2 + 3) * 5`.
   *
   * TODO Include some context as a paremeter, which will be needed when we
   * want to insert parentheses only where absolutely necessary. *)
  method private needs_parens : G.any -> bool

  (* Subclasses should normally override print_expr_without_parens rather than
   * print_expr, unless they want to modify the parentheses-insertion behavior in
   * a way that is not possible by overriding needs_parens. Clients should not
   * call this directly. *)
  method private print_expr_without_parens :
    G.expr -> (Immutable_buffer.t, string) result
end

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Ok if and only if `f` returns `Ok for all elements. Short circuits when
 * the first `Error is encountered.
 *
 * TODO Make tail recursive?
 * TODO Move to Common?
 *)
let rec map_all f = function
  | [] -> Ok []
  | hd :: tl ->
      let/ hd = f hd in
      let/ tl = map_all f tl in
      Ok (hd :: tl)

(* Create an error message using the current backtrace. This should give people
 * enough information to track down which node leads to the printing issue
 * without getting too verbose. Unfortunately we might miss callstack entries
 * due to tail call optimization. If that becomes a problem we should switch to
 * a different error-reporting tactic. *)
let print_fail () =
  let callstack = Printexc.(get_callstack 10 |> raw_backtrace_to_string) in
  let msg = spf "Could not print AST:\n%s" callstack in
  Error msg

(*****************************************************************************)
(* Helper classes to inherit from *)
(*****************************************************************************)

(* We could consider partially replacing this with an autogenerated reduce
 * visitor, where `plus` simply returns `Error`. That could cut down on the
 * boilerplate of handling e.g. the any case, but since every node needs to be
 * handled explicitly for complete pretty-printing, it might be more trouble
 * than it's worth. *)
class base_printer : printer_t =
  object (self)
    method print_any =
      function
      | G.E expr -> self#print_expr expr
      | G.Ar arg -> self#print_argument arg
      (* TODO Handle other kinds *)
      | _ -> print_fail ()

    method print_expr e =
      let/ res = self#print_expr_without_parens e in
      (* An alternative would be to simply have the language-specific printers
       * add parentheses when they implement `print_opcall` and any other
       * expressions that need parentheses. Doing it here instead has the
       * advantage that subclasses need only concern themselves with correctly
       * printing a given node in isolation, and don't need to worry about
       * whether it needs to be wrapped in parentheses.
       *
       * Otherwise, this logic would need to be duplicated in all of the
       * language printers as well as the hybrid printer used for autofix. There
       * will surely be some language-specific differences in parenthesis
       * insertion, but there is enough commonality between languages that much
       * of the logic should be reusable. *)
      Ok (self#add_parens_if_needed (G.E e) res)

    method print_dot_access _ _ _ = print_fail ()

    method print_field_name =
      function
      | G.FN name -> self#print_name name
      | G.FDynamic e -> self#print_expr e

    method private print_expr_without_parens { e; _ } = self#print_expr_kind e
    method print_expr_kind _ = print_fail ()
    method print_argument _ = print_fail ()
    method print_arguments _ = print_fail ()
    method print_unbracketed_arguments _ = print_fail ()
    method print_name _ = print_fail ()
    method print_ident _ = print_fail ()

    method print_call e args =
      match e.G.e with
      (* Binary operator expressions are desugared to calls, but are different
       * enough that they merit a separate method here. *)
      | G.IdSpecial (G.Op _, _) -> self#print_opcall e args
      (* TODO also handle other kinds of IdSpecial *)
      | _ -> self#print_ordinary_call e args

    method print_opcall _ _ = print_fail ()
    method print_ordinary_call _ _ = print_fail ()

    (* TODO Add more nodes as needed. *)

    (* Currently is overly defensive, and inserts parentheses whenever they
     * *might* be needed. At some point, we should print them only when they are
     * actually needed. *)
    method private needs_parens =
      function
      | G.E { e = G.Call ({ e = G.IdSpecial (G.Op _, _); _ }, _); _ } -> true
      | _ -> false

    method private add_parens_if_needed any res =
      (* TODO Is it possible for the result to already be wrapped in parens? If
       * so, we should check for them here to avoid double-wrapping. *)
      if self#needs_parens any then combine [ b "("; res; b ")" ] else res
  end

(* Printing for constructs that are identical across many languages. Unlike
 * base_printer, it's not expected that every printer inherit from this. But
 * many printers should. This can be split up as needed so that printers can use
 * pieces a la carte, but care should be taken to avoid making the inheritance
 * graph overly complicated.
 *
 * This is virtual rather than a subclass of base_printer so that language
 * printers can pull pieces from this and other classes as needed to concisely
 * build up a complete printer. *)
class virtual common_printer =
  object (self)
    method print_arguments ((_b1, args, _b2) : G.arguments) =
      let/ args = self#print_unbracketed_arguments args in
      (* TODO Consider using original tokens for parens when available? *)
      Ok (combine [ b "("; args; b ")" ])

    method print_unbracketed_arguments args =
      let/ args = map_all self#print_argument args in
      Ok (combine ~sep:", " args)

    method print_argument =
      function
      | G.Arg e -> self#print_expr e
      | _ -> print_fail ()

    method print_expr_kind =
      function
      | G.Call (e, args) -> self#print_call e args
      | G.N name -> self#print_name name
      | G.DotAccess (e, tok, field) -> self#print_dot_access e tok field
      | _ -> print_fail ()

    method print_name =
      function
      | G.Id (ident, _) -> self#print_ident ident
      | G.IdQualified _ -> print_fail ()

    method print_ident (str, _) = Ok (b str)

    method print_ordinary_call e args =
      let/ e = self#print_expr e in
      let/ args = self#print_arguments args in
      Ok (combine [ e; args ])

    method print_opcall (e : G.expr) (args : G.arguments) =
      match args with
      | _, [ l; r ], _ ->
          let/ l = self#print_argument l in
          let/ e = self#print_expr e in
          let/ r = self#print_argument r in
          Ok (combine ~sep:" " [ l; e; r ])
      | _ -> print_fail ()

    method virtual print_expr : G.expr -> (Immutable_buffer.t, string) result

    method virtual print_call
        : G.expr -> G.arguments -> (Immutable_buffer.t, string) result

    method virtual print_dot_access
        : G.expr -> G.tok -> G.field_name -> (Immutable_buffer.t, string) result
  end

(*****************************************************************************)
(* Languages ugly printers *)
(*****************************************************************************)

class python_printer : printer_t =
  object (self)
    inherit base_printer
    inherit! common_printer

    method! print_dot_access e _tok field =
      let/ e = self#print_expr e in
      let/ field = self#print_field_name field in
      Ok (combine [ e; b "."; field ])
  end

class jsts_printer : printer_t =
  object (_self)
    inherit base_printer
    inherit! common_printer
  end

class ocaml_printer : printer_t =
  object (_self)
    inherit base_printer
  end
