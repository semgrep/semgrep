(* "Ugly" printers print syntactically-correct code without attempting to
 * format it nicely (as opposed to a pretty printer such as those based
 * on Wadler's "Prettier Printer" paper).
 * It would be difficult, if not impossible, to print code that conforms
 * to everyone's style guides, linters, and
 * formatters, so the prettifying of code is left to other tools
 * (e.g., gofmt for Go, ocamlformat for OCaml, prettier for Javascript)
 *
 * UglyPrinter can print code snippets, not just full programs. Any of the
 * `print_` methods can be used by consumers.
 *
 * If an "Ugly" printer encounters a construct that it is unable to handle,
 * it returns an error string result
 *)

class type printer_t = object
  method print_any : AST_generic.any -> (Immutable_buffer.t, string) result

  (* Prints an expression, inserting parentheses around it if needed. *)
  method print_expr : AST_generic.expr -> (Immutable_buffer.t, string) result

  method print_expr_kind :
    AST_generic.expr_kind -> (Immutable_buffer.t, string) result

  method print_argument :
    AST_generic.argument -> (Immutable_buffer.t, string) result

  method print_arguments :
    AST_generic.arguments -> (Immutable_buffer.t, string) result

  method print_unbracketed_arguments :
    AST_generic.argument list -> (Immutable_buffer.t, string) result

  method print_dot_access :
    AST_generic.expr ->
    AST_generic.tok ->
    AST_generic.field_name ->
    (Immutable_buffer.t, string) result

  method print_field_name :
    AST_generic.field_name -> (Immutable_buffer.t, string) result

  method print_name : AST_generic.name -> (Immutable_buffer.t, string) result
  method print_ident : AST_generic.ident -> (Immutable_buffer.t, string) result

  method print_call :
    AST_generic.expr ->
    AST_generic.arguments ->
    (Immutable_buffer.t, string) result

  method print_opcall :
    AST_generic.expr ->
    AST_generic.arguments ->
    (Immutable_buffer.t, string) result

  method print_ordinary_call :
    AST_generic.expr ->
    AST_generic.arguments ->
    (Immutable_buffer.t, string) result

  (* Clients should not (and cannot) call the methods below *)
  method private needs_parens : AST_generic.any -> bool

  method private print_expr_without_parens :
    AST_generic.expr -> (Immutable_buffer.t, string) result
end

class python_printer : printer_t
class jsts_printer : printer_t
class ocaml_printer : printer_t
