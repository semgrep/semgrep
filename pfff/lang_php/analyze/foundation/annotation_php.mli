(*s: annotation_php.mli *)
type annotation =
  | Owner of unixname
  | Emails of (email * notification_kind option) list
  | Status of string

  (* deprecated *)
  | Author of string

  | CalledFromPhpsh
  | CalledOutsideTfb
  | CalledDynamically
  | NotDeadCode
  | Have_THIS_FUNCTION_EXPIRES_ON

  | DataProvider of method_callback

  | Inject
  | Generics of string

  | Other of string

 and unixname = string

 and email = string
 and notification_kind = 
   | Immediate
   | Consistent
   | Daily

 and method_callback =
   | Method of string
   | MethodExternal of string (* class *) * string

exception AnnotationPb of string * Cst_php.info

(* The returned parse_info is the one associated with the whole comment.
 * We use it in cmf to raise errors like use of undefined function when
 * a a comment references a bad function name as a data provider for
 * a test.
 *)
val annotations_of_program_with_comments: 
  Parse_php.program_with_comments -> (annotation * Cst_php.info) list

(* Helper. The string is the string of a comment (with its markers). *)
val extract_annotations: string -> Cst_php.info -> annotation list

val vof_annotation: annotation -> OCaml.v
val str_debug_of_annotation: annotation -> string

val annotations_before: Cst_php.tok -> Parser_php.token list -> annotation list
val annotations_after: Cst_php.tok -> Parser_php.token list -> annotation list
(*x: annotation_php.mli *)
(*e: annotation_php.mli *)
