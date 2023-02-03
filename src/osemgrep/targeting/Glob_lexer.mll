{
(*
   Parse a glob pattern according to glob(3) and glob(7), which is the
   POSIX standard for old-fashioned shell globbing patterns for matching
   file paths. This is what gitignore uses.

   Examples:

   *.c          # all local files with a '.c' extension
   /tmp/**      # all valid paths under '/tmp/'
   Thing.ml?    # matches 'Thing.ml' as well as 'Thing.mli', 'Thing.mll', etc.
   [a-z0-9]     # matches a single character in these ranges
*)

type char_class =
  | Class_char of char
  | Range of char * char

type token =
  | Slash
  | Char of char
  | Char_class of char_class
  | Question
  | Star
  | Starstar

exception Syntax_error of string

let syntax_error msg =
  raise (Syntax_error msg)
}

rule tokens acc = parse
| '/'      { tokens (Slash :: acc) lexbuf }
| "**"     { (* only special if it occupies a whole path component. This
                is dealt with later. *)
             tokens (Slashslash :: acc) lexbuf }
| '*'      { tokens (Star :: acc) lexbuf }
| '?'      { tokens (Question :: acc) lexbuf }
| '['      { let cc = char_class lexbuf in
             tokens (Char_class cc :: acc) lexbuf }
| "[["     { let cc = start_char_class (Class_char '[') lexbuf in
             tokens (Char_class cc :: acc) lexbuf }
| '\\' (_ as c)
           { Char c }
| eof      { [] }

and char_class acc = parse
| ']'      { List.rev acc }
| ([^']'] as a) '-' ([^']'] as b)
           { char_class (Range (a, b) :: acc) lexbuf }
| [^']'] as c
           { char_class (Class_char c :: acc) lexbuf }
| eof      { syntax_error "malformed glob pattern: missing ']'" }

{
  type component_fragment =
    | Char of char
    | Char_class of char_class
    | Question
    | Star

  type component =
    | Component of component_fragment list
    | Ellipsis (* ** *)

  type t = component list

  let group_by_path_component (xs : token list) =

  let parse_string str =
    Lexing.from_string str
    |> tokens
    |> postprocess_tokens
}
