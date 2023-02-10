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

module M = Glob_matcher

type token =
  | Slash
  | Char of char
  | Char_class of M.char_class
  | Question
  | Star
  | Starstar

exception Syntax_error of string

(* Prepend characters from string in reverse order *)
let chars_of_string str acc =
  String.fold_left (fun acc c -> Char c :: acc) acc str

let syntax_error msg =
  raise (Syntax_error msg)
}

let neg = ['^' '!']

rule tokens acc = parse
| '/'      { tokens (Slash :: acc) lexbuf }
| "**"     { (* only special if it occupies a whole path component. This
                is dealt with later. *)
             tokens (Starstar :: acc) lexbuf }
| '*'      { tokens (Star :: acc) lexbuf }
| '?'      { tokens (Question :: acc) lexbuf }
| '[' (neg? as negation)
      ('['? as literal_bracket)
           {
             let range_acc =
               if literal_bracket <> "" then
                 [M.Class_char '[']
               else
                 []
             in
             let ranges = char_class range_acc lexbuf in
             let complement = (negation <> "") in
             tokens (Char_class { complement; ranges } :: acc) lexbuf
           }
| '\\' (_ as c)
           { tokens (Char c :: acc) lexbuf }
| [^ '/' '*' '?' '[' '\\']+ as str
           { tokens (chars_of_string str acc) lexbuf }
| eof      { List.rev acc }

and char_class acc = parse
| ']'      { List.rev acc }
| ([^']'] as a) '-' ([^']'] as b)
           { char_class (Range (a, b) :: acc) lexbuf }
| [^']'] as c
           { char_class (Class_char c :: acc) lexbuf }
| eof      { syntax_error "malformed glob pattern: missing ']'" }

{
  (*
     Recover the structure as a list of slash-separated components.
     TODO: use menhir instead of the code below if it's too hard to follow
   *)
  let rec components (xs : token list) : M.component list =
    match xs with
    | Starstar :: ([] | Slash :: _ as xs) ->
        Ellipsis :: components xs
    | Starstar :: xs ->
        (* not an actual ellipsis, convert to two stars, simplify to
           a single star, and try again *)
        components (Star :: xs)
    | xs ->
        let frags, xs = fragments [] xs in
        Component frags :: components xs

  and fragments acc (xs : token list) : M.component_fragment list * _ =
    match xs with
    | (Slash | Starstar) :: _
    | [] ->
        List.rev acc, xs
    | Char c :: xs -> fragments (Char c :: acc) xs
    | Char_class x :: xs -> fragments (Char_class x :: acc) xs
    | Question :: xs -> fragments (Question :: acc) xs
    | Star :: xs -> fragments (Star :: acc) xs

  let parse_string str =
    Lexing.from_string str
    |> tokens []
    |> components
}
