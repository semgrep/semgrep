(*
   AST and matching of a glob pattern against a path.
   This is purely syntaxic: the file system is not accessed.

   We could use Re.Glob from the ocaml-re library but it doesn't expose
   the AST of the glob pattern, and this prevents us from making
   transformations required by gitignore such as treating the pattern
   'foo/bar' as equivalent to '/foo/bar' but not treat 'foo' as '/foo'.
*)

type char_class_range = Class_char of char | Range of char * char
type char_class = { complement : bool; ranges : char_class_range list }

type component_fragment =
  | Char of char
  | Char_class of char_class
  | Question
  | Star

type component = Component of component_fragment list | Ellipsis
type pattern = component list
type path = FPath.t
type matcher = { source : string; re : Re.t }

let compile ~source:_ _pat = failwith "todo"
let run _matcher _path = failwith "todo"
let source matcher = matcher.source
