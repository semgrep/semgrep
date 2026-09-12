(*
   Abbreviations for common words, to be used in generated names.
   These are meant to be better than truncating a word at an arbitrary
   offset.
*)

let list = [
  "alternative", "alt";
  "anonymous", "anon";
  "application", "app";
  "argument", "arg";
  "arguments", "args";
  "array", "array";
  "assignment", "assign";
  "assignments", "assigns";
  "begin", "begin";
  "beginning", "begin";
  "binary", "bin";
  "binding", "bind";
  "blank", "blank";
  "block", "blk";
  "blocks", "blks";
  "body", "body";
  "boolean", "bool";
  "break", "brk";
  "call", "call";
  "character", "char";
  "choice", "choice";
  "command", "cmd";
  "condition", "cond";
  "constant", "cst";
  "content", "content";
  "contents", "content";
  "curly", "curl";
  "description", "desc";
  "divide", "div";
  "elif", "elif";
  "else", "else";
  "elsif", "elsif";
  "exception", "exc";
  "expression", "exp";
  "field", "field";
  "finish", "fin";
  "float", "float";
  "function", "func";
  "ident", "id";
  "identifier", "id";
  "integer", "int";
  "keyword", "kw";
  "length", "len";
  "line", "line";
  "list", "list";
  "literal", "lit";
  "method", "meth";
  "minus", "minus";
  "multiply", "mul";
  "name", "name";
  "newline", "nl";
  "number", "num";
  "object", "obj";
  "operator", "op";
  "option", "opt";
  "optional", "opt";
  "parameter", "param";
  "parameters", "params";
  "parentheses", "parens";
  "parenthesis", "paren";
  "parenthesized", "paren";
  "pattern", "pat";
  "percent", "perc";
  "plus", "plus";
  "primary", "prim";
  "program", "prog";
  "reference", "ref";
  "references", "refs";
  "regex", "regex";
  "regexp", "regex";
  "resolution", "resol";
  "return", "ret";
  "scope", "scope";
  "sequence", "seq";
  "simple", "simple";
  "slash", "slash";
  "space", "sp";
  "star", "star";
  "start", "start";
  "statement", "stmt";
  "statements", "stmts";
  "string", "str";
  "symbol", "symb";
  "terminator", "term";
  "times", "times";
  "token", "tok";
  "unary", "un";
  "variable", "var";
  "vector", "vec";
  "yield", "yield";
  "escape", "esc";
  "interpolation", "interp";
]

(*
   Table for abbreviating the 3 forms:
   - "identifier" -> "id"
   - "Identifier" -> "Id"
*)
let table =
  let tbl = Hashtbl.create 100 in
  List.iter (fun (orig, abbr) ->
    Hashtbl.replace tbl
      (String.lowercase_ascii orig) (String.lowercase_ascii abbr);
    Hashtbl.replace tbl
      (String.capitalize_ascii orig) (String.capitalize_ascii abbr);
  ) list;
  tbl

let is_plural s =
  let len = String.length s in
  len >= 3
  && s.[len-1] = 's'
  && s.[len-2] <> 's'

let truncate_string s trunc_len =
  let len = String.length s in
  if len > trunc_len then
    String.sub s 0 trunc_len
  else
    s

let is_allcaps s =
  try
    String.iter (function
      | 'A'..'Z' -> ()
      | _ -> raise Exit
    ) s;
    true
  with Exit -> false

let abbreviate_word ?(max_len = 6) ?(trunc_len = 4) s =
  if is_allcaps s then
    s
  else
    match Hashtbl.find_opt table s with
    | Some abbr -> abbr
    | None ->
        let plural = is_plural s in
        let max_len = if plural then max_len + 1 else max_len in
        let trunc_len = if plural then trunc_len + 1 else trunc_len in
        if String.length s > max_len then
          let abbrev = truncate_string s trunc_len in
          if plural then
            abbrev ^ "s"
          else
            abbrev
        else
          s

let ascii_word_separator = Str.regexp "[^A-Za-z]+"

(*
   Abbreviate the words identified within a phrase or identifier.
*)
let words ?max_len ?trunc_len s =
  let components = Str.bounded_full_split ascii_word_separator s 0 in
  List.map (function
    | Str.Delim sep -> sep
    | Str.Text word -> abbreviate_word ?max_len ?trunc_len word
  ) components
  |> String.concat ""
