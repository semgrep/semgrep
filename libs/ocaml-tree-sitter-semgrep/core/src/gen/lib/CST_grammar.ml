(*
   Type definition of a normalized tree-sitter grammar description.
*)

type ident = string
[@@deriving show {with_path = false}]

type token_description =
  | Constant of string
  (* A constant string defining a token.
     It is used as the name for the pattern if it's not the right-hand side
     of a named rule. *)

  | Pattern of string
  (* A regexp defining a set of valid tokens.
     It is used as the name for the pattern if it's not the right-hand side
     of a named rule.

     Sample pattern node in grammar.js:

      /[a-z]+/

     here:

      Pattern "[a-z]+"  (* shows up as "type":"[a-z]+" in tree-sitter
                           output. The actual token is extracted from
                           the source file using location fields. *)

     == Problem ==

     grammar.js:

      choice(/a+/, "a+")

     here:

      Choice [Pattern "a+", String "a+"]

     This presumably results in indistinguishable "type":"a+" fields in the
     tree-sitter output. This is a problem if they must be interpreted
     differently.
  *)

  | Token
  (* derived from a token() or token.immediate() construct.
     It may be derived from complex rules (omitted here) which result in
     a single token. *)

  | External
  (* a symbol declared in the 'externals' list and produced by an external
     C parser. *)
[@@deriving show {with_path = false}]

type token = {
  name: string;
  (* the node 'type' in tree-sitter's output.
     It may not be an alphanumeric identifier.

     It is the enclosing rule name or alias, if there's one.
     Otherwise it's the 'value' field.

     The token(seq(...)) construct can result in a token without a name.
     We treat these as errors. See the '/tests/token' example.

     grammar.js:
       times: $ => "*"
     name: "times"

     grammar.js:
       repeat($.times)
     name of the repeated element: "times"

     grammar.js:
       repeat("*")
     name of the repeated element: "*"
  *)

  is_inlined: bool;
  (* always true for Constant tokens whose name is not an alphanumeric
     identifier. May be true or false for other kinds of tokens, depending
     on whether the type expression uses the 'Token.t' type directly
     (inlined) or the 'name' field (not inlined). *)

  description: token_description;
  (* informational *)
}
[@@deriving show {with_path = false}]

type rule_body =
  (* atomic (leaves) *)
  | Symbol of ident
  | Token of token
  | Blank
  (* matches any sequence without consuming it. *)

  (* composite (nodes) *)
  | Repeat of rule_body (* zero or more *)
  | Repeat1 of rule_body (* one or more *)
  | Optional of rule_body (* zero or one *)
  | Choice of (ident * rule_body) list
  (* (name, type) where the name is the name of the ocaml constructor
     suitable for use a classic or polymorphic variant, e.g. "Exp_int". *)

  | Seq of rule_body list
  | Alias of ident * rule_body
  (* Alias (dummy rule name, anonymous rule)
     Note that argument order in JS is reversed. It's alias(value, name)
     rather than (name, value).

     This is used as a hack to work around pattern nodes (/.../)
     or token nodes (token(...)) that don't show up in the CST produced
     by tree-sitter. Wrapping such constructs in a alias() allows the CST
     node to show up e.g. /[a-z]+/ would become
     alias(/[a-z]+/, $.pat_1234) with $.pat_1234 being a dummy rule
     with a unique name.

     An earlier workaround used to consist solely in creating a named
     rule for the pattern or token, but this would change the parsing
     semantics in some cases.

     Note that aliases that exist in the source grammar are always
     discarded because in general they're ambiguous names and it's simpler
     to ignore them.
  *)
[@@deriving show {with_path = false}]

type rule = {
  name: ident;
  is_rec: bool;

  is_inlined_rule: bool;
  (* An inlined rule is a rule name that is ignored in the tree-sitter
     grammar because it's not the grammar's entry point and no other
     rule references it. This refers to the inlining performed in
     Simplify_grammar.ml according to the 'inline' section of grammar.
  *)

  is_inlined_type: bool;
  (* An inlined type is a type definition that was inlined everywhere
     it occurs in other type expressions.

     For example, consider the rule 'foo':
       type foo = [`Bar|`Baz]
       and type program = foo option

     Inlining 'foo' results in
       type program = [`Bar|`Baz] option
       type foo = [`Bar|`Baz]

     The rule 'foo' is now marked as inlined, and it can be ignored for
     some purposes.
  *)

  body: rule_body;
}
[@@deriving show {with_path = false}]

type grammar = {
  name: string;
  entrypoint: string;

  rules: rule list list;
  (* rules, grouped and sorted in dependency order. *)

  extras: string list;
  (* rules names for constructs that can occur anywhere independently from
     the grammar, such as comments. Other extras such as string literals
     and patterns were removed because we don't need them. *)
}
[@@deriving show {with_path = false}]

(* alias *)
type t = grammar
[@@deriving show {with_path = false}]

(* Make an efficient rule-lookup function. *)
let make_rule_lookup grammar =
  let tbl = Hashtbl.create 100 in
  List.iter (fun rule_group ->
    List.iter (fun (rule : rule) -> Hashtbl.add tbl rule.name rule) rule_group
  ) grammar.rules;
  fun name ->
    Hashtbl.find_opt tbl name
