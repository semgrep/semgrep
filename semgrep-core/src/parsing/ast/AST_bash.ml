(*
   Bash AST type definition.

   References:
   - man page: https://man7.org/linux/man-pages/man1/bash.1.html#SHELL_GRAMMAR
*)

(*
   Disable warnings against records with identical field names.
   Typically, it's the 'loc' field.
*)
[@@@warning "-30"]

(*****************************************************************************)
(* Token info *)
(*****************************************************************************)
(*
   We keep braces and such because they provide the location of the
   original region of code.
*)

type tok = Parse_info.t [@@deriving show]

(*
   A location is made of the first token and the last token in the source
   code. Each node of the AST has a field that's either a loc or a
   single tok.
*)
type loc = tok * tok

type 'a wrap = 'a * tok [@@deriving show]

type 'a bracket = tok * 'a * tok [@@deriving show]

(*****************************************************************************)
(* AST definition *)
(*****************************************************************************)

type todo = TODO of loc

type pipeline_bar = Bar of tok | Bar_ampersand of tok

type unary_control_operator =
  | Foreground of (* ';' or '\n' or ';;' *) tok
  | Background of (* & *) tok

type redirect = todo

(*
   Redirections can occur anywhere within a simple command. We extract
   them into a list while preserving their order, which matters.
*)
type command_with_redirects = {
  loc : loc;
  command : command;
  redirects : redirect list;
}

(*
   A command is anything that can be chained into a pipeline.

   These are all the different kinds of commands as presented in the
   Bash man page. IO redirections are hoisted up into the
   'command_with_redirects' wrapper.

   The syntax doesn't allow for nested coprocesses, but we allow it in
   the AST because it's simpler this way.

   The "grammar" section of the man page doesn't cover assignments or
   anything below that.
*)
and command =
  | Simple_command of loc * simple_command
  | Compound_command of loc * compound_command
  | Coprocess of loc * string option * (* simple or compound *) command
  | Assignment of loc * assignment
  | Declaration of loc * declaration
  | Negated_command of loc * tok * command
  | Function_definition of loc * function_definition

(*
   Example of a so-called simple command:

   MOO=42 cowsay $options 2>&1 --force
   ^^^^^^
 assignment
          ^^^^^^
          arg 0
                 ^^^^^^^^
                   arg 1
                 (before
                expansion)
                               ^^^^^^^
                                arg 2
                          ^^^^
                       redirect

   The argument list is a list of words to be expanded at run time.
   In the example, $options (no quotes) would expand into any number
   of actual arguments for the command to run (argument 0 after expansion).
*)
and simple_command = {
  loc : loc;
  assignments : assignment list;
  arguments : expression list;
}

(*
   Sample pipeline

   foo -x 2>&1 | bla floob &
   ^^^^^^^^^^^   ^^^^^^^^^
    command0     command1
   ^           ^           ^
 no bar       bar         pipeline control operator

   The first command of a pipeline is of the form (None, cmd). The
   commands after that are of the form (Some bar, cmd).

   The following is actually a list of two pipelines but we call this
   a pipeline for convenience when working with the AST:

     a | b && c | d
*)
and pipeline =
  | Command of loc * command_with_redirects
  | Pipeline of loc * pipeline * pipeline_bar * command_with_redirects
  | Control_operator of loc * pipeline * unary_control_operator

(*
   Sample bash list (blist)

   foo | bar; baz
   ^^^^^^^^^
   pipeline 0
              ^^^
            pipeline 1

   The following type definition is redundant but it's meant to be easy
   to construct and to read from:
*)
and blist =
  | Seq of loc * blist * blist
  | And of loc * blist * tok (* && *) * blist
  | Or of loc * blist * tok (* || *) * blist
  | Pipelines of loc * pipeline list
  | Empty of loc

(*
   All the commands that are called "compound commands" in the man page.
*)
and compound_command =
  | Subshell of loc * subshell
  | Command_group of loc * command_group
  | Arithmetic_expression of loc * arithmetic_expression
  | Conditional_expression of loc * conditional_expression
  | For_loop of loc * for_loop
  | For_loop_c_style of loc * for_loop_c_style
  | Select of loc * select
  | Case of loc * case
  | If of loc * if_
  | While_loop of loc * while_
  | Until_loop of loc * until_

(*
   Bash syntax allows the body of a function definition only to a
   compound command but it doesn't matter in the AST that we also
   allow simple commands.
*)
and function_definition = {
  loc : loc;
  function_ : string wrap option;
  func_name : string wrap;
  func_body : command;
}

and subshell = todo

and command_group = todo

and arithmetic_expression = todo

and conditional_expression = todo

and for_loop = todo

and for_loop_c_style = todo

and select = todo

and case = todo

and if_ = todo

and while_ = todo

and until_ = todo

and assignment = todo

and declaration = todo

and expression =
  | Word of string wrap
  | String of string_fragment list bracket
  | String_fragment of loc * string_fragment
  | Raw_string of string wrap
  | Ansii_c_string of string wrap
  | Special_character of string wrap
  | String_expansion of string wrap
  | Concatenation of loc * expression list
  | Semgrep_ellipsis of tok
  | Semgrep_metavariable of string wrap
  | Equality_test of loc * eq_op * right_eq_operand
  | Expression_TODO of loc

(* Fragment of a double-quoted string *)
and string_fragment =
  | String_content of string wrap
  | Expansion of loc * expansion
  | Command_substitution of (* $(foo; bar) or `foo; bar` *) blist bracket

(* $foo or something like ${foo ...} *)
and expansion =
  | Simple_expansion of loc * tok * variable_name
  | Complex_expansion of complex_expansion bracket

and variable_name =
  | Simple_variable_name of string wrap
  | Special_variable_name of string wrap

and complex_expansion =
  | Variable of loc * variable_name
  | Complex_expansion_TODO of loc

and eq_op = EQTILDE of (* "=~" *) tok | EQEQ of (* "==" *) tok

and right_eq_operand = Literal of loc * expression | Regexp of string wrap

(* A program is a list of pipelines *)
type program = blist

(*[@@deriving show { with_path = false }]*)

(*****************************************************************************)
(* Extraction of the first token, used for its location info *)
(*****************************************************************************)

let left_loc_tok (left, right) = if Parse_info.is_fake left then right else left

let right_loc_tok (left, right) =
  if Parse_info.is_fake right then left else right

(*
   Form a new location from a leftmost location and a rightmost location.
   We try to mitigate problems due to fake tokens.
*)
let range a b =
  let start_tok = left_loc_tok a in
  let end_tok = right_loc_tok b in
  if Parse_info.is_fake end_tok then (start_tok, start_tok)
  else if Parse_info.is_fake start_tok then (end_tok, end_tok)
  else (start_tok, end_tok)

let fake_tok = Parse_info.fake_info "fake"

let fake_loc = (fake_tok, fake_tok)

let compare_loc (a, _) (b, _) = Parse_info.compare_pos a b

let min_loc_tok a b =
  let tok_a = left_loc_tok a in
  let tok_b = left_loc_tok b in
  if Parse_info.is_fake tok_a then tok_b
  else if Parse_info.is_fake tok_b then tok_a
  else if Parse_info.compare_pos tok_a tok_b <= 0 then tok_a
  else tok_b

let max_loc_tok a b =
  let tok_a = right_loc_tok a in
  let tok_b = right_loc_tok b in
  if Parse_info.is_fake tok_b then tok_a
  else if Parse_info.is_fake tok_a then tok_b
  else if Parse_info.compare_pos tok_a tok_b <= 0 then tok_b
  else tok_a

(*
   Return the span of two ranges (locations) while trying to eliminate
   fake positions (fake tokens).
   If it's known that 'a' starts before 'b' and doesn't overlap,
   then use 'range' instead.
*)
let union_loc a b = (min_loc_tok a b, max_loc_tok a b)

(*
   Return the span of a list of ranges (locations).
   The location is extracted with the provided 'get_loc' function.
*)
let list_loc get_loc l =
  match l with
  | [] -> fake_loc
  | first :: other ->
      List.fold_left
        (fun loc x -> union_loc loc (get_loc x))
        (get_loc first) other

let wrap_loc (_, tok) : loc = (tok, tok)

let bracket_loc (start_tok, _, end_tok) : loc = (start_tok, end_tok)

let todo_loc (TODO loc) = loc

let pipeline_bar_loc = function
  | Bar tok -> (tok, tok)
  | Bar_ampersand tok -> (tok, tok)

let unary_control_operator_loc = function
  | Foreground tok -> (tok, tok)
  | Background tok -> (tok, tok)

let redirect_loc x = todo_loc x

let command_with_redirects_loc x = x.loc

let command_loc = function
  | Simple_command (loc, _) -> loc
  | Compound_command (loc, _) -> loc
  | Coprocess (loc, _, _) -> loc
  | Assignment (loc, _) -> loc
  | Declaration (loc, _) -> loc
  | Negated_command (loc, _, _) -> loc
  | Function_definition (loc, _) -> loc

let simple_command_loc (x : simple_command) = x.loc

let pipeline_loc = function
  | Command (loc, _) -> loc
  | Pipeline (loc, _, _, _) -> loc
  | Control_operator (loc, _, _) -> loc

let blist_loc = function
  | Seq (loc, _, _) -> loc
  | And (loc, _, _, _) -> loc
  | Or (loc, _, _, _) -> loc
  | Pipelines (loc, _) -> loc
  | Empty loc -> loc

let compound_command_loc = function
  | Subshell (loc, _) -> loc
  | Command_group (loc, _) -> loc
  | Arithmetic_expression (loc, _) -> loc
  | Conditional_expression (loc, _) -> loc
  | For_loop (loc, _) -> loc
  | For_loop_c_style (loc, _) -> loc
  | Select (loc, _) -> loc
  | Case (loc, _) -> loc
  | If (loc, _) -> loc
  | While_loop (loc, _) -> loc
  | Until_loop (loc, _) -> loc

let function_definition_loc (x : function_definition) = x.loc

let subshell_loc (x : subshell) = todo_loc x

let command_group_loc (x : command_group) = todo_loc x

let arithmetic_expression_loc (x : arithmetic_expression) = todo_loc x

let conditional_expression_loc (x : conditional_expression) = todo_loc x

let for_loop_loc (x : for_loop) = todo_loc x

let for_loop_c_style_loc (x : for_loop_c_style) = todo_loc x

let select_loc (x : select) = todo_loc x

let case_loc (x : case) = todo_loc x

let if_loc (x : if_) = todo_loc x

let while_loc (x : while_) = todo_loc x

let until_loc (x : until_) = todo_loc x

let assignment_loc (x : assignment) = todo_loc x

let assignment_list_loc (x : assignment list) = list_loc assignment_loc x

let declaration_loc (x : declaration) = todo_loc x

let expression_loc = function
  | Word x -> wrap_loc x
  | String x -> bracket_loc x
  | String_fragment (loc, _) -> loc
  | Raw_string x -> wrap_loc x
  | Ansii_c_string x -> wrap_loc x
  | Special_character x -> wrap_loc x
  | String_expansion x -> wrap_loc x
  | Concatenation (loc, _) -> loc
  | Semgrep_ellipsis tok -> (tok, tok)
  | Semgrep_metavariable x -> wrap_loc x
  | Equality_test (loc, _, _) -> loc
  | Expression_TODO loc -> loc

let string_fragment_loc = function
  | String_content x -> wrap_loc x
  | Expansion (loc, _) -> loc
  | Command_substitution x -> bracket_loc x

let expansion_loc = function
  | Simple_expansion (loc, _, _) -> loc
  | Complex_expansion x -> bracket_loc x

let variable_name_loc = function
  | Simple_variable_name x -> wrap_loc x
  | Special_variable_name x -> wrap_loc x

let complex_expansion_loc = function
  | Variable (loc, _) -> loc
  | Complex_expansion_TODO loc -> loc

let command_substitution_loc x = bracket_loc x

let eq_op_loc = function
  | EQTILDE (* "=~" *) tok -> (tok, tok)
  | EQEQ (* "==" *) tok -> (tok, tok)

let right_eq_operand_loc = function
  | Literal (loc, _) -> loc
  | Regexp x -> wrap_loc x

(*****************************************************************************)
(* Helpers for users of the module *)
(*****************************************************************************)

let concat_blists (x : blist list) : blist =
  match List.rev x with
  | [] -> assert false
  | last_blist :: blists ->
      let end_ = blist_loc last_blist in
      List.fold_left
        (fun acc blist ->
          let start = blist_loc blist in
          let loc = range start end_ in
          Seq (loc, blist, acc))
        last_blist blists
