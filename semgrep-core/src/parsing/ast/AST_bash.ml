(*
   Bash AST type definition.

   References:
   - man page: https://man7.org/linux/man-pages/man1/bash.1.html#SHELL_GRAMMAR
*)

(*****************************************************************************)
(* Token info *)
(*****************************************************************************)
(*
   We keep braces and such because they provide the location of the
   original region of code.
*)

type tok = Parse_info.t [@@deriving show]

type 'a wrap = 'a * tok [@@deriving show]

type 'a bracket = tok * 'a * tok [@@deriving show]

(*****************************************************************************)
(* AST definition *)
(*****************************************************************************)

type todo = TODO

type pipeline_bar = Bar of tok | Bar_ampersand of tok

type unary_control_operator =
  | Foreground of (* ';' or '\n' or ';;' *) tok
  | Background of (* & *) tok

type redirect = todo

(*
   Redirections can occur anywhere within a simple command. We extract
   them into a list while preserving their order, which matters.
*)
type command_with_redirects = { command : command; redirects : redirect list }

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
  | Simple_command of simple_command
  | Compound_command of compound_command
  | Coprocess of string option * (* simple or compound *) command
  | Assignment of assignment
  | Declaration of declaration
  | Negated_command of tok * command
  | Function_definition of function_definition

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
  | Command of command_with_redirects
  | Pipeline of pipeline * pipeline_bar * command_with_redirects
  | Control_operator of pipeline * unary_control_operator

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
  | Seq of (blist * blist)
  | And of (blist * tok (* && *) * blist)
  | Or of (blist * tok (* || *) * blist)
  | Pipelines of pipeline list
  | Empty

(*
   All the commands that are called "compound commands" in the man page.
*)
and compound_command =
  | Subshell of subshell
  | Command_group of command_group
  | Arithmetic_expression of arithmetic_expression
  | Conditional_expression of conditional_expression
  | For_loop of for_loop
  | For_loop_c_style of for_loop_c_style
  | Select of select
  | Case of case
  | If of if_
  | While_loop of while_
  | Until_loop of until_

(*
   Bash syntax allows the body of a function definition only to a
   compound command but it doesn't matter in the AST that we also
   allow simple commands.
*)
and function_definition = {
  func_function : string wrap option;
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
  | String_fragment of string_fragment
  | Raw_string of string wrap
  | Ansii_c_string of string wrap
  | Special_character of string wrap
  | String_expansion of string wrap
  | Concatenation of expression list
  | Semgrep_ellipsis of tok
  | Semgrep_metavariable of string wrap
  | Expression_TODO

(* Fragment of a double-quoted string *)
and string_fragment =
  | String_content of string wrap
  | Expansion of expansion
  | Command_substitution of blist bracket

(* $foo or something like ${foo ...} *)
and expansion =
  | Simple_expansion of (tok * variable_name)
  | Complex_expansion of complex_expansion bracket

and variable_name =
  | Simple_variable_name of string wrap
  | Special_variable_name of string wrap

and complex_expansion = Variable of variable_name | Complex_expansion_TODO

(* $(foo; bar) or `foo; bar` *)
and command_substitution = blist bracket

(* A program is a list of pipelines *)
type program = blist

(*[@@deriving show { with_path = false }]*)

let concat_lists (x : blist list) : blist =
  match List.rev x with
  | [] -> Empty
  | last_list :: lists ->
      List.fold_left (fun acc list -> Seq (list, acc)) last_list lists
