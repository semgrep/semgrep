(* Martin Jambon
 *
 * Copyright (C) 2021 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
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
(* Input kind *)
(*****************************************************************************)

(* The type of input. Each results in a slightly different generic AST. *)
type input_kind = Pattern | Program

(*****************************************************************************)
(* Token info *)
(*****************************************************************************)
(*
   We keep braces and such because they provide the location of the
   original region of code.
*)

type tok = Tok.t [@@deriving show]

(*
   A location is made of the first token and the last token in the source
   code. Each node of the AST has a field that's either a loc or a
   single tok.
*)
type loc = Tok_range.t
type 'a wrap = 'a * tok [@@deriving show]
type 'a bracket = tok * 'a * tok [@@deriving show]

(*****************************************************************************)
(* AST definition *)
(*****************************************************************************)

type todo = TODO of loc

(*
  The effect of '+=' depends on the type of variable:
  - usual string variables: append to the previous value
  - integer variables (created with 'declare -i'): add to the previous value
  - array variables: append to the previous array
*)
type assignment_operator = (* = *) Set | (* += *) Add

(* See list and descriptions at
   https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html
*)
type unary_test_operator =
  | FD_refers_to_terminal (* -t fd *)
  | Is_shell_option_enabled (* -o optname *)
  | Is_shell_variable_set (* -v varname *)
  | Is_shell_variable_a_name_ref (* -R varname. See also: declare -n *)
  | Is_empty_string (* -z *)
  | Is_nonempty_string (* -n *)
  (* Tests on files *)
  | File_exists (* -a, -e *)
  | Is_block_special_file (* -b *)
  | Is_character_special_file (* -c *)
  | Is_directory (* -d *)
  | Is_regular_file (* -f *)
  | Has_SGID_bit (* -g *)
  | Is_symlink (* -h, -L *)
  | Has_sticky_bit (* -k *)
  | Is_named_pipe (* -p *)
  | Is_readable (* -r *)
  | Is_nonempty_file (* -s *)
  | Has_SUID_bit (* -u *)
  | Is_writable (* -w *)
  | Is_executable (* -x *)
  | Is_owned_by_effective_group_id (* -G *)
  | Was_modified_since_last_read (* -N *)
  | Is_owned_by_effective_user_id (* -O *)
  | Is_socket (* -S *)
  | Other_unary_test_operator

(* See list and descriptions at
   https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html
*)
type binary_test_operator =
  | Same_physical_file (* -ef *)
  | File_newer_than (* -nt; true if only first file exists *)
  | File_older_than (* -ot; true if only first file exists *)
  | String_equal (* = always, == outside of [[ ]] *)
  | String_pattern_matching (* == within [[ ]] *)
  | String_not_equal (* != *)
  | String_lesser_than (* < within [[ ]] *)
  | String_greater_than (* > within [[ ]] *)
  (* Comparison of int literals with the 'test' command and of arithmetic
     expressions within [[ ]].

      test 9 -lt 10         --> true
      test 8+1 -lt 10       --> syntax error: '8+1' is not an int literal
      [[ 8+1 -lt 10 ]]      --> true
  *)
  | Int_equal
  | Int_not_equal
  | Int_lesser_than
  | Int_lesser_equal
  | Int_greater_than
  | Int_greater_equal
  | Other_binary_test_operator

type pipeline_bar = Bar | Bar_ampersand

(*
   Pipeline/command terminators that indicate that the command should
   run in the foreground i.e. synchronously: ';' or '\n' or ';;'
*)
type fg_op = Fg_newline | Fg_semi | Fg_semisemi
type unary_control_operator = Foreground of fg_op | (* & *) Background

type write_kind =
  | Write_truncate (* '>' *)
  | Write_append (* '>>' *)
  | Write_force_truncate

(* '>|' *)

type write_redir_src =
  | Stdout of tok
  | Stdout_and_stderr of tok
  | File_descriptor of int wrap

(* The destination for writes (">") or the source for reads ("<") *)
type file_redir_target =
  | File of expression
  | File_descriptor of int wrap
  | Stdout_and_stderr (*'&>' or '>&' *) of tok
  | Close_fd (* '-' *) of tok

and file_redirect =
  | Read of tok * file_redir_target
  | Write of write_redir_src * write_kind wrap * file_redir_target
  | Read_write

(* <> *)
and redirect =
  | File_redirect of loc * file_redirect
  | Read_heredoc of todo
  | Read_herestring of todo

(*
   Redirections can occur anywhere within a simple command. We extract
   them into a list while preserving their order, which matters.
*)
and cmd_redir = { loc : loc; command : command; redirects : redirect list }

(*
   A command is anything that can be chained into a pipeline.

   These are all the different kinds of commands as presented in the
   Bash man page. IO redirections are hoisted up into the
   'cmd_redir' wrapper.

   The syntax doesn't allow for nested coprocesses, but we allow it in
   the AST because it's simpler this way.

   The "grammar" section of the man page doesn't cover assignments or
   anything below that.
*)
and command =
  | Simple_command of simple_command
  (* &&/|| combine two commands into one.
     They don't form a list of pipelines like 'man bash' says. *)
  | And of loc * pipeline * tok (* && *) * pipeline
  | Or of loc * pipeline * tok (* || *) * pipeline
  (* What the manual refers to as "compound commands" *)
  | Subshell of loc * blist bracket
  | Command_group of loc * blist bracket
  | Sh_test of loc * sh_test
  | Bash_test of loc * bash_test
  | Arithmetic_expression of loc * arithmetic_expression
  | For_loop of
      loc
      * (* for *) tok
      * (* loop variable *)
        variable_name
      * (* in *)
      (tok * expression list) option
      * (* do *) tok
      * blist
      * (* done *) tok
  | For_loop_c_style of
      (* TODO: represent the loop header: for (( ... )); *)
      loc * blist
  | Select (* same syntax as For_loop *) of
      loc
      * (* select *) tok
      * (* loop variable *)
        variable_name
      * (* in *)
      (tok * expression list) option
      * (* do *) tok
      * blist
      * (* done *) tok
  | Case of
      loc
      * (* case *) tok
      * expression
      * (* in *) tok
      * case_clause list
      * (* esac *) tok
  | If of
      loc
      * (* if *) tok
      * blist
      * (* then *) tok
      * blist
      * elif list
      * else_ option
      * (* fi *) tok
  | While_loop of
      loc * (* while *) tok * blist * (* do *) tok * blist * (* done *) tok
  | Until_loop of
      loc * (* until *) tok * blist * (* do *) tok * blist * (* done *) tok
  (* Other commands *)
  | Coprocess of loc * string option * (* simple or compound *) command
  | Assignment of assignment
  | Declaration of declaration
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
  | Command of cmd_redir
  | Pipeline of loc * pipeline * pipeline_bar wrap * cmd_redir
  | Control_operator of loc * pipeline * unary_control_operator wrap

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
  | Pipelines of loc * pipeline list
  | Empty of loc

(*
   Bash syntax allows the body of a function definition only to a
   compound command but it doesn't matter in the AST that we also
   allow simple commands.
*)
and function_definition = {
  loc : loc;
  function_ : tok option;
  name : variable_name;
  body : command;
}

(* [ ... ] *)
and sh_test = test_expression bracket

(* [[ ... ]] *)
and bash_test = test_expression bracket

(*
   Arithmetic expressions are really a different language.
   Eventually, we should parse them separately from test expressions
   ([...] and [[...]]) and they should have their own AST.
   See https://www.shell-tips.com/bash/math-arithmetic-calculation/
   for a gentle tutorial.
*)
and arithmetic_expression = todo bracket

(* Only the last clause may not have a terminator. *)
and case_clause =
  loc
  * expression list
  * (* paren *) tok
  * blist
  * case_clause_terminator option

and case_clause_terminator =
  | Break of (* ;; *) tok
  | Fallthrough of (* ;& *) tok
  | Try_next of (* ;;& *) tok

and elif = loc * (* elif *) tok * blist * (* then *) tok * blist
and else_ = loc * (* else *) tok * blist

(* Declarations and optionally some assignments. Covers things like

     declare -i a=1 b=2
     declare -a arr brr
     export PATH
     local foo=$1
     unset x y z

   Yes, 'unset' is treated as a declaration too.

   TODO: add support for assigning to an array cell
   TODO: add support for assigning an array literal
*)
and declaration = {
  loc : loc;
  declarations : variable_name list;
  assignments : assignment list;
  attributes : declaration_attribute wrap list;
  unknowns : expression list;
}

and assignment = {
  loc : loc;
  lhs : string wrap;
  assign_op : assignment_operator wrap;
  rhs : expression;
}

(*
   Some of these attributes are mutually compatible, some aren't, and it's
   not obvious.
   For example, -i (integer) is compatible with -a (array) and with -l
   (lowercase)!
*)
and declaration_attribute =
  | Array (* -a *)
  | Associative_array (* -A *)
  | Function (* '-f' (makes sense only with '-p') *)
  | Function_short (* '-F' *)
  | Global (* -g *)
  | Integer (* -i *)
  | Local (* 'local' *)
  | Lowercase (* -l *)
  | Nameref (* -n *)
  | Print (* -p *)
  | Readonly (* 'readonly' or '-r' *)
  | Trace (* '-t' *)
  | Uppercase (* -u *)
  | Export (* 'export' or '-x' *)
  | Unset (* 'unset' *)
  | Unsetenv

(* 'unsetenv' *)
and expression =
  | Word of (* unquoted string *) string wrap (* TODO: bracket *)
  | Special_character of (* unquoted string *) string wrap
  | String of (* "..." *) string_fragment list bracket
  | String_fragment of (* $x ${...} $(...) `...` ... *) loc * string_fragment
  | Raw_string of (* '...' *) string wrap
  | Ansii_c_string of (* $'...' *) string wrap
  | Concatenation of loc * expression list
  | Expr_semgrep_ellipsis of (* ... *) tok
  | Expr_semgrep_deep_ellipsis of (* <... x ...> *) loc * expression bracket
  | Expr_semgrep_metavar of (* $X in pattern mode *) string wrap
  | Equality_test of loc * eq_op * right_eq_operand (* should it be here? *)
  | Empty_expression of loc
  | Array of (* ( a b ) *) loc * expression list bracket
  | Process_substitution of (* <( echo 'x' ) *) loc * blist bracket

(* Fragment of a double-quoted string *)
and string_fragment =
  | String_content of string wrap
  | Expansion of (* $X in program mode, ${X}, ${X ... } *) loc * expansion
  | Command_substitution of (* $(foo; bar) or `foo; bar` *) blist bracket
  | Frag_semgrep_metavar of (* $X in pattern mode *) string wrap
  | Frag_semgrep_named_ellipsis of (* $...X *) string wrap

(* $foo or something like ${foo ...} *)
and expansion =
  | Simple_expansion of loc * variable_name
  | Complex_expansion of complex_expansion bracket

and variable_name =
  | Simple_variable_name of string wrap
  | Special_variable_name of string wrap
  | Var_semgrep_metavar of string wrap

and complex_expansion =
  | Variable of loc * variable_name
  | Complex_expansion_TODO of loc

and eq_op = EQTILDE of (* "=~" *) tok | EQEQ of (* "==" *) tok
and right_eq_operand = Literal of loc * expression | Regexp of string wrap

and test_expression =
  | T_expr of loc * expression
  | T_unop of loc * unary_test_operator wrap * expression
  | T_binop of loc * expression * binary_test_operator wrap * expression
  | T_not of loc * tok * test_expression
  | T_and of loc * test_expression * tok * test_expression
  | T_or of loc * test_expression * tok * test_expression
  | T_todo of loc

(* A program is a list of pipelines *)
type program = blist
(*[@@deriving show { with_path = false }]*)

(*****************************************************************************)
(* Helpers (see also AST_bash_loc.ml and AST_bash_builder.ml) *)
(*****************************************************************************)

let variable_name_wrap = function
  | Simple_variable_name x
  | Special_variable_name x
  | Var_semgrep_metavar x ->
      x

let variable_name_tok x = variable_name_wrap x |> snd

let rec first_command_of_pipeline pip :
    cmd_redir * unary_control_operator wrap option =
  match pip with
  | Command x -> (x, None)
  | Pipeline (_loc, pip, _bar, _cmd) -> first_command_of_pipeline pip
  | Control_operator (_loc, pip, op) ->
      let cmd, _ = first_command_of_pipeline pip in
      (cmd, Some op)
