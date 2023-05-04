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
(* Extraction of the first token, used for its location info *)
(*****************************************************************************)

(*
   Convert a pair (loc, x) to a bracket, which uses a leading and trailing
   token to indicate the location.
*)
let bracket (loc : loc) x : 'a bracket =
  let start, end_ = loc in
  (start, x, end_)

let wrap_loc (_, tok) : loc = (tok, tok)
let bracket_loc (start_tok, _, end_tok) : loc = (start_tok, end_tok)
let todo_loc (TODO loc) = loc

let command_loc = function
  | Simple_command x -> x.loc
  | And (loc, _, _, _) -> loc
  | Or (loc, _, _, _) -> loc
  | Subshell (loc, _) -> loc
  | Command_group (loc, _) -> loc
  | Sh_test (loc, _) -> loc
  | Bash_test (loc, _) -> loc
  | Arithmetic_expression (loc, _) -> loc
  | For_loop (loc, _, _, _, _, _, _) -> loc
  | For_loop_c_style (loc, _) -> loc
  | Select (loc, _, _, _, _, _, _) -> loc
  | Case (loc, _, _, _, _, _) -> loc
  | If (loc, _, _, _, _, _, _, _) -> loc
  | While_loop (loc, _, _, _, _, _) -> loc
  | Until_loop (loc, _, _, _, _, _) -> loc
  | Coprocess (loc, _, _) -> loc
  | Assignment x -> x.loc
  | Declaration x -> x.loc
  | Negated_command (loc, _, _) -> loc
  | Function_definition (loc, _) -> loc

let pipeline_loc = function
  | Command x -> x.loc
  | Pipeline (loc, _, _, _) -> loc
  | Control_operator (loc, _, _) -> loc

let blist_loc = function
  | Seq (loc, _, _) -> loc
  | Pipelines (loc, _) -> loc
  | Empty loc -> loc

let sh_test_loc (x : sh_test) =
  let open_, _, close = x in
  (open_, close)

let bash_test_loc (x : bash_test) =
  let open_, _, close = x in
  (open_, close)

let arithmetic_expression_loc (x : arithmetic_expression) =
  let open_, _, close = x in
  (open_, close)

let case_clause_loc ((loc, _, _, _, _) : case_clause) = loc

let case_clause_terminator_tok = function
  | Break tok
  | Fallthrough tok
  | Try_next tok ->
      tok

let case_clause_terminator_loc x =
  let tok = case_clause_terminator_tok x in
  (tok, tok)

let elif_loc (x : elif) =
  let loc, _elif, _cond, _then, _body = x in
  loc

let else_loc (x : else_) =
  let loc, _else, _body = x in
  loc

let assignment_loc (x : assignment) = x.loc

let assignment_list_loc (x : assignment list) =
  Tok_range.of_list assignment_loc x

let declaration_loc (x : declaration) = x.loc

let expression_loc = function
  | Word x -> wrap_loc x
  | String x -> bracket_loc x
  | String_fragment (loc, _) -> loc
  | Raw_string x -> wrap_loc x
  | Ansii_c_string x -> wrap_loc x
  | Special_character x -> wrap_loc x
  | Concatenation (loc, _) -> loc
  | Expr_semgrep_ellipsis tok -> (tok, tok)
  | Expr_semgrep_deep_ellipsis (loc, _) -> loc
  | Expr_semgrep_metavar x -> wrap_loc x
  | Equality_test (loc, _, _) -> loc
  | Empty_expression loc -> loc
  | Array (loc, _) -> loc
  | Process_substitution (loc, _) -> loc

let string_fragment_loc = function
  | String_content x -> wrap_loc x
  | Expansion (loc, _) -> loc
  | Command_substitution x -> bracket_loc x
  | Frag_semgrep_metavar x -> wrap_loc x
  | Frag_semgrep_named_ellipsis x -> wrap_loc x

let expansion_loc = function
  | Simple_expansion (loc, _) -> loc
  | Complex_expansion x -> bracket_loc x

let variable_name_wrap = function
  | Simple_variable_name x
  | Special_variable_name x
  | Var_semgrep_metavar x ->
      x

let variable_name_tok x = variable_name_wrap x |> snd
let variable_name_loc x = variable_name_wrap x |> wrap_loc

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

let test_expression_loc = function
  | T_expr (loc, _)
  | T_unop (loc, _, _)
  | T_binop (loc, _, _, _)
  | T_not (loc, _, _)
  | T_and (loc, _, _, _)
  | T_or (loc, _, _, _)
  | T_todo loc ->
      loc

let write_redir_src_loc (x : write_redir_src) =
  match x with
  | Stdout tok
  | Stdout_and_stderr tok
  | File_descriptor (_, tok) ->
      (tok, tok)

let file_redir_target_loc (x : file_redir_target) =
  match x with
  | File e -> expression_loc e
  | File_descriptor w -> wrap_loc w
  | Stdout_and_stderr tok -> (tok, tok)
  | Close_fd tok -> (tok, tok)

let redirect_loc (x : redirect) =
  match x with
  | File_redirect (loc, _) -> loc
  | Read_heredoc x -> todo_loc x
  | Read_herestring x -> todo_loc x

let cmd_redir_loc x = x.loc

(*****************************************************************************)
(* Helpers for users of the module *)
(*****************************************************************************)

let concat_blists (x : blist list) : blist =
  match List.rev x with
  | [] ->
      (* TODO: use actual location in the program rather than completely
         fake location *)
      Empty Tok_range.unsafe_fake_loc
  | last_blist :: blists ->
      let end_ = blist_loc last_blist in
      List.fold_left
        (fun acc blist ->
          let start = blist_loc blist in
          let loc = Tok_range.range start end_ in
          Seq (loc, blist, acc))
        last_blist blists

let add_redirects_to_command (cmd_r : cmd_redir) (redirects : redirect list) :
    cmd_redir =
  let all_locs = cmd_r.loc :: Common.map redirect_loc redirects in
  let loc = Tok_range.of_list (fun loc -> loc) all_locs in
  { cmd_r with loc; redirects = cmd_r.redirects @ redirects }

let rec add_redirects_to_last_command_of_pipeline pip redirects : pipeline =
  match pip with
  | Command cmd_r -> Command (add_redirects_to_command cmd_r redirects)
  | Pipeline (loc, pip, bar, cmd_r) ->
      let cmd_r = add_redirects_to_command cmd_r redirects in
      let loc = Tok_range.range loc cmd_r.loc in
      Pipeline (loc, pip, bar, cmd_r)
  | Control_operator (loc, pip, op) ->
      let pip = add_redirects_to_last_command_of_pipeline pip redirects in
      let loc = Tok_range.range loc (pipeline_loc pip) in
      Control_operator (loc, pip, op)

let rec first_command_of_pipeline pip :
    cmd_redir * unary_control_operator wrap option =
  match pip with
  | Command x -> (x, None)
  | Pipeline (_loc, pip, _bar, _cmd) -> first_command_of_pipeline pip
  | Control_operator (_loc, pip, op) ->
      let cmd, _ = first_command_of_pipeline pip in
      (cmd, Some op)

(*
   We use this only to analyze and simplify a pattern. This loses the location
   information if the list is empty.
*)
let flatten_blist blist : pipeline list =
  let rec flatten acc blist =
    match blist with
    | Seq (_loc, a, b) ->
        let acc = flatten acc a in
        flatten acc b
    | Pipelines (_loc, pips) -> List.rev_append pips acc
    | Empty _loc -> acc
  in
  flatten [] blist |> List.rev

(*
   Simple expressions returned by this function:

     foo
     $foo
     ""

   Not simple expressions:

     foo bar
     foo;
     foo > bar
     foo &
     foo | bar
*)
let rec pipeline_as_expression pip : expression option =
  match pip with
  | Command cmd_r -> (
      match (cmd_r.redirects, cmd_r.command) with
      | [], Simple_command cmd -> (
          match (cmd.assignments, cmd.arguments) with
          | [], [ arg0 ] -> Some arg0
          | _ -> None)
      | _ -> None)
  | Pipeline _ -> None
  | Control_operator (_loc, pip, (op, _tok)) -> (
      match op with
      | Foreground Fg_newline -> pipeline_as_expression pip
      | Foreground (Fg_semi | Fg_semisemi)
      | Background ->
          None)

(*
   This is necessary to that a pattern 'foo' is not translated into to a
   function/command call.
*)
let blist_as_expression blist : expression option =
  match flatten_blist blist with
  | [ pip ] -> pipeline_as_expression pip
  | _ -> None
