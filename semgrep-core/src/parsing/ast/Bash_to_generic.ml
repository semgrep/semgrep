(*
   Convert Bash-specific AST to generic AST.

   A Bash program is organized into the following hierarchy:

   - List: a list of pipelines. A whole program is a list.

       echo hello | tr h y; echo bye

   - Pipeline: a list of commands whose standard output and input are
     connected

       echo hello | tr h y

   - Command: everything else including calls to external programs and
     control structures that the language has to offer.

     Simple command:

       echo hello

     For loop, a compound command:

       for i in 1 2 3; do echo "$i"; done

     Variable assignment:

       answer=42

     + many others

   - Literals: space-separated elements within a simple command, which will
     be expanded into a possibly different number of elements.

       hello
       "$x"
       $items
       "$(cat foo/bar)"
       $?

       etc.

   Mapping to the generic AST:

   pipeline -> pipeline statement, made of a sequence of statements
   command -> statement
   simple command -> function call, which is an expression
   variable assignment -> variable assignment
   function definition -> function definition
   for loop -> for loop, which is a statement
*)

open! Common
open AST_bash
module G = AST_generic

(*module H = AST_generic_helpers*)

let rec list_ (x : list_) : G.stmt list = List.map pipeline x |> List.flatten

and pipeline (x : pipeline) : G.stmt list =
  let commands, _control_op = x in
  (* for now, unsupported constructs are simply omitted from the generic AST *)
  List.filter_map
    (fun (_opt_bar, cmd_redir) -> command_with_redirects cmd_redir)
    commands

and command_with_redirects (x : command_with_redirects) : G.stmt option =
  let { command = cmd; redirects } = x in
  ignore redirects;
  command cmd

and command (cmd : command) : G.stmt option =
  match cmd with
  | Simple_command { assignments = _; arguments } -> (
      match arguments with
      | [] -> None
      | arg0 :: args ->
          let args = List.map (fun x -> G.Arg (expression x)) args in
          let e = G.Call (expression arg0, G.fake_bracket args) in
          Some (G.s (ExprStmt (e, G.fake ""))) )
  | Compound_command _ -> None
  | Coprocess _ -> None
  | Assignment _ -> None
  | Declaration _ -> None
  | Negated_command _ -> None
  | Function_definition _ -> None

and expression (e : expression) : G.expr =
  let todo = G.L (String ("", G.fake "")) in
  match e with
  | Word x -> G.L (G.Atom (G.fake "", x))
  | String _ -> todo
  | String_fragment frag -> (
      match frag with
      | String_content x -> G.L (G.Atom (G.fake "", x))
      | Expansion ex -> expansion ex
      | Command_substitution (_open, _x, _close) -> todo )
  | Raw_string _ -> todo
  | Ansii_c_string _ -> todo
  | Special_character _ -> todo
  | String_expansion _ -> todo
  | Concatenation _ -> todo
  | Semgrep_ellipsis _ -> todo
  | Semgrep_metavariable _ -> todo
  | Expression_TODO -> todo

(*
   '$' followed by a variable to transform and expand into a list.
   We treat this as a function call.
*)
and expansion (x : expansion) : G.expr =
  let todo = G.L (String ("", G.fake "")) in
  match x with
  | Simple_expansion (dollar_tok, var_name) ->
      let arg =
        match var_name with
        | Simple_variable_name name | Special_variable_name name ->
            G.Arg (G.N (G.Id (name, G.empty_id_info ())))
      in
      let function_ = G.N (G.Id (("$", dollar_tok), G.empty_id_info ())) in
      let e = G.Call (function_, G.fake_bracket [ arg ]) in
      e
  | Complex_expansion _ -> todo

let program x = list_ x

let any x = G.Ss (program x)
