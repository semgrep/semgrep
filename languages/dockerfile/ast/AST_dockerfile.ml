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
   Dockerfile AST type definition.

   Language reference:
   https://docs.docker.com/engine/reference/builder/

   Extends AST_bash!
*)

(*****************************************************************************)
(* Token info *)
(*****************************************************************************)

type tok = Tok.t
type 'a wrap = 'a * tok
type 'a bracket = tok * 'a * tok
type loc = Tok_range.t

(*****************************************************************************)
(* AST definition *)
(*****************************************************************************)

type shell_compatibility =
  | Sh (* Bourne shell or Bash *)
  | Cmd (* Windows cmd *)
  | Powershell (* Windows powershell *)
  | Other of
      (* name of the shell extracted from the command name
         in a SHELL directive *)
      string

type string_or_metavar =
  | Str_string of string wrap
  | Str_semgrep_metavar of string wrap

(* $foo or something like ${foo ...} *)
type expansion =
  | Expand_var of (* FOO in ${FOO} *) string wrap
  | Expand_semgrep_metavar of (* $FOO in ${$FOO} *) string wrap

(* Fragment of a string possibly containing variable expansions
   or semgrep metavariables. This is similar to what we do in AST_bash.ml.

   TODO: evaluate String_content fragments so that they can be compared
   regardless of escaping.
*)
type double_quoted_string_fragment =
  | Dbl_string_content of (* escaped *) string wrap
  | Dbl_expansion of (* $X in program mode, ${X}, ${X ... } *) (loc * expansion)
  | Dbl_frag_semgrep_metavar of (* $X in pattern mode *) string wrap

type docker_string_fragment =
  | Unquoted of string wrap
  (* Using 'string wrap bracket' rather than 'string bracket' since it's the
     type expected by the generic AST. *)
  | Single_quoted of (loc * string wrap bracket)
  | Double_quoted of (loc * double_quoted_string_fragment list bracket)
  (* Expansion and Frag_semgrep_metavar: see Dbl_expansion and
     Dbl_frag_semgrep_metavar *)
  | Expansion of (loc * expansion)
  | Frag_semgrep_metavar of string wrap

type ident_or_metavar = Ident of string wrap | Semgrep_metavar of string wrap

type key_or_metavar =
  | Key of docker_string_fragment
  | Semgrep_metavar of string wrap

(* A string which is possibly the concatenation of several fragments.

   We need to be able to represent this:

     a'b'"c$D${E}f"$G

   a: Unquoted
   b: Single_quoted
   c: Dbl_string_content
   d: Dbl_expansion or Dbl_frag_semgrep_metavar
   e: Dbl_expansion
   f: Dbl_string_content
   g: Expansion

   After evaluation of the escape sequences, it can be simplified into
   fewer fragments of 3 kinds:
   - string data
   - variable expansion
   - semgrep metavariable (in the context of a semgrep pattern only)

   In our example, the fragments would be: "abc", $D, $E, "f", $G.
   This simplification may or may not be performed when converting to
   Semgrep's generic AST.
*)
type docker_string = loc * docker_string_fragment list

type heredoc_template = {
  (* '<<EOF' or similar *)
  opening : tok;
  (* matching 'EOF' *)
  closing : tok;
  (* TODO: parse the heredoc opener
     (* "EOF" *)
     name: string;
     (* true for '<<EOF', false for '<<"EOF"' *)
     evaluate_variables: bool;
     (* true for '<<-EOF', false for '<<EOF' *)
     ignore_leading_tabs: bool;
  *)
  (* The template body is a string but it's really a template when
     'evaluate_variable' is true. *)
  body : string wrap;
}

type str_or_ellipsis =
  | Str_str of docker_string
  | Str_template of heredoc_template
  | Str_semgrep_ellipsis of tok

type array_elt =
  (* JSON string array, not to be confused with ordinary double-quoted strings
     that don't obey the JSON syntax (see docker_string). *)
  | Arr_string of loc * string wrap bracket
  | Arr_metavar of string wrap
  | Arr_ellipsis of tok

type string_array = array_elt list bracket

(* A fragment of a shell instruction. If all fragments of a shell instruction
   are constants, we stitch them together and parse them with our
   Bash parser (if the current shell is Sh or Bash), avoiding this type. *)
type shell_fragment =
  | Constant_shell_fragment of string wrap
  | Heredoc_template of heredoc_template

(*
   The default shell depends on the platform on which docker runs (Unix or
   Windows). For now, we assume the shell is the Bourne shell which we
   treat as Bash. In the future, we could require the caller to specify
   if the dockerfile is for Windows and call is a dockerfile-windows dialect.
   Guessing the platform from the base image name would be possible in
   some cases but always, so that may not be a good solution.

   Here we have an Other case which is used after a SHELL directive
   which changes the shell to an unsupported shell (i.e. not sh or bash).

   old name: argv_or_shell
*)
type command =
  | Argv of loc * (* [ "cmd", "arg1", "arg2$X" ] *) string_array
  (* !!! This is where we embed Bash constructs (Bash lists) !!! *)
  | Sh_command of loc * AST_bash.blist
  | Other_shell_command of shell_compatibility * string wrap
  | Command_semgrep_ellipsis of tok
  | Command_semgrep_named_ellipsis of string wrap
  (* If the instruction contains at least one heredoc template, we don't
     know the shell command statically and we produce such a template.
     Only the RUN instruction supports heredocs in commands, not CMD,
     not HEALTHCHECK. *)
  | Shell_command_template of loc * shell_fragment list

type param =
  loc * (tok (* -- *) * string wrap (* name *) * tok (* = *) * string wrap)

(* For positions where `...` is a valid parameter. TODO should we refactor to
 * allow ellipsis everywhere parameters may be? *)
type param_or_ellipsis = ParamParam of param | ParamEllipsis of tok

(* value *)

type image_spec = {
  loc : loc;
  name : docker_string;
  tag : (tok (* : *) * docker_string) option;
  digest : (tok (* @ *) * docker_string) option;
}

(* 'ENV key=value' where the key is an identifier, not a string template. *)
type env_pair =
  | Env_pair of loc * ident_or_metavar * tok (* = *) * docker_string
  | Env_semgrep_ellipsis of tok

(* 'LABEL key=value' where the key can be a string template. *)
type label_pair =
  | Label_pair of loc * key_or_metavar * tok (* = *) * docker_string
  | Label_semgrep_ellipsis of tok

(* value *)

type protocol = TCP | UDP
type path_or_ellipsis = str_or_ellipsis

type array_or_paths =
  | Array of loc * string_array
  | Paths of loc * path_or_ellipsis list

type run_param =
  | Param of param
  | Mount_param of loc * string wrap * (loc * string wrap * string wrap) list

(* old name: cmd *)
type cmd_instr = loc * string wrap * run_param list * command

type healthcheck =
  | Healthcheck_none of tok
  | Healthcheck_ellipsis of tok
  | Healthcheck_cmd of loc * param_or_ellipsis list * cmd_instr
  | Healthcheck_semgrep_metavar of string wrap

type expose_port =
  | Expose_port of (* port/protocol *) string wrap * string wrap option
  | Expose_fragment of docker_string_fragment
  | Expose_semgrep_ellipsis of tok

type add_or_copy =
  loc * string wrap * param list * path_or_ellipsis list * docker_string

type instruction =
  | From of
      loc
      * string wrap
      * param option
      * image_spec
      * (tok (* as *) * docker_string) option
  | Run of cmd_instr
  | Cmd of cmd_instr
  | Label of loc * string wrap * label_pair list
  | Expose of loc * string wrap * expose_port list (* 123/udp 123 56/tcp *)
  | Env of loc * string wrap * env_pair list
  | Add of add_or_copy
  | Copy of add_or_copy
  | Entrypoint of loc * string wrap * command
  | Volume of loc * string wrap * array_or_paths
  | User of
      loc
      * string wrap
      * docker_string (* user *)
      * (tok (* : *) * docker_string) (* group *) option
  | Workdir of loc * string wrap * docker_string
  | Arg of loc * string wrap * ident_or_metavar * (tok * docker_string) option
  | Onbuild of loc * string wrap * instruction
  | Stopsignal of loc * string wrap * docker_string
  | Healthcheck of loc * string wrap * healthcheck
  | Shell (* changes the shell :-/ *) of loc * string wrap * string_array
  | Maintainer (* deprecated *) of loc * string wrap * string_or_metavar
  | Cross_build_xxx
      (* e.g. CROSS_BUILD_COPY;
         TODO: who uses this exactly? and where is it documented? *) of
      loc * string wrap * string wrap
  | Instr_semgrep_ellipsis of tok
  | Instr_semgrep_metavar of string wrap

type program = instruction list
