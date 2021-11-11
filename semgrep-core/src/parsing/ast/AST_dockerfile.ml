(*
   Dockerfile AST type definition.

   Language reference:
   https://docs.docker.com/engine/reference/builder/

   Extends AST_bash.
*)

module B = AST_bash

(*****************************************************************************)
(* Token info *)
(*****************************************************************************)

type tok = Parse_info.t

type loc = tok * tok

type 'a wrap = 'a * tok

type 'a bracket = tok * 'a * tok

(*****************************************************************************)
(* AST definition *)
(*****************************************************************************)

(* A simplified, non-recursive version of AST_bash.string_fragment *)
type string_fragment =
  | String_content of string wrap
  | Expansion of (* $X in program mode, ${X}, ${X ... } *) loc * B.expansion

(* Re-using the type used for double-quoted strings in bash *)
type quoted_string = string_fragment list bracket

type str = Unquoted of string wrap | Quoted of loc * quoted_string

(*
   The default shell depends on the platform on which docker runs (Unix or
   Windows). For now, we assume the shell is the Bourne shell which we
   treat as Bash. In the future, we could require the caller to specify
   if the dockerfile is for Windows and call is a dockerfile-windows dialect.
   Guessing the platform from the base image name would be possible in
   some cases but always, so that may not be a good solution.

   Here we have an Other case which is used after a SHELL directive
   which changes the shell to an unsupported shell (i.e. not sh or bash).
*)
type argv_or_shell =
  | Argv of (* [ "cmd", "arg1", "arg2$X" ] *) quoted_string list bracket
  | Sh_command of AST_bash.blist
  | Other_shell_command of string wrap

type param = tok (* -- *) * string wrap (* name *) * tok (* = *) * string wrap

(* value *)

type image_spec = {
  name : string_fragment;
  tag : (tok (* : *) * string_fragment) option;
  digest : (tok (* @ *) * string_fragment) option;
}

type image_alias = string_fragment

type label_pair = string wrap (* key *) * tok (* = *) * str (* value *)

type protocol = TCP | UDP

type path = string_fragment list

type string_array = string wrap list bracket

type array_or_paths = Array of loc * string_array | Path of loc * path list

type cmd = loc * tok * argv_or_shell

type healthcheck = Healthcheck_none of tok | Healthcheck_cmd of cmd

type instruction =
  | From of
      loc
      * tok
      * param option
      * image_spec
      * (tok (* as *) * image_alias) option
  | Run of loc * tok * argv_or_shell
  | Cmd of cmd
  | Label of loc * tok * label_pair list
  | Expose of loc * tok * int wrap (* port number *) * protocol wrap
  | Env of loc * tok * label_pair list
  | Add of loc * tok * param option * path * path
  | Copy of loc * tok * param option * path * path
  | Entrypoint of loc * tok * argv_or_shell
  | Volume of loc * tok * array_or_paths
  | User of
      loc
      * tok
      * string_fragment (* user *)
      * (tok (* : *) * string_fragment) (* group *) option
  | Workdir of loc * tok * path
  | Arg of loc * tok * string wrap
  | Onbuild of loc * tok * instruction
  | Stopsignal of loc * tok * string_fragment
  | Healthcheck of loc * tok * healthcheck
  | Shell (* changes the shell :-/ *) of loc * tok * string_array
  | Maintainer (* deprecated *) of loc * string wrap
  | Cross_build_xxx
      (* e.g. CROSS_BUILD_COPY;
         TODO: who uses this exactly? and where is it documented? *) of
      loc * tok * string wrap

type program = instruction list
