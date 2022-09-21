(*
   Dockerfile AST type definition.

   Language reference:
   https://docs.docker.com/engine/reference/builder/

   Extends AST_bash.
*)

(*****************************************************************************)
(* Token info *)
(*****************************************************************************)

type tok = Parse_info.t
type loc = Loc.t
type 'a wrap = 'a * tok
type 'a bracket = tok * 'a * tok

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

type var_or_metavar =
  | Var_ident of string wrap
  | Var_semgrep_metavar of string wrap

type string_or_metavar =
  | Str_string of string wrap
  | Str_semgrep_metavar of string wrap

(* $foo or something like ${foo ...} *)
type expansion =
  | Expand_var of (* FOO in ${FOO} *) string wrap
  | Expand_semgrep_metavar of (* $FOO in ${$FOO} *) string wrap

(* Fragment of a string possibly containing variable expansions
   or semgrep metavariables. This is similar to what we do in AST_bash.ml. *)
type string_fragment =
  | String_content of string wrap
  | Expansion of (* $X in program mode, ${X}, ${X ... } *) (loc * expansion)
  | Frag_semgrep_metavar of (* $X in pattern mode *) string wrap

(* Used for quoted and unquoted strings for now.
   Must be created with the 'create_str' function to ensure that
   consecutive literal fragments are collapsed.
   Converting str -> unchecked_str is done with the type coercion operator :>
*)
type unchecked_str = loc * string_fragment list
type str = private unchecked_str

val create_str : unchecked_str -> str

type str_or_ellipsis = Str_str of str | Str_semgrep_ellipsis of tok

type array_elt =
  | Arr_string of str
  | Arr_metavar of string wrap
  | Arr_ellipsis of tok

type string_array = array_elt list bracket

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
  | Command_semgrep_ellipsis of tok
  | Argv of loc * (* [ "cmd", "arg1", "arg2$X" ] *) string_array
  | Sh_command of loc * AST_bash.blist
  | Other_shell_command of shell_compatibility * string wrap

type param =
  loc * (tok (* -- *) * string wrap (* name *) * tok (* = *) * string wrap)

(* value *)

type image_spec = {
  loc : loc;
  name : str;
  tag : (tok (* : *) * str) option;
  digest : (tok (* @ *) * str) option;
}

type image_alias = str

type label_pair =
  | Label_semgrep_ellipsis of tok
  | Label_pair of loc * var_or_metavar (* key *) * tok (* = *) * str

(* value *)

type protocol = TCP | UDP
type path = str
type path_or_ellipsis = str_or_ellipsis

type array_or_paths =
  | Array of loc * string_array
  | Paths of loc * path_or_ellipsis list

type cmd = loc * string wrap * argv_or_shell

type healthcheck =
  | Healthcheck_semgrep_metavar of string wrap
  | Healthcheck_none of tok
  | Healthcheck_cmd of loc * param list * cmd

type expose_port =
  | Expose_semgrep_ellipsis of tok
  | Expose_port of (* port/protocol *) string wrap * string wrap option
  | Expose_fragment of string_fragment

type instruction =
  | From of
      loc
      * string wrap
      * param option
      * image_spec
      * (tok (* as *) * image_alias) option
  | Run of cmd
  | Cmd of cmd
  | Label of loc * string wrap * label_pair list
  | Expose of loc * string wrap * expose_port list (* 123/udp 123 56/tcp *)
  | Env of loc * string wrap * label_pair list
  | Add of loc * string wrap * param option * path_or_ellipsis list * path
  | Copy of loc * string wrap * param option * path_or_ellipsis list * path
  | Entrypoint of loc * string wrap * argv_or_shell
  | Volume of loc * string wrap * array_or_paths
  | User of
      loc
      * string wrap
      * str (* user *)
      * (tok (* : *) * str) (* group *) option
  | Workdir of loc * string wrap * path
  | Arg of loc * string wrap * var_or_metavar * (tok * str) option
  | Onbuild of loc * string wrap * instruction
  | Stopsignal of loc * string wrap * str
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

(***************************************************************************)
(* Location extraction *)
(***************************************************************************)

(*
   The functions below extract the location associated with any node.
   They all run in O(1) with respect to the tree depth.
*)
val wrap_tok : 'a wrap -> tok
val wrap_loc : 'a wrap -> loc
val bracket_loc : 'a bracket -> loc
val var_or_metavar_tok : var_or_metavar -> tok
val var_or_metavar_loc : var_or_metavar -> loc
val expansion_tok : expansion -> tok
val expansion_loc : expansion -> loc
val string_fragment_loc : string_fragment -> loc
val str_loc : str -> loc
val str_or_ellipsis_loc : str_or_ellipsis -> loc
val quoted_string_loc : 'a bracket -> loc
val argv_or_shell_loc : argv_or_shell -> loc
val param_loc : param -> loc
val image_spec_loc : image_spec -> loc
val image_alias_loc : str -> loc
val label_pair_loc : label_pair -> loc
val string_array_loc : 'a bracket -> loc
val array_or_paths_loc : array_or_paths -> loc
val cmd_loc : cmd -> loc
val healthcheck_loc : healthcheck -> loc
val expose_port_loc : expose_port -> loc
val instruction_loc : instruction -> loc
