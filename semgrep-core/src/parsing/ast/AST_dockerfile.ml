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

(* Used for quoted and unquoted strings for now *)
type str = loc * string_fragment list
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

let wrap_tok ((_, tok) : _ wrap) = tok
let wrap_loc ((_, tok) : _ wrap) = (tok, tok)
let bracket_loc ((open_, _, close) : _ bracket) = (open_, close)

let var_or_metavar_tok = function
  | Var_ident (_, tok) -> tok
  | Var_semgrep_metavar (_, tok) -> tok

let var_or_metavar_loc x =
  let tok = var_or_metavar_tok x in
  (tok, tok)

let expansion_tok = function
  | Expand_var (_, tok) -> tok
  | Expand_semgrep_metavar (_, tok) -> tok

let expansion_loc x =
  let tok = expansion_tok x in
  (tok, tok)

let string_fragment_loc = function
  | String_content (_, tok) -> (tok, tok)
  | Expansion (loc, _) -> loc
  | Frag_semgrep_metavar (_, tok) -> (tok, tok)

let str_loc ((loc, _) : str) = loc

let str_or_ellipsis_loc = function
  | Str_str str -> str_loc str
  | Str_semgrep_ellipsis tok -> (tok, tok)

(* Re-using the type used for double-quoted strings in bash *)
let quoted_string_loc = bracket_loc

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
let argv_or_shell_loc = function
  | Command_semgrep_ellipsis tok -> (tok, tok)
  | Argv (loc, _) -> loc
  | Sh_command (loc, _) -> loc
  | Other_shell_command (_, x) -> wrap_loc x

let param_loc ((loc, _) : param) : loc = loc
let image_spec_loc (x : image_spec) = x.loc
let image_alias_loc = str_loc

let label_pair_loc = function
  | Label_semgrep_ellipsis tok -> (tok, tok)
  | Label_pair (loc, _, _, _) -> loc

let string_array_loc = bracket_loc

let array_or_paths_loc = function
  | Array (loc, _)
  | Paths (loc, _) ->
      loc

let cmd_loc ((loc, _, _) : cmd) = loc

let healthcheck_loc = function
  | Healthcheck_semgrep_metavar (_, tok) -> (tok, tok)
  | Healthcheck_none tok -> (tok, tok)
  | Healthcheck_cmd (loc, _, _) -> loc

let expose_port_loc = function
  | Expose_semgrep_ellipsis tok -> (tok, tok)
  | Expose_port ((_, tok1), Some (_, tok2)) -> (tok1, tok2)
  | Expose_port ((_, tok), None) -> (tok, tok)
  | Expose_fragment x -> string_fragment_loc x

let instruction_loc = function
  | From (loc, _, _, _, _) -> loc
  | Run (loc, _, _) -> loc
  | Cmd cmd -> cmd_loc cmd
  | Label (loc, _, _) -> loc
  | Expose (loc, _, _) -> loc
  | Env (loc, _, _) -> loc
  | Add (loc, _, _, _, _) -> loc
  | Copy (loc, _, _, _, _) -> loc
  | Entrypoint (loc, _, _) -> loc
  | Volume (loc, _, _) -> loc
  | User (loc, _, _, _) -> loc
  | Workdir (loc, _, _) -> loc
  | Arg (loc, _, _, _) -> loc
  | Onbuild (loc, _, _) -> loc
  | Stopsignal (loc, _, _) -> loc
  | Healthcheck (loc, _, _) -> loc
  | Shell (loc, _, _) -> loc
  | Maintainer (loc, _, _) -> loc
  | Cross_build_xxx (loc, _, _) -> loc
  | Instr_semgrep_ellipsis tok -> (tok, tok)
  | Instr_semgrep_metavar (_, tok) -> (tok, tok)
