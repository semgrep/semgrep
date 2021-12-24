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

type quoted_string = string wrap

type str = Unquoted of string wrap | Quoted of string wrap

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
  | Argv of Loc.t * (* [ "cmd", "arg1", "arg2$X" ] *) quoted_string list bracket
  | Sh_command of Loc.t * AST_bash.blist
  | Other_shell_command of shell_compatibility * string wrap

type param =
  Loc.t * (tok (* -- *) * string wrap (* name *) * tok (* = *) * string wrap)

(* value *)

type image_spec = {
  loc : Loc.t;
  name : string wrap;
  tag : (tok (* : *) * string wrap) option;
  digest : (tok (* @ *) * string wrap) option;
}

type image_alias = string wrap

type label_pair = string wrap (* key *) * tok (* = *) * str (* value *)

type protocol = TCP | UDP

type path = string wrap

type string_array = string wrap list bracket

type array_or_paths =
  | Array of Loc.t * string_array
  | Paths of Loc.t * path list

type cmd = Loc.t * string wrap * argv_or_shell

type healthcheck =
  | Healthcheck_none of tok
  | Healthcheck_cmd of Loc.t * param list * cmd

type instruction =
  | From of
      Loc.t
      * string wrap
      * param option
      * image_spec
      * (tok (* as *) * image_alias) option
  | Run of cmd
  | Cmd of cmd
  | Label of Loc.t * string wrap * label_pair list
  | Expose of Loc.t * string wrap * string wrap list (* 123/udp 123 56/tcp *)
  | Env of Loc.t * string wrap * label_pair list
  | Add of Loc.t * string wrap * param option * path * path
  | Copy of Loc.t * string wrap * param option * path * path
  | Entrypoint of Loc.t * string wrap * argv_or_shell
  | Volume of Loc.t * string wrap * array_or_paths
  | User of
      Loc.t
      * string wrap
      * string wrap
      (* user *)
      * (tok (* : *) * string wrap) (* group *) option
  | Workdir of Loc.t * string wrap * path
  | Arg of Loc.t * string wrap * string wrap * (tok * str) option
  | Onbuild of Loc.t * string wrap * instruction
  | Stopsignal of Loc.t * string wrap * string wrap
  | Healthcheck of Loc.t * string wrap * healthcheck
  | Shell (* changes the shell :-/ *) of Loc.t * string wrap * string_array
  | Maintainer (* deprecated *) of Loc.t * string wrap * string wrap
  | Cross_build_xxx
      (* e.g. CROSS_BUILD_COPY;
         TODO: who uses this exactly? and where is it documented? *) of
      Loc.t * string wrap * string wrap
  | Instr_semgrep_ellipsis of tok

type program = instruction list

(***************************************************************************)
(* Location extraction *)
(***************************************************************************)

let wrap_tok (_, tok) = tok

let wrap_loc (_, tok) = (tok, tok)

let bracket_loc (open_, _, close) = (open_, close)

(* Re-using the type used for double-quoted strings in bash *)
let quoted_string_loc = bracket_loc

let str_loc = function
  | Unquoted x -> wrap_loc x
  | Quoted x -> wrap_loc x

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
  | Argv (loc, _) -> loc
  | Sh_command (loc, _) -> loc
  | Other_shell_command (_, x) -> wrap_loc x

let param_loc ((loc, _) : param) : Loc.t = loc

let image_spec_loc (x : image_spec) = x.loc

let image_alias_loc = wrap_loc

let label_pair_loc (((_, start), _, str) : label_pair) =
  (start, str_loc str |> snd)

let string_array_loc = bracket_loc

let array_or_paths_loc = function
  | Array (loc, _)
  | Paths (loc, _) ->
      loc

let cmd_loc ((loc, _, _) : cmd) = loc

let healthcheck_loc = function
  | Healthcheck_none tok -> (tok, tok)
  | Healthcheck_cmd (loc, _, _) -> loc

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
