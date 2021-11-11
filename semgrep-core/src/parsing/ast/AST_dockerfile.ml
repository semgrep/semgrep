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

type shell_compatibility =
  | Sh (* Bourne shell or Bash *)
  | Cmd (* Windows cmd *)
  | Powershell (* Windows powershell *)
  | Other of
      (* name of the shell extracted from the command name
         in a SHELL directive *)
      string

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
  | Other_shell_command of shell_compatibility * string wrap

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

type cmd = loc * string wrap * argv_or_shell

type healthcheck = Healthcheck_none of tok | Healthcheck_cmd of cmd

type instruction =
  | From of
      loc
      * string wrap
      * param option
      * image_spec
      * (tok (* as *) * image_alias) option
  | Run of loc * string wrap * argv_or_shell
  | Cmd of cmd
  | Label of loc * string wrap * label_pair list
  | Expose of loc * string wrap * int wrap (* port number *) * protocol wrap
  | Env of loc * string wrap * label_pair list
  | Add of loc * string wrap * param option * path * path
  | Copy of loc * string wrap * param option * path * path
  | Entrypoint of loc * string wrap * argv_or_shell
  | Volume of loc * string wrap * array_or_paths
  | User of
      loc
      * string wrap
      * string_fragment (* user *)
      * (tok (* : *) * string_fragment) (* group *) option
  | Workdir of loc * string wrap * path
  | Arg of loc * string wrap * string wrap
  | Onbuild of loc * string wrap * instruction
  | Stopsignal of loc * string wrap * string_fragment
  | Healthcheck of loc * string wrap * healthcheck
  | Shell (* changes the shell :-/ *) of loc * string wrap * string_array
  | Maintainer (* deprecated *) of loc * string wrap * string wrap
  | Cross_build_xxx
      (* e.g. CROSS_BUILD_COPY;
         TODO: who uses this exactly? and where is it documented? *) of
      loc * string wrap * string wrap

type program = instruction list

(***************************************************************************)
(* Location extraction *)
(***************************************************************************)

let wrap_loc (_, tok) = (tok, tok)

let bracket_loc (open_, _, close) = (open_, close)

let list_loc = B.list_loc

let string_fragment_loc = function
  | String_content x -> wrap_loc x
  | Expansion (loc, _) -> loc

(* Re-using the type used for double-quoted strings in bash *)
let quoted_string_loc = bracket_loc

let str_loc = function
  | Unquoted x -> wrap_loc x
  | Quoted (loc, _) -> loc

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
  | Argv x -> bracket_loc x
  | Sh_command x -> B.blist_loc x
  | Other_shell_command (_, x) -> wrap_loc x

let param_loc ((tok, _, _, value) : param) : loc = (tok, snd value)

let image_spec_loc (x : image_spec) =
  let start = string_fragment_loc x.name in
  let end_ = start in
  let end_ =
    match x.tag with
    | None -> end_
    | Some (_, x) -> string_fragment_loc x
  in
  let end_ =
    match x.digest with
    | None -> end_
    | Some (_, x) -> string_fragment_loc x
  in
  (start, end_)

let image_alias_loc = string_fragment_loc

let label_pair_loc (((_, start), _, str) : label_pair) =
  (start, str_loc str |> snd)

let path_loc (x : path) = list_loc string_fragment_loc x

let string_array_loc = bracket_loc

let array_or_paths_loc = function
  | Array (loc, _)
  | Path (loc, _) ->
      loc

let cmd_loc ((loc, _, _) : cmd) = loc

let healthcheck_loc = function
  | Healthcheck_none tok -> (tok, tok)
  | Healthcheck_cmd cmd -> cmd_loc cmd

let instruction_loc = function
  | From (loc, _, _, _, _) -> loc
  | Run (loc, _, _) -> loc
  | Cmd cmd -> cmd_loc cmd
  | Label (loc, _, _) -> loc
  | Expose (loc, _, _, _) -> loc
  | Env (loc, _, _) -> loc
  | Add (loc, _, _, _, _) -> loc
  | Copy (loc, _, _, _, _) -> loc
  | Entrypoint (loc, _, _) -> loc
  | Volume (loc, _, _) -> loc
  | User (loc, _, _, _) -> loc
  | Workdir (loc, _, _) -> loc
  | Arg (loc, _, _) -> loc
  | Onbuild (loc, _, _) -> loc
  | Stopsignal (loc, _, _) -> loc
  | Healthcheck (loc, _, _) -> loc
  | Shell (loc, _, _) -> loc
  | Maintainer (loc, _, _) -> loc
  | Cross_build_xxx (loc, _, _) -> loc
