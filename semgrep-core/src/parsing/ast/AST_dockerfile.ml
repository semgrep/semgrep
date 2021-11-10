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

type tok = Parse_info.t [@@deriving show]
type loc = tok * tok [@@deriving show]
type 'a wrap = 'a * tok [@@deriving show]
type 'a bracket = tok * 'a * tok [@@deriving show]

(*****************************************************************************)
(* AST definition *)
(*****************************************************************************)

(* A simplified, non-recursive version of AST_bash.string_fragment *)
type string_fragment =
  | String_content of string wrap
  | Expansion of (* $X in program mode, ${X}, ${X ... } *) loc * B.expansion

(* Re-using the type used for double-quoted strings in bash *)
type quoted_string = string_fragment list bracket

type str =
  | Unquoted of string wrap
  | Quoted of loc * quoted_string

type argv_or_shell =
  | Argv of (* [ "cmd", "arg1", "arg2$X" ] *) quoted_string list bracket
  | Shell_command of AST_bash.blist

type param =
  tok (* -- *)
  * string wrap (* name *)
  * tok (* = *)
  * string wrap (* value *)

type image_spec = {
  name : string_fragment;
  tag : (tok (* : *) * string_fragment) option;
  digest : (tok (* @ *) * string_fragment) option;
}

type image_alias = string_fragment

type label_pair = string wrap (* key *) * tok (* = *) * str (* value *)

type protocol = TCP | UDP

type path = string_fragment list

type array_or_paths =
  | Array of loc * string wrap list bracket
  | Path of loc * path list

type instruction =
  | From of
      loc
      * tok
      * param option
      * image_spec
      * (tok (* as *) * image_alias) option
  | Run of loc * tok * argv_or_shell
  | Cmd of loc * tok * argv_or_shell
  | Label of loc * tok * label_pair list
  | Expose of loc * tok * int wrap (* port number *) * protocol wrap
  | Env of loc * tok * label_pair list
  | Add of loc * tok * param option * path * path
  | Copy of loc * tok * param option * path * path
  | Entrypoint of loc * tok * argv_or_shell
  | Volume of loc * tok * array_or_paths
  | User of loc * tok *
  | Workdir of loc * tok *
  | Arg of loc * tok *
  | Onbuild of loc * tok *
  | Stopsignal of loc * tok *
  | Healthcheck of loc * tok *
  | Shell of loc * tok *
  | Maintainer  of loc * (* deprecated *)
  | Cross_build_xxx
  (* e.g. CROSS_BUILD_COPY;
     TODO: who uses this exactly? *)
    of loc * tok * string wrap
[@@deriving show]

type program =
  | Instruction of instruction
  | Comment of tok wrap
[@@deriving show]
