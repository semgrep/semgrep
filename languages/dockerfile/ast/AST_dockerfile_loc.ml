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

open AST_dockerfile

(***************************************************************************)
(* Location extraction *)
(***************************************************************************)

let wrap_tok ((_, tok) : _ wrap) = tok
let wrap_loc ((_, tok) : _ wrap) = (tok, tok)
let bracket_loc ((open_, _, close) : _ bracket) = (open_, close)

let expansion_tok = function
  | Expand_var (_, tok) -> tok
  | Expand_semgrep_metavar (_, tok) -> tok

let _expansion_loc x =
  let tok = expansion_tok x in
  (tok, tok)

let _double_quoted_string_fragment_loc = function
  | Dbl_string_content (_, tok) -> (tok, tok)
  | Dbl_expansion (loc, _) -> loc
  | Dbl_frag_semgrep_metavar (_, tok) -> (tok, tok)

let docker_string_fragment_loc (x : docker_string_fragment) =
  match x with
  | Unquoted (_, tok) -> (tok, tok)
  | Single_quoted (loc, _) -> loc
  | Double_quoted (loc, _) -> loc
  | Expansion (loc, _) -> loc
  | Frag_semgrep_metavar (_, tok) -> (tok, tok)

let ident_or_metavar_loc = function
  | Ident (_, tok) -> (tok, tok)
  | Semgrep_metavar (_, tok) -> (tok, tok)

let key_or_metavar_loc = function
  | Key x -> docker_string_fragment_loc x
  | Semgrep_metavar (_, tok) -> (tok, tok)

let docker_string_loc ((loc, _) : docker_string) = loc
let heredoc_template_loc (x : heredoc_template) = (x.opening, x.closing)

let str_or_ellipsis_loc = function
  | Str_str str -> docker_string_loc str
  | Str_template x -> heredoc_template_loc x
  | Str_semgrep_ellipsis tok -> (tok, tok)

(* Re-using the type used for double-quoted strings in bash *)
let _quoted_string_loc = bracket_loc

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
let command_loc = function
  | Command_semgrep_ellipsis tok -> (tok, tok)
  | Command_semgrep_named_ellipsis x -> wrap_loc x
  | Argv (loc, _) -> loc
  | Sh_command (loc, _) -> loc
  | Other_shell_command (_, x) -> wrap_loc x
  | Shell_command_template (loc, _) -> loc

let param_loc ((loc, _) : param) : loc = loc

let param_or_ellipsis_loc : param_or_ellipsis -> loc = function
  | ParamParam param -> param_loc param
  | ParamEllipsis tok -> (tok, tok)

let image_spec_loc (x : image_spec) = x.loc

let env_pair_loc = function
  | Env_semgrep_ellipsis tok -> (tok, tok)
  | Env_pair (loc, _, _, _) -> loc

let label_pair_loc = function
  | Label_semgrep_ellipsis tok -> (tok, tok)
  | Label_pair (loc, _, _, _) -> loc

let _string_array_loc = bracket_loc

let array_or_paths_loc = function
  | Array (loc, _)
  | Paths (loc, _) ->
      loc

let cmd_instr_loc ((loc, _, _, _) : cmd_instr) = loc

let healthcheck_loc = function
  | Healthcheck_semgrep_metavar (_, tok) -> (tok, tok)
  | Healthcheck_ellipsis tok -> (tok, tok)
  | Healthcheck_none tok -> (tok, tok)
  | Healthcheck_cmd (loc, _, _) -> loc

let expose_port_loc = function
  | Expose_semgrep_ellipsis tok -> (tok, tok)
  | Expose_port ((_, tok1), Some (_, tok2)) -> (tok1, tok2)
  | Expose_port ((_, tok), None) -> (tok, tok)
  | Expose_fragment x -> docker_string_fragment_loc x

let instruction_loc = function
  | From (loc, _, _, _, _) -> loc
  | Run cmd -> cmd_instr_loc cmd
  | Cmd cmd -> cmd_instr_loc cmd
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
