(*
   Library defining the semgrep command-line interface.

   This module determines the subcommand invoked on the command line
   and has another module handle it as if it were an independent command.
   We don't use Cmdliner to dispatch subcommands because it's a too
   complicated and we never show a help page for the whole command anyway
   since we fall back to the 'scan' subcommand if none is given.
*)

open Printf

(* This is used to determine if we should fall back to assuming 'scan'. *)
let known_subcommands =
  [ "ci"; "login"; "logout"; "lsp"; "publish"; "scan"; "shouldafound" ]

(* Exit with a code that a proper semgrep implementation would never return.
   Uncaught OCaml exception result in exit code 2.
   This is to ensure that the tests that expect error status 2 fail. *)
let missing_subcommand () =
  eprintf "This semgrep subcommand is not implemented\n%!";
  Exit_code.not_implemented_in_osemgrep

let main_help_msg =
  {|Usage: semgrep [OPTIONS] COMMAND [ARGS]...

  To get started quickly, run `semgrep scan --config auto`

  Run `semgrep SUBCOMMAND --help` for more information on each subcommand

  If no subcommand is passed, will run `scan` subcommand by default

Options:
  -h, --help  Show this message and exit.

Commands:
  ci            The recommended way to run semgrep in CI
  login         Obtain and save credentials for semgrep.dev
  logout        Remove locally stored credentials to semgrep.dev
  lsp           [EXPERIMENTAL] Start the Semgrep LSP server
  publish       Upload rule to semgrep.dev
  scan          Run semgrep rules on files
  shouldafound  Report a false negative in this project.
|}

let dispatch_subcommand argv =
  match Array.to_list argv with
  | [] -> assert false
  | [ _; ("-h" | "--help") ] ->
      print_string main_help_msg;
      Exit_code.ok
  | argv0 :: args -> (
      let subcmd, subcmd_args =
        match args with
        | [] -> ("scan", [])
        | arg1 :: other_args ->
            if List.mem arg1 known_subcommands then (arg1, other_args)
            else
              (* No valid subcommand was found.
                 Assume the 'scan' subcommand was omitted and insert it. *)
              ("scan", arg1 :: other_args)
      in
      let subcmd_argv =
        let subcmd_argv0 = argv0 ^ "-" ^ subcmd in
        subcmd_argv0 :: subcmd_args |> Array.of_list
      in
      match subcmd with
      | "ci" -> missing_subcommand ()
      | "login" -> missing_subcommand ()
      | "logout" -> missing_subcommand ()
      | "lsp" -> missing_subcommand ()
      | "publish" -> missing_subcommand ()
      | "scan" -> Semgrep_scan.main subcmd_argv
      | "shouldafound" -> missing_subcommand ()
      | _ -> (* should have defaulted to 'scan' above *) assert false)

let main argv =
  Printexc.record_backtrace true;
  dispatch_subcommand argv
