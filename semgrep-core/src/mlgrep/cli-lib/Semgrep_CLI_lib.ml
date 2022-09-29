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
  exit 98

let dispatch_subcommand argv =
  match Array.to_list argv with
  | [] -> assert false
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
      | "scan" -> Scan.main subcmd_argv
      | "shouldafound" -> missing_subcommand ()
      | _ -> (* should have defaulted to 'scan' above *) assert false)

let main argv =
  Printexc.record_backtrace true;
  dispatch_subcommand argv
