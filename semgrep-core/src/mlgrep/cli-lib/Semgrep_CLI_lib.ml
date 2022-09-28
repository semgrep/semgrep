(*
   Library defining the semgrep command-line interface.

   This module determines the subcommand invoked on the command line
   and has another module handle it as if it were an independent command.
   We don't use Cmdliner to dispatch subcommands because it's a too
   complicated and we never show a help page for the whole command anyway
   since we fall back to the 'scan' subcommand if none is given.
*)

(* This is used to determine if we should fall back to assuming 'scan'. *)
let known_subcommands =
  [ "ci"; "login"; "logout"; "lsp"; "publish"; "scan"; "shouldafound" ]

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
      | "ci" -> failwith "this subcommand is not implemented"
      | "login" -> failwith "this subcommand is not implemented"
      | "logout" -> failwith "this subcommand is not implemented"
      | "lsp" -> failwith "this subcommand is not implemented"
      | "publish" -> failwith "this subcommand is not implemented"
      | "scan" -> Scan.main subcmd_argv
      | "shouldafound" -> failwith "this subcommand is not implemented"
      | _ -> (* should have defaulted to 'scan' above *) assert false)

let main argv =
  Printexc.record_backtrace true;
  dispatch_subcommand argv
