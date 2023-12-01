let string_of_run ~trim cmd =
  let out = Cmd.bos_apply Bos.OS.Cmd.run_out cmd in
  Bos.OS.Cmd.out_string ~trim out

let lines_of_run ~trim cmd =
  let out = Cmd.bos_apply Bos.OS.Cmd.run_out cmd in
  Bos.OS.Cmd.out_lines ~trim out

let status_of_run ?quiet = Cmd.bos_apply (Bos.OS.Cmd.run_status ?quiet)
