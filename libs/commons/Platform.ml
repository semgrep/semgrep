(* TODO: other options for Windows! *)
type arch = Arm | Arm64 | X86_64 | OtherArch of string
type kernel = Darwin | Linux | OtherKernel of string

(* TODO: should use CapExec.string_of_run instead of with_open_process_in *)

let arch (caps : < Cap.exec >) =
  CapExec.with_open_process_in caps#exec "uname -m" (fun chan ->
      let s =
        In_channel.input_all chan |> String.trim |> String.lowercase_ascii
      in
      match s with
      | "arm" -> Arm
      | "arm64" -> Arm64
      | "x86_64" -> X86_64
      | _ -> OtherArch s)

let kernel (caps : < Cap.exec >) =
  CapExec.with_open_process_in caps#exec "uname" (fun chan ->
      let s =
        In_channel.input_all chan |> String.trim |> String.lowercase_ascii
      in
      match s with
      | "darwin" -> Darwin
      | "linux" -> Linux
      | _ -> OtherKernel s)

(* TODO? || Sys.cygwin? *)
let is_windows = Sys.win32
