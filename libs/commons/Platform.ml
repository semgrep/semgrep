(* TODO: other options for Windows! *)
type arch = Arm | Arm64 | X86_64 | OtherArch of string
type kernel = Darwin | Linux | OtherKernel of string

let arch () =
  let chan = Unix.open_process_in "uname -m" in
  let s = In_channel.input_all chan |> String.trim |> String.lowercase_ascii in
  match s with
  | "arm" -> Arm
  | "arm64" -> Arm64
  | "x86_64" -> X86_64
  | _ -> OtherArch s

let kernel () =
  let chan = Unix.open_process_in "uname" in
  let s = In_channel.input_all chan |> String.trim |> String.lowercase_ascii in
  match s with
  | "darwin" -> Darwin
  | "linux" -> Linux
  | _ -> OtherKernel s
