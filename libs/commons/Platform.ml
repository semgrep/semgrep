(* TODO: other options for Windows! *)
type arch = Arm | Arm64 | X86_64 | OtherArch of string
type kernel = Darwin | Linux | OtherKernel of string

let arch () =
  UCmd.with_open_process_in "uname -m" (fun chan ->
      let s =
        In_channel.input_all chan |> String.trim |> String.lowercase_ascii
      in
      match s with
      | "arm" -> Arm
      | "arm64" -> Arm64
      | "x86_64" -> X86_64
      | _ -> OtherArch s)

let kernel () =
  UCmd.with_open_process_in "uname" (fun chan ->
      let s =
        In_channel.input_all chan |> String.trim |> String.lowercase_ascii
      in
      match s with
      | "darwin" -> Darwin
      | "linux" -> Linux
      | _ -> OtherKernel s)
