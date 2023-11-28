(* TODO: maybe use Sys.os_type? *)
type arch = Arm | Arm64 | X86_64 | OtherArch of string
type kernel = Darwin | Linux | OtherKernel of string

val arch : unit -> arch
val kernel : unit -> kernel
