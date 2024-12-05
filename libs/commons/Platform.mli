type arch = Arm | Arm64 | X86_64 | OtherArch of string

(* note that Sys.os_type uses "unix" for both Darwin and Linux *)
type kernel = Darwin | Linux | OtherKernel of string

(* We need Cap.exec because both functions are calling 'uname' internally.
 * You should avoid using those functions and prefer if possible
 * Sys.os_type, Sys.{unix,win32,cygwin}
 *)

val arch : < Cap.exec > -> arch
val kernel : < Cap.exec > -> kernel

(* alias for Sys.win32 *)
val is_windows : bool
