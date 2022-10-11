(* Semgrep_bridge_core.ml *)
(* Entry point for semgrep as a shared library. *)

(*
  This is invoked by bridge_ml.c and accepts a function pointer wrapped
  as an OCaml value:

  * read_file_fnptr: Map a file name to either:
      - (file contents, 0), if successful, or
      - (UTF-8 error message, error code), if not.

  The numeric error codes are defined by the BridgeErrorCode enum
  defined in bridge_ml.h.  This code raises corresponding exceptions.

  The interface is very similar to that of 'semgrep_analyze' in the
  Python 'semgrep_bridge_python' module, which is defined and documented
  in bridge_py.c.

  The Fnptr.t objects are only valid for the duration of this call.
*)
let semgrep_cli_lib_main (argv : string array)
    (read_file_fnptr : (string, bytes * int) Fnptr.t) : unit =
  (* Call the fnptr and yield the result or raise an exception. *)
  let bridge_read_file (fname : string) : bytes =
    (* Invoke the function pointer, which implements an in-memory
     * file system of sorts. *)
    let contents, code = Fnptr.call read_file_fnptr fname in

    (* Except for being out of memory, we raise Sys_error here so that
     * callers prepared to handle typical I/O errors like "file not
     * found" will work. *)
    match code with
    | 0 -> contents
    | 1 -> raise Out_of_memory
    | 2 -> raise (Sys_error "read_file callback failed with unknown error.")
    (* The 'contents' contains the error message. *)
    | 3 -> raise (Sys_error (Bytes.to_string contents))
    | __else__ ->
        raise
          (Sys_error
             ("read_file callback failed with code " ^ string_of_int code ^ "."))
  in

  (* TODO: Do something with this!  For now, the OCaml code just ignores
   * the callback and reads files from disk.  Either the OCaml code
   * needs to read files via this callback, or the Python CLI needs to
   * copy the target files into /tmp and pass their new names. *)
  ignore bridge_read_file;

  (* Call the main analysis. *)
  Core_CLI.main argv

(* Register the entry point so it can be called from C. *)
let () = Callback.register "semgrep_cli_lib_main" semgrep_cli_lib_main

(* Turn an exception object into a string. *)
let exn_to_string (e : exn) : string = Printexc.to_string e

(* Register this as well. *)
let () = Callback.register "exn_to_string" exn_to_string

(* EOF *)
