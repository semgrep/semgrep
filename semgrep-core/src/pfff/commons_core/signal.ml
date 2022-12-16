(*
   Utilities to deal with signal numbers.
*)

let known_signals = [
  Sys.sigabrt, "sigabrt", "Abnormal termination";
  Sys.sigalrm, "sigalrm", "Timeout";
  Sys.sigfpe, "sigfpe", "Arithmetic exception";
  Sys.sighup, "sighup", "Hangup on controlling terminal";
  Sys.sigill, "sigill", "Invalid hardware instruction";
  Sys.sigint, "sigint", "Interactive interrupt (ctrl-C)";
  Sys.sigkill, "sigkill", "Termination (cannot be ignored)";
  Sys.sigpipe, "sigpipe", "Broken pipe";
  Sys.sigquit, "sigquit", "Interactive termination";
  Sys.sigsegv, "sigsegv", "Invalid memory reference";
  Sys.sigterm, "sigterm", "Termination";
  Sys.sigusr1, "sigusr1", "Application-defined signal 1";
  Sys.sigusr2, "sigusr2", "Application-defined signal 2";
  Sys.sigchld, "sigchld", "Child process terminated";
  Sys.sigcont, "sigcont", "Continue";
  Sys.sigstop, "sigstop", "Stop";
  Sys.sigtstp, "sigtstp", "Interactive stop";
  Sys.sigttin, "sigttin", "Terminal read from background process";
  Sys.sigttou, "sigttou", "Terminal write from background process";
  Sys.sigvtalrm, "sigvtalrm", "Timeout in virtual time";
  Sys.sigprof, "sigprof", "Profiling interrupt";
  Sys.sigbus, "sigbus", "Bus error";

  (* Since OCaml 4.03 *)
  Sys.sigpoll, "sigpoll", "Pollable event";
  Sys.sigsys, "sigsys", "Bad argument to routine";
  Sys.sigtrap, "sigtrap", "Trace/breakpoint trap";
  Sys.sigurg, "sigurg", "Urgent condition on socket";
  Sys.sigxcpu, "sigxcpu", "Timeout in cpu time";
  Sys.sigxfsz, "sigxfsz", "File size limit exceeded";
]

let signal_tbl = lazy (
  let tbl = Hashtbl.create 100 in
  List.iter (fun (id, name, descr) ->
    Hashtbl.replace tbl id (name, descr)
  ) known_signals;
  tbl
)

let get_info id =
  Hashtbl.find_opt (Lazy.force signal_tbl) id

let get_name id =
  match get_info id with
  | None -> None
  | Some (name, _) -> Some name

let get_descr id =
  match get_info id with
  | None -> None
  | Some (_, descr) -> Some descr
