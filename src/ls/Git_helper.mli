val is_git_repo : unit -> bool Lwt.t
val dirty_lines_of_file : string -> (int * int) array Lwt.t
val dirty_files : unit -> string list Lwt.t
