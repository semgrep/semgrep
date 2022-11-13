(*
   Shared parameters, options, and help messages for the semgrep CLI.
*)
open Cmdliner

let help_page_bottom =
  [
    `S Manpage.s_authors;
    `P "r2c <support@r2c.dev>";
    `S Manpage.s_bugs;
    `P
      "If you encounter an issue, please report it at\n\
      \      https://github.com/returntocorp/semgrep/issues";
  ]
