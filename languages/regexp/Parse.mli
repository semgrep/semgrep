(*
   Copyright (c) 2022-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Public entry point for parsing regexps.

   The default implementation tries to be compatible with the latest
   version of PCRE, documented at
   https://www.pcre.org/original/doc/html/pcrepattern.html

   To define or select a particular regexp dialect, see Dialect.mli.

   For a description of all the features and which dialects support what,
   consult the site regular-expressions.info.
   - cheatsheet: https://www.regular-expressions.info/refquick.html
   - dialect comparison: https://www.regular-expressions.info/refcharacters.html

   Missing features (see 'man pcresyntax'):
   - disable extra features not applicable for the chosen dialect. Requires
     lexbuf rollback (maybe requires switching to sedlex, not sure).
     - perl
     - javascript
     - python
     - oniguruma/ruby
     - pcre without pcre_javascript_compat (\uXXXX)
*)

(* Parse a file *)
val file : ?conf:Conf.t -> Fpath.t -> AST.t

(* Parse a string *)
val string : ?conf:Conf.t -> string -> AST.t

(* Alias for 'file' *)
val parse : ?conf:Conf.t -> Fpath.t -> AST.t
