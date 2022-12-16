(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
*)
open Parser_sql

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let is_eof = function
  | EOF _ -> true
  | _ -> false


let visitor_info_of_tok f = function
  | T_SELECT ii -> T_SELECT (f ii)
  | T_INSERT ii -> T_INSERT (f ii)
  | T_UPDATE ii -> T_UPDATE (f ii)
  | T_DELETE ii -> T_DELETE (f ii)
  | T_FROM ii -> T_FROM (f ii)
  | T_WHERE ii -> T_WHERE (f ii)
  | T_AS ii -> T_AS (f ii)
  | T_SET ii -> T_SET (f ii)
  | T_INTO ii -> T_INTO (f ii)
  | T_REPLACING ii -> T_REPLACING (f ii)
  | T_SOFT ii -> T_SOFT (f ii)
  | T_VALUES ii -> T_VALUES (f ii)
  | T_JOIN ii -> T_JOIN (f ii)
  | T_ON ii -> T_ON (f ii)
  | T_LEFT ii -> T_LEFT (f ii)
  | T_OUTER ii -> T_OUTER (f ii)
  | T_DUPLICATE ii -> T_DUPLICATE (f ii)
  | T_KEY ii -> T_KEY (f ii)
  | T_ALL ii -> T_ALL (f ii)
  | T_DISTINCT ii -> T_DISTINCT (f ii)
  | T_IN ii -> T_IN (f ii)
  | T_UNION ii -> T_UNION (f ii)
  | T_ORDER ii -> T_ORDER (f ii)
  | T_BY ii -> T_BY (f ii)
  | T_ASC ii -> T_ASC (f ii)
  | T_DESC ii -> T_DESC (f ii)
  | T_LIMIT ii -> T_LIMIT (f ii)
  | T_LIKE ii -> T_LIKE (f ii)
  | T_NULLX ii -> T_NULLX (f ii)
  | T_COMMIT ii -> T_COMMIT (f ii)
  | T_BEGIN ii -> T_BEGIN (f ii)
  | T_ROLLBACK ii -> T_ROLLBACK (f ii)
  | T_MEMCACHE_DIRTY ii -> T_MEMCACHE_DIRTY (f ii)
  | T_NAME ii -> T_NAME (f ii)
  | T_INTNUM ii -> T_INTNUM (f ii)
  | T_STRING ii -> T_STRING (f ii)
  | T_DNUMBER ii -> T_DNUMBER (f ii)
  | APPROXNUM ii -> APPROXNUM (f ii)
  | TPLUS ii -> TPLUS (f ii)
  | TMINUS ii -> TMINUS (f ii)
  | TMUL ii -> TMUL (f ii)
  | TDIV ii -> TDIV (f ii)
  | TOPAR ii -> TOPAR (f ii)
  | TCPAR ii -> TCPAR (f ii)
  | T_OR ii -> T_OR (f ii)
  | T_AND ii -> T_AND (f ii)
  | T_NOT ii -> T_NOT (f ii)
  | TOPNOT ii -> TOPNOT (f ii)
  | TOPOR ii -> TOPOR (f ii)
  | TOPAND ii -> TOPAND (f ii)
  | TOPNEG ii -> TOPNEG (f ii)
  | T_COMPARISON ii -> T_COMPARISON (f ii)
  | TSEMICOLON ii -> TSEMICOLON (f ii)
  | TCOMMA ii -> TCOMMA (f ii)
  | TDOT ii -> TDOT (f ii)
  | TUnknown ii -> TUnknown (f ii)
  | EOF ii -> EOF (f ii)
