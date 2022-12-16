(*
   Some predefined dialects.
*)

open Conf

type t =
  | Go
  | Java
  | Javascript
  | PCRE
  | PCRE_extended
  | Perl
  | Perl_x
  | Perl_xx
  | Python

let to_string = function
  | Go -> "go"
  | Java -> "java"
  | Javascript -> "javascript"
  | PCRE -> "pcre"
  | PCRE_extended -> "pcre_extended"
  | Perl -> "perl"
  | Perl_x -> "perl_x"
  | Perl_xx -> "perl_xx"
  | Python -> "python"

let of_string = function
  | "go" -> Some Go
  | "java" -> Some Java
  | "javascript" -> Some Javascript
  | "pcre" -> Some PCRE
  | "pcre_extended" -> Some PCRE_extended
  | "perl" -> Some Perl
  | "perl_x" -> Some Perl_x
  | "perl_xx" -> Some Perl_xx
  | "python" -> Some Python
  | _ -> None

let default_conf = {
  pcre_dotall = false;
  pcre_multiline = false;
  pcre_ucp = false;
  pcre_javascript_compat = false;
  with_comment_groups = true;
  ignore_whitespace = false;
  ignore_whitespace_in_char_classes = false;
  ignore_hash_comments = false;
}

(* https://www.pcre.org/original/doc/html/pcrepattern.html *)
let pcre = default_conf

let pcre_extended = {
  default_conf with
  ignore_whitespace = true;
  ignore_hash_comments = true;
}

(* https://perldoc.perl.org/perlre *)
let perl = pcre

let perl_x = pcre_extended

let perl_xx = {
  pcre_extended with
  ignore_whitespace_in_char_classes = true;
}

(* https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions/Cheatsheet *)
let javascript = {
  pcre with
  pcre_javascript_compat = true;
}

(* https://docs.python.org/3/library/re.html *)
let python = default_conf

(* https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html *)
let java = default_conf

(* https://github.com/google/re2/wiki/Syntax *)
let go = {
  default_conf with with_comment_groups = false
}

let conf = function
  | Go -> go
  | Java -> java
  | Javascript -> javascript
  | PCRE -> pcre
  | PCRE_extended -> pcre_extended
  | Perl -> perl
  | Perl_x -> perl_x
  | Perl_xx -> perl_xx
  | Python -> python
