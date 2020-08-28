
type php = 
  | AI of (int * php) list 
  | AS of (string * php) list 
  | S of string 
  | I of int 
  | B of bool 
  | F of float 
  | N

val parse_string: string -> php

val vof_php: php -> OCaml.v
