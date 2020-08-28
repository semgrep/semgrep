(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * PHP serialization 
 * http://php.net/manual/en/function.serialize.php
 *
 * src: found on the web at ????
 * I just un-extlib-ized it.
 * 
 * Anatomy of a serialize()'ed value:
 * 
 * String
 * s:size:value;
 * 
 * Integer
 * i:value;
 * 
 * Boolean
 * b:value; (does not store "true" or "false", does store '1' or '0')
 * 
 * Null
 * N;
 * 
 * Array
 * a:size:{key definition;value definition;(repeated per element)}
 * 
 * Object
 * O:strlen(object name):object name:object size:{s:strlen(property name):property name:property definition;(repeated per property)}
 * 
 * String values are always in double quotes
 * Array keys are always integers or strings
 * "null => 'value'" equates to 's:0:"";s:5:"value";',
 * "true => 'value'" equates to 'i:1;s:5:"value";',
 * "false => 'value'" equates to 'i:0;s:5:"value";',
 * "array(whatever the contents) => 'value'" equates to an "illegal offset type" warning because you can't use an
 * array as a key; however, if you use a variable containing an array as a key, it will equate to 's:5:"Array";s:5:"value";',
 * and
 * attempting to use an object as a key will result in the same behavior as using an array will.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type php = 
  | AI of (int * php) list 
  | AS of (string * php) list 
  | S of string 
  | I of int 
  | B of bool 
  | F of float 
  | N
 (* with tarzan *)

(*****************************************************************************)
(* Meta *)
(*****************************************************************************)

let rec vof_php =
  function
  | AI v1 ->
      let v1 =
        OCaml.vof_list
          (fun (v1, v2) ->
             let v1 = OCaml.vof_int v1
             and v2 = vof_php v2
             in OCaml.VTuple [ v1; v2 ])
          v1
      in OCaml.VSum (("AI", [ v1 ]))
  | AS v1 ->
      let v1 =
        OCaml.vof_list
          (fun (v1, v2) ->
             let v1 = OCaml.vof_string v1
             and v2 = vof_php v2
             in OCaml.VTuple [ v1; v2 ])
          v1
      in OCaml.VSum (("AS", [ v1 ]))
  | S v1 -> let v1 = OCaml.vof_string v1 in OCaml.VSum (("S", [ v1 ]))
  | I v1 -> let v1 = OCaml.vof_int v1 in OCaml.VSum (("I", [ v1 ]))
  | B v1 -> let v1 = OCaml.vof_bool v1 in OCaml.VSum (("B", [ v1 ]))
  | F v1 -> let v1 = OCaml.vof_float v1 in OCaml.VSum (("F", [ v1 ]))
  | N -> OCaml.VSum (("N", []))

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* open Prelude *)
let (>>) x f = f x
let ($) f g = function x -> f (g x)

(* pad: from extString.ml *)
let _string_init len f =
  let s = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.unsafe_set s i (f i)
  done;
  s |> Bytes.to_string

(*****************************************************************************)
(* Serialized string -> php *)
(*****************************************************************************)

(*
let rec parse_one = parser
  | [< ''a'; '':'; n=number; '':'; ''{'; a=parse_array; ''}' >] -> ignore n;(*check n (List.length a);*) a
  | [< ''b'; '':'; n=number; '';' >] -> B (0 <> n)
  | [< ''d'; '':'; f=parse_float_semi; >] -> F f
  | [< n=parse_int >] -> I n
  | [< s=parse_str >] -> S s
  | [< ''N'; '';' >] -> N
and number t = parse_nat 0 t
and parse_nat n = parser (* overflow test?* *)
  | [< ''0'..'9' as c; t >] -> let digit = Char.code c - Char.code '0' in parse_nat (n * 10 + digit) t
  | [< >] -> n
and integer = parser
  | [< ''-'; t >] -> - (number t)
  | [< t >] -> number t
and parse_int = parser
  | [< ''i'; '':'; n=integer; '';' >] -> n
and parse_float_semi t = (* ugly, because of one look ahead token FIXME *)
  let buf = Scanf.Scanning.from_function (fun () -> Stream.next t) in
  Scanf.bscanf buf "%f;" (fun f -> f)
and parse_str = parser
  | [< ''s'; '':'; n=number; '':'; ''"'; s=take_string n; ''"'; '';' >] -> s
and take_string n t = string_init n (fun _ -> Stream.next t)
and parse_array = parser
  | [< k=parse_int; v=parse_one; a=parse_int_array [k,v] >] -> AI a
  | [< k=parse_str; v=parse_one; a=parse_str_array [k,v] >] -> AS a
  | [< >] -> AI [] (* empty array *)
and parse_int_array acc = parser
  | [< k=parse_int; v=parse_one; t >] -> parse_int_array ((k,v)::acc) t
  | [< >] -> List.rev acc
and parse_str_array acc = parser
  | [< k=parse_str; v=parse_one; t >] -> parse_str_array ((k,v)::acc) t
  | [< >] -> List.rev acc
*)
let parse_one _ = failwith "TODO: port use of camlp4"

let parse stream =
  let show () =
    let tail = Stream.npeek 10 stream >> List.map (String.make 1) >> String.concat "" in
    Printf.sprintf "Position %u : %s" (Stream.count stream) tail
  in
  try
    let r = parse_one stream in
    Stream.empty stream; r
  with
  | Stream.Error _ | Stream.Failure -> failwith (show ())

let parse_string = parse $ Stream.of_string

(*****************************************************************************)
(* php -> Serialized string *)
(*****************************************************************************)

(*
module_Out = struct

(** Combinators to build values of [php] type *)

(* pad: from extList.ml *)

(** Serialize [php] value *)

(* pad:from IO.ml *)


end
*)
(*
let to_string v =
  let out = Out.io_output_string () in
  Out.output out v;
  Out.io_close_out o ???
*)
