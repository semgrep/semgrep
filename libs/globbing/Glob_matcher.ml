(*
   AST and matching of a glob pattern against a path.
   This is purely syntactic: the file system is not accessed.

   We could use Re.Glob from the ocaml-re library for parsing the patterns
   but it doesn't expose the AST of the glob pattern, and this prevents us
   from making transformations required by gitignore such as treating
   the pattern 'foo/bar' as equivalent to '/foo/bar' but not treat
   'foo' as '/foo'. However, we use ocaml-re to produce the regexp tree
   and then execute it to match a path given as a string.
*)

open Printf

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type loc = {
  source_name : string;
  source_kind : string option;
  line_number : int;
  line_contents : string;
}

let show_loc x =
  Printf.sprintf "%s, line %i: %s" x.source_name x.line_number x.line_contents

type char_class_range = Class_char of char | Range of char * char
[@@deriving show { with_path = false }]

type char_class = { complement : bool; ranges : char_class_range list }
[@@deriving show { with_path = false }]

type segment_fragment =
  | Char of char
  | Char_class of char_class
  | Question
  | Star
[@@deriving show { with_path = false }]

type segment = Segment of segment_fragment list | Any_subpath
[@@deriving show { with_path = false }]

type pattern = segment list [@@deriving show]
type t = { source : loc; re : Re.re }

let string_loc ?(source_name = "<pattern>") ~source_kind pat =
  { source_name; source_kind; line_number = 1; line_contents = pat }

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* / *)
let root_pattern = [ Segment []; Segment [] ]

(* remove the leading slash unless it's a trailing slash *)
let remove_leading_slash xs =
  match xs with
  | [ Segment []; Segment [] ] as xs -> xs
  | Segment [] :: xs -> xs
  | xs -> xs

(* remove the trailing slash unless it's a leading slash *)
let remove_trailing_slash xs =
  let rec loop xs =
    match xs with
    | [] -> []
    | [ Segment [] ] ->
        (* ignore trailing slash that's not a leading slash *) []
    | x :: xs -> x :: loop xs
  in
  match xs with
  (* preserve leading slash *)
  | Segment [] :: xs -> Segment [] :: loop xs
  | xs -> loop xs

let append a b = remove_trailing_slash a @ remove_leading_slash b

let of_path_segments segments =
  Common.map
    (fun s ->
      let chars =
        Stdcompat.String.fold_right (fun c acc -> Char c :: acc) s []
      in
      Segment chars)
    segments

let slash = Re.char '/'
let not_slash = Re.compl [ slash ]

let map_frag (frag : segment_fragment) : Re.t =
  match frag with
  | Char c -> Re.char c
  | Char_class { complement; ranges } ->
      let cset =
        Common.map
          (fun range ->
            match range with
            | Class_char c -> Re.char c
            | Range (a, b) -> Re.rg a b)
          ranges
      in
      if complement then Re.compl cset else Re.alt cset
  | Question -> not_slash
  | Star -> Re.rep not_slash

let map_seg (seg : segment_fragment list) : Re.t =
  Re.seq (Common.map map_frag seg)

let rec map pat =
  match pat with
  | [ Segment seg ] -> [ map_seg seg; Re.eos ]
  | [ Any_subpath ] -> []
  | Segment seg :: pat -> map_seg seg :: slash :: map pat
  | Any_subpath :: pat -> Re.rep (Re.seq [ Re.rep not_slash; slash ]) :: map pat
  | [] -> [ Re.eos ]

(* Create a pattern that's left-anchored and right-anchored *)
let map_root pat = Re.seq (Re.bos :: map pat)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* Compile a pattern into an ocaml-re regexp for fast matching *)
let compile ~source pat =
  let re = map_root pat |> Re.compile in
  { source; re }

(* This is used during unit testing. *)
let debug = ref false

let run matcher path =
  let res = Re.execp matcher.re path in
  if !debug then
    (* expensive string concatenation; may not be suitable for logger#debug *)
    printf "** pattern: %S  path: %S  matches: %B\n"
      matcher.source.line_contents path res;
  res

let source matcher = matcher.source

let show x =
  let re_info =
    Re.pp_re Format.str_formatter x.re;
    Format.flush_str_formatter ()
  in
  sprintf "pattern at %s:\n%s" (show_loc x.source) re_info
