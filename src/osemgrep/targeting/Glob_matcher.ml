(*
   AST and matching of a glob pattern against a path.
   This is purely syntaxic: the file system is not accessed.

   We could use Re.Glob from the ocaml-re library but it doesn't expose
   the AST of the glob pattern, and this prevents us from making
   transformations required by gitignore such as treating the pattern
   'foo/bar' as equivalent to '/foo/bar' but not treat 'foo' as '/foo'.
*)

open Printf

type loc = { source_name : string; line_number : int; line_contents : string }

let show_loc x =
  Printf.sprintf "%s, line %i: %s" x.source_name x.line_number x.line_contents

type char_class_range = Class_char of char | Range of char * char
[@@deriving show { with_path = false }]

type char_class = { complement : bool; ranges : char_class_range list }
[@@deriving show { with_path = false }]

type component_fragment =
  | Char of char
  | Char_class of char_class
  | Question
  | Star
[@@deriving show { with_path = false }]

type component = Component of component_fragment list | Ellipsis
[@@deriving show { with_path = false }]

type pattern = component list [@@deriving show]
type t = { source : loc; re : Re.re }

let string_loc pat =
  { source_name = "<pattern>"; line_number = 0; line_contents = pat }

(* / *)
let root_pattern = [ Component []; Component [] ]

(* remove the leading slash unless it's a trailing slash *)
let remove_leading_slash xs =
  match xs with
  | [ Component []; Component [] ] as xs -> xs
  | Component [] :: xs -> xs
  | xs -> xs

(* remove the trailing slash unless it's a leading slash *)
let remove_trailing_slash xs =
  let rec loop xs =
    match xs with
    | [] -> []
    | [ Component [] ] ->
        (* ignore trailing slash that's not a leading slash *) []
    | x :: xs -> x :: loop xs
  in
  match xs with
  (* preserve leading slash *)
  | Component [] :: xs -> Component [] :: loop xs
  | xs -> loop xs

let append a b = remove_trailing_slash a @ remove_leading_slash b

let of_path_components components =
  Common.map
    (fun s ->
      let chars = String.fold_right (fun c acc -> Char c :: acc) s [] in
      Component chars)
    components

let slash = Re.char '/'
let not_slash = Re.compl [ slash ]

let map_frag (frag : component_fragment) : Re.t =
  let open Re in
  match frag with
  | Char c -> char c
  | Char_class { complement; ranges } ->
      let cset =
        Common.map
          (fun range ->
            match range with
            | Class_char c -> char c
            | Range (a, b) -> rg a b)
          ranges
      in
      if complement then compl cset else alt cset
  | Question -> not_slash
  | Star -> rep not_slash

let map_comp (comp : component_fragment list) : Re.t =
  let open Re in
  seq (Common.map map_frag comp)

let rec map pat =
  let open Re in
  match pat with
  | [ Component comp ] -> [ map_comp comp; eos ]
  | [ Ellipsis ] -> []
  | Component comp :: pat -> map_comp comp :: slash :: map pat
  | Ellipsis :: pat -> rep (seq [ rep not_slash; slash ]) :: map pat
  | [] -> [ eos ]

let map_root pat =
  let open Re in
  seq (bos :: map pat)

(* Compile a pattern into an ocaml-re regexp for fast matching *)
let compile ~source pat =
  let re = map_root pat |> Re.compile in
  { source; re }

let debug = false

let run matcher path =
  let res = Re.execp matcher.re path in
  if debug then
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
