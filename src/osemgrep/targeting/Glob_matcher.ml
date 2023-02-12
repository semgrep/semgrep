(*
   AST and matching of a glob pattern against a path.
   This is purely syntaxic: the file system is not accessed.

   We could use Re.Glob from the ocaml-re library but it doesn't expose
   the AST of the glob pattern, and this prevents us from making
   transformations required by gitignore such as treating the pattern
   'foo/bar' as equivalent to '/foo/bar' but not treat 'foo' as '/foo'.
*)

type loc = {
  source_name : string;
  line_number : int;
  line_contents : string;
}

type char_class_range = Class_char of char | Range of char * char
type char_class = { complement : bool; ranges : char_class_range list }

type component_fragment =
  | Char of char
  | Char_class of char_class
  | Question
  | Star

type component = Component of component_fragment list | Ellipsis
type pattern = component list

type compiled_pattern = {
  source : loc;
  re : Re.re
}

(* / *)
let root_pattern = [Component []; Component []]

let slash = Re.char '/'
let not_slash = Re.compl [slash]

let map_frag (frag : component_fragment) : Re.t =
  let open Re in
  match frag with
  | Char c -> char c
  | Char_class { complement; ranges } ->
      let cset =
        List.fold_left
          (fun acc range ->
             match range with
             | Class_char c -> char c
             | Range (a, b) -> alt [acc; rg a b]
          )
          (set "") ranges
      in
      if complement then
        compl [cset]
      else
        cset
  | Question -> not_slash
  | Star -> rep not_slash

let map_comp (comp : component_fragment list) : Re.t =
  let open Re in
  List.fold_left (fun acc frag -> seq [acc; map_frag frag]) empty comp

let rec map pat =
  let open Re in
  match pat with
  | [Component comp] ->
      seq [map_comp comp; eos]
  | [Ellipsis] ->
      empty
  | Component comp :: pat ->
      seq [map_comp comp; slash; map pat]
  | Ellipsis :: pat ->
      seq [rep (seq [rep not_slash; slash]);
           map pat]
  | [] ->
      eos

let map_root pat =
  let open Re in
  seq [bos; map pat]

(* Compile a pattern into an ocaml-re regexp for fast matching *)
let compile ~source pat =
  let re =
    map_root pat
    |> Re.compile
  in
  { source; re }

let run matcher path =
  Re.execp matcher.re path

let source matcher = matcher.source
