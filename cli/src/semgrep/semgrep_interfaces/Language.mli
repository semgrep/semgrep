(* Generated file. Do not edit. *)

(* All the programming languages for which Semgrep has dedicated support. *)
type t =
| Apex
| Bash
| C
| Cairo
| Circom
| Clojure
| Cpp
| Csharp
| Dart
| Dockerfile
| Fga
| Elixir
| Go
| Gosu
| Hack
| Html
| Java
| Js
| Json
| Jsonnet
| Julia
| Kotlin
| Lisp
| Lua
| Move_on_sui
| Move_on_aptos
| Ocaml
| Php
| Powershell
| Promql
| Protobuf
| Python2
| Python3
| Python
| Ql
| R
| Ruby
| Rust
| Scala
| Scheme
| Solidity
| Swift
| Terraform
| Ts
| Vue
| Xml
| Yaml

(*
   Maturity of the support for the programming language as shown to the
   public. The constructors are sorted by increasing maturity, allowing
   meaningful sorting using the default 'compare'.
*)
type maturity =
| Develop
| Alpha
| Beta
| Ga

(*
   Information about a supported programming language for which we have
   a dedicated parser (target analyzer). Some of this information can also be
   used for the purpose of target selection.
*)
type info = {
  id: t;
  id_string: string;
  name: string;
  keys: string list;
  exts: string list;
  maturity: maturity;
  example_ext: string option;
  excluded_exts: string list;
  reverse_exts: string list option;
  shebangs: string list;
  tags: string list;
}

(*
   List of all the programming languages for which Semgrep has dedicated
   support. This list is sufficient to produce fast lookup tables implementing
   to_string, of_string, etc.
*)
val list : info list
