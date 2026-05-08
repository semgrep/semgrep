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

let list = [
{
  id = Apex;
  id_string = "apex";
  name = "Apex";
  keys = [{|apex|}];
  exts = [{|.cls|}];
  maturity = Develop;
  example_ext = None;
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [{|is_proprietary|}];
};
{
  id = Bash;
  id_string = "bash";
  name = "Bash";
  keys = [{|bash|}; {|sh|}];
  exts = [{|.bash|}; {|.sh|}];
  maturity = Alpha;
  example_ext = Some {|.sh|};
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [{|bash|}; {|sh|}];
  tags = [];
};
{
  id = C;
  id_string = "c";
  name = "C";
  keys = [{|c|}];
  exts = [{|.c|}; {|.h|}];
  maturity = Alpha;
  example_ext = None;
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [];
};
{
  id = Cairo;
  id_string = "cairo";
  name = "Cairo";
  keys = [{|cairo|}];
  exts = [{|.cairo|}];
  maturity = Alpha;
  example_ext = None;
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [];
};
{
  id = Circom;
  id_string = "circom";
  name = "Circom";
  keys = [{|circom|}];
  exts = [{|.circom|}];
  maturity = Develop;
  example_ext = None;
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [];
};
{
  id = Clojure;
  id_string = "clojure";
  name = "Clojure";
  keys = [{|clojure|}];
  exts = [{|.clj|}; {|.cljs|}; {|.cljc|}; {|.edn|}];
  maturity = Alpha;
  example_ext = None;
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [];
};
{
  id = Cpp;
  id_string = "cpp";
  name = "C++";
  keys = [{|cpp|}; {|c++|}];
  exts = [{|.cc|}; {|.cpp|}; {|.cxx|}; {|.c++|}; {|.pcc|}; {|.tpp|}; {|.C|}; {|.h|}; {|.hh|}; {|.hpp|}; {|.hxx|}; {|.inl|}; {|.ipp|}];
  maturity = Alpha;
  example_ext = Some {|.cpp|};
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [];
};
{
  id = Csharp;
  id_string = "csharp";
  name = "C#";
  keys = [{|csharp|}; {|c#|}];
  exts = [{|.cs|}];
  maturity = Ga;
  example_ext = None;
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [];
};
{
  id = Dart;
  id_string = "dart";
  name = "Dart";
  keys = [{|dart|}];
  exts = [{|.dart|}];
  maturity = Develop;
  example_ext = None;
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [];
};
(*
  'Dockerfile' is the only standard name for Dockerfiles.
  The extension '.Dockerfile' is cited in the official documentation as
  a popular extension. Whatever naming scheme is used in practice and is
  not ambiguous is welcome here.
*)
{
  id = Dockerfile;
  id_string = "dockerfile";
  name = "Dockerfile";
  keys = [{|dockerfile|}; {|docker|}];
  exts = [{|.dockerfile|}; {|.Dockerfile|}; {|Dockerfile|}; {|dockerfile|}];
  maturity = Alpha;
  example_ext = Some {|.dockerfile|};
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [];
};
{
  id = Fga;
  id_string = "fga";
  name = "Fga";
  keys = [{|fga|}; {|openfga|}];
  exts = [{|.fga|}];
  maturity = Alpha;
  example_ext = Some {|.fga|};
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [];
};
{
  id = Elixir;
  id_string = "elixir";
  name = "Elixir";
  keys = [{|ex|}; {|elixir|}];
  exts = [{|.ex|}; {|.exs|}];
  maturity = Alpha;
  example_ext = None;
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [{|is_proprietary|}];
};
{
  id = Go;
  id_string = "go";
  name = "Go";
  keys = [{|go|}; {|golang|}];
  exts = [{|.go|}];
  maturity = Ga;
  example_ext = None;
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [];
};
{
  id = Gosu;
  id_string = "gosu";
  name = "Gosu";
  keys = [{|gosu|}];
  exts = [{|.gs|}];
  maturity = Develop;
  example_ext = None;
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [{|is_proprietary|}];
};
{
  id = Hack;
  id_string = "hack";
  name = "Hack";
  keys = [{|hack|}];
  exts = [{|.hack|}; {|.hck|}; {|.hh|}];
  maturity = Develop;
  example_ext = Some {|.hack|};
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [{|hhvm|}];
  tags = [];
};
{
  id = Html;
  id_string = "html";
  name = "HTML";
  keys = [{|html|}];
  exts = [{|.htm|}; {|.html|}];
  maturity = Alpha;
  example_ext = Some {|.html|};
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [];
};
{
  id = Java;
  id_string = "java";
  name = "Java";
  keys = [{|java|}];
  exts = [{|.java|}];
  maturity = Ga;
  example_ext = None;
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [];
};
{
  id = Js;
  id_string = "js";
  name = "JavaScript";
  keys = [{|js|}; {|javascript|}];
  exts = [{|.cjs|}; {|.js|}; {|.jsx|}; {|.mjs|}];
  maturity = Ga;
  example_ext = Some {|.jsx|};
  excluded_exts = [{|.min.js|}];
  reverse_exts = None;
  shebangs = [{|node|}; {|js|}; {|nodejs|}];
  tags = [{|is_js|}];
};
{
  id = Json;
  id_string = "json";
  name = "JSON";
  keys = [{|json|}];
  exts = [{|.json|}; {|.ipynb|}];
  maturity = Ga;
  example_ext = None;
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [];
};
{
  id = Jsonnet;
  id_string = "jsonnet";
  name = "Jsonnet";
  keys = [{|jsonnet|}];
  exts = [{|.jsonnet|}; {|.libsonnet|}];
  maturity = Alpha;
  example_ext = None;
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [];
};
{
  id = Julia;
  id_string = "julia";
  name = "Julia";
  keys = [{|julia|}];
  exts = [{|.jl|}];
  maturity = Alpha;
  example_ext = None;
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [];
};
{
  id = Kotlin;
  id_string = "kotlin";
  name = "Kotlin";
  keys = [{|kt|}; {|kotlin|}];
  exts = [{|.kt|}; {|.kts|}; {|.ktm|}];
  maturity = Beta;
  example_ext = Some {|.kt|};
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [];
};
{
  id = Lisp;
  id_string = "lisp";
  name = "Lisp";
  keys = [{|lisp|}];
  exts = [{|.lisp|}; {|.cl|}; {|.el|}];
  maturity = Alpha;
  example_ext = None;
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [];
};
{
  id = Lua;
  id_string = "lua";
  name = "Lua";
  keys = [{|lua|}];
  exts = [{|.lua|}];
  maturity = Alpha;
  example_ext = None;
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [{|lua|}];
  tags = [];
};
(*
  Move language with SUI flavor
*)
{
  id = Move_on_sui;
  id_string = "move_on_sui";
  name = "Move on Sui";
  keys = [{|move_on_sui|}];
  exts = [{|.move|}];
  maturity = Develop;
  example_ext = None;
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [];
};
(*
  Move language with Aptos flavor
*)
{
  id = Move_on_aptos;
  id_string = "move_on_aptos";
  name = "Move on Aptos";
  keys = [{|move_on_aptos|}];
  exts = [{|.move|}];
  maturity = Develop;
  example_ext = None;
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [];
};
{
  id = Ocaml;
  id_string = "ocaml";
  name = "OCaml";
  keys = [{|ocaml|}];
  exts = [{|.ml|}; {|.mli|}];
  maturity = Alpha;
  example_ext = Some {|.ml|};
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [{|ocaml|}; {|ocamlscript|}];
  tags = [];
};
{
  id = Php;
  id_string = "php";
  name = "PHP";
  keys = [{|php|}];
  exts = [{|.php|}; {|.tpl|}; {|.phtml|}];
  maturity = Ga;
  example_ext = None;
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [{|php|}];
  tags = [];
};
{
  id = Powershell;
  id_string = "powershell";
  name = "Powershell";
  keys = [{|powershell|}];
  exts = [{|.ps1|}];
  maturity = Alpha;
  example_ext = None;
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [];
};
{
  id = Promql;
  id_string = "promql";
  name = "Prometheus Query Language";
  keys = [{|promql|}];
  exts = [{|.promql|}];
  maturity = Alpha;
  example_ext = None;
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [];
};
{
  id = Protobuf;
  id_string = "protobuf";
  name = "Protocol Buffers";
  keys = [{|proto|}; {|protobuf|}; {|proto3|}];
  exts = [{|.proto|}];
  maturity = Develop;
  example_ext = None;
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [];
};
{
  id = Python2;
  id_string = "python2";
  name = "Python 2";
  keys = [{|python2|}];
  exts = [{|.py|}; {|.pyi|}];
  maturity = Develop;
  example_ext = Some {|.py|};
  excluded_exts = [];
  reverse_exts = Some [];
  shebangs = [{|python|}; {|python2|}];
  tags = [{|is_python|}];
};
{
  id = Python3;
  id_string = "python3";
  name = "Python 3";
  keys = [{|python3|}];
  exts = [{|.py|}; {|.pyi|}];
  maturity = Develop;
  example_ext = Some {|.py|};
  excluded_exts = [];
  reverse_exts = Some [];
  shebangs = [{|python|}; {|python3|}];
  tags = [{|is_python|}];
};
{
  id = Python;
  id_string = "python";
  name = "Python";
  keys = [{|py|}; {|python|}];
  exts = [{|.py|}; {|.pyi|}];
  maturity = Ga;
  example_ext = Some {|.py|};
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [{|python|}; {|python2|}; {|python3|}];
  tags = [{|is_python|}];
};
{
  id = Ql;
  id_string = "ql";
  name = "QL";
  keys = [{|ql|}];
  exts = [{|.ql|}; {|.qll|}];
  maturity = Alpha;
  example_ext = Some {|.ql|};
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [];
};
{
  id = R;
  id_string = "r";
  name = "R";
  keys = [{|r|}];
  exts = [{|.r|}; {|.R|}];
  maturity = Alpha;
  example_ext = Some {|.R|};
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [];
};
{
  id = Ruby;
  id_string = "ruby";
  name = "Ruby";
  keys = [{|ruby|}];
  exts = [{|.rb|}];
  maturity = Ga;
  example_ext = None;
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [{|ruby|}];
  tags = [];
};
{
  id = Rust;
  id_string = "rust";
  name = "Rust";
  keys = [{|rust|}];
  exts = [{|.rs|}];
  maturity = Alpha;
  example_ext = None;
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [{|run-cargo-script|}];
  tags = [];
};
{
  id = Scala;
  id_string = "scala";
  name = "Scala";
  keys = [{|scala|}];
  exts = [{|.scala|}];
  maturity = Ga;
  example_ext = None;
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [{|scala|}];
  tags = [];
};
{
  id = Scheme;
  id_string = "scheme";
  name = "Scheme";
  keys = [{|scheme|}];
  exts = [{|.scm|}; {|.ss|}];
  maturity = Alpha;
  example_ext = None;
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [];
};
{
  id = Solidity;
  id_string = "solidity";
  name = "Solidity";
  keys = [{|solidity|}; {|sol|}];
  exts = [{|.sol|}];
  maturity = Alpha;
  example_ext = None;
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [];
};
{
  id = Swift;
  id_string = "swift";
  name = "Swift";
  keys = [{|swift|}];
  exts = [{|.swift|}];
  maturity = Alpha;
  example_ext = None;
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [];
};
{
  id = Terraform;
  id_string = "terraform";
  name = "Terraform";
  keys = [{|tf|}; {|hcl|}; {|terraform|}];
  exts = [{|.tf|}; {|.hcl|}; {|.tfvars|}];
  maturity = Ga;
  example_ext = None;
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [];
};
{
  id = Ts;
  id_string = "ts";
  name = "TypeScript";
  keys = [{|ts|}; {|typescript|}];
  exts = [{|.ts|}; {|.tsx|}];
  maturity = Ga;
  example_ext = Some {|.tsx|};
  excluded_exts = [{|.d.ts|}];
  reverse_exts = None;
  shebangs = [{|ts-node|}];
  tags = [{|is_js|}];
};
{
  id = Vue;
  id_string = "vue";
  name = "Vue";
  keys = [{|vue|}];
  exts = [{|.vue|}];
  maturity = Develop;
  example_ext = None;
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [];
};
{
  id = Xml;
  id_string = "xml";
  name = "XML";
  keys = [{|xml|}];
  exts = [{|.xml|}; {|.plist|}];
  maturity = Alpha;
  example_ext = Some {|.xml|};
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [];
};
{
  id = Yaml;
  id_string = "yaml";
  name = "YAML";
  keys = [{|yaml|}];
  exts = [{|.yml|}; {|.yaml|}];
  maturity = Alpha;
  example_ext = Some {|.yaml|};
  excluded_exts = [];
  reverse_exts = None;
  shebangs = [];
  tags = [];
};
]
