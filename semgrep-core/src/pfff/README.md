# pfff

[![CircleCI](https://circleci.com/gh/returntocorp/pfff.svg?style=svg)](https://circleci.com/gh/returntocorp/pfff)

pfff is a set of tools and APIs to perform static analysis, code
visualizations, code navigations, or style-preserving source-to-source
transformations such as refactorings on source code. There is good
support for Javascript, Python, C, Java, Go, and PHP. There is also preliminary
support for other languages such as C++, Ruby, Rust, C#, Html, CSS, Erlang,
Lisp, Haskell, Skip, and SQL. There is also very good support for
OCaml code so that the framework can be used on the code of pfff
itself.

For each languages there are mainly 2 libraries, for instance
`parsing_php.cma` and `analysis_php.cma`, that you can
embed in your own application if you need to process PHP code. See the
`demos/` directory for example of use of the pfff API. See also
`docs/manual/Parsing_xxx.pdf` and `docs/manual/Analyzis_xxx.pdf` for
more documentation on how to use or extend pfff.

pfff is also made of few tools:
 - `pfff`, which allows to test the different parsers on a single file
 - `stags`, an Emacs tag generator
 - `pfff_db`, which does some global analysis on a set of source files and
   store the data in a marshalled form in a file somewhere (e.g. `/tmp/db.json`)

A few pfff-related tools are now in their own repositories:
 - `sgrep` and `spatch`, a bug-finder and refactoring tool
 - `codemap`, which is a gtk and cairo based source code
   visualizer/navigator/searcher leveraging
   the information computed by `pfff_db` and `codegraph_build`
 - `codegraph_build`, a source code indexer
 - `codegraph`, a package/module/class dependency visualizer leveraging
   the information computed previously by `codegraph_build`
 - `codequery`, an interactive tool a la SQL to query information
   about the structure of a codebase using Prolog as the query engine
 - `scheck`, a bug finder

For more information, look at the pfff wiki here:
 http://github.com/returntocorp/pfff/wiki/Main
as well as the docs/manual/ directory.

## Usage for `pfff`
```sh
$ pfff -parse_php demos/foo.php
```
or
```sh
$ pfff -dump_php demos/foo.php
```

You can also look at `pfff --help`

## Usage for `pfff_db`
```sh
$ ./pfff_db -lang ml -o /tmp/pfff.json ~/pfff
```

to analyze all the `.ml` and `.mli` files under `~/pfff` and store metadata
information (the database) in `/tmp/pfff.json`

## Adding a Language
1. In `commons/file_type.ml`, add your language under the appropriate programming
   language type in
   `type file_type`
   and add in matching to the file extension of your language in
   `file_type_of_file2`
2. In `commons/file_type.mli`, add you language under the appropriate programming
   language type in
   `type file_type`
3. In `lang_GENERIC/parsing/Lang.ml`, add your language and necessary code in
   * `type t`
   * `list_of_lang`
   * `langs_of_filename`
   * `string_of_lang`
   * `ext_of_lang`
4. In `lang_GENERIC/parsing/Lang.mli`, add your language in `type t`.
5. In `lang_GENERIC/parsing/Parse_generic.ml`, add you language
   and necessary code into:
   * `parse_pattern`
   * `parse_with_lang`

## More information

Look at the pfff wiki here: http://github.com/returntocorp/pfff/wiki/Main
