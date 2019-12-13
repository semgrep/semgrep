To compile sgrep you first need to install OCaml and its
package manager OPAM. See https://opam.ocaml.org/doc/Install.html

Once OPAM is installed you need to install the dependent
libraries pfff, and the build system dune:

```
$ opam install pfff
$ opam install dune
```

Then you can compile the program with:

```
$ dune build
```

Then to test sgrep on a file, for example tests/GENERIC/test.py
run:

```
$ ./_build/default/bin/main_sgrep.exe -e foo tests/GENERIC/test.py
...
```
