Joust: a Java lexer, parser, and pretty-printer written in OCaml
Copyright (C) 2001  Eric C. Cooper <ecc@cmu.edu>
Copyright (C) 2022  Eric C. Cooper <ecc@cmu.edu>

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public License
version 2.1 as published by the Free Software Foundation.

This library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
license.txt for more details.

----
Some modifications done by Yoann Padioleau.

See https://github.com/ecc1/joust for the original code.

----

Pretty-printing options:
  pretty.ml should be symbolically linked to one of the following:
    pretty_canon.ml
      prints expressions in canonical form (fully parenthesized)
    pretty_bare.ml
      prints expressions in bare form with fewer parentheses
    pretty_min.ml (recommended)
      uses operator precedence to print expressions with minimal parentheses
  *** WARNING: always do "make clean" after changing the symlink ***

Java source code used for testing:
  Java 2 SDK <http://www.sun.com/software/communitysource/java2/index.html>
  Java 2 documentation examples <http://java.sun.com/j2se/1.3/docs.html>
  NetBeans <http://www.netbeans.org/downloads.html>

  find /usr/local/... -name '*.java' > java-files

Regression tests:
  xargs ./fixpoint.sh < java-files >& test.out
  diff java-files test.out

  xargs ./precedence.sh < java-files >& test.out
  diff java-files test.out
