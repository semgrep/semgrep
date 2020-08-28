Joust: a Java lexer, parser, and pretty-printer written in OCaml
Copyright (C) 2001  Eric C. Cooper <ecc@cmu.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

On Debian GNU/Linux systems, the complete text of the GNU General
Public License can be found in /usr/share/common-licenses/GPL.

----
Some modifications done by Yoann Padioleau.


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
