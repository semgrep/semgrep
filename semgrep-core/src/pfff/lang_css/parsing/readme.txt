=========================================================================
| README for CCSS							|
|									|
| Dario Teixeira (dario.teixeira@yahoo.com)				|
| March 2010								|
=========================================================================

1. Overview
===========

CCSS is a preprocessor/pretty-printer for CSS (Cascading Style Sheets).
It extends the CSS language with support for declaration of variables and
basic arithmetic operations (addition, subtraction, multiplication, division).
The programme is supposed to be used as a filter: it reads the CSS source
from stdin and outputs its result on stdout.


2. Features
===========

2.1. Variables
==============

With CCSS, you may declare and use variables in your CSS code (in fact, CCSS
variables are actually constants, since they are assigned upon declaration and
may not be changed).  Variable identifiers must begin with an uppercase letter,
and be followed by any combination of letters, numbers, and the characters
'-' (dash) and '_' (underscore).  The value assigned to a variable may be
any CSS expression -- not only quantities -- as the code below demonstrates:

Foo: 20em;
Bar: 1px solid black;

h1	{
	width: Foo;
	border: Bar;
	}


2.2. Arithmetic
===============

CCSS extends CSS expressions with basic arithmetic operations (addition,
subtraction, multiplication, and division).  The operands must be CSS
quantities (either dimensionless or with an attached unit), or other
expressions that ultimately resolve into a quantity.  Moreover, variables
whose value is a quantity (or an expression which resolves into a quantity)
may also be used as operand.

The operators are '+', '-', '*', and '÷'.  Note that multiplication and
division have precedence over addition and subtraction, but you may use
parentheses to group operations.  Consider thus the following input:

Left: 10em;
Right: 5em;
Total: Left + Right;

h1	{
	padding: (1.5em + 0.5em) * 2;
	width: 2 * Total;
	}


CCSS will produce the following output:

h1
	{
	padding: 4em;
	width: 30em;
	}


The reader will have noticed that CCSS must be unit-aware when performing
arithmetic.  As a matter of fact, the programme performs a basic sanity
check of units, and will complain if you try, for example, to add "1em" with
"10px".  By default, CCSS will not make any attempt to convert units even if
they are convertible, such "cm" and "mm".  If you wish for CCSS to attempt
unit conversion, please provide option "--convert" on the command line
(short version "-c").


Units can be grouped into four categories, and conversion is possible if the
units belong to the same category.  Upon conversion, the result will be the
same unit as the first operand.  The categories and corresponding units are
as follows:

   length: mm, cm, in, pt, pc
    angle: deg, grad, rad
     time: ms, s
frequency: hz, khz


As an illustration of unit conversion, the result for all the following
arithmetic operations is the same, "2in":

h1
	{
	foo1: 1in + 1in;
	foo2: 1in + 2.54cm;
	foo3: 1in + 25.4mm;
	foo4: 1in + 72pt;
	foo5: 1in + 6pc;
	}


3. Dependencies
===============

Menhir is used as the parser generator [1], while scanning is done with Ulex [2].
Other requirements are ExtLib [3] and Pcre-ocaml [4].


4. Building and installing
==========================

The build system relies on Ocamlbuild.  Enter 'make' in the 'src' directory
for building the bytecode and native binaries.


5. License
==========

The CCSS library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public License
version 2.1 as published by the Free Software Foundation, with the
special exception on linking described in file LICENSE.


References
==========

[1] http://cristal.inria.fr/~fpottier/menhir/
[2] http://www.cduce.org/download.html
[3] http://code.google.com/p/ocaml-extlib/
[4] http://www.ocaml.info/home/ocaml_sources.html#pcre-ocaml
