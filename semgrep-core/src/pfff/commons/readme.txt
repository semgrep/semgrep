This directory builds a common.cma library and also optionally
multiple commons_xxx.cma small libraries. The reason not to just build
a single one is that some functionnalities require external libraries
(like Berkeley DB, MPI, etc) or special version of OCaml (like for the
backtrace support) and I don't want to penalize the user by forcing
him to install all those libs before being able to use some of my
common helper functions. So, common.ml and other files offer
convenient helpers that do not require to install anything. In some
cases I have directly included the code of those external libs when
there are simple such as for ANSITerminal in ocamlextra/, and for
dumper.ml I have even be further by inlining its code in common.ml so
one can just do a open Common and have everything. Then if the user
wants to, he can also leverage the other commons_xxx libraries by
explicitely building them after he has installed the necessary
external files.

For many configurable things we can use some flags in ml files,
and have some -xxx command line argument to set them or not,
but for other things flags are not enough as they will not remove
the header and linker dependencies in Makefiles. A solution is
to use cpp and pre-process many files that have such configuration
issue. Another solution is to centralize all the cpp issue in one
file, features.ml.in, that acts as a generic wrapper for other
librairies and depending on the configuration actually call
the external library or provide a fake empty services indicating
that the service is not present.
So you should have a ../configure that call cpp on features.ml.in
to set those linking-related configuration settings.
