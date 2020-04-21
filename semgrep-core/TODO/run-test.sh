#!/bin/sh
# Run the unit test program.

# Make sure it is available.  Use the bytecode version so what I am
# debugging and running are the same thing.
dune build ./_build/default/tests/test.bc || exit

SCRIPT_DIR=../../js-taint/src/scripts
if [ "x$1" = "x-debug" ]; then
    shift
    exec $SCRIPT_DIR/run-ocamldebug.sh ./_build/default/tests/test.bc \
        -verbose "$@"

else
    exec ./_build/default/tests/test.bc -verbose "$@"
fi
