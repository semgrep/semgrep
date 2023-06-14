#!/bin/sh
# Start ocamldebug with a few conveniences.

# This script is meant to be run from the project root.

if [ "$1" = "" ]; then
    echo "usage: $0 ./_build/default/dir/prog.bc [args...]"
    exit 2
fi

prog="$1"

# smcpeak: The formatting of this 'case' statement is screwy due to
# the pre-commit trigger running 'shfmt', which is evidently buggy.
case "$prog" in
*.bc) ;;

\
    *)
    echo "Program to run ('$prog') must have .bc suffix, i.e., be OCaml bytecode."
    exit 2
    ;;
esac

# Make sure the program is up to date.
dune build "$prog" || exit

# Make sure 'rlwrap' is installed.
if ! which rlwrap >/dev/null 2>&1; then
    echo "To use ocamldebug you really want rlwrap for command recall, etc."
    echo "Install it first with something like: sudo apt install rlwrap"
    exit 2
fi

# Print some hints since ocamldebug is hard to use.
cat <<EOF

ocamldebug hints:
* info modules: List all program modules.  Helpful to get their names.
* set arguments: to set the command-line arguments of the debugged program
* break @ [<module>] <line>: Insert breakpoint at line.
  - NOTE: Always defaults to frame 0 module even after "up"!
  - Sometimes has to be placed on the preceding line to work.
* break [<module>.]<function>: Break at function.
  - Must run program first, e.g.: "r", "break ...", "g 0", "r", "up"... .
  - Useful breakpoint: OUnit.assert_failure (or Alcotest equivalent)
* r[un], s[tep], n[ext]: gdb-like program running.
* bt, frame, up, down: gdb-like call stack navigation.
* g[oto] <n>: Jump forward or backward in time.
* print <var>: Print a variable.
* fin[ish]: Run until current function returns.
* print *: Print the most recent function result (e.g., after finish).
* list [startline [endline]]: Print source code around current location.
* help [<command>]: List commands or info about one.
* quit: Exit (also Ctrl-D).

EOF

exec rlwrap ocamldebug "$@"
