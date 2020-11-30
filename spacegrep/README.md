# spacegrep

This is an attempt at providing a grep-like tool for finding patterns
in human-readable code written in any language. It has the following
properties:

* A document is interpreted as a nested sequence of ascii words,
  ascii punctuation, and other bytes.
* `...` allows skipping non-matching elements, up to 10 lines down the
  last match.
* `$X` matches any word.
* The interpretation of a document can be inspected with the
  `spacecat` command.
* Indentation determines primary nesting in the document.
* Common ascii braces `()`, `[]`, and `{}` introduce secondary nesting but
  only within single lines. Therefore, misinterpreted or mismatched
  braces don't disturb the structure of the rest of document.
* The document must be at least as indented as the pattern:
  any indentation specified in the pattern must be honored in the document.

Sample pattern `exec.pat`:
```
exec(...)
```

Sample document `exec.doc`:
```
import exec as safe_function
safe_function(user_input)

exec("ls")

exec(some_var)

some_exec(foo)

exec (foo)

exec (
    bar
)

# exec(foo)

print("exec(bar)")
```

Output:
```
$ spacegrep -p exec.pat -d exec.doc
exec("ls")
exec(some_var)
exec (foo)
exec (
    bar
)
# exec(foo)
print("exec(bar)")
```

### Install

```
make build  # builds the spacegrep binary
make install # puts spacegrep on your $PATH
```
