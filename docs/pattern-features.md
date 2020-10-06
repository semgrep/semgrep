# Pattern Features

This document describes `semgrep` pattern features and provides pattern
examples. Patterns are specified with the `--pattern` (or `-e`) flag. Multiple
coordinating patterns may be specified in a configuration file. See
[configuration files](/docs/configuration-files.md) for more information.

Contents:

* [Pattern Features](#pattern-features)
  * [Expression Matching](#expression-matching)
  * [Metavariables](#metavariables)
  * [Ellipsis Operator](#ellipsis-operator)
    * [Function Calls](#function-calls)
    * [Method Calls](#method-calls)
    * [Function Definitions](#function-definitions)
    * [Class Definitions](#class-definitions)
    * [Strings](#strings)
    * [Arrays](#arrays)
    * [Conditionals and Loops](#conditionals-and-loops)
  * [Deep Expression Operator](#deep-expression-operator)
  * [Equivalences](#equivalences)
    * [Imports](#imports)
    * [Constants](#constants)
  * [Typed Metavariables](#typed-metavariables)
* [Limitations](#limitations)
  * [Statement Types](#statement-types)
  * [Partial Statements](#partial-statements)

## Pattern Features

### Expression Matching

Expression matching searches code for the given pattern. This pattern can match
a full expression, or be part of a subexpression:

```text
pattern: 1 + func(42)
```

```python
foo(1 + func(42)) + bar()
```

### Metavariables

**Metavariables** are an abstraction that Semgrep provides when you want to
match something but you don't know exactly what it is ahead of time. You can
think of *metavariables* like a [capture
group](https://regexone.com/lesson/capturing_groups) in regular expressions.

Metavariables are used to track a value across a specific code scope. This
includes variables, functions, arguments, classes, object methods, imports,
exceptions, and more.

Metavariable names look like `$X`, `$WIDGET`, or `$USERS_2`. They can only
contain uppercase characters, or `_`, or digits, and must start with an
uppercase character or `_`. Names like `$x` or `$some_value` are invalid.

This pattern will match the following pieces of code:

```text
pattern: $X + $Y
```

```python
foo() + bar()
```

```python
current + total
```

Patterns can also be used to match imports:

```text
pattern: import $X
```

```python
import random
```

Re-using metavariables shows their true power. We can re-use a metavariable
to detect useless assignments:

```text
pattern: |
  $X = $Y
  $X = $Z
```

*Note the YAML `|` operator allows for [multiline strings](https://yaml-multiline.info/).*

```python
initial_value = 10  # Oops, useless assignment
initial_value = get_initial_value()
```

### Ellipsis Operator

The **ellipsis operator** (`...`) abstracts away sequences of zero or more
arguments, statements, characters, and more.

In other words, it allows you to gloss over the details so you can focus on
what's important.

#### Function Calls

The ellipsis operator can be used to search for specific function calls or
function calls with specific arguments. To search for all calls to a specific
function, regardless of its arguments:

```text
pattern: insecure_function(...)
```

Note that functions or classes must be referenced by their full path. E.g.

* `django.utils.safestring.mark_safe(...)` not `mark_safe(...)`
* `System.out.println(...)` not `println(...)`

We can also search for calls with arguments after a match:

```text
pattern: func(1, ...)
```

```python
func(1, "extra stuff", False)
func(1)  # Matches no arguments as well
```

Or calls with arguments before a match:

```text
pattern: func(..., 1)
```

```python
func("extra stuff", False, 1)
func(1)  # Matches no arguments as well
```

Or calls where an argument appears anywhere:

```text
pattern: requests.get(..., verify=False, ...)
```

```python
requests.get(verify=False, url=URL)
requests.get(URL, verify=False, timeout=3)
requests.get(URL, verify=False)
```

The keyword argument value can also be matched:

```text
pattern: $FUNC(..., $KEY=$VALUE, ...)
```

#### Method Calls

The ellipsis operator can be used to search for method calls on a specific
object type:

```text
pattern: $OBJECT.extractall(...)
```

```python
tarball.extractall('/path/to/directory')  # Oops, potential arbitrary file overwrite
```

#### Function Definitions

The ellipsis operator can be used in function argument lists or in the function
body. To find function definitions with [mutable default arguments](https://docs.python-guide.org/writing/gotchas/#mutable-default-arguments):

```text
pattern: |
  def $FUNC(..., $ARG={}, ...):
      ...
```

*Note the YAML `|` operator allows for [multiline strings](https://yaml-multiline.info/).*

```python
def parse_data(parser, data={}):  # Oops, mutable default arguments
    pass
```

#### Class Definitions

The ellipsis operator can be used in class definitions. To find classes that
inherit from a certain parent:

```text
pattern: |
  class $CLASS(InsecureBaseClass):
      ...
```

*Note the YAML `|` operator allows for [multiline strings](https://yaml-multiline.info/).*

```python
class DataRetriever(InsecureBaseClass):
    def __init__(self):
        pass
```

#### Strings

The ellipsis operator can be used to search for strings containing any data:

```text
pattern: crypto.set_secret_key("...")
```

```python
crypto.set_secret_key("HARDCODED SECRET")
```

[OCaml regular expressions](https://caml.inria.fr/pub/docs/manual-ocaml/libref/Str.html)
can also be used to search for specific strings:

```text
pattern: requests.get("=~/.*dev\.corp\.com.*/")
```

```python
requests.get("api.dev.corp.com")  # Oops, development API left in
```

#### Binary operations

The ellipsis operator can be used to match any number of arguments to
binary operations.

```text
pattern: $X = 1 + 2 + ...
```

```python
foo = 1 + 2 + 3 + 4
```

#### Arrays

The ellipsis operator can be used to match literal arrays:

```text
pattern: user_list = [..., 10]
```

```python
user_list = [8, 9, 10]
```

#### Conditionals and Loops

The ellipsis operator can be used inside conditionals or loops:

```text
pattern: |
  if $CONDITION:
      ...
```

*Note the YAML `|` operator allows for [multiline strings](https://yaml-multiline.info/).*

```python
if can_make_request:
    check_status()
    make_request()
    return
```

A metavariable can also be used to match a conditional or loop body if the
body statement information will be re-used later:

```text
pattern: |
  if $CONDITION:
      $BODY
```

```python
if can_make_request:
    single_request_statement()
```

**Note you can’t match a half statement; both of the examples above must
specify the contents of the condition's body (e.g. `$BODY` or `...`),
otherwise they are not valid patterns.**

### Deep Expression Operator

You may want to match an expression that could be nested deep within another
expression. An example of this is looking for a pattern anywhere within an if
statement. To do this, use the deep expression operator:
`<... [your_pattern] ...>`. This will match your pattern in the current
expression context and recursively in any subexpressions.

For example, this pattern:

```yaml
pattern: |
  if <... $USER.is_admin() ...>:
    ...
```

will match:

```python
if user.authenticated() and user.is_admin() and user.has_group(gid):
  ...
```

The deep expression operator will work in:
* if statements -- `if <... $X ...>:`
* nested calls -- `sql.query(<... $X ...>)`
* operands of a binary expression -- `"..." + <... $X ...>`
* and any other expression context.

### Equivalences

Equivalences are another key Semgrep concept. Semgrep automatically searches
for code that is semantically equivalent.

#### Imports

Equivalent imports using aliasing or submodules will be matched.

```text
pattern: subprocess.Popen(...)
```

```python
import subprocess.Popen as sub_popen
sub_popen('ls')
```

```text
pattern: foo.bar.baz.qux(...)
```

```python
from foo.bar import baz
baz.qux()
```

#### Constants

Languages supporting constants allow for constant propagation. In other words,
the constant's value is considered equivalent to a literal value. The following
patterns will catch each respective code snippet:

**Javascript:**

```text
pattern: crypto.subtle.digest("SHA-1", ...);
```

```javascript
crypto.subtle.digest("SHA-1", "text");

const LITERAL = "SHA-1";
crypto.subtle.digest(LITERAL, "text");

const LIT = "SHA";
crypto.subtle.digest(LIT + "-1", "text");

const LIT = "SHA";
crypto.subtle.digest(`${LIT}-1`, "text");
```

**Go:**

```text
pattern: crypto.hash("MD5", ...)
```

```go
crypto.hash("MD5", "text")

const algorithm = "MD5"
crypto.hash(algorithm, "text")
```

**Python:**

```text
pattern: hashlib.new("MD5", ...).digest()
```

```python
import hashlib

ALGORITHM = "MD5"

def get_digest(data):
    return hashlib.new(ALGORITHM, data=data).digest()
```

**Java:**

```text
pattern: $MD.getInstance("MD5");
```

```java
import java.security.MessageDigest;

class Hash {

    public final String algorithm = "MD5";

    public static void main(String[] args) {
        MessageDigest md = MessageDigest.getInstance(algorithm);
    }
}
```

### Typed Metavariables

Typed metavariables only match a metavariable if it is declared as a specific type. For
example, you may want to specifically check that `==` is never used for
strings.

**Java:**

```text
pattern: $X == (String $Y)
```

```java
public class Example {
    public int foo(String a, int b) {
        // Matched
        if (a == "hello") {
            return 1;
        }

        // Not matched
        if (b == 2) {
            return -1;
        }
    }
}
```

**Go:**

```text
pattern: "$X == ($Y : string)"
```

```go
func main() {
    var x string
    var y string
    var a int
    var b int

    // Matched
    if x == y {
       x = y
    }

    // Not matched
    if a == b {
       a = b
    }
}
```

#### Limitations:

Currently, since matching happens within a single file, this is only guaranteed
to work for local variables and arguments. Additionally, it only understands
types on a shallow level. For example, if you have `int[] A`, it will not
recognize `A[0]` as an integer. If you have a class with fields, you will not be
able to use typechecking on field accesses, and it will not recognize the
class' field as the expected type. Literal types are understood to a limited
extent.

Semgrep for Go currently does not recognize the type of all variables when declared on the
same line. That is, the following will not take both `a` and `b` as `int`s:

```go
var a, b = 1, 2
```

## Limitations

### Statements Types

`semgrep` handles statements different than other expressions like imports. For
example, the following pattern will match these statements:

```text
pattern: foo
```

```python
foo()
bar + foo
foo(1, 2)
```

It will not match the following:

```python
import foo
```

### Partial Statements

Partial statements are not valid patterns. For example, the following are
invalid:

```text
pattern: 1+
```

```text
pattern: if $CONDITION:
```
