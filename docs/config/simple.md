# Simple

This document describes `semgrep` pattern features and provides pattern examples.

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
  * [Equivalences](#equivalences)
    * [Imports](#imports)
    * [Constants](#constants)
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

Metavariables are used to track a value across a specific code scope. This
includes variables, functions, arguments, classes, object methods, imports,
exceptions, and more.

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

The ellipsis operator (`...`) abstracts away sequences. In other words, it
allows you to gloss over the details so you can focus on what's important.

#### Function Calls

The ellipsis operator can be used to search for specific function calls or
function calls with specific arguments. To search for all calls to a specific
function:

```text
pattern: insecure_function(...)
```

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

### Equivalences

`semgrep` will handle certain types of equivalent code.

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

JavaScript code will match propagated constants:

```text
pattern: api("literal");
```

```javascript
api("literal");

const LITERAL = "literal";
api(LITERAL);

const LIT = "lit";
api(LIT + "eral");

const LIT = "lit";
api(`${LIT}eral`);
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
foo(1,2)
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
