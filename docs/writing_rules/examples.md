
## Auditing Dangerous Function Use

Using Semgrep to audit dangerous function calls is easy.

1. Match a function call by name.
2. Filter out hardcoded strings.
3. Look explicitly for dangerous keyword arguments.

Let's do an example with the `subprocess` module in Python.

**Match the function call by name.** The ellipsis operator `...` abstracts away whole segments of code. Effectively, it says "I don't care about what's in here."

```yaml
patterns:
- pattern: subprocess.call(...)
```

```python
import subprocess
import sys

subprocess.call("echo 'hello'") # Matches here
subprocess.call("grep -R {} .".format(sys.argv[1])) # Matches here
subprocess.call("grep -R {} .".format(sys.argv[1]), shell=True) # Matches here
subprocess.call("grep -R {} .".format(sys.argv[1]), shell=True, cwd="/home/user") # Matches here
subprocess.run("grep -R {} .".format(sys.argv[1]), shell=True) # Doesn't match here
```

https://semgrep.live/eq5Z

**Filter out hardcoded strings.** The ellipsis operator can be used inside quotes to represent any string literal. We can filter out static strings by using this with the `pattern-not` clause.

```yaml
patterns:
- pattern-not: subprocess.call("...")
- pattern: subprocess.call(...)
```

```python
import subprocess
import sys

subprocess.call("echo 'hello'") # Doesn't match here anymore!
subprocess.call("grep -R {} .".format(sys.argv[1])) # Matches here
subprocess.call("grep -R {} .".format(sys.argv[1]), shell=True) # Matches here
subprocess.call("grep -R {} .".format(sys.argv[1]), shell=True, cwd="/home/user") # Matches here
subprocess.run("grep -R {} .".format(sys.argv[1]), shell=True) # Doesn't match here
```

https://semgrep.live/v8X8

**Look explicitly for dangerous keyword arguments.** You may want to match only when [certain keyword arguments are present](https://docs.python.org/3/library/subprocess.html#security-considerations). We can write keyword arguments just like in Python into our pattern. Combined with the ellipsis operator, this pattern will match if `shell=True` appears at the end of the sequence of arguments.

```yaml
patterns:
- pattern-not: subprocess.call("...")
- pattern: subprocess.call(..., shell=True)
```

```python
import subprocess
import sys

subprocess.call("echo 'hello'") # Doesn't match
subprocess.call("grep -R {} .".format(sys.argv[1])) # Doesn't match here anymore!
subprocess.call("grep -R {} .".format(sys.argv[1]), shell=True) # Matches here
subprocess.call("grep -R {} .".format(sys.argv[1]), shell=True, cwd="/home/user") # Oops! We don't match here anymore either!
subprocess.run("grep -R {} .".format(sys.argv[1]), shell=True) # Doesn't match here
```

https://semgrep.live/d8J6

Semgrep will match `(..., shell=True)` only when `shell=True` is the last argument. To fix this, we can use the ellipsis operator on both sides of `shell=True`.

```yaml
patterns:
- pattern-not: subprocess.call("...")
- pattern: subprocess.call(..., shell=True, ...)
```

```python
import subprocess
import sys

subprocess.call("echo 'hello'") # Doesn't match
subprocess.call("grep -R {} .".format(sys.argv[1])) # Doesn't match
subprocess.call("grep -R {} .".format(sys.argv[1]), shell=True) # Matches here
subprocess.call("grep -R {} .".format(sys.argv[1]), shell=True, cwd="/home/user") # Matches here too!
subprocess.run("grep -R {} .".format(sys.argv[1]), shell=True) # Doesn't match here
```

https://semgrep.live/ZqKW

**Bonus: Match any `subprocess` function with `shell=True`.** As you probably noticed, `subprocess.run` is subject to the same issue as `subprocess.call`. `subprocess.run` [was made available in Python 3.5](https://docs.python.org/3/library/subprocess.html#older-high-level-api). We can match both `subprocess.call` and `subprocess.run` by using **metavariables**. Metavariables let you match any code expression. To use metavariables in Semgrep, use the dollar sign as a prefix and all capital letters. In this example, we will use `subprocess.$FUNC`. The name can be anything -- it's just a like a variable in a normal language and will "hold" the expression it matches.

```yaml
patterns:
- pattern-not: subprocess.$FUNC("...")
- pattern: subprocess.$FUNC(..., shell=True, ...)
```

```python
import subprocess
import sys

subprocess.call("echo 'hello'") # Doesn't match
subprocess.call("grep -R {} .".format(sys.argv[1])) # Doesn't match
subprocess.call("grep -R {} .".format(sys.argv[1]), shell=True) # Matches here
subprocess.call("grep -R {} .".format(sys.argv[1]), shell=True, cwd="/home/user") # Matches here
subprocess.run("grep -R {} .".format(sys.argv[1]), shell=True) # Matches here too!
```

https://semgrep.live/nJ9d

## Enforce Specific Use of an API

Sometimes you may wish to enforce the specific use of a function's API. There are many examples of this, such as `subprocess.call(..., shell=True, ...)` above; you may wish to match and fail any commit where `shell=True`. This is easy to do in Semgrep, as seen in the above section.

However, there are also function APIs that are insecure by default--or insecure depending on context, such as Jinja2, which does [not enable autoescaping by default](https://github.com/pallets/jinja/blob/2a8515d2e53a2be475d9df3fe44e308501201a95/src/jinja2/environment.py#L296). Jinja2 is an arbitrary templating engine, so this makes sense in non-web contexts. This may not be obvious, though, and if you are working directly with the Jinja2 engine in a web context you want to make sure `autoescape=True`.

(Not to scare anyone: Flask, for instance, [autoescapes templates with the '.html' extension](https://github.com/pallets/jinja/blob/2a8515d2e53a2be475d9df3fe44e308501201a95/src/jinja2/environment.py#L296).)

This is an interesting case because we want to enforce the **presence** of `autoescape=True`. *Matching* this is easy:

```yaml
patterns:
- pattern: jinja2.Environment(..., autoescape=True, ...)
```

But what we *want* is to alert when the *opposite* conditions are met. Therefore we want to match (1) when `autoescape=False` **and** (2) when `autoescape` is not present in the function call at all! This pattern will match when `jinja2.Environment()` does not contain `autoescape=True`:

```yaml
patterns:
- pattern-not: jinja2.Environment(..., autoescape=True, ...)
- pattern: jinja2.Environment(...)
```

TODO: semgrep.live link

This can be generalized with the following approach:

1. Match the function call by name.
2. Filter out good patterns.

### Secure Cookies in Flask

Another example of this approach is [setting secure cookies in Flask](https://semgrep.live/EwB5).

```yaml
patterns:
- pattern-not: flask.response.set_cookie(..., httponly=True, secure=True,...)
- pattern: flask.response.set_cookie(...)
```

## Ensure One Function is Called Before Another

You can ensure one function is called before another in Semgrep by utilizing the `pattern-not-inside` clause. The approach will be:

1. Match the last function call by name.
2. Filter out when the right function is called above.

**Match the last function call by name.** Let's use [this Java example from semgrep.live](https://semgrep.live/7K9G). We want to make sure `verify_transaction` is called before `make_transaction`. First, match the function call that should be called last. In this case, it's `make_transaction`. (Yes, they're normally called "methods." Stick with me.)

```yaml
patterns:
- pattern: make_transaction(...);
```

https://semgrep.live/QrbJ

**Filter out when the right function is called above.** Next, we can filter out the case where `verify_transaction` appears above. To do this, we will use the `pattern-not-inside` clause. `pattern-not-inside` will filter out **ranges**, inclusive of the ellpisis operator. The patterns look like this:

```yaml
patterns:
- pattern-not-inside: |
    verify_transaction(...);
    ...
- pattern: make_transaction(...);
```

(The pipe (`|`) is YAML syntax that permits a multi-line string.)

If I were to describe this pattern in English, it would read: Filter out any matches inside statements after `verify_transaction`, otherwise match `make_transaction`. Written another way: match `make_transaction` only when `verify_transaction` is not above.

https://semgrep.live/8Gzg

**Bonus: Ensure the same variable is used in both functions.** The above patterns has three matches in the given examples. There is a fourth case to match where the *wrong* `Transaction` object is verified. To match only when the same variable is used in both functions, we can use a metavariable. Just like variables, a Semgrep pattern will match only when the metavariables are the same wherever it is used. We can augment the pattern like this to catch the fourth case:

```yaml
patterns:
- pattern-not-inside: |
    verify_transaction($TRANSACTION);
    ...
- pattern: make_transaction($TRANSACTION);
```

https://semgrep.live/gxRR

### Secure Cookies in Java

A real example of this is setting the secure flag on cookies in Java. `Cookie` objects are added to `HttpServletResponse` objects, and to set the secure flag, `setSecure(true)` must be called on the `Cookie` object prior to its addition.

```java
Cookie cookie = new Cookie("key", "value");
cookie.setSecure(true);
response.addCookie(cookie);
```

Matching this is the same approach as `make_transaction` above.

1. Match `response.addCookie(...)`.
2. Filter out when `setSecure(true)` is called.

Since we can't know the name of the variables in advance, we can use metavariables for the `Cookie` and `HttpServletResponse` objects. The patterns look like this:

```yaml
patterns:
- pattern-not-inside: |
    $COOKIE.setSecure(true);
    ...
- pattern: $RESP.addCookie($COOKIE);
```

https://semgrep.live/L1gX

## Find All Routes in an Application

1. Specify the route pattern (annotations).
2. Use metavariables to match specific components.