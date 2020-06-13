
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

However, there are also function APIs that are insecure by default--or insecure depending on context, such as Jinja2, which does [not enable autoescaping by default](https://github.com/pallets/jinja/blob/2a8515d2e53a2be475d9df3fe44e308501201a95/src/jinja2/environment.py#L296). Jinja2 is an arbitrary templating engine, so this makes sense in non-web contexts. This may not be obvious, and if you are working directly with the Jinja2 engine in a web context you want to make sure `autoescape=True`.

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

Another example of this approach is [setting secure cookies](https://semgrep.live/EwB5).



## Ensure One Function is Called Before Another

1. Match function call by name.
2. Filter out when the good function is above.

### Java Cookie

1. Match addCookie(...).
2. Filter out when setSecure(...) is called.

## Find All Routes in an Application

1. Specify the route pattern (annotations).
2. Use metavariables to match specific components.