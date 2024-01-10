def foo(a, b):
    return foo(bar(1)) # nosemgrep: rules.match-foo, rules.match-bar