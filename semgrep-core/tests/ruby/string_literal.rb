def foo

    # ERROR: match
    "hello"

    "hello #{name}"

    # ERROR: match
    "hello" + name

    # ERROR: match
    foo = "str"

    # ERROR: match
    "hello #{foo}"

    name

end
