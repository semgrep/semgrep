def foo(x):
    match x:
        case { 'a': 42 }:
            print("foo")
        case { "a": 42 }:
            print("foo")
        case {'''a''': 42 }:
            print("foo")
        case { r"a": 42 }:
            print("foo")
        case { 1: 42 }:
            print("foo")
        case { 1+2j: 42 }:
            print("foo")