# Changelog

This project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [0.4.9]() - 2019-10-23

### Changed

- Only print out number of configs and rules when running with verbose flag
- Match let and const to var in javascript:
    ```
    var $F = "hello"
    ```
    will now match any of the following expressions:
    ```javascript
    var foo = "hello";
    let bar = "hello";
    const baz = "hello";
    ```

### Added

- Print out --dump-ast
- Print out version with `--version`
- Allow ... in arrays
    ```
    [..., 1]
    ```
    will now match
    ```
    [3, 2, 1]
    ```
- Support Metavariable match on keyword arguments in python:
    ```
    foo(..., $K=$B, ...)
    ```
    will now match
    ```
    foo(1, 2, bar=baz, 3)
    ```
- Support python fstring variable substitution
- Constant propogation in javascript:
    ```
    api("literal");
    ```
    will now match with any of the following:
    ```javascript
    api("literal");

    const LITERAL = "literal";
    api(LITERAL);

    const LIT = "lit";
    api(LIT + "eral");

    const LIT = "lit";
    api(`${LIT}eral`);
    ```

- Deep statement matching
- Unified import resolution in python:
    ```
    import foo.bar.baz
    ```

    will not match any of the following statements:

    ```python
    import foo.bar.baz
    import foo.bar.baz.qux
    import foo.bar.baz as flob
    import foo.bar.baz.qux as flob
    from foo.bar import baz
    from foo.bar.baz import qux
    from foo.bar import baz as flob
    from foo.bar.bax import qux as flob
    ```
- Support lambda expressions in javascript #288
- Support arrow function in javascript
    ```
    (a) => { ... }
    ```
    will now match:

    ```javascript
    foo( (a) => { console.log("foo"); });
    foo( a => console.log("foo"));

    // arrows are normalized in regular Lambda, so an arrow pattern
    // will match also old-style anynonous function.
    foo(function (a) { console.log("foo"); });
    ```
- Python implicit string concatenation
    ```
    $X = "..."
    ```
    will now match
    ```python
    # python implicitly concatenates strings
    foo = "bar"       "baz"              "qux"
    ```
- Resolve alias in attributes and decorators in python
    ```
    @foo.bar.baz
    def $X(...):
        ...
    ```
    will now match
    ```python
    from foo.bar import baz

    @baz
    def qux():
        print("hello")
    ```
### Fixed

- Handle misordered multiple object destructuring assignments in javascript:
    ```
    var {foo, bar} = qux;
    ```
    will now match
    ```
    var {bar, baz, foo} = qux;
    ```
- Defining properties/functions in different order:
    ```
    var $F = {
        two: 2,
        one: 1
    };
    ```
    will now match both
    ```javascript
    var foo = {
      two: 2,
      one: 1
    };

    var bar = {
        one: 1,
        two: 2
    };
    ```
- Metavariables were not matching due to go parser adding empty statements in golang


## [0.4.8] - 2020-03-09

### Added
* Constant propagation for some langauges. Golang example:
```
pattern: dangerous1("...")
will match:

const Bar = "password"
func foo() {
     dangerous1(Bar);
}
```

* Import matching equivalences
```
pattern: import foo.bar.a2
matches code: from foo.bar import a1, a2
```

* Deep expression matching - see (#264)
```
pattern: bar();
matches code: print(bar())
```
