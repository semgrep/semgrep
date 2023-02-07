Parsy is vendored in order to add incremental line and column tracking functionality, which speeds up our lockfile parsers signficantly.

The edits replace the integer index into a stream with a triple of integers representing offset, line and column information.

offset is what index originally was: how many individual elements of the stream have been consumed.

When parser input is a string line and column are updated incrementally with each character of the input string.

When parser input is not a string (which can never happen with our type stubs) line and column are both set to -1 and ignored.

Matthew is planning to merge this change upstream into parsy, but it's currently backwards incompatible, so we're vendoring until
that's solved, the change is merged, and parsy is released.
