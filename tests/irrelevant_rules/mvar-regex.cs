using System;

public class Test
{
    public static void Main()
    {
        // 'SqlFoobar' is not one of the fields that the rule expects.
        // In this case, `$PATTERN` cannot be affected by constant-folding,
        // so we can use the `metavariable-regex` for pre-filtering.
        x.SqlFooBar = string.Format("foobar");
    }
}
