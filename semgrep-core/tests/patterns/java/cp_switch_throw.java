// https://github.com/returntocorp/semgrep/issues/4265

// We want to test that the CFG is correctly constructed despite the last
// statement of the `switch` being a `throw`, which interrupts execution.
// The problem was that the `break` in `case A` was not resolved correctly,
// and there was no path connecting `foo = something` to `foo == null`.

public class Test
{
    public bool test()
    {
        Foo foo = null;

        //ERROR:
        assert (foo == null);

        try (Bar b = bar())
        {
            switch (scrut)
            {
                case A:
                    foo = something;
                    break;

                default:
                    throw new Exn();
            }
        }

        //OK:
        return foo == null;
    }
}
