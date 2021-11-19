// https://github.com/returntocorp/semgrep/issues/4265

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
