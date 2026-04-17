package org.bouncycastle.util;

import java.io.IOException;

public class Exceptions
{
    public static IllegalArgumentException illegalArgumentException(String message, final Throwable cause)
    {
        //ERROR: match
        return new IllegalArgumentException(message)
        {
            public Throwable getCause()
            {
                return cause;
            }
        };
    }

    public static IllegalStateException illegalStateException(String message, final Throwable cause)
    {
        return new IllegalStateException(message)
        {
            public Throwable getCause()
            {
                return cause;
            }
        };
    }

    public static void test()
    {
        //ERROR: match
        throw new IllegalArgumentException("simple");

        //ERROR: match
        throw new IllegalArgumentException("with body")
        {
            public String getMessage()
            {
                return "custom";
            }
        };
    }
}
