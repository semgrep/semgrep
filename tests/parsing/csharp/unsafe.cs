using System;

class Unsafe
{
    private static unsafe void addOne(int* i)
    {
        *i = *i + 1;
    }
    
    public static void Main()
    {
        unsafe
        {
            int i = 3;
            addOne(&i);
            Console.WriteLine(i);
        }
    }
}
