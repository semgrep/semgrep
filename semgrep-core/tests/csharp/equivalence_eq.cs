public class Test
{
    public static void Test()
    {
        int a = 1;
        int b = 2;

        //ERROR:
        if (a + b == a + b) {
            return 1;
        }

        //ERROR:
        if (a + b != a + b) {
            return 1;
        }

        return 0;
    }
}
