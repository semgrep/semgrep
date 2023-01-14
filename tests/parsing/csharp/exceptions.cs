using System;
using System.Data.SqlTypes;

class HelloWorldExceptions
{
    public static void Main()
    {
        try
        {
            int.Parse("hello");
        }
        catch (SqlNullValueException)
        {
            Console.WriteLine("sql");
        }
        catch (FormatException e) when (e.ToString().Contains("world"))
        {
            Console.WriteLine("world");
        }
        catch (FormatException e) when (e.ToString().Contains("Input"))
        {
            Console.WriteLine("hello");
        }
        catch
        {
            Console.WriteLine("catch");
        }
        finally
        {
            Console.WriteLine("world");
        }
    }
}