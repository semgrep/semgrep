using System;
using System.Threading.Tasks;

class HelloWorldAsync
{
    public static string GetGreeting()
    {
        return "hello world";
    }
    
    public static async Task<string> GetGreetingAsync()
    {
        return "hello world";
    }
    
    public static async Task Main()
    {
        var greeting = await Task.Run(() => GetGreeting());
        Console.WriteLine(greeting);
        Console.WriteLine(await GetGreetingAsync());
        var greeting2 = await GetGreetingAsync();
    }
}