using System;

class HelloWorldEvents
{
    private event EventHandler<string> StringEvent;

    public event EventHandler<string> AccessorEvent {
        add { StringEvent += value; }
        remove { StringEvent -= value; }
    }

    public static void Main()
    {
        var instance = new HelloWorldEvents();
        instance.Run();
    }
    
    public void Run() {
        StringEvent += PrintString;
        StringEvent.Invoke(this, "Hello world");
    }

    private void PrintString(object sender, string e)
    {
        Console.WriteLine(e);
    }
}