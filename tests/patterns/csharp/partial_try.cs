// MATCH:
try 
{
    int result = 10 / 0; // division by zero
}
catch (Exception ex) 
{
    Console.WriteLine("handled"); // code to handle the exception
}