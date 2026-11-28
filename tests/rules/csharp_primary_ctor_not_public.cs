// Primary-constructor parameters of a class or struct are private captures,
// not public fields.
public class Container(string data) { }

public struct Point(int x, int y) { }

public class Real {
    // ruleid: public-field
    public string name;
}

// A positional record parameter is a public property, not a field
// declaration in the source: no synthetic field to match either.
public record Person(string Name);
