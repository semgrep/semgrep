// Test Java's qualified super syntax used in diamond inheritance

// 1. Qualified super method invocation (Type.super.method())
interface Left {
    default String getData() {
        return "left";
    }
}

interface Right {
    default String getData() {
        return "right";
    }
}

class Diamond implements Left, Right {
    @Override
    public String getData() {
        // Qualified super disambiguates which interface's default method to call
        return Left.super.getData();
    }

    public String getOther() {
        return Right.super.getData();
    }
}

// 2. Qualified super field access (Type.super.field)
class Parent {
    protected int value = 10;
}

class Child extends Parent {
    protected int value = 20;

    class Inner {
        int getValue() {
            // Access outer class's parent field
            return Child.super.value;
        }
    }
}

// 3. Qualified super constructor invocation (outer.super())
class Outer {
    class Inner {
    }
}

class ExtendedInner extends Outer.Inner {
    ExtendedInner(Outer outer) {
        // Explicit constructor invocation with qualified super
        outer.super();
    }
}
