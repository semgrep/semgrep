// Test Scala's qualified super syntax

// 1. Qualified super with mixin type: super[Type].member
trait Left {
    def getData(): String = "left"
}

trait Right {
    def getData(): String = "right"
}

class Diamond extends Left with Right {
    override def getData(): String = {
        // Qualified super disambiguates which trait's implementation to call
        super[Left].getData()
    }

    def getOther(): String = {
        super[Right].getData()
    }
}

// 2. Prefixed super without mixin qualifier: Outer.super.member
class Parent {
    def getValue(): Int = 10
}

class Child extends Parent {
    override def getValue(): Int = 20

    class Inner {
        def getParentValue(): Int = {
            // Access outer class's parent implementation
            Child.super.getValue()
        }
    }
}

// 3. combine both of the above: Outer.super[Type].member
class Outer extends Left with Right {
    // Disambiguate the conflicting `getData` implementations from Left/Right.
    override def getData(): String = super[Left].getData()

    class Inner {
        def getOuterLeft(): String = {
            // Access outer class's Left trait implementation
            Outer.super[Left].getData()
        }

        def getOuterRight(): String = {
            Outer.super[Right].getData()
        }
    }
}