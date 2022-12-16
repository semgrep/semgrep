// note that this currently does not yet parse with tree-sitter-java
// as of Jan 2021, but the pfff parser accepts it.

import static com.foo.FOO$BAR; // $ in import
import com.SomeClass;

class Foo {
    public enum Enumy {
        FINE,
        FOO$BAR // $ in Enum name
    }

    public whatever(String arg) {
        System.out.println(SomeClass.FOO$BAR); // Note $

        switch (arg) {
            case Enum.FOO$BAR: // Note $
            break;
        }

    }
}
