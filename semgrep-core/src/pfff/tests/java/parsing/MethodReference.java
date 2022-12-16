interface Operation {
    Greet printout(String greeting);
}

class Greet {
    public Greet(String greeting){
        System.out.print(greeting);
    }
}

public class Foo {
    public static void main(String[] args) {
        Operation op = Greet::new;
        op.printout("Hello World!");

    }
}

class More {
    public static void main(String[] args) {

      buildConfigRules.forEach(graphBuilder::addToIndex);
    }

}
