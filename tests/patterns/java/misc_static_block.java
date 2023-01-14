public class TestClass {
    //ERROR: match
    static {
        System.out.printf("Static initial %s\n", TestClass.class);
    }

    {
        System.out.printf("Empty block initial %s\n", this.getClass());
    }

    public TestClass() {
        System.out.printf("Initial %s\n", this.getClass());
    }
}
