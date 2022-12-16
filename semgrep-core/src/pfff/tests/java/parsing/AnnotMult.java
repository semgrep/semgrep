interface Foo {

     @Test(timeout = 10 * 60 * 1000)
     public void testFailingMessage() throws IOException;
}
