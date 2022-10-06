class Foo {
    public void func() {
        String[] strSplit = request.getParameter("h").split(":");
        ArrayList<String> students = new ArrayList<String>(Arrays.asList(strSplit));
        //ruleid: test
        students.forEach((n) -> Runtime.getRuntime().exec(n));
    }
}
