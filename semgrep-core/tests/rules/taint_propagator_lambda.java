class Test {

    public void func1() {
        String nodePred = request.getParameter("h");
        String nodeSucc = nodePred.toString();
        //ruleid: test
        Runtime.getRuntime().exec(nodeSucc);
    }

    public void func2() {
        String nodePred = request.getParameter("h");
        String nodeSucc = fnTest(nodePred);
        //ruleid: test
        Runtime.getRuntime().exec(nodeSucc);
    }

    public void func3() {
        String nodePred = request.getParameter("h");
        NodeSucc nodeSucc = new NodeSucc(nodePred);
        //ruleid: test
        Runtime.getRuntime().exec(nodeSucc);
    }

    public void func4() {
        String[] strSplit = request.getParameter("h").split(":");
        ArrayList<String> students = new ArrayList<String>(Arrays.asList(strSplit));
        //ruleid: test
        students.forEach((n) -> Runtime.getRuntime().exec(n));
    }

}