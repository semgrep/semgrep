class Test {
@Override
    public void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        FileInputStream fis = new java.FileInputStream(fileName);

        FileInputStream fis = new java.io.FileInputStream(fileName);

        FileOutputStream fos = new java.io.FileOutputStream(new java.io.FileInputStream(fileName).getFD());
    }
}
