class Test {
@Override
    public void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        // MATCH: 
        FileInputStream fis = new java.FileInputStream(fileName);

        // MATCH:
        FileInputStream fis = new java.io.FileInputStream(fileName);

        // MATCH: 
        FileOutputStream fos = new java.io.FileOutputStream(new java.io.FileInputStream(fileName).getFD());
    }
}
