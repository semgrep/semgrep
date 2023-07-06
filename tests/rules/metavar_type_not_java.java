public class BenchmarkTest02388 extends HttpServlet {
    public void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        PrintWriter pWriter = response.getWriter();
        // ok: no-direct-response-writer
        pWriter.println(
                "Hash Test java.security.MessageDigest.getInstance(java.lang.String) executed");
        // ruleid: no-direct-response-writer
        pWriter.println(request.input);

        SafeWriter sWriter = response.getSafeWriter();
        // ok: no-direct-response-writer
        sWriter.println(request.input);
    }
}
